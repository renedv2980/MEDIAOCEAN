*          DATA SET NEWRI39T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEWRI39    AT LEVEL 003 AS OF 01/28/88                      
*PHASE T32039A,+0                                                               
         TITLE 'T32039 - SRATE PAY REPORT'                                      
T32039   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**P5PR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T32039,RB,RA      RA = 2ND BASE REG                              
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R6,ANETWS2                                                       
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         L     R7,ANETWS4        * ANETWS4 = MYWORK AREA                        
         USING MYD,R7                                                           
         LA    R1,HEADING        * ANETWS2 = NDDEMBLK                           
         ST    R1,SPECS                                                         
         LA    R1,HDRTN          * ANETWS3 = CLIST                              
         ST    R1,HEADHOOK                                                      
         SPACE  1                                                               
* SET UP SORTER *                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         B     INIT2                                                            
         SPACE 1                                                                
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=100'                                   
         SPACE 1                                                                
*                                                                               
INIT2    CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BAS   RE,REPMOD                                                        
XIT      XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
REPMOD   NTR1                                                                   
         XC    SUBTIME(48),SUBTIME CLEAR ACCUMULATORS                           
*                                                                               
         MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBSPLOPT,X'C0'      SPLIT EVEN IF POOL                           
         MVI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         NETGO NVDEMOPT,DMCB       ACT SCHED/EST DEMO BUT NOT FOR PFB           
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALCLI                                                  
         BNE   REP1                                                             
         L     R2,NBAIO                                                         
         USING CLTHDR,R2                                                        
         L     RE,CLIST                                                         
         L     RF,ANETWS3                                                       
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         DROP  R2                                                               
REP1     CLI   NBMODE,NBREQLST                                                  
         BE    REP50                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   GETUNIT                                                          
         B     REP2                                                             
         EJECT                                                                  
*                                                                               
REP2     LA    R2,SRTREC           UNIT REC - PREPARE FOR SORTER                
         USING SRTRECD,R2                                                       
         CLI   PRDALL,C'Y'                                                      
         BNE   REP2C                                                            
         CLC   CUSREP,NBSREP                                                    
         BE    *+8                                                              
         BAS   RE,GETREP                                                        
*                                                                               
REP2C    MVC   SRTDATE,NBACTDAT                                                 
         MVC   SRTSUBLN,NBACTSUB                                                
         MVC   SRTNBAIO,NBAIO                                                   
         MVC   SRTNET,NBACTNET                                                  
         MVC   SRTPRGN,NBPROGNM                                                 
         MVC   SRTPROG,NBACTPRG                                                 
         MVC   SRTEST,NBACTEST                                                  
         MVC   SRTPACK,NBACTPAK                                                 
         MVC   SRTPROD(1),NBPRD                                                 
         MVC   SRTPROD+1,NBPRD2                                                 
         MVC   SRTSLEN,NBLEN                                                    
         MVC   SRTDYPT,NBSELDP                                                  
         MVC   SRTTIME,NBACTUAL                                                 
         MVC   SRTINT,NBINTEG                                                   
*-- HANDLE SPECIAL-RATE ELEMENTS                                                
         MVC   AIO,AIO1                                                         
         MVC   KEY+21(4),NBAIO                                                  
         GOTO1 GETREC                                                           
         L     R4,AIO1                                                          
         USING NURECD,R4                                                        
         CLC   NUKEY(17),NBACTAM                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   NUKEY+17(1),NBACTEST                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   NUKEY+18(1),NBACTSUB                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   NUKEY+19(1),NBACTDP                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         L     R4,NBAIO                                                         
         USING NUSPRD,R4                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETUNIT                                                          
*                                                                               
REP5     BAS   RE,NEXTEL                                                        
         BNE   GETUNIT                                                          
*                                                                               
REP8     CLC   NUSPRREP,SRTPAYEE                                                
         BE    REP12                                                            
         BAS   RE,PUTSRT                                                        
*                                                                               
         MVC   CUSREP,NUSPRREP                                                  
         BAS   RE,GETREP                                                        
*                                                                               
REP12    LA    RE,SRTCUTI                                                       
         CLI   NUSPRTYP,C'U'                                                    
         BE    REP25                                                            
         LA    RE,4(RE)                                                         
*                                                                               
         CLI   NUSPRTYP,C'S'                                                    
         BE    REP25                                                            
         LA    RE,4(RE)                                                         
*                                                                               
         CLI   NUSPRTYP,C'B'                                                    
         BE    REP25                                                            
         LA    RE,4(RE)                                                         
*                                                                               
REP25    L     RF,0(RE)                                                         
         ICM   R1,15,NUSPRAMT                                                   
         AR    RF,R1                                                            
         ST    RF,0(RE)                                                         
*                                                                               
         B     REP5                GET NEXT ELEMENT                             
         DROP  R4                                                               
*                                                                               
PUTSRT   NTR1                                                                   
         OC    SRTTIME(24),SRTTIME                                              
         BZ    XIT                                                              
         LA    R2,SRTREC                                                        
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
REP50    DS    0H                                                               
         LA    R2,P1                                                            
         USING PLINED,R2                                                        
         USING SRTRECD,R6                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,4(R1)                                                         
         LTR   R6,R6                                                            
         BZ    REP100              NO MORE DATA                                 
         BAS   RE,COMBREC                                                       
         CLC   CUSORT(10),SRTRECD  SEE IF LINE BREAK                            
         BE    REP50                                                            
         EJECT                                                                  
*---MOVE DATA TO PRINT LINE                                                     
*                                                                               
REP55    OC    SUBTIME(24),SUBTIME                                              
         BNZ   REP60                                                            
         MVC   SUBTIME(24),SRTTIME                                              
*---SPECIAL-REP CODE                                                            
REP60    MVC   PPAYEE,SRTPAYEE                                                  
*---DATE                                                                        
         GOTO1 DATCON,DMCB,(2,SRTDATE),(4,PDATE)                                
         MVI   PDATE+5,C'-'                                                     
         EDIT  (1,SRTSUBLN),(2,PDATE+6),ALIGN=LEFT                              
*---NETWORK                                                                     
         MVC   PNET,SRTNET                                                      
*---PROGRAM NAME                                                                
         MVC   PPROGN,SRTPRGN                                                   
*---PRODUCT(S) CODE                                                             
         BAS   RE,GETPRD                                                        
*---ACTUAL COST                                                                 
REP70    EDIT  (4,SUBTIME),(11,PTIME),FLOAT=-                                   
*---INTEGRATION COST                                                            
         EDIT  (4,SUBINTG),(11,PINTGR),FLOAT=-                                  
*---CUT-IN COST                                                                 
         EDIT  (4,SUBCUTI),(11,PCUTIN),FLOAT=-                                  
*---COPY-SPLIT COST                                                             
         EDIT  (4,SUBCOPY),(11,PCOPY),FLOAT=-                                   
*---BLACKOUT COST                                                               
         EDIT  (4,SUBBLKO),(11,PBLKOUT),FLOAT=-                                 
*---OTHER COST                                                                  
         EDIT  (4,SUBOTHR),(11,POTHER),FLOAT=-                                  
         XC    SUBTIME(24),SUBTIME     CLEAR SUBTOTALS                          
*---IF TOTAL LINE GO NO FURTHER                                                 
         OC    TOTSW,TOTSW                                                      
         BNZ   REP80                                                            
         DROP  R2                                                               
*---INITIALIZE LINE 2                                                           
         LA    R2,P2                                                            
         USING PLINED2,R2                                                       
*---SPECIAL-REP NAME                                                            
         MVC   PPAYEEN,SRTREPN                                                  
*---PROGRAM CODE                                                                
         MVC   PPROG,SRTPROG                                                    
         MVI   PPROG+6,C'/'                                                     
*---ESTIMATE CODE                                                               
         EDIT  (1,SRTEST),(3,PEST),ALIGN=LEFT                                   
         MVI   PEST+3,C'/'                                                      
*---PACKAGE CODE                                                                
         EDIT  (1,SRTPACK),(3,PPACK),ALIGN=LEFT                                 
*---SPOT LENGTH                                                                 
         EDIT  (1,SRTSLEN),(3,PLEN),ALIGN=LEFT                                  
*---DAYPART                                                                     
         MVC   PDAYPT,SRTDYPT                                                   
*---PRINT THE LINES                                                             
         BAS   RE,PRINTIT                                                       
         DROP  R2                                                               
*                                                                               
         LA    R2,P1                                                            
         USING PLINED,R2                                                        
*                                                                               
REP80    CLC   CUSORT(3),SRTRECD   SEE IF PAYEE BREAK                           
         BE    REP90                                                            
         OC    TOTSW,TOTSW                                                      
         BNZ   REP90                                                            
         MVI   TOTSW,X'FF'                                                      
         MVC   PPAYEE,CUSORT                                                    
         MVC   PDATE,=2CL4'* * '                                                
         MVC   PDATE+8(9),=CL9'T O T A L'                                       
         MVC   PDATE+18(8),=2CL4' * *'                                          
         MVC   CUSORT,SRTRECD                                                   
*                                                                               
         MVC   SUBTIME(24),TOTTIME                                              
         XC    TOTTIME(24),TOTTIME                                              
         BAS   RE,COMBREC                                                       
         B     REP70                                                            
REP90    MVC   CUSORT,SRTRECD                                                   
         MVI   TOTSW,0                                                          
         B     REP50                                                            
REP100   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         DROP  R2,R6                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R6,ANETWS2                                                       
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         CLI   CLTITLE,1           OWN TITLE PROVIDED                           
         BE    HDR2                                                             
         MVC   H1+43(25),=C'SPECIAL REP COST ANALYSIS'                          
         MVC   H2+43(25),=C'-------------------------'                          
         B     HDR2A                                                            
HDR2     DS    0H                                                               
         NETGO NVTITOUT,DMCB                                                    
HDR2A    MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+17(20),SPLCLIN                                                
         MVC   H4(8),=C'ESTIMATE'                                               
         MVC   H4+10(8),SPLEST                                                  
         CLI   SPLDPT,X'40'                                                     
         BNH   *+16                                                             
         MVC   H4+97(7),=C'DAYPART'                                             
         MVC   H4+106(8),SPLDPT                                                 
         CLI   SPLPAK,X'40'                                                     
         BNH   HEADS                                                            
         MVC   H4+50(8),=C'PACKAGE='                                            
         MVC   H4+58(8),SPLPAK                                                  
         DROP  R5                                                               
HEADS    MVC   H6(132),=CL132'PAYEE   DATE       NET  PROGRAM NAME     X        
                 PRODUCT       TIME      INTEGRATION    CUT-IN     COPYX        
               -SPLIT    BLACKOUT       OTHER     '                             
*                                                                               
         MVC   H7(50),=CL50'PAYEE NAME              PROGRAM/EST/PACK   X        
               LEN  DP'                                                         
*                                                                               
         MVC   H8(132),=CL132'----------------------  ---------------- X        
                 -------    -----------  -----------  -----------  ----X        
               -------  -----------  -----------  '                             
*                                                                               
HDX      B     XIT                                                              
         EJECT                                                                  
*----ROUTINE TO DIG OUT SPECIAL REP NAME                                        
*                                                                               
GETREP   NTR1                                                                   
         USING SRTRECD,R2                                                       
         MVC   AIO,AIO2                                                         
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOTFILE                      
         LA    R4,KEY                                                           
         USING REPREC,R4                                                        
         XC    REPKEY,REPKEY                                                    
         MVC   REPKTYPE,=CL2'RN'                                                
         EDIT  (2,NBSREP),(3,REPKREP),FILL=0                                    
         MVC   REPKAGY,NBSELAGY                                                 
         MVC   FILENAME,=C'SPTDIR '                                             
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL '                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   CUSREP,REPKREP                                                   
         MVC   CUSREPN,RNAME                                                    
         MVC   SRTPAYEE,REPKREP                                                 
         MVC   SRTREPN,RNAME                                                    
         SPACE 1                                                                
         NETGO NVSETUNT,DMCB       RESET TO READ UNITFILE                       
         XC    FILENAME,FILENAME                                                
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
* DISPLAY PRODUCT CODES                                                         
*                                                                               
DISPRD   NTR1                      TEST IF ANY PRODUCT ALLOCATED                
         LA    R5,P1                                                            
         USING PLINED,R5                                                        
         USING SRTRECD,R6                                                       
         LA    R2,PPROD            R2 POINTS TO OUTPUT                          
         LA    R1,SRTPROD                                                       
         BAS   R8,GETPRD                                                        
         LA    R2,2(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   1(R1),0                                                          
         BE    DISPRDX                                                          
         LR    R3,R2               SAVE POSITION OF COMMA                       
         LA    R2,1(R2)            POSITION R2 FOR START OF PROD. CODE          
         LA    R1,1(R1)                                                         
         BAS   R8,GETPRD                                                        
         MVI   0(R3),C'*'          SEPARATE PRODUCT CODES WITH A STAR           
         SPACE                                                                  
DISPRDX  B     XIT                                                              
         DROP  R5,R6                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO GET PRODUCT CODE FROM CLIENT LIST                              
*                                                                               
GETPRD   NTR1                                                                   
         LA    R0,220                                                           
         L     RF,ANETWS3          CLIENT LIST                                  
         CLC   0(1,R1),3(RF)       TEST FOR PRODUCT NUMBER                      
         BE    *+14                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   0(3,R2),0(RF)       EXTRACT PRODUCT CODE                         
         B     XIT                 RETURN NO CALLER                             
         SPACE 2                                                                
* SUB-ROUTINE TO GET PRODUCT CODE FROM CLIENT LIST                              
*                                                                               
COMBREC  NTR1                                                                   
         USING SRTRECD,R6                                                       
         CLC   CUSORT,SRTRECD                                                   
         BE    CMBR050                                                          
         MVC   SUBTIME(24),LINTIME                                              
         XC    LINTIME(24),LINTIME                                              
         CLC   CUSORT(3),SRTRECD                                                
         BNE   XIT                                                              
*                                                                               
*---TOTAL ACTUAL                                                                
CMBR050  ICM   RE,15,SRTTIME                                                    
         ICM   RF,15,LINTIME                                                    
         AR    RF,RE                                                            
         ST    RF,LINTIME                                                       
         ICM   RF,15,TOTTIME                                                    
         AR    RF,RE                                                            
         ST    RF,TOTTIME                                                       
*---TOTAL INTEGRATION                                                           
         ICM   RE,15,SRTINT                                                     
         ICM   RF,15,LININTG                                                    
         AR    RF,RE                                                            
         ST    RF,LININTG                                                       
         ICM   RF,15,TOTINTG                                                    
         AR    RF,RE                                                            
         ST    RF,TOTINTG                                                       
*---TOTAL CUT-IN                                                                
         ICM   RE,15,SRTCUTI                                                    
         ICM   RF,15,LINCUTI                                                    
         AR    RF,RE                                                            
         ST    RF,LINCUTI                                                       
         ICM   RF,15,TOTCUTI                                                    
         AR    RF,RE                                                            
         ST    RF,TOTCUTI                                                       
*---TOTAL COPY-SPLIT                                                            
         ICM   RE,15,SRTCOPY                                                    
         ICM   RF,15,LINCOPY                                                    
         AR    RF,RE                                                            
         ST    RF,LINCOPY                                                       
         ICM   RF,15,TOTCOPY                                                    
         AR    RF,RE                                                            
         ST    RF,TOTCOPY                                                       
*---TOTAL BLACKOUT                                                              
         ICM   RE,15,SRTBLKT                                                    
         ICM   RF,15,LINBLKO                                                    
         AR    RF,RE                                                            
         ST    RF,LINBLKO                                                       
         ICM   RF,15,TOTBLKO                                                    
         AR    RF,RE                                                            
         ST    RF,TOTBLKO                                                       
*---TOTAL OTHER                                                                 
         ICM   RE,15,SRTOTHER                                                   
         ICM   RF,15,LINOTHR                                                    
         AR    RF,RE                                                            
         ST    RF,LINOTHR                                                       
         ICM   RF,15,TOTOTHR                                                    
         AR    RF,RE                                                            
         ST    RF,TOTOTHR                                                       
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H3,45,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         SPACE                                                                  
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
ASSFLG   DS    CL1                 * FROM EDIT                                  
POLFLG   DS    CL1                 * FROM EDIT                                  
CLTITLE  DS    CL40                * FROM EDIT                                  
PRDALL   DS    CL1                 * FROM EDIT                                  
NETALL   DS    CL1                 * FROM EDIT                                  
ESTALL   DS    CL1                 * FROM EDIT                                  
*                                                                               
CUSREP   DS    CL2                 SAVED SPECIAL REP (BINARY)                   
CUSREPN  DS    CL22                REP NAME                                     
CUSORT   DS    CL10                SORT COMPARE FIELD                           
*                                                                               
TOTSW    DS    CL1                 TOTAL SWITCH                                 
*                                                                               
SRTREC   DS    CL100                                                            
*                                                                               
*                                  SUBTOTALS BY SREP                            
SUBTIME  DS    F                   TIME                                         
SUBINTG  DS    F                   INTERGRATION                                 
SUBCUTI  DS    F                   CUT-IN                                       
SUBCOPY  DS    F                   COPY-SPLIT                                   
SUBBLKO  DS    F                   BLACKOUT                                     
SUBOTHR  DS    F                   OTHER                                        
*                                                                               
*                                  LINE TOTALS BY SREP                          
LINTIME  DS    F                   TIME                                         
LININTG  DS    F                   INTERGRATION                                 
LINCUTI  DS    F                   CUT-IN                                       
LINCOPY  DS    F                   COPY-SPLIT                                   
LINBLKO  DS    F                   BLACKOUT                                     
LINOTHR  DS    F                   OTHER                                        
*                                                                               
*                                  TOTALS BY SREP                               
TOTTIME  DS    F                   TIME                                         
TOTINTG  DS    F                   INTERGRATION                                 
TOTCUTI  DS    F                   CUT-IN                                       
TOTCOPY  DS    F                   COPY-SPLIT                                   
TOTBLKO  DS    F                   BLACKOUT                                     
TOTOTHR  DS    F                   OTHER                                        
         EJECT                                                                  
*                                                                               
SRTRECD  DSECT                    SORT LINE DSECT                               
SRTPAYEE DS    CL3                                                              
SRTDATE  DS    CL2                                                              
SRTSUBLN DS    CL1                                                              
SRTNBAIO DS    CL4                                                              
SRTNET   DS    CL3                                                              
SRTPRGN  DS    CL16                                                             
SRTPROG  DS    CL6                                                              
SRTEST   DS    CL1                                                              
SRTPACK  DS    CL1                                                              
SRTPROD  DS    CL2                                                              
SRTSLEN  DS    CL1                                                              
SRTDYPT  DS    CL1                                                              
SRTREPN  DS    CL22                                                             
SRTTIME  DS    CL4                                                              
SRTINT   DS    CL4                                                              
SRTCUTI  DS    CL4                                                              
SRTCOPY  DS    CL4                                                              
SRTBLKT  DS    CL4                                                              
SRTOTHER DS    CL4                                                              
         DS    CL7                                                              
SRTEND   DS    CL1                                                              
*                                                                               
PLINED   DSECT                    PRINT LINE DSECT                              
PPAYEE   DS    CL3                                                              
         DS    CL5                                                              
PDATE    DS    CL8                                                              
         DS    CL3                                                              
PNET     DS    CL3                                                              
         DS    CL2                                                              
PPROGN   DS    CL16                                                             
         DS    CL3                                                              
PPROD    DS    CL7                                                              
         DS    CL4                                                              
PTIME    DS    CL11                                                             
         DS    CL2                                                              
PINTGR   DS    CL11                                                             
         DS    CL2                                                              
PCUTIN   DS    CL11                                                             
         DS    CL2                                                              
PCOPY    DS    CL11                                                             
         DS    CL2                                                              
PBLKOUT  DS    CL11                                                             
         DS    CL2                                                              
POTHER   DS    CL11                                                             
         DS    CL1                                                              
PEND     DS    CL1                                                              
*                                                                               
*                                                                               
PLINED2  DSECT                    PRINT LINE DSECT                              
PPAYEEN  DS    CL22                                                             
         DS    CL2                                                              
PPROG    DS    CL6                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PPACK    DS    CL3                                                              
         DS    CL5                                                              
PLEN     DS    CL3                                                              
         DS    CL2                                                              
PDAYPT   DS    CL2                                                              
         DS    CL81                                                             
PEND2    DS    CL1                                                              
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE8D                                                       
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRI39T  05/01/02'                                      
         END                                                                    
