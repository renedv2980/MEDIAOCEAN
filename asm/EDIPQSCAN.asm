*          DATA SET EDIPQSCAN  AT LEVEL 053 AS OF 05/27/20                      
*PHASE EDPQSCNA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE DATCON                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE SCANNER                                                                
*                                                                               
EDPQSCAN CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDPQSCAN,WORK=VWRKAREA                                         
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         L     RA,ACOMMON                                                       
         USING COMMON,RA                                                        
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         ST    RD,SAVERD           SET EXIT FROM CHAIN RD                       
*                                                                               
         USING CIDATAD,R6                                                       
*                                                                               
         L     RC,AWORKD           GET SOME W/S FOR THIS MODULE                 
         USING WORKD,RC                                                         
         BRAS  RE,INIT             INITIALISE IT ALL                            
         BNE   MAINX                                                            
*                                                                               
MAIN02   BRAS  RE,SCANPQ           SCAN PQS FOR JOBS                            
         BNE   MAINX               ERROR EXIT                                   
         BRAS  RE,PROCPQ           PROCESS PQ REPORTS                           
         BRAS  RE,WAIT             WAIT FOR TIMER POP OR OPS COMMAND            
         BE    MAIN02              AND BACK FOR NEXT                            
*                                                                               
MAINX    BRAS  RE,MQFREE           FREE ANY MQ CONNECTIONS THAT EXIST           
         B     XBASE                                                            
*                                                                               
VWRKAREA DC    V(WORKAREA)                                                      
ACOMMON  DC    A(COMMON)                                                        
AWORKD   DC    A(WORKD)                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WAIT UNTIL TIMER POP OR OPERATOR POST                    *         
***********************************************************************         
WAIT     NTR1  BASE=*,LABEL=*                                                   
         OC    LASTTIME,LASTTIME   FIRST TIME THROUGH?                          
         BNZ   *+14                NO                                           
         MVC   WAITSEC2,WAITSECS   WAIT FULL WAITSECS TIME                      
         B     WAIT02                                                           
*                                                                               
         TIME  BIN                                                              
         S     R0,LASTTIME         - ONLY IF END OF THE DAY                     
         BNP   WAIT02              AT EOD, WAIT LIKE LAST CYCLE                 
         L     R1,WAITSECS                                                      
         SR    R1,R0                                                            
         BNP   EXITOK                                                           
         ST    R1,WAITSEC2                                                      
*                                                                               
WAIT02   XC    TIMERECB,TIMERECB                                                
         STIMERM SET,ID=STIMER1,BINTVL=WAITSEC2,EXIT=TIMERXIT                   
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         JNZ   *+2                                                              
*                                                                               
         MVC   PLINE(20),=CL20'Entering WAIT'                                   
         BRAS  RE,PRNT                                                          
*                                                                               
         WAIT  1,ECBLIST=ECBLST    WAIT FOR TIMER POP OR OPERATOR               
*                                                                               
         MVC   PLINE(20),=CL20'Exiting WAIT'                                    
         BRAS  RE,PRNT                                                          
*                                                                               
         TM    TIMERECB,X'40'                                                   
         BO    EXITOK              TIMER POPPED                                 
*                                                                               
         STIMERM CANCEL,ID=STIMER1                                              
         LTR   RF,RF               OPERATOR INTERRUPTED -- CANCEL TIMER         
         JNZ   *+2                                                              
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    EXITOK              NO                                           
         BRAS  RE,CHKOPER                                                       
*                                                                               
         CLI   OPERSTOP,YES        OPERATOR STOP REQUESTED?                     
         BE    EXITL               YES                                          
         B     EXITOK                                                           
*                                                                               
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
                                                                                
TIMERECB DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS PQ REPORTS FROM XMIT TABLE                                  *         
***********************************************************************         
PROCPQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM31 ,                   GET NEXT LOGICAL REPORT                      
         L     RF,ASRTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   COUNTER,0(RF)       NUMBER OF ENTRIES IN TABLE                   
         SAM24                                                                  
*                                                                               
         MVC   ASRTPTR,ASRTTBL     SET CURRENT ENTRY POINTER                    
*                                                                               
PRPQ10   ICM   RE,15,COUNTER                                                    
         BZ    PRPQ90              NO MORE ENTRY                                
*                                                                               
         SAM31 ,                   GET NEXT LOGICAL REPORT                      
         L     RF,ASRTPTR                                                       
         MVC   TBLNTRY,0(RF)       SAVE A COPY OF THE TABLE ENTRY               
         SAM24                                                                  
*                                                                               
         LA    R3,TBLNTRY                                                       
         USING SRTTABLD,R3                                                      
*                                                                               
         L     R6,ACITABL          PRINT QUEUE INFO TABLE                       
         ZIC   R0,SRTPRTQ                                                       
         BCTR  R0,0                                                             
         MHI   R0,CITBLLNQ                                                      
         AR    R6,R0               R6 = A(THIS PQ ENTRY)                        
*                                                                               
         LA    RF,WORK             USE REP NUM TO INITIALIZE BUFFER             
         XC    WORK,WORK                                                        
         USING UKRECD,RF                                                        
         MVC   UKSRCID,=H'1'                                                    
         MVC   UKREPNO,SRTREFNO                                                 
         MVI   UKFLAG,UKFLNUM+UKFLCIA                                           
         GOTO1 VDMGR,DMCB,INDEX,CFPQID,WORK,R,ACXREC                            
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
         DROP  RF                                                               
*                                                                               
         BRAS  RE,XFERREP                                                       
*                                                                               
         L     RE,COUNTER                                                       
         BCTR  RE,0                COUNTER-1                                    
         ST    RE,COUNTER                                                       
*                                                                               
         L     RE,ASRTPTR          PT TO NEXT ENTRY                             
         AHI   RE,SRTTBLQ                                                       
         ST    RE,ASRTPTR                                                       
*                                                                               
         B     PRPQ10                                                           
*                                                                               
PRPQ90   DS    0H                  CLEAR BINSCH TABLE                           
         SAM31                                                                  
         L     RE,ASRTTBL          A(START OF SRTTBL)                           
         L     RF,SRTTBMAX         MAX # TABLE ENTRY                            
         MHI   RF,SRTTBLQ          . . . TIMES RECLEN = TABLE SIZE              
         XCEFL                                                                  
*                                  CLEAR TABLE COUNTER                          
         L     RE,ASRTTBL                                                       
         AHI   RE,-8               RE = A(ENTRIES#)                             
         XC    0(4,RE),0(RE)                                                    
         SAM24                                                                  
*                                                                               
         GOTO1 =V(DMISGENQ),DMCB,C'TASK'    CLEAN-UP TASK PRTQ ENQUEUES         
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
***********************************************************************         
* LOCATE ALL REPORTS TO BE TRANSMITTED ON ALL PRINT QUEUES            *         
***********************************************************************         
SCANPQ   NTR1  BASE=*,LABEL=*                                                   
         MVC   PLINE(30),=CL30'Beginning scanning PQ'                           
         BRAS  RE,PRNT                                                          
*                                                                               
SCAN20   TIME  BIN                                                              
         ST    R0,LASTTIME         SAVE PQ SCAN @ THIS TIME                     
         L     R6,ACITABL          PRINT QUEUE INFO                             
*                                                                               
SCAN40   L     R5,ARPTNUMS                                                      
         LR    R0,R5               CLEAR REPORT NUMBERS TABLE                   
         LHI   R1,NUMRPTSQ                                                      
         MHI   R1,L'RPTNUMS                                                     
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   PLINE(18),=CL18'Beginning scan of'                               
         MVC   PLINE+18(L'CFPQID),CFPQID                                        
         BRAS  RE,PRNT                                                          
*                                                                               
         XR    R0,R0                                                            
SCAN50   XC    PQINDEX,PQINDEX                                                  
         GOTO1 VDMGR,DMCB,(0,SEQ),CFPQID,PQINDEX,R,ACXREC                       
         TM    8(R1),X'80'         END OF FILE?                                 
         BO    SCAN80              YES                                          
*                                                                               
         LA    R3,R                                                             
         USING PQPLD,R3                                                         
         CLI   QLCLASS,C'G'        CLASS G?                                     
         BNE   SCAN50                                                           
*                                                                               
         TM    TSTMODE,X'80'       TEST MODE?                                   
         BZ    SCAN60A                                                          
         CLI   TSTPQALL,YES        MATCH ON ANY TEST STATUS?                    
         BE    SCAN60B             YES                                          
*                                                                               
SCAN60A  TM    QLSTAT,QLSTAC       STATUS ACTIVE?                               
         BZ    SCAN50              NO - SKIP IT                                 
         TM    QLSTAT,QLSTTE       STATUS TEMP?                                 
         BO    SCAN50              YES - SKIP IT                                
*                                                                               
SCAN60B  TM    QLATTB,QLATJOBI     DOES REPORT CONTAIN JCL?                     
         BO    SCAN50              YES -- IGNORE IT FOR NOW                     
         TM    QLATTB,QLATERR      IS REPORT IN ERROR?                          
         BO    SCAN50              YES -- IGNORE IT                             
*                                                                               
         L     RF,AUSRIDS          TEST USERID= FILTERS                         
         OC    0(L'USRIDS,RF),0(RF)                                             
         BZ    SCAN71                                                           
*                                                                               
SCAN70   OC    0(L'USRIDS,RF),0(RF)                                             
         BZ    SCAN50                                                           
         CLC   QLSRCID,0(RF)       REPORT MATCHES FILTER?                       
         BE    SCAN71              YES - PASS                                   
         AHI   RF,L'USRIDS                                                      
         B     SCAN70                                                           
*                                                                               
SCAN71   L     RF,AEXUSIDS         TEST EXUSERID= FILTERS                       
         OC    0(L'USRIDS,RF),0(RF)                                             
         BZ    SCAN74                                                           
*                                                                               
SCAN72   OC    0(L'USRIDS,RF),0(RF)                                             
         BZ    SCAN74                                                           
         CLC   QLSRCID,0(RF)       REPORT MATCHES FILTER?                       
         BE    SCAN50              YES - FAIL                                   
         AHI   RF,L'USRIDS                                                      
         B     SCAN72                                                           
*                                                                               
SCAN74   CLC   SUBFILT,SPACES      IS THERE A SUB-ID FILTER?                    
         BE    SCAN76              NO                                           
         CLC   QLSUBID,SUBFILT     YES - DOES THIS REPORT MATCH FILTER?         
         BNE   SCAN50                                                           
*                                                                               
SCAN76   MVC   0(2,R5),QLREPNO     REMEMBER THIS REPORT NUMBER                  
         AHI   R5,2                                                             
         AHI   R0,1                                                             
         C     R5,ARPTNUMX                                                      
         BL    SCAN50                                                           
         LHI   R0,24                                                            
         BRAS  RE,SYSMESS                                                       
         ABEND 911,DUMP            INCREASE NUMRPTSQ                            
         DROP  R3                                                               
*                                                                               
SCAN80   L     R5,ARPTNUMS         EXAMINE THE REPORTS                          
         MVC   PLINE(18),=CL18'Completed scan of'                               
         MVC   PLINE+18(L'CFPQID),CFPQID                                        
         MVC   PLINE+25(40),=CL40'XXXXX Reports match filter criteria'          
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PLINE+25(5),DUB                                                  
         BRAS  RE,PRNT                                                          
*                                                                               
SCAN85   OC    0(2,R5),0(R5)       END OF LIST?                                 
         BZ    SCAN90              YES                                          
*                                                                               
         LA    RF,WORK             USE REP NUM TO INITIALIZE BUFFER             
         XC    WORK,WORK                                                        
         USING UKRECD,RF                                                        
         MVC   UKSRCID,=H'1'                                                    
         MVC   UKREPNO,0(R5)                                                    
         MVI   UKFLAG,UKFLNUM+UKFLCIA                                           
         GOTO1 VDMGR,DMCB,INDEX,CFPQID,WORK,R,ACXREC                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
         BRAS  RE,SORTREP          ADD THIS REPORT TO TABLE                     
         AHI   R5,2                CHECK OUT NEXT REPORT                        
         B     SCAN85                                                           
*                                                                               
SCAN90   GOTO1 VDMGR,DMCB,BUFFER,CFPQID,0,0,ACXREC                              
*                                                                               
         AHI   R6,CITBLLNQ         BUMP TO NEXT PQ                              
         CLI   0(R6),X'FF'         END OF LIST?                                 
         BNE   SCAN40                                                           
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE ADD PQ REPORT TO TABLE SORTED                          *         
***********************************************************************         
SORTREP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 VDMGR,DMCB,RANDOM,CFPQID,0,R,ACXREC                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         CLI   QLCLASS,C'G'        CLASS G?                                     
         BNE   EXITOK                                                           
*                                                                               
         TM    TSTMODE,X'80'       TEST MODE?                                   
         BZ    SR20                                                             
         CLI   TSTPQALL,YES        MATCH ON ANY TEST STATUS?                    
         BE    SR30                YES                                          
*                                                                               
SR20     TM    QLSTAT,QLSTAC       STATUS ACTIVE?                               
         BZ    EXITOK                                                           
*                                                                               
SR30     TM    QLATTB,QLATJOBI     DOES REPORT CONTAIN JCL?                     
         BO    EXITOK              YES -- IGNORE IT FOR NOW                     
         TM    QLATTB,QLATERR      IS REPORT IN ERROR?                          
         BO    EXITOK              YES -- IGNORE IT                             
*                                                                               
         LA    R3,TBLNTRY                                                       
         XC    TBLNTRY,TBLNTRY                                                  
         USING SRTTABLD,R3                                                      
         MVC   SRTCRTIM,QLAGELT    SAVE REPORT CREATION TIME                    
         MVC   SRTCRDAT,QLDATEL    SAVE REPORT CREATION DATE - CMPRSD           
         TM    QLTYP1,QLTYNCD                                                   
         BO    SR31                                                             
         GOTO1 =V(DATCON),DMCB,(2,SRTCRDAT),(30,SRTCRDAT)                       
SR31     MVC   SRTPRTQ,CFPQINUM                                                 
         MVC   SRTPQKEY,QLKEY                                                   
         DROP  R2,R3                                                            
*                                                                               
         SAM31                                                                  
         L     RF,ASRTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
         GOTO1 =V(BINSRCH),DMCB,TBLNTRY,ASRTTBL,,(1,SRTTBLQ),SRTTBLQ,  +        
               SRTTBMAX                                                         
         L     RF,ASRTTBL                                                       
         AHI   RF,-8               NUMBER OF ENTRIES IN TABLE                   
         MVC   0(4,RF),DMCB+8      UPDATE TABLE SIZE                            
         SAM24                                                                  
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* THIS ROUTINE GET XA STORAGE FOR XMIT TABLE                          *         
***********************************************************************         
BLDSRTBL NTR1  BASE=*,LABEL=*                                                   
         L     R3,SRTTBMAX         MAX # TABLE ENTRY                            
         MHI   R3,SRTTBLQ          . . . TIMES RECLEN = TABLE SIZE              
         LR    R4,R3               SAVE LENGTH OF TABLE                         
         AHI   R3,16               ROOM FOR LABEL, COUNTER, MAX ENTRY           
*                                                                               
         SAM31                                                                  
         STORAGE OBTAIN,LENGTH=(3),LOC=ANY,BNDRY=PAGE                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(8,R1),=C'*XMITTBL'                                             
         XC    8(4,R1),8(R1)       CLEAR COUNTER                                
         MVC   12(4,R1),SRTTBMAX                                                
         LA    R1,16(R1)           BUMP PAST LABEL, COUNTER, MAX#               
         ST    R1,ASRTTBL          A(START OF SRTTBL)                           
         LR    RE,R1               CLEAR TABLE                                  
         LR    RF,R4                                                            
         XCEFL                                                                  
*                                                                               
         SAM24                                                                  
         B     EXITOK                                                           
         LTORG                                                                  
                                                                                
***********************************************************************         
* THIS ROUTINE BREAKS UP A PRINT QUEUE REPORT                         *         
* INTO LOGICAL REPORTS AND ADDS THEIR ENTRIES TO THE MASTER MQ QUEUE  *         
* INPUT: R3 = A(XMIT TABLE ENTRY)                                     *         
*        R6 = A(CIDATA)                                               *         
***********************************************************************         
XFERREP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERR#,0                                                           
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 VDMGR,DMCB,RANDOM,CFPQID,0,R,ACXREC                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         CLI   QLCLASS,C'G'        CLASS G?                                     
         BNE   EXITOK                                                           
*                                                                               
         TM    TSTMODE,X'80'       TEST MODE?                                   
         BZ    XFR020                                                           
         CLI   TSTPQALL,YES        MATCH ON ANY TEST STATUS?                    
         BE    XFR021              YES                                          
*                                                                               
XFR020   TM    QLSTAT,QLSTAC       STATUS ACTIVE?                               
         BZ    EXITOK                                                           
*                                                                               
         USING SRTTABLD,R3                                                      
XFR021   DS    0H                                                               
         CLC   SRTPQKEY,QLKEY      IS THE REPORT STILL THERE?                   
         BNE   EXITOK                                                           
         MVC   HALF,QLDATEL                                                     
         TM    QLTYP1,QLTYNCD                                                   
         BO    XFR022                                                           
         GOTO1 =V(DATCON),DMCB,(2,QLDATEL),(30,HALF)                            
XFR022   CLC   SRTCRDAT,HALF                                                    
         BNE   EXITOK                                                           
         CLC   SRTCRTIM,QLAGELT                                                 
         BNE   EXITOK                                                           
         DROP  R3                                                               
*                                                                               
         MVC   RPTCRTIM,QLAGELT    SAVE REPORT CREATION TIME                    
         MVC   RPTCRDAT,HALF       SAVE REPORT CREATION DATE - CMPRSD           
         MVC   RPTPQTYP,QLTYPE     SAVE REPORT TYPE                             
         MVC   RPTSECAG,QLPIDNUM   SAVE REPORT Security Agency                  
         MVC   RPTPIDNO,QLPIDNUM+2 SAVE REPORT PID#                             
                                                                                
***********************************************************************         
* Create unqiue number for this PQ entry called Group Id                        
***********************************************************************         
         TIME  BIN                                                              
         XGR   GRE,GRE                                                          
         XGR   GRF,GRF                                                          
         LR    RE,R1               L YYYYDDDF into RE                           
         SRLG  GRE,GRE,4           Remove sign                                  
         SLLG  GRE,GRE,32          Shift YYYYDDD into High have of GRE          
         C     R0,SSSSSTHB         See if same as lasts report                  
         JNE   *+8                 No so we are good                            
         AHI   R0,1                Add one (1/100s) to make it unique           
         ST    R0,SSSSSTHB         Save value for next report                   
         CVD   R0,DUB                                                           
         L     RF,DUB+4            Load SSSSSTHC into RF                        
         OILL  GRF,X'000F'         Convert sign to X'0F'                        
         OGR   GRF,GRE             Create YYYYDDDSSSSSTHF into GRF              
         STG   GRF,SAVGRPID        Save value in GroupId                        
*                                                                               
         MVI   ENDRPT,NO                                                        
         MVI   ENDLRPT,NO                                                       
         MVI   EMPTYRPT,YES        ASSUME REPORT HAS NO DATA TO SEND            
         MVI   DIDPUT,NO                                                        
         XC    RPTLOGNO,RPTLOGNO   RESET LOGICAL REPORT COUNTER                 
         XC    PQS,PQS             CLEAR PQS                                    
*                                                                               
         LA    R3,XTRAMES2                                                      
         MVC   XTRAMES2,SPACES                                                  
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         EDIT  (R0),(5,0(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),C','                                                       
         MVC   1(3,R3),QLSUBID                                                  
         MVI   4(R3),C','                                                       
         AHI   R3,5                                                             
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         EDIT  (R0),(5,0(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         BRAS  RE,GETIDN           GET ID RECORD (NAME BACK IN DUB)             
         BE    *+12                                                             
         BRAS  RE,MARKBAD                                                       
         B     EXITOK                                                           
*                                                                               
         MVC   RPTUID,DUB          SAVE ALPHA USERNAME                          
         MVC   RPTUIDNO,QLSRCID    SAVE USERID NUMBER                           
         MVC   RPTSUBID,QLSUBID    SAVE SUB-ID                                  
         MVC   RPTREFNO,QLREPNO    SAVE REFERENCE NUMBER                        
*                                                                               
         LA    R3,XTRAMES2                                                      
         MVC   XTRAMES2,SPACES                                                  
         MVC   XTRAMES2(L'RPTUID),RPTUID                                        
         LA    R3,XTRAMES2+L'RPTUID                                             
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C' '                                                       
         AHI   R3,2                                                             
*                                                                               
         MVC   0(3,R3),QLSUBID                                                  
         MVI   3(R3),C','                                                       
         AHI   R3,4                                                             
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,QLREPNO                                                     
         EDIT  (R0),(5,0(R3)),0,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
         TM    SVAGOPTS,CTAGUAT    UAT AGENCY?                                  
         BZ    XFR040                                                           
*                                                                               
         MVC   PLINE(22),=CL22'Skipping report(UAT): '                          
         MVC   PLINE+22(L'XTRAMES2),XTRAMES2                                    
         BRAS  RE,PRNT                                                          
         BRAS  RE,SETPRTD          SET STATUS PRINTED                           
         B     EXITOK                                                           
*                                                                               
XFR040   SAM31 ,                   GET NEXT LOGICAL REPORT                      
         L     R4,AMSGBUFF                                                      
         USING MSGHDRD,R4                                                       
         LR    R0,R4               FIRST CLEAR MESSAGE BUFFER                   
         L     R1,MAXMSGLN                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         AHI   R4,MSGHDRLQ         Reserve area to store MSGHDR                 
         SAM24                                                                  
*                                                                               
         MVI   SKIPBXF,NO          Remove BXF section                           
         MVI   HAVEHDR,NO                                                       
         MVI   ISEMAIL,NO                                                       
         MVI   ISDIRTY,NO          SET NO DATA SO FAR                           
         MVI   ISDARE,NO                                                        
         MVI   ISBDE,NO            ASSUME NOT BDE-EMAIL                         
         MVI   HASBXF,NO                                                        
         MVI   ISPDF,NO                                                         
         MVI   ISECN,NO                                                         
         MVI   ISELINK,NO                                                       
         MVI   ISXRR,NO            Extreme Reach                                
         MVI   XHDR,NO             Extended header default is no                
         CLI   ERR#,1              Special error case                           
         BNE   XFR050                                                           
         MVI   ERR#,0              Record already in buffer                     
                                                                                
*********************************************************************           
* Find *HDR* card to start processing                                           
*********************************************************************           
H        USING EDIHDRD,R                                                        
XFR050   CLI   ENDRPT,SKIP         R has already been read in                   
         BE    XFR052                                                           
         BRAS  RE,READPQ           Read next card                               
         BE    XFR052              Have a record in R                           
         MVI   ENDRPT,YES          END OF PHYSICAL REPORT ON DISK               
         MVI   HAVEHDR,NO                                                       
         B     XFR120                                                           
*                                                                               
XFR052   MVI   ENDRPT,NO                                                        
         CLC   =C'*HDR*',H.EDIHDR  Is this a header record?                     
         BNE   XFR054              No -- Find beginning of logical rpt          
         CLI   HAVEHDR,YES         Do I already have one?                       
         BE    XFR053              Yes, so that cann't be good                  
         MVI   HAVEHDR,YES         Have one now                                 
         CLI   H.EDIOTHER,EDIBXFQ  BXF format                                   
         BNE   XFR070                                                           
         MVI   HASBXF,YES                                                       
         B     XFR070              Error, can't have another                    
*                                                                               
XFR053   MVI   ERR#,1              Err=1 - Duplicate HDR card                   
         B     XFR120              Report error in log                          
*                                                                               
XFR054   CLI   HAVEHDR,YES         Do I have one?                               
         BNE   XFR050              No, so keep searching for one                
                                                                                
*********************************************************************           
* Disect the card info                                                          
*********************************************************************           
XFR070   CLI   TRACE,YES                                                        
         BNE   XFR071                                                           
         MVC   PLINE+1(80),H.EDIHDRD                                            
         BRAS  RE,PRNT                                                          
                                                                                
XFR071   MVI   ISDIRTY,YES         SET SOMETHING IS READ                        
         MVI   ENDLRPT,NO                                                       
         MVI   EMPTYRPT,NO         REPORT HAS DATA IN TO SEND                   
         MVC   RLEN,DMCB+22                                                     
*                                                                               
         CLC   =C'*** END OF DDS MESSAGE ***',R+1                               
         BNE   XFR072                                                           
         MVI   ENDLRPT,YES         END OF REPORT - SEND IT AND CONTINUE         
         MVI   HAVEHDR,NO          Reset                                        
         MVI   SKIPBXF,NO          Reset                                        
         B     XFR120                                                           
*                                                                               
XFR072   CLI   SKIPBXF,YES                                                      
         BE    XFR050                                                           
         CLC   =C'*** BXF REPORT SECTION ***',R+1                               
         BNE   XFR075                                                           
         MVI   HASBXF,YES                                                       
         CLI   ISXRR,YES                                                        
         BE    XFR075                                                           
         CLI   ISECN,YES                                                        
         BE    XFR075                                                           
         CLI   ISELINK,YES                                                      
         BE    XFR075                                                           
         CLI   ISPDF,YES                                                        
         BE    XFR075                                                           
         CLI   IS360,YES           PM360 Needs BXF XML                          
         BE    XFR075                                                           
XFR074   MVI   SKIPBXF,YES         Remove BXF section                           
         B     XFR050              Next line                                    
*                                                                               
XFR075   CLC   =C'*HDR*',H.EDIHDR  IS THIS A HEADER RECORD?                     
         BNE   XFR088              NO -- FIND BEGINNING OF LOGICAL RPT          
*                                                                               
         LH    RF,RPTLOGNO         UP LOGICAL REPORT COUNT                      
         AHI   RF,1                                                             
         STH   RF,RPTLOGNO                                                      
*                                                                               
         CHI   RF,5000             MORE THAN 5000 LOGICAL REPORTS?              
         BH    XFR250              YES - SKIP THE REST                          
*                                                                               
         MVC   DESTINAT,H.EDIDESID DESTINATION (UP TO 25 CHARACTERS)            
         MVC   DESTFMT,H.EDIFDEST  SAVE FORMATTED DESTINATION (IF ANY)          
         CLC   =C'EDICT=',DESTINAT       KEY OF EDICT RECORD GIVEN?             
         BNE   XFR076                                                           
         MVC   DUB,DESTINAT+6      YES -- FIND RECEIVER IN TABLE                
         BRAS  RE,BLDDEST                                                       
         BE    XFR078                                                           
*                                  Error if not found                           
         CLC   =C'*BDE',DUB        THESE ARE SPECIAL AND NEED RECORDS           
         JE    *+2                                                              
         CLC   =C'*BIAS',DUB                                                    
         JE    *+2                                                              
         CLC   =C'*DDSDARA',DUB                                                 
         JE    *+2                                                              
         CLC   =C'*DDSDARR',DUB                                                 
         JE    *+2                                                              
         CLC   =C'*ENCODA',DUB                                                  
         JE    *+2                                                              
         CLC   =C'*TVSCAN',DUB                                                  
         JE    *+2                                                              
         B     XFR210              Skip                                         
*                                                                               
XFR076   MVC   DUB,RPTUID          GET DEFAULT DESITNATION                      
         BRAS  RE,BLDDEST                                                       
         BNE   XFR210              Skip                                         
*                                                                               
XFR078   LA    R2,DESTNTRY                                                      
         USING DESTTABD,R2                                                      
         CLI   DESTMETS,EDIEXRRQ   Extreme Reach Fax                            
         BNE   XFR080                                                           
         MVI   ISXRR,YES                                                        
         MVI   XHDR,YES                                                         
*                                                                               
XFR080   CLI   H.EDITTYPE,EDIEMALQ IS IT AN E-MAIL TRANSMISSION?                
         BNE   XFR082              NO                                           
         MVI   ISEMAIL,YES                                                      
         B     XFR100              YES - CONTINUE                               
*                                                                               
XFR082   CLI   H.EDITTYPE,EDITPDFQ IS PDF                                       
*        BE    *+10                NO                                           
*        CLC   =C'EDICT=*OPTICAPDF',H.EDIDESID  Is optica PDF?                  
         BNE   XFR084              NO                                           
         MVI   ISPDF,YES                                                        
         MVI   XHDR,YES                                                         
*                                                                               
XFR084   CLC   =C'EDICT=*BDE',H.EDIDESID IS IT A BDE TRANSACTION?               
         BNE   *+8                 NO                                           
         MVI   ISBDE,YES           YES - IGNORE HDR DST & FORMATTED DST         
*                                                                               
         CLI   DESTMETS,EDIECNQ    ECN                                          
         BNE   XFR085                                                           
         MVI   ISECN,YES                                                        
         MVI   XHDR,YES                                                         
*                                                                               
XFR085   CLI   DESTMETS,EDIEASYQ   EASYLINK                                     
         BNE   *+8                                                              
         MVI   ISELINK,NO          We don't support EasiLink any longer         
         DROP  R2                                                               
*                                                                               
         CLI   H.EDIDARE,EDIDARQ   IS IT A DARE REPORT (FOR EASYLINK)?          
         BNE   *+8                 YES                                          
         MVI   ISDARE,YES                                                       
*                                                                               
         CLI   XHDR,YES            Extended header?                             
         BNE   XFR100                                                           
         AHI   R4,MSGHDLQ2-MSGHDRLQ   Re-adjust msg header length               
         B     XFR100                                                           
*                                                                               
XFR088   CLI   ISBDE,YES           BDE TRANSACTION?                             
         BNE   XFR090                                                           
         CLC   =C'++DDS',R+1                                                    
         BNE   XFR100                                                           
         CLC   =C'RCP',R+12        IS THIS THE RECIPIENT CARD?                  
         BNE   XFR100              NO                                           
         MVC   DESTFMT,R+16        SAVE FORMATTED DESTINATION (IF ANY)          
         B     XFR100                                                           
*                                                                               
XFR090   DS    0H                                                               
         CLC   =C'++DDS',R+1                                                    
         BNE   XFR100                                                           
         CLC   =C'PQS',R+12        IS THIS THE PQS CARD?                        
         BNE   XFR100              NO                                           
         MVC   PQS,R+16            SAVE PQS (IF ANY)                            
         B     XFR100                                                           
         DROP  H                   EDIHDRD                                      
*                                                                               
XFR100   DS    0H                                                               
         LR    R1,R4                                                            
         AHI   R1,4                                                             
         AH    R1,RLEN                                                          
         C     R1,AMSGBUFX                                                      
         BNH   XFR110                                                           
         MVC   PLINE(20),=CL20'too big - report: '                              
         MVC   PLINE+21(L'XTRAMES2),XTRAMES2                                    
         MVC   PLINE+39(22),=CL22'skipped - continuing'                         
         BRAS  RE,PRNT                                                          
         B     XFR300                                                           
*                                                                               
XFR110   SAM31 ,                   MOVE CARD INTO MQ BUFFER                     
         LA    R0,4(R4)            Area to store length in chr format           
         LH    R1,RLEN             Length of data to move in                    
         LA    RE,R                                                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LH    R0,RLEN             LENGTH NEEDS TO BE A CL4 IN MESSAGE          
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(4,R4),DUB                                                      
         SAM24                                                                  
*                                                                               
         AH    R4,RLEN         >>> BOUNDS CHECK HERE                            
         AHI   R4,4                                                             
*                                                                               
         CLI   ENDLRPT,YES                                                      
         BNE   XFR050              NEXT DATA FOR REPORT                         
*                                                                               
XFR120   CLI   ISDIRTY,NO          CHECK ANYTHING TO DO                         
         BE    XFR230              NO                                           
*                                                                               
         CLI   ISEMAIL,YES         IS THIS AN EMAIL?                            
         BE    XFR210              YES - IGNORE IT FOR NOW                      
*                                                                               
         CLC   =C'*DDSTEST*',DESTINAT DDS TEST ONLY?                            
         BE    XFR210              Skip                                         
*                                                                               
XFR160   LA    R2,DESTNTRY                                                      
         USING DESTTABD,R2                                                      
         CLC   =C'EDICT=',DESTINAT       USE RECEIVER'S TRAN METH               
         BNE   XFR170                    NO - USE SENDER'S                      
         MVC   PTYPE,=CL8'PDF'                                                  
         CLI   ISPDF,YES                                                        
         BE    XFR180                                                           
         CLI   BDE,YES             Must be BDE then                             
         BNE   XFR210              Skip                                         
*                                                                               
         MVC   PTYPE,=CL8'BDEMAIL'                                              
         CLI   DESTMETR,EDIBDFQ    BDE - FTP                                    
         BNE   XFR168                                                           
         MVC   PTYPE,=CL8'BDE FTP'                                              
         CLC   =C'BDF',RPTSUBID                                                 
         BNE   XFR200              BDF go via EDICTA subtask                    
         MVC   PTYPE,=CL8'BDF'                                                  
         B     XFR210              Skip                                         
*                                                                               
XFR168   CLI   DESTMETR,EDIBDEQ    BDE - EMAIL (DDSINBOX)                       
         BE    XFR200                                                           
         B     XFR210              Skip                                         
*                                                                               
XFR170   CLI   FAX,YES                                                          
         BNE   XFR210              Skip                                         
         MVC   PTYPE,=CL8'ECN'                                                  
         CLI   DESTMETS,EDIECNQ    ECN                                          
         BE    XFR200                                                           
*                                                                               
         MVC   PTYPE,=CL8'EASYLINK'                                             
         CLI   DESTMETS,EDIEASYQ   EASYLINK                                     
         BE    XFR200                                                           
*                                                                               
         MVC   PTYPE,=CL8'XTMREACH'                                             
         CLI   DESTMETS,EDIEXRRQ   Extreme Reach                                
         BNE   XFR180                                                           
         MVI   ISXRR,YES                                                        
         B     XFR200                                                           
*                                                                               
XFR180   MVC   PTYPE,=CL8'BXF'                                                  
         CLI   DESTMETS,EDIBXFOQ   BXF only                                     
         BNE   XFR190                                                           
*        CLI   HASBXF,YES                                                       
*        JNE   XFR210              Shouldn't happen, so skip                    
         CLI   IS360,YES                                                        
         BE    XFR200                                                           
         CLI   ISPDF,YES                                                        
         BE    XFR200                                                           
         B     XFR210              Skip                                         
*                                                                               
XFR190   CLI   DESTMETS,EDIPDFQ    PDF?                                         
         BNE   XFR210              Skip                                         
         CLI   PDF,YES                                                          
         BNE   XFR210              Skip                                         
         DROP  R2                                                               
*                                                                               
XFR200   DS    0H                                                               
         CLI   ERR#,1                                                           
         BNE   XFR202                                                           
         MVC   PLINE(24),=CL24'Unexpected *HDR* card:'                          
         MVC   PLINE+24(L'XTRAMES2),XTRAMES2                                    
         MVC   PLINE+47(L'PTYPE),PTYPE                                          
         BRAS  RE,PRNT                                                          
         B     XFR210              No MQ put                                    
*                                                                               
XFR202   MVC   PLINE(24),=CL24'Transferring report: '                           
         MVC   PLINE+24(L'XTRAMES2),XTRAMES2                                    
         MVC   PLINE+47(L'PTYPE),PTYPE                                          
         BRAS  RE,PRNT                                                          
*                                                                               
XFR204   CLI   ENDRPT,YES          Did we already hit EOR?                      
         BE    XFR209                                                           
         MVI   ENDRPT,SKIP         Set to skip read since read ahead            
         BRAS  RE,READPQ           See if end of file                           
         BE    XFR206              No                                           
         MVI   ENDRPT,YES          Routine FMTHDR uses this flag                
         MVI   HAVEHDR,NO                                                       
         B     XFR209                                                           
*                                                                               
H        USING EDIHDRD,R                                                        
XFR206   CLI   ENDLRPT,YES         Need to find start of next report            
         JNE   *+2                 Should have been                             
         CLC   =C'*HDR*',H.EDIHDR  Is this a header record?                     
         BNE   XFR204              No, so find the next one                     
         DROP  H                                                                
*                                                                               
XFR209   CLI   HASBXF,YES                                                       
         BE    XFR209A                                                          
         MVI   IS360,NO            Turn off IS360                               
         CLI   ISPDF,YES                                                        
         BE    XFR209A             Deal with PFD if it wants                    
         LA    R2,DESTNTRY                                                      
         USING DESTTABD,R2                                                      
         CLI   DESTMETS,EDIBXFOQ   BXF only?                                    
         BNE   XFR209A             Okay keep going                              
         MVC   PLINE(22),=CL22'Skip, BXF not found:'                            
         MVC   PLINE+22(L'XTRAMES2),XTRAMES2                                    
         BRAS  RE,PRNT                                                          
         BRAS  RE,SETPRTD          Skip report set PRINTED                      
         B     XFR210              Skip no BXF to send                          
         DROP  R2                                                               
*                                                                               
XFR209A  BRAS  RE,FMTHDR           SET UP HEADER                                
         LHI   RF,MQPMO_DEFAULT_CONTEXT                                         
         ST    RF,PUTOPTS_OPTIONS                                               
*                                                                               
         LR    R0,R4               SET LENGTH OF DATA                           
         S     R0,AMSGBUFF                                                      
         ST    R0,DATALEN                                                       
*                                                                               
         MVC   MQPUTBUF,AMSGBUFF   SET A(BUFFER)                                
         LA    R2,MQPUT                                                         
         BRAS  RE,CALLMQ                                                        
         BNE   XFR280              FAILED TO PUT MQ MESSAGE                     
         MVI   DIDPUT,YES                                                       
*                                                                               
XFR210   SAM31 ,                   CLEAR BUFFER AND RESET HEADER                
         L     R4,AMSGBUFF                                                      
         LR    R0,R4                                                            
         L     R1,MAXMSGLN                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SAM24                                                                  
*                                                                               
         CLI   ENDRPT,YES          TEST END OF PHYSICAL RPT                     
         BNE   XFR040                                                           
*                                                                               
XFR220   CLI   EMPTYRPT,YES        ANY DATA TO SEND IN REPORT?                  
         BNE   XFR230              YES                                          
*                                                                               
         BRAS  RE,SETPRTD          EMPTY REPORT SET STATUS PRINTED              
         MVC   PLINE(23),=CL23'Empty Report - report: '                         
         MVC   PLINE+24(L'XTRAMES2),XTRAMES2                                    
         MVC   PLINE+39(40),=CL40'marked printed - continuing'                  
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
XFR230   CLI   DIDPUT,NO                                                        
         BE    EXITOK                                                           
         BRAS  RE,SETPRTD          SET STATUS PRINTED                           
         LA    R2,MQCMIT                                                        
         BRAS  RE,CALLMQ                                                        
         B     EXITOK                                                           
*                                                                               
XFR250   EQU   *                                                                
         BRAS  RE,SETPRTD          SET STATUS PRINTED                           
         LA    R2,MQCMIT                                                        
         BRAS  RE,CALLMQ           SEND ONLY 5000 REPORTS                       
         MVI   FERN,11                                                          
         BRAS  RE,ERRMSG                                                        
         MVC   PLINE+10(L'XTRAMES2),XTRAMES2                                    
         MVC   PLINE+30(40),=CL40'marked printed - continuing'                  
         BRAS  RE,PRNT                                                          
         MVC   ERRMSG1R,XTRAMES2                                                
         GOTO1 VDMGR,DMCB,=C'OPMSG',('ERRMSG1Q',ERRMSG1)                        
         B     EXITOK                                                           
*                                                                               
XFR280   EQU   *                                                                
         LHI   R0,28                                                            
         BRAS  RE,SYSMESS                                                       
         GOTO1 VDMGR,DMCB,=C'OPMSG',('ERRMSG2Q',ERRMSG2)                        
         B     EXITL                                                            
*                                                                               
XFR300   CLI   DIDPUT,NO                                                        
         BE    EXITOK                                                           
         BRAS  RE,SETPRTD          SET STATUS PRINTED                           
         LA    R2,MQCMIT                                                        
         BRAS  RE,CALLMQ                                                        
         MVI   FERN,12                                                          
         BRAS  RE,ERRMSG                                                        
         MVC   PLINE+10(L'XTRAMES2),XTRAMES2                                    
         MVC   PLINE+30(40),=CL40'marked printed - continuing'                  
         BRAS  RE,PRNT                                                          
         MVC   ERRMSG3R,XTRAMES2                                                
         GOTO1 VDMGR,DMCB,=C'OPMSG',('ERRMSG3Q',ERRMSG3)                        
         B     EXITOK                                                           
                                                                                
**********************************************************************          
* Read next record of PrintQ report                                             
* BE  if have next record                                                       
* BNE if we are at end of report                                                
**********************************************************************          
READPQ   ST    RE,SVRE                                                          
         XC    R,R                                                              
         XC    RLEN,RLEN                                                        
         GOTOR VDMGR,DMCB,(X'01',READ),CFPQID,0,R,ACXREC,0                      
         L     RE,SVRE                                                          
         CLI   8(R1),0             READ NEXT CARD                               
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
ERRMSG1  DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
         DC    C'Too many logical reports - report: '                           
ERRMSG1R DS    CL(L'XTRAMES2)                                                   
ERRMSG1Q EQU   *-ERRMSG1                                                        
*                                                                               
ERRMSG2  DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
         DC    C'MQPUT FAILED!'                                                 
ERRMSG2Q EQU   *-ERRMSG2                                                        
*                                                                               
ERRMSG3  DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
         DC    C'Logical report too big - report: '                             
ERRMSG3R DS    CL(L'XTRAMES2)                                                   
ERRMSG3Q EQU   *-ERRMSG3                                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* SET REPORT STATUS TO PRINTED                                        *         
***********************************************************************         
SETPRTD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    TSTMODE,X'80'       TEST MODE ON?                                
         BZ    *+12                NO                                           
         CLI   TSTPQPUR,YES        REALLY WANT TO PURGE IT?                     
         BNE   EXITOK              NO - EXIT                                    
*                                                                               
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 VDMGR,DMCB,RANDOM,CFPQID,0,R,ACXREC                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
*                                                                               
         LA    R4,WORK             BUILD PRINT QUEUE INDEX                      
         XC    WORK,WORK                                                        
         USING UKRECD,R4                                                        
         MVC   UKSRCID,QLSRCID                                                  
         MVC   UKSUBID,QLSUBID                                                  
         MVC   UKREPNO,QLREPNO                                                  
*                                                                               
         GOTO1 VDMGR,DMCB,INDEX,CFPQID,WORK,R,ACXREC                            
         CLI   DMCB+8,0            REPORT FOUND?                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    PQS,PQS                                                          
         BZ    SETP20                                                           
*                                                                               
*THE 2ND & 3RD BYTE IN PQS FOR PQ STATUS IS IN USE YET.                         
*                                                                               
         MVC   UKINFO(1),PQS                                                    
         GOTO1 VDMGR,DMCB,CLARET,CFPQID,WORK,R,ACXREC                           
*                                                                               
         MVC   PLINE(23),=CL23'Change Class - report: '                         
         MVC   PLINE+23(L'XTRAMES2),XTRAMES2                                    
         MVC   PLINE+40(3),=CL3'( )'                                            
         MVC   PLINE+41(1),PQS                                                  
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
SETP20   GOTO1 VDMGR,DMCB,PRINTED,CFPQID,WORK,R,ACXREC                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   UKFLAG,UKFLHRS      HOURS PASSED BACK IN UKINFO                  
         MVC   UKINFO,=H'1'        SET RETAIN TIME TO ONE HOUR                  
         GOTO1 VDMGR,DMCB,RETAIN,CFPQID,WORK,R,ACXREC                           
         B     EXITOK                                                           
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET DATA FROM EDICT RECORD FOR THIS USER                            *         
* NTRY:  DUB = USERID                                                 *         
***********************************************************************         
BLDDEST  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,DESTNTRY                                                      
         USING DESTTABD,R2                                                      
*                                                                               
         LA    R5,KEY                                                           
         USING EDIKEYD,R5                                                       
         XC    EDIKEY,EDIKEY       CLEAR KEY                                    
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
         MVC   EDINAME,DUB         USER ALPHA                                   
*                                  FORCE TO READ FROM DISK                      
         GOTO1 VDMGR,DMCB,(X'24',DMRDHI),CTFILE,(R5),AIO,0                      
*                                                                               
         L     R5,AIO                                                           
         CLC   EDIKEY,KEY                                                       
         BE    BD02                                                             
*                                                                               
         MVI   FERN,10                                                          
         BRAS  RE,ERRMSG                                                        
         MVC   PLINE+10(L'RPTUID),RPTUID                                        
         BRAS  RE,PRNT                                                          
         B     EXITL                                                            
*                                                                               
BD02     L     R4,AIO                                                           
         AHI   R4,28                                                            
         XR    RF,RF                                                            
         USING EDILNKD,R4                                                       
*                                                                               
BD04     CLI   EDILNKEL,0                                                       
         JE    *+2                                                              
*                                                                               
         CLI   EDILNKEL,EDILNKEQ                                                
         BE    BD06                                                             
         IC    RF,EDILNKLN                                                      
         BXH   R4,RF,BD04                                                       
*                                                                               
BD06     MVC   DESTNAME,EDINAME    USERID                                       
         MVC   DESTMETS,EDIMETHS   METHOD OF SENDING TRANSMISSIONS              
         MVC   DESTMETR,EDIMETHR   METHOD OF RECEIVING TRANSMISSIONS            
         MVI   IS360,NO                                                         
         TM    EDIOPTS,EDIPM360    Send to PM360                                
         BZ    BD08                                                             
         MVI   IS360,YES                                                        
         MVI   XHDR,YES                                                         
*                                                                               
BD08     MVC   DESTADVN,EDIADVNO   ADV EASYLINK MAILBOX NUMBER                  
         MVC   DESTREPN,EDIREPNO   REP EASYLINK MAILBOX NUMBER                  
         MVC   DESTNJEC,EDINJEC    NJE CLASS                                    
         MVC   DESTNJEN,EDINJEN    NJE NODE                                     
         MVC   DESTNJEU,EDINJEU    NJE USERID                                   
         MVC   DESTFTPO,EDIFTPO    FTP OPERATING SYSTEM                         
         MVC   DESTFTPL,EDIFTPL    FTP REMOTE LU NAME                           
         MVC   DESTFTPU,EDIFTPU    FTP APPC USERID                              
         MVC   DESTFTPP,EDIFTPP    FTP APPC PASSWORD                            
         MVC   DESTFTPS,EDIFTPS    FTP APPC SERVER CLASS                        
         MVC   DESTFTPF,EDIFFLGS   FTP FLAGS                                    
         MVC   DESTADNA,EDIADNA    ADVANTIS ACCOUNT                             
         MVC   DESTADNU,EDIADNU    ADVANTIS USERID                              
         MVC   DESTADNC,EDIADNC    ADVANTIS CLASS                               
         MVC   DESTCOLL,EDICOLL    COLUMBINE LUID                               
         MVC   DESTCOLU,EDICOLU    COLUMBINE APPC USERID                        
         MVC   DESTCOLP,EDICOLP    COLUMBINE APPC PASSWORD                      
         MVC   DESTBDECN,SPACES                                                 
         MVC   DESTBDECN(L'EDIBDECN),EDIBDECN     BDE COMMAN NAME               
         MVC   DESTBDEOP,EDIBDEOP  BDE RECEIVER'S OPERATING SYSTEM              
         MVC   DESTBDEEN,EDIBDEEN  BDE ENCRYPTION (NONE,BLOWFISH,3DES)          
         MVC   DESTBDECM,EDIBDECM  BDE COMPRESS                                 
         MVC   DESTBDESF,EDIBDESF  BDE DELETE SENT FILE                         
         MVC   DESTBDECA,EDIBDECA  BDE CONVERT TO ASCII                         
         MVC   DESTBDECP,EDIBDECP  BDE CODE PAGE                                
         MVC   DESTBDEFN,EDIBDEFN  BDE FAILURE NOTIFICATION                     
         MVC   DESTBDEBI,EDIBDEBI  BDE BINARY DATA TRANSFER                     
         MVC   DESTUIDN,RPTUIDNO                                                
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* READ ID RECORD GIVEN NUMBER AND EXTRACT NAME                        *         
* NTRY:  R0    = ID NUMBER                                            *         
* EXIT:  DUB   = ID NAME OR ZEROS IF NOT FOUND                        *         
***********************************************************************         
GETIDN   NTR1  BASE=*,LABEL=*                                                   
         XC    DUB,DUB                                                          
         XC    SVAGOPTS,SVAGOPTS                                                
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
X        USING CTIREC,KEY                                                       
         MVI   X.CTIKTYP,CTIKTYPQ                                               
         STCM  R0,3,X.CTIKNUM                                                   
         DROP  X                                                                
*                                                                               
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
         CLI   8(R1),0             USERID NOT FOUND                             
         BNE   EXITL                                                            
*                                                                               
         L     R4,AIO                                                           
         AHI   R4,28                                                            
         XR    RF,RF                                                            
*                                                                               
GIDN02   DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    GIDN03                                                           
*                                                                               
         CLI   0(R4),CTDSCELQ      (X'02') DESCRIPTION ELEMENT                  
         BNE   *+10                                                             
         MVC   DUB,CTDSC-CTDSCD(R4) RETURN USER-ID NAME                         
*                                                                               
         CLI   0(R4),CTAGYELQ      (X'06') AGY ALPHA ID ELEMENT                 
         BNE   *+10                                                             
         MVC   AGYALPHA,CTAGYID-CTAGYD(R4)                                      
*                                                                               
         IC    RF,1(R4)            ELEMENT LENGTH                               
         BXH   R4,RF,GIDN02                                                     
                                                                                
***********************************************************************         
* READ SYSTEM ACCESS RECORD, AND SAVE AGENCY OPTIONS FLAG, IF PRESENT           
***********************************************************************         
GIDN03   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5REC,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYALPHA                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
         CLI   8(R1),0                                                          
         JNE   *+2                 ACCESS RECORD NOT FOUND                      
*                                                                               
         L     R4,AIO                                                           
         AHI   R4,28                                                            
         XR    RF,RF                                                            
*                                                                               
GIDN04   DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    EXITOK                                                           
*                                                                               
         CLI   0(R4),CTAGDELQ      AGY GROUP DETAIL ELEM? (X'B4')               
         BNE   *+10                                                             
         MVC   SVAGOPTS,CTAGOPTS-CTAGDD(R4)                                     
*                                                                               
         IC    RF,1(R4)            ELEMENT LENGTH                               
         BXH   R4,RF,GIDN04                                                     
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* THIS IS AN INVALID REPORT - SET AS PRINTED SO YOU CAN CONTINUE      *         
***********************************************************************         
MARKBAD  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETPRTD          UNKNOWN USER ID MARK REPORT PRINTED          
         MVC   PLINE(23),=CL23'Userid error - report: '                         
         MVC   PLINE+23(L'XTRAMES2),XTRAMES2                                    
         MVC   PLINE+39(40),=CL40'marked printed - continuing'                  
         BRAS  RE,PRNT                                                          
*                                                                               
         LHI   R0,25                OUTPUT ERROR TO CONSOLE                     
         BRAS  RE,SYSMESS                                                       
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ ONE LOGICAL REPORT FROM A PRINT QUEUE REPORT                   *         
* BUILD REPORT IN MSGBUFF READY TO PUT TO MQ                          *         
***********************************************************************         
FMTHDR   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,DESTNTRY                                                      
         USING DESTTABD,R2                                                      
*                                                                               
         BRAS  RE,CHKBDESN         RETURN BDE SENDER # BASE ON USERID#          
*                                                                               
         SAM31                                                                  
         L     R4,AMSGBUFF                                                      
         USING MSGHDRD,R4                                                       
         MVC   MSGID,=CL16' '                                                   
*                                                                               
         CLC   =C'EDICT=',DESTINAT KEY OF EDICT RECORD GIVEN?                   
         BNE   FMT10                                                            
         CLI   DESTMETS,EDIPDFQ    PDF                                          
         BNE   FMT05                                                            
         MVC   MSGID,=CL16'SENDPDF*********'                                    
*        MVC   MSGID+14(1),DSPACE  DSPACE                                       
         B     FMT20                                                            
*                                                                               
FMT05    CLI   DESTMETR,EDIBDFQ    BDE - FTP                                    
         BNE   FMT08                                                            
         MVC   MSGID,=CL16'BDEFTP_*********'                                    
         MVC   MSGID+7(1),DSPACE                                                
FMT08    MVC   MSGID+8(1),BDESN    RETURN VALUE FROM CHKBDESN                   
         B     FMT20                                                            
*                                                                               
FMT10    CLI   DESTMETS,EDIBXFOQ   BXF Only                                     
         BNE   FMT11                                                            
         MVC   MSGID,=CL16'SEND___*********'                                    
         MVC   MSGID+7(1),DSPACE                                                
         MVI   ANYFAX,NO                                                        
         CLI   ISPDF,YES                                                        
         BNE   FMT10B                                                           
         MVC   MSGID+8(3),=C'PDF' Send copy to Optica (PDF)                     
         MVI   ANYFAX,YES                                                       
*                                                                               
FMT10B   CLI   IS360,YES                                                        
         BNE   FMT10C                                                           
         MVC   MSGID+11(3),=C'360' Send BXF to PM360                            
         MVI   ANYFAX,YES                                                       
*                                                                               
FMT10C   CLI   ANYFAX,YES                                                       
         JE    FMT20                                                            
         DC    H'00'            Not sure how we got here                        
*                                                                               
FMT11    CLI   DESTMETS,EDIECNQ   ECN                                           
         BNE   FMT12                                                            
         MVC   MSGID,=CL16'ECNFAX_*********'                                    
         MVC   MSGID+7(1),DSPACE                                                
         CLI   ISPDF,YES                                                        
         BNE   *+10                                                             
         MVC   MSGID+8(3),=C'PDF' Send copy to Optica (PDF)                     
         CLI   HASBXF,YES                                                       
         BNE   FMT20              Can't send if no BXF included                 
         CLI   IS360,YES                                                        
         BNE   *+10                                                             
         MVC   MSGID+11(3),=C'360' Send copy to PM360Q                          
         B     FMT20                                                            
*                                                                               
FMT12    CLI   DESTMETS,EDIEASYQ   EASYLINK    (No longer supported)            
         BNE   FMT14                                                            
         MVC   MSGID,=CL16'FAXING_*********'                                    
         MVC   MSGID+7(1),DSPACE                                                
         B     FMT20                                                            
*                                                                               
FMT14    CLI   DESTMETS,EDIEXRRQ   Extreme Reach                                
         BNE   FMT16                                                            
         MVC   MSGID,=CL16'XRRFAX_*********'                                    
         MVC   MSGID+7(1),DSPACE                                                
         CLI   ISPDF,YES                                                        
         BNE   *+10                                                             
         MVC   MSGID+11(3),=C'PDF' Send copy to Optica (PDF)                    
         B     FMT20                                                            
*                                                                               
FMT16    CLI   DESTMETS,EDIENCOQ   ENCODA                                       
         JNE   *+2                                                              
         MVC   MSGID,=CL16'ENCODA**********'                                    
*                                                                               
FMT20    MVC   MSGUSR,RPTUID       OUTPUT REPORT DETAILS                        
         GOTO1 VHEXO31,DMCB,RPTUIDNO,DUB,L'RPTUIDNO,00                          
         MVC   MSGUSRNO,DUB                                                     
         MVC   MSGPQSUB,RPTSUBID                                                
         GOTO1 VHEXO31,DMCB,RPTREFNO,MSGPQNUM,L'RPTREFNO,0                      
         GOTO1 VHEXO31,DMCB,RPTDTTM,MSGHTIME,L'RPTDTTM,0                        
         GOTO1 VHEXO31,DMCB,RPTLOGNO,MSGLOGNO,L'RPTLOGNO,0                      
         GOTO1 VHEXO31,DMCB,RPTPQTYP,MSGPQTYP,L'RPTPQTYP,0                      
*                                                                               
         LHI   R0,MSGHDRLQ                                                      
         CLI   XHDR,NO                                                          
         BE    FMT90                                                            
         MVC   MSGDSPCE,DSPACE                                                  
         MVC   MSGSECAG,RPTSECAG                                                
         GOTO1 VHEXO31,DMCB,RPTPIDNO,MSGPERID,L'RPTPIDNO,0                      
         MVC   MSGGRPID,SAVGRPID   Save value in GroupId                        
         MVI   MSGEOR,NO                                                        
         CLI   ENDRPT,YES                                                       
         BNE   *+8                                                              
         MVI   MSGEOR,YES                                                       
         LHI   R0,MSGHDLQ2                                                      
*                                                                               
FMT90    CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGHLEN,DUB         SET HEADER LENGTH AS CL4                     
         B     EXITOK                                                           
         DROP  R2,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*CHECK THE BDE SENDER NUMBER BASED ON THE PQ USER ID NUMBER           *         
***********************************************************************         
CHKBDESN NTR1  BASE=*,LABEL=*                                                   
         MVI   BDESN,C'*'          ASSUME NO BDE SENDER # ASSIGNED              
         OC    BDESNDRS,BDESNDRS   ANY INPUT BDE SENDER #?                      
         BZ    CBNX                NO - EXIT                                    
*                                                                               
         CLC   =C'ECR',RPTSUBID    PQ SUBID=ECR FOR REP EC                      
         BE    CBN08                                                            
         MVI   BDESN,C'*'          NOT ECR, ASSIGN TO DEFAULT SENDER#1          
         B     CBNX                                                             
CBN08    DS    0H                                                               
*                                                                               
         L     RF,ABDESOV          BDE SENDER OVERRIDE TABLE                    
CBN10    OC    0(2,RF),0(RF)       ANY MORE ENTRIES?                            
         BZ    CBN50               NO - HASH USERID#                            
         CLC   RPTUID,1(RF)        USERID MATCHES?                              
         BE    *+12                                                             
         AHI   RF,L'BDESOV                                                      
         B     CBN10                                                            
*                                                                               
         MVC   BDESN,0(RF)         MOVE IN BDE SENDER OVERRIDE #                
         B     CBNX                                                             
*                                                                               
CBN50    SR    RE,RE               PREPARE FOR DIVIDE                           
         SR    RF,RF                                                            
         ICM   RF,3,RPTUIDNO                                                    
         D     RE,BDESNDRS         RE=REMAINDER                                 
         AHI   RE,1                + 1                                          
         STC   RE,BDESN                                                         
         OI    BDESN,X'F0'         BINARY CONVERT TO CHAR# (MAX#=9)             
*                                                                               
CBNX     B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         LR    R0,RC                                                            
         LHI   R1,WORKL                                                         
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR ALL THAT LOVELY W/S                    
*                                                                               
         ZAP   LINE,P99                                                         
         MVC   TITLE,VCTITLE                                                    
*                                                                               
         LHI   R0,1                                                             
         BRAS  RE,SYSMESS          STARTING INITIALISE                          
*                                                                               
         BRAS  RE,GETCARDS         READ IN INPUT CARDS                          
         BL    EXITL                                                            
         BRAS  RE,VALCARDS         VALIDATE INPUT CARDS                         
         BL    EXITL                                                            
*                                                                               
         ZAP   LINE,P99                                                         
         MVC   TITLE,DWTITLE                                                    
*                                                                               
         BRAS  RE,BLDSRTBL                                                      
*                                                                               
         BRAS  RE,MQINIT                                                        
         BL    EXITL                                                            
         BRAS  RE,SETOPS           SET UP OPERATOR INPUT                        
         BL    EXITL                                                            
         BRAS  RE,BLDPQTAB                                                      
         BL    EXITL                                                            
         BRAS  RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
*                                                                               
**       MVC   SUBFILT,=CL3'MQS'       <<<<<<<DEBUG                             
*                                                                               
         L     R0,MAXMSGLN                                                      
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    INIT02                                                           
         LHI   R0,21                                                            
         BRAS  RE,SYSMESS          GETMAIN FAILED                               
         ABEND 911,DUMP                                                         
*                                                                               
INIT02   ST    R1,AMSGBUFF                                                      
         L     R0,MAXMSGLN                                                      
         AR    R1,R0                                                            
         ST    R1,AMSGBUFX                                                      
*                                                                               
INITX    LHI   R0,2                                                             
         BRAS  RE,SYSMESS          COMPLETED INITIALISE                         
         B     EXITOK                                                           
*                                                                               
VCTITLE  DC    CL(L'TITLE)'Input cards to EDI Scheduler'                        
DWTITLE  DC    CL(L'TITLE)'EDI Scheduler output log'                            
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF PRINT QUEUE REPORTS THAT NEED TRANSFERRED             *         
***********************************************************************         
BLDPQTAB NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,26                                                            
         BRAS  RE,SYSMESS                                                       
*                                                                               
         GOTO1 VDMGR,DMCB,GLIST,PRTQUE,AIO,0,ACXREC                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         L     R4,32(R4)           SAVE A(PRINT QUEUE LIST)                     
         XR    R3,R3                                                            
         IC    R3,0(R4)            NUMBER OF PRINT QUEUES                       
         SR    R2,R2               REPORT COUNTER                               
         L     R6,ACITABL          A(PRINT QUEUE TABLE)                         
*                                                                               
BP10     LA    R4,8(R4)            BUMP TO NEXT PRINT QUEUE                     
         XC    0(CITBLLNQ,R6),0(R6)                                             
         MVC   DUB(4),=C'PRTQ'     CONSTRUCT PRINT QUEUE NAME                   
         MVC   DUB+4(1),1(R4)                                                   
         MVC   CFPQENUM,4(R4)                                                   
         GOTO1 VDMGR,DMCB,BUFFER,DUB,0,0,ACXREC                                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,ACXREC           EXTRACT AND SAVE PQ FILE DATA                
         MVC   CIDATA,12(R1)                                                    
         L     RF,VENQDEQ          V(DMENQDEQ)                                  
         ST    RF,CIENQDEQ                                                      
******************for backward compatible**********************                 
         LHI   RF,24               LENGTH OF OLD PQ KEY                         
         CLI   CIDATA+16,0                                                      
         BNE   *+8                                                              
         LHI   RF,40               LENGTH OF NEW PQ KEY                         
         STH   RF,CINDXLN                                                       
         MVC   CFPQID,DUB                                                       
         MVC   CFPQINUM,0(R4)      PQ FILE INTERNAL NUMBER                      
******************for backward compatible**********************                 
         L     RF,8(R1)            DISPLACEMENT TO PQ SAVE AREA                 
         A     RF,ACXREC                                                        
         ST    RF,APQSAVE          A(PQ SAVE AREA)                              
*                                                                               
         LA    R6,CITBLLNQ(R6)     BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,BP10             BACK FOR NEXT PRINT QUEUE FILE               
*                                                                               
         MVI   0(R6),X'FF'         MARK END OF FILE LIST                        
*                                                                               
         LHI   R0,27                                                            
         BRAS  RE,SYSMESS                                                       
         B     EXITOK                                                           
         EJECT                                                                  
*&&DO                                                                           
*AH3 moved to EDIXRR                                                            
***********************************************************************         
* Extreem Reach XML Summary report                                              
* Build list of Unique Fax ids to send ER a report                    *         
***********************************************************************         
XRRRPT   NTR1  BASE=*,LABEL=*                                                   
         STC   R1,XMLMODE                                                       
         SAM31                                                                  
         CHI   XMLMODE,1           Initialize                                   
         BNE   XRRRPT20                                                         
         OC    @SXRRRPT,@SXRRRPT                                                
         BNZ   XRRRPT10                                                         
         L     R3,MAXRPTLN         Build area for report                        
         LR    R4,R3                                                            
*                                                                               
         STORAGE OBTAIN,LENGTH=(3),LOC=ANY,BNDRY=PAGE                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(16,R1),=C'*XRRRPT**XRRRPT*'                                    
         LA    R1,16(R1)           BUMP PAST LABEL, COUNTER, MAX#               
         ST    R1,@SXRRRPT         A(Start of report buffer)                    
         AR    R4,R1                                                            
         ST    R4,XMLBUFSZ         Size of report buffer                        
         ST    R4,@EXRRRPT         A(End of report buffer)                      
*                                                                               
XRRRPT10 L     RE,@SXRRRPT                                                      
         ST    RE,@CXRRRPT         Current start location                       
         L     R1,XMLBUFSZ                                                      
         LR    RE,R1               CLEAR TABLE                                  
         LR    RF,R4                                                            
         XCEFL                                                                  
*                                                                               
         L     R3,@CXRRRPT                                                      
         GOTOR XMLTAG,XMLPARM,=C'message',STARTAG                               
         GOTOR XMLTAGS,XMLPARM,=C'moSystem',=C'DS'                              
         GOTOR XMLTAGS,XMLPARM,=C'hostEnvironment',(L'SYS,SYS)                  
         GOTOR XMLTAGS,XMLPARM,=C'tenant',(L'AGYALPHA,AGYALPHA)                 
         GOTOR XMLTAGS,XMLPARM,=C'client',(L'CLIENT,CLIENT)                     
         GOTOR XMLTAGS,XMLPARM,=C'hostUniqueReference',                +        
               (L'REFNUM,REFNUM)                                                
         GOTOR XMLTAGS,XMLPARM,=C'trafficEnvironment',(L'ENV,ENV)               
         GOTOR XMLTAGS,XMLPARM,=C'uuid',NODATAQ                                 
         GOTOR XMLTAG,XMLPARM,=C'key',ENDTAG                                    
         GOTOR XMLTAG,XMLPARM,=C'requestPayload',STARTAG                        
         GOTOR XMLTAG,XMLPARM,=C'report',STARTAG                                
         GOTOR XMLTAG,XMLPARM,=C'reportType',=C'instructions'                   
         GOTOR XMLTAG,XMLPARM,=C'groupid',GROUPID                               
         GOTOR XMLTAG,XMLPARM,=C'groupList',GROUPID                             
         ST    R3,@CXRRRPT                                                      
         J     EXITOK                                                           
*                                                                               
XRRRPT20 CLI   XMLMODE,2           Add unique id                                
         JEE   XRRRPT40                                                         
         GOTOR XMLTAG,XMLPARM,=C'id',RPTID                                      
         ST    R3,@CXRRRPT                                                      
*                                                                               
XRRRPT40 CLI   XMLMODE,3           Finish up and put                            
         JNE   XRRRPTX                                                          
         GOTOR XMLTAG,XMLPARM,=C'groupList',ENDTAG                              
         GOTOR XMLTAG,XMLPARM,=C'report',ENDTAG                                 
         GOTOR XMLTAG,XMLPARM,=C'requestPayload',ENDTAG                         
         GOTOR XMLTAG,XMLPARM,=C'message',ENDTAG                                
         ST    R3,@CXRRRPT                                                      
*                                                                               
XRRRPTX  J     EXITOK                                                           
         LTORG                                                                  
                                                                                
***********************************************************************         
* Remove spaces at end of value                                                 
***********************************************************************         
REMSPACE CLI   0(R3),C' '          Remove space                                 
         JH    REMSPACX                                                         
         BRCT  R3,REMSPACE                                                      
*                                                                               
REMSPACX AHI   R3,1                Back to space                                
         BR    RE                                                               
                                                                                
***********************************************************************         
* Format single XML tag                                                         
* P1 = tag                                                                      
* P2 = 0 start tag                                                              
*      1 end   tag                                                              
***********************************************************************         
XMLTAG   STM   RE,R2,XMLREGS                                                    
         MVI   0(R3),C'<'          Open < for XML tag                           
         AHI   R3,1                                                             
         CLI   7(R1),1             P2=0 start, P2=1 end                         
         JL    XMLS10                                                           
         JH    *+2                 Invalid call                                 
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
*                                                                               
XMLS10   LLC   RF,0(R1)            Length of tag                                
         BCTR  RF,0                                                             
         L     RE,0(,R1)                                                        
         NILH  GRE,X'00FF'                                                      
         EXRL  RF,EX_TAG                                                        
         LA    R3,1(RF,R3)         Increment R3                                 
         MVI   0(R3),C'>'          Close tag                                    
         AHI   R3,1                                                             
         LM    RE,R2,XMLREGS                                                    
         BSM   0,RE                                                             
                                                                                
***********************************************************************         
* Format XML tags and field                                                     
***********************************************************************         
XMLTAGS  STM   RE,R2,XMLREGS                                                    
         MVI   0(R3),C'<'          Open < for XML tag                           
         AHI   R3,1                                                             
         LLC   RF,0(R1)            Length of tag                                
         BCTR  RF,0                                                             
         L     RE,0(,R1)                                                        
         NILH  GRE,X'00FF'                                                      
         EXRL  RF,EX_TAG                                                        
         LA    R3,1(RF,R3)         Increment R3                                 
         MVI   0(R3),C'>'          Close tag                                    
         AHI   R3,1                                                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,4(R1)          Length of field                              
         JZ    XMLT62              No length then just end XML                  
         BCTR  RF,0                                                             
         L     RE,4(,R1)                                                        
         NILH  GRE,X'00FF'         Clear HOB                                    
*                                                                               
         LARL  R2,XMLTRTAB                                                      
         EXRL  RF,TRTXML           Replace &, <, > with control char            
         JZ    XMLT50              Good as is                                   
         AHI   RF,1                                                             
XMLT05   MVC   0(1,R3),0(RE)       Move one character at a time                 
*                                                                               
         CLI   0(R1),C'&&'                                                      
         JNE   XMLT10                                                           
         MVC   0(4,R3),=C'&&amp'                                                
         AHI   R3,4                                                             
         J     XMLT30                                                           
*                                                                               
XMLT10   CLI   0(R1),C'<'                                                       
         JNE   XMLT20                                                           
         MVC   0(3,R3),=C'&&lt'                                                 
         AHI   R3,3                                                             
         J     XMLT30                                                           
*                                                                               
XMLT20   CLI   0(R1),C'>'                                                       
         JNE   XMLT30                                                           
         MVC   0(3,R3),=C'&&gt'                                                 
         AHI   R3,3                                                             
*                                                                               
XMLT30   AHI   R3,1                                                             
         AHI   R1,1                                                             
         JCT   RF,XMLT05           Next character                               
         J     XMLT60                                                           
*                                                                               
XMLT50   DS    0H                                                               
*        LLC   RF,4(R1)            Length of field                              
*        BCTR  RF,0                                                             
         EXRL  RF,EX_XML                                                        
         LA    R3,1(RF,R3)                                                      
*                                                                               
XMLT60   BRAS  RE,REMSPACE                                                      
XMLT62   MVI   0(R3),C'<'          End XML tag                                  
         MVI   1(R3),C'/'                                                       
         AHI   R3,2                                                             
         L     RE,0(,R1)           Reload Tag address                           
         NILH  GRE,X'00FF'                                                      
         LLC   RF,0(,R1)           Tag length                                   
         BCTR  RF,0                                                             
         EXRL  RF,EX_TAG           Move in tag                                  
         LA    R3,1(RF,R3)         Increment R3                                 
         MVI   0(R3),C'>'          Close tag                                    
         AHI   R3,1                                                             
*                                                                               
         LM    RE,R2,XMLREGS                                                    
         BSM   0,RE                                                             
         LTORG                                                                  
*                                                                               
XMLMODE  DS    X                                                                
STARTAG  EQU   0                                                                
ENDTAG   EQU   1                                                                
NODATAQ  EQU   0                                                                
*TRNSLTE  TR    0(0,RE),CHARTAB                                                 
TRTXML   TRT   0(0,R3),0(R2)                                                    
EX_TAG   DS    0H                                                               
EX_XML   MVC   0(0,R3),0(RE)                                                    
XMLREGS  DS    5F                               RE,RF,R0,R1                     
***********************************************************************         
* &, >, < are invalid for XML. Need to replace with                             
* (all are followed by a semicolon which I can not type in this editor          
* &amp                                                                          
* &GT                                                                           
* &LT                                                                           
***********************************************************************         
XMLTRTAB DC    XL256'00'                                                        
         ORG   XMLTRTAB+C'&&'                                                   
         DC    X'FF'                                                            
         ORG   XMLTRTAB+C'>'                                                    
         DC    X'FF'                                                            
         ORG   XMLTRTAB+C'<'                                                    
         DC    X'FF'                                                            
         ORG                                                                    
         DROP  RB                                                               
*&&                                                                             
***********************************************************************         
* INITIALISE MQ QUEUES                                                *         
* NOTE: IN A DELIBERATE ATTEMPT TO SIMPLIFY THE NAMES OF THE MQ       *         
*       QUEUE MANAGER, THE MASTER QUEUE AND LOG QUEUES ARE ALL HARD   *         
***********************************************************************         
MQINIT   NTR1  BASE=*,LABEL=*                                                   
         MVI   CONOK,NO                                                         
         LHI   R0,9                                                             
         BRAS  RE,SYSMESS          BEGINNING MQ INITIALISE                      
*                                                                               
         TM    TSTMODE,X'80'       TEST MODE?                                   
         BZ    MQINI20             NO                                           
         MVC   QMGR,TSTMQMGR       USE TEST MQ Q MGR/QUEUE                      
         MVC   QMASTER,TSTMQQUE                                                 
         B     MQINI80                                                          
*                                                                               
MQINI20  MVC   QMASTER,QMASTERT                                                 
*&&DO*&& MVC   SYS,=C'TST'                                                      
*&&DO*&& MVC   ENV,=C'DEV'                                                      
         CLI   DSPACE,C'T'                                                      
         BE    MQINI80                                                          
*&&DO*&& MVC   SYS,=C'FQA'                                                      
*&&DO*&& MVC   ENV,=C'QA1'                                                      
         MVC   QMASTER,QMASTERQ                                                 
         CLI   DSPACE,C'Q'                                                      
         BE    MQINI80                                                          
*&&DO*&& MVC   SYS,=C'CSC'                                                      
*&&DO*&& MVC   ENV,=C'YDY'                                                      
         MVC   QMASTER,QMASTERC                                                 
         CLI   DSPACE,C'C'                                                      
         BE    MQINI80                                                          
*&&DO*&& MVC   SYS,=C'REP'                                                      
*&&DO*&& CLI   DSPACE,C'R'                                                      
*&&DO*&& MVC   ENV,=C'PRD'                                                      
*&&DO*&& BE    *+10                                                             
*&&DO*&& MVC   SYS,=C'ADV'                                                      
         MVC   QMASTER,QMASTERP                                                 
*                                                                               
MQINI80  MVC   MAINQ_OBJECTNAME,QMASTER                                         
         MVC   PLINE(14),=C'MQ Queue Mgr: '                                     
         MVC   PLINE+15(L'QMGR),QMGR                                            
         BRAS  RE,PRNT                                                          
         MVC   PLINE(14),=C'MQ Queue Name:'                                     
         MVC   PLINE+15(L'MAINQ_OBJECTNAME),MAINQ_OBJECTNAME                    
         BRAS  RE,PRNT                                                          
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF EXTERNAL ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,AMQCT                                                         
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,AMQOPENQ                                                      
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,AMQCMIT                                                       
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,AMQCLOSE                                                      
         LOAD  DE=CSQBDISC                                                      
         ST    R0,AMQDISC                                                       
         LOAD  DE=CSQBPUT                                                       
         ST    R0,AMQPUT                                                        
*                                                                               
         LA    R2,MQCT                                                          
         BRAS  RE,CALLMQ           CONNECT TO MQ                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,11                                                            
         BRAS  RE,SYSMESS          CONNECTED TO MQ QMGR                         
*                                                                               
         LHI   RF,MQOO_OUTPUT                                                   
         ST    RF,OPN_OPTS                                                      
         LA    R2,MQOPENQ                                                       
         BRAS  RE,CALLMQ           OPEN MASTER QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,12                                                            
         BRAS  RE,SYSMESS          OPENED MASTER QUEUE                          
*                                                                               
         LHI   R0,10                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ INITIALISE                      
         MVI   CONOK,YES                                                        
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISCONNECT FROM MQ CLEANLY                               *         
***********************************************************************         
MQFREE   NTR1  BASE=*,LABEL=*                                                   
         CLI   CONOK,YES                                                        
         BNE   EXITOK                                                           
*                                                                               
         LHI   R0,14                                                            
         BRAS  RE,SYSMESS          OPENED MASTER QUEUE                          
*                                                                               
*??      LHI   RF,??                                                            
         XR    RF,RF                                                            
         ST    RF,CLS_OPTS                                                      
         LA    R2,MQCLOSE                                                       
         BRAS  RE,CALLMQ           OPEN MASTER QUEUE                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,16                                                            
         BRAS  RE,SYSMESS          CLOSED MQ MASTER QUEUE                       
         LA    R2,MQDISC                                                        
         BRAS  RE,CALLMQ           DISCONNECT MQ QMGR                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LHI   R0,15                                                            
         BRAS  RE,SYSMESS          COMPLETED MQ DEALLOCATION                    
         MVI   CONOK,NO                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL MQ                                                  *         
* NTRY: R2          = A(PARAMETER STRUCTURE)                          *         
* EXIT: MQ_CC       = MQ COMPLETION CODE                              *         
*       MQ_RC       = MQ REASON CODE                                  *         
*       CC SET EQ     WHEN CALL OKAY                                  *         
***********************************************************************         
CALLMQ   NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
*                                                                               
         L     RF,16(R2)           RF = A(MQ ROUTINE)                           
         LA    R3,20(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         SAM24                                                                  
         CLC   MQ_CC,=A(MQCC_OK)                                                
         BNE   CMQ02                                                            
*                                                                               
         CLI   TRACE,YES                                                        
         BNE   EXITOK                                                           
         MVI   PLINE,C'+'          '+' MEANS IT'S AN MQ CALL                    
         MVC   PLINE+1(16),0(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+30(12),=C'Completed ok'                                    
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
CMQ02    MVI   PLINE,C'+'                                                       
         MVC   PLINE+1(16),0(R2)                                                
         MVC   PLINE+20(09),=C'**ERROR**'                                       
         MVC   PLINE+30(08),=CL08'Warning '                                     
         CLC   MQ_CC,=A(MQCC_WARNING)                                           
         BE    CMQ06                                                            
         MVC   PLINE+30(08),=CL08'Failed  '                                     
         CLC   MQ_CC,=A(MQCC_FAILED)                                            
         BE    CMQ06                                                            
         MVC   PLINE+30(08),=CL08'Unknown '                                     
         EDIT  MQ_CC,(7,PLINE+38),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
CMQ06    MVC   PLINE+46(3),=C'RC='                                              
         EDIT  MQ_RC,(5,PLINE+49),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
         L     RF,AWHY                                                          
CMQ08    CLI   0(RF),X'FF'         SEE IF WE HAVE TEXT FOR THE PROBLEM          
         BE    CMQ10                                                            
         CLC   MQ_RC,0(RF)                                                      
         BE    *+12                                                             
         AHI   RF,28                                                            
         B     CMQ08                                                            
*                                                                               
         MVC   PLINE+60(24),4(RF)                                               
*                                                                               
CMQ10    BRAS  RE,PRNT                                                          
         B     EXITL                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
SETOPS   NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,7                                                             
         BRAS  RE,SYSMESS          BEGINNING SETTING OPERATOR COMMS             
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         MVC   ECBLST,AOPERECB                                                  
*                                                                               
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
*                                                                               
         LHI   R0,8                                                             
         BRAS  RE,SYSMESS          COMPLETED SETTING OPERATOR COMMS             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ IN ALL INPUT CARDS                                             *         
* FORMAT INTO A SCANNER BLOCK AND SAVE FOR VALIDATION                 *         
***********************************************************************         
GETCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,3                                                             
         BRAS  RE,SYSMESS          STARTING READING INPUT CARDS                 
*                                                                               
         XC    TSTMODE,TSTMODE     CLEAR TEST MODE FLAG                         
         XC    TSTPQALL,TSTPQALL   CLEAR TEST PQ SCAN ALL OPTION                
         XC    TSTPQPUR,TSTPQPUR   CLEAR TEST PQ PURGE OPTION                   
         XC    TSTMQMGR,TSTMQMGR   CLEAR TEST MQ Q MGR                          
         XC    TSTMQQUE,TSTMQQUE   CLEAR TEST MQ QUEUE                          
*                                                                               
         L     R3,ASCANTAB         SAVE IN SCANTAB FOR PROCESSING               
         USING SCANBLKD,R3                                                      
*                                                                               
GCD02    GOTO1 VCARDS,PLIST,CARDIO,=C'RE00'                                     
         MVC   PLINE(L'CARDIO),CARDIO                                           
         BRAS  RE,PRNT             ALWAYS PRINT THE LINE                        
*                                                                               
         CLI   CARDIO,C'*'         IGNORE COMMENTS                              
         BE    GCD02                                                            
         CLC   =C'/*',CARDIO       END OF CARDS?                                
         BE    GCDX                YES                                          
*                                                                               
         CLC   =C'TEST:',CARDIO    CHECK FOR SPECIAL TEST CARDS                 
         BNE   *+12                                                             
         BRAS  RE,CHKTCRD          GET THE SPECIAL TEST CARDS                   
         B     GCD02               NEXT CARD                                    
*                                                                               
         GOTO1 VSCANNER,PLIST,(C'C',CARDIO),((R3))                              
         XR    RF,RF                                                            
         ICM   RF,1,4(R1)          RF=NUMBER OF INPUT PARAMETERS                
         BZ    GCD04                                                            
*                                                                               
         LH    R0,PARMCNT          INCREMENT NUMBER OF PARAMETERS               
         AR    R0,RF                                                            
         CHI   R0,PARMCNTQ                                                      
         BH    GCD06                                                            
         STH   R0,PARMCNT                                                       
*                                                                               
         MHI   RF,SCBLKLQ          GO TO NEXT FREE SLOT IN SCANTAB              
         AR    R3,RF                                                            
         B     GCD02                                                            
                                                                                
GCD04    MVI   FERN,04             INVALID FORMAT TO PARAMETER CARD             
         BRAS  RE,ERRMSG                                                        
         B     EXITL                                                            
*                                                                               
GCD06    MVI   FERN,05             TOO MANY PARAMETERS                          
         BRAS  RE,ERRMSG                                                        
         B     EXITL                                                            
*                                                                               
GCDX     LHI   R0,4                                                             
         BRAS  RE,SYSMESS          COMPLETED READING INPUT CARDS                
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS IN SCANNER BLOCK                           *         
***********************************************************************         
VALCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,5                                                             
         BRAS  RE,SYSMESS          STARTING VALIDATE INPUT CARDS                
         MVI   GOTERR,NO                                                        
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,3,PARMCNT                                                     
         BZ    EXITOK                                                           
*                                                                               
VCD02    L     R3,ASCANTAB                                                      
         USING SCANBLKD,R3                                                      
*                                                                               
VCD04    XR    RF,RF               RECONSTRUCT INPUT PARAMETER                  
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    *+10                                                             
         MVC   PLINE(0),SC1STFLD                                                
         LA    RE,PLINE+1(RF)                                                   
*                                                                               
         CLI   SC2NDLEN,0          ANY SECOND HALF TO PARAMETER                 
         BE    VCD06                                                            
         MVI   0(RE),C'='                                                       
         MVC   1(L'SC2NDFLD,RE),SC2NDFLD                                        
*                                                                               
VCD06    BRAS  RE,PRNT             PRINT RECONSTRUCTED PARAMETER                
*                                                                               
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         USING CARDTABD,R4                                                      
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         LENGTH OF PARAMETER                          
         BCTR  RF,0                                                             
*                                                                               
VCD08    CLI   0(R4),255           END OF TABLE?                                
         BNE   *+16                                                             
         MVI   FERN,01             FLAG UNKNOWN PARAMETER CARD                  
         BRAS  RE,ERRMSG                                                        
         B     VCD14                                                            
*                                                                               
         CLM   RF,1,CMIN           CHECK LENGTHS ARE OK                         
         BL    VCD10                                                            
         CLM   RF,1,CMAX                                                        
         BH    VCD10                                                            
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD12                                                            
         CLC   SC1STFLD(0),CARDTXT                                              
*                                                                               
VCD10    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD08                                                            
*                                                                               
VCD12    ICM   RF,15,CARDVAL       GO AND VALIDATE THIS INPUT                   
         BASR  RE,RF                                                            
         BE    *+8                                                              
         BRAS  RE,ERRMSG           PRINT ERROR MESSAGE                          
*                                                                               
VCD14    AHI   R3,SCBLKLQ          NEXT LINE IN SCANTAB                         
         BCT   R2,VCD04            ANY MORE PARMS INPUT?                        
*                                                                               
         ZAP   LINE,P99            FORCE PAGE THROW                             
*                                                                               
         LHI   R0,6                                                             
         BRAS  RE,SYSMESS          ENDED VALIDATE INPUT CARDS                   
*                                                                               
         CLI   GOTERR,YES          SET CC BASED ON ERRORS                       
         BNE   EXITOK                                                           
         B     EXITL                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DSPACE= CARD                                          *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VCDSPACE NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,1                                                       
         BE    *+12                                                             
         MVI   FERN,02                                                          
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,ASSB                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),SC2NDFLD                          
*                                                                               
         MVC   DSPACE,SSODSPAC-SSOOFF(RF)                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF DDSIO= CARD                                           *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCDDSIO  NTR1  BASE=*,LABEL=*                                                   
         CLI   SC2NDLEN,8                                                       
         BNH   *+12                                                             
         MVI   FERN,03                                                          
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,VDDSIO                                                     
         MVC   0(8,RF),SC2NDFLD                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF TRACE= CARD                                           *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCTRACE  NTR1  BASE=*,LABEL=*                                                   
         MVC   TRACE,SC2NDFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF QMGR= OVERRIDE CARD (OVERRIDES MQ1P DEFAULT CL10 MAX) *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCQMGR   NTR1  BASE=*,LABEL=*                                                   
         MVC   QMGR(L'SC2NDFLD),SC2NDFLD                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF MQQMGRNAME= OVERRIDE CARD (MAX 48)                    *         
***********************************************************************         
VCQNAME  NTR1  BASE=*,LABEL=*                                                   
         MVC   QMASTER,CARDIO+11   OVERRIDE QUEUE MANAGER                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF WAITSECS CARD - CONTROLS PQ SCAN RATE                 *         
***********************************************************************         
VCWAIT   NTR1  BASE=*,LABEL=*                                                   
         TM    SC2NDVAL,SCNUMQ                                                  
         BO    *+12                                                             
         MVI   FERN,06             NEED A NUMBER                                
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,SC2NDNUM                                                   
         ST    RF,WAITSECS                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF USERID= CARD - FILTER FOR SPECIFIC USERS              *         
***********************************************************************         
VCUSER   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
X        USING CTIREC,KEY                                                       
         MVI   X.CTIKTYP,CTIKTYPQ                                               
         MVC   X.CTIKID,SC2NDFLD   USER-ID                                      
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
         CLI   8(R1),0                                                          
         BE    VCUS02                                                           
         MVI   FERN,07                                                          
         B     EXITL                                                            
         DROP  X                                                                
*                                                                               
VCUS02   L     R4,AIO                                                           
         AHI   R4,CTIDATA-CTIREC   FIND ID ELEMENT (X'02')                      
         USING CTDSCD,R4                                                        
         XR    RF,RF                                                            
*                                                                               
VCUS04   CLI   CTDSCEL,0           END OF RECORD?                               
         BNE   *+12                NO                                           
         MVI   FERN,08                                                          
         B     EXITL                                                            
*                                                                               
         CLI   CTDSCEL,CTDSCELQ    DESCRIPTION ELEMENT                          
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R4,RF,VCUS04                                                     
*                                                                               
         L     RF,AUSRIDS                                                       
         LHI   R0,USRIDSMX                                                      
*                                                                               
VCUS06   OC    0(L'USRIDS,RF),0(RF)                                             
         BZ    VCUS08                                                           
         AHI   RF,L'USRIDS                                                      
         BCT   R0,VCUS06                  CHECK FOR NEXT EMPTY ENTRY            
         MVI   FERN,09                                                          
         B     EXITL                                                            
*                                                                               
VCUS08   MVC   0(L'USRIDS,RF),CTDSC       HEX USERID FILTER                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF EXUSERID= CARD - FILTER FOR SPECIFIC USER EXCLUDES    *         
***********************************************************************         
VCEXUSER NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
X        USING CTIREC,KEY                                                       
         MVI   X.CTIKTYP,CTIKTYPQ                                               
         MVC   X.CTIKID,SC2NDFLD   USER-ID                                      
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,KEY,AIO,0                               
         CLI   8(R1),0                                                          
         BE    VCEU02                                                           
         MVI   FERN,07                                                          
         B     EXITL                                                            
         DROP  X                                                                
*                                                                               
VCEU02   L     R4,AIO                                                           
         AHI   R4,CTIDATA-CTIREC   FIND ID ELEMENT (X'02')                      
         USING CTDSCD,R4                                                        
         XR    RF,RF                                                            
*                                                                               
VCEU04   CLI   CTDSCEL,0           END OF RECORD?                               
         BNE   *+12                NO                                           
         MVI   FERN,08                                                          
         B     EXITL                                                            
*                                                                               
         CLI   CTDSCEL,CTDSCELQ    DESCRIPTION ELEMENT                          
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R4,RF,VCEU04                                                     
*                                                                               
         L     RF,AEXUSIDS                                                      
         LHI   R0,USRIDSMX                                                      
*                                                                               
VCEU06   OC    0(L'EXUSIDS,RF),0(RF)                                            
         BZ    VCEU08                                                           
         AHI   RF,L'EXUSIDS                                                     
         BCT   R0,VCEU06                  CHECK FOR NEXT EMPTY ENTRY            
         MVI   FERN,09                                                          
         B     EXITL                                                            
*                                                                               
VCEU08   MVC   0(L'USRIDS,RF),CTDSC       HEX USERID FILTER                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATION OF LOGGING=CARD (DEFAULT IS 'Y' UNLESS YOU HAVE AN 'N')  *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCLOG    NTR1  BASE=*,LABEL=*                                                   
         MVC   LOG,SC2NDFLD                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUBID FILTER                                               *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCSUBID  NTR1  BASE=*,LABEL=*                                                   
         MVC   SUBFILT,SC2NDFLD                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SEND FAXES (EASYLINK AS IT IS RIGHT NOW)                   *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCFAXGO  NTR1  BASE=*,LABEL=*                                                   
         MVC   FAX,SC2NDFLD                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SEND PDF                                                   *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCPDFGO  NTR1  BASE=*,LABEL=*                                                   
         MVC   PDF,SC2NDFLD                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SEND BDE TRANSACTIONS                                      *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCBDEGO  NTR1  BASE=*,LABEL=*                                                   
         MVC   BDE,SC2NDFLD                                                     
         B     EXITOK                                                           
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE BDE SENDERS #                                              *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCBDESND NTR1  BASE=*,LABEL=*                                                   
         TM    SC2NDVAL,SCNUMQ                                                  
         BO    *+12                                                             
         MVI   FERN,06             NEED A NUMBER                                
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,SC2NDNUM                                                   
         BZ    VCBDNO                                                           
         CHI   RF,9                                                             
         BH    VCBDNO                                                           
         ST    RF,BDESNDRS                                                      
         B     EXITOK                                                           
*                                                                               
VCBDNO   MVI   FERN,13             MUST BE 1 TO 9                               
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BDE SENDERS # OVERRIDE PER USERID                          *         
* NTRY: R3     = A(SCANBLK LINE)                                      *         
***********************************************************************         
VCBDESOV NTR1  BASE=*,LABEL=*                                                   
         L     RF,ABDESOV                                                       
         LHI   R0,BDESOVMX                                                      
*                                                                               
VBOV06   OC    0(L'BDESOV,RF),0(RF)                                             
         BZ    VBOV08                                                           
         AHI   RF,L'BDESOV                                                      
         BCT   R0,VBOV06                  CHECK FOR NEXT EMPTY ENTRY            
         MVI   FERN,14                                                          
         B     EXITL                                                            
*                                                                               
VBOV08   MVC   0(L'BDESOV,RF),SC2NDFLD    HEX USERID FILTER                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR MESSAGE                                      *         
***********************************************************************         
ERRMSG   NTR1  BASE=*,LABEL=*                                                   
         MVI   GOTERR,YES          SET GOT AN ERROR                             
*                                                                               
         LA    RF,ERRND                                                         
         CLI   FERN,0              UNDEFINED ERROR                              
         BE    ERRM02                                                           
*                                                                               
         XR    RF,RF               INDEX INTO ERROR TABLE                       
         IC    RF,FERN                                                          
         BCTR  RF,0                                                             
         MHI   RF,EMSGL                                                         
         A     RF,AERRMSG          RF = A(ERROR MESSAGE)                        
*                                                                               
ERRM02   MVC   PLINE,SPACES                                                     
         MVC   PLINE(L'ERRHDR),ERRHDR                                           
         MVC   PLINE+L'ERRHDR(EMSGL),0(RF)                                      
         BRAS  RE,PRNT                                                          
*                                                                               
         MVI   FERN,0                                                           
         B     EXITOK                                                           
*                                                                               
ERRHDR   DC    C'   *** ERROR *** '                                             
ERRND    DC    CL45'Improperly defined error'                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PLINE TO A PRINT LINE                             *         
***********************************************************************         
PRNT     NTR1  BASE=*,LABEL=*                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+00(2),PRNTTIME                                                 
         MVI   P+02,C':'                                                        
         MVC   P+03(2),PRNTTIME+2                                               
         MVI   P+05,C':'                                                        
         MVC   P+06(2),PRNTTIME+4                                               
         MVI   P+08,C'.'                                                        
         MVC   P+09(2),PRNTTIME+6                                               
         MVC   P+12(L'PLINE),PLINE                                              
         MVC   PLINE,SPACES                                                     
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
*                                                                               
PRNTDUB  DS    D                   FOR PRNT ROUTINE ONLY                        
PRNTTIME DS    CL9                 FOR PRNT ROUTINE ONLY                        
         EJECT                                                                  
***********************************************************************         
* PUT A MESSAGE TO THE OPERATOR CONSOLE AND OPTIONALLY GET A REPLY.   *         
* NTRY: R0        = MESSAGE NUMBER                                    *         
*       XTRAMESS  = OPTIONAL CL8 TO BE INSERTED INTO MESSAGE          *         
*       XTRAMES2  = OPTIONAL CL16 TO BE INSERTED INTO MESSAGE         *         
*       R0 ==  0  = SPECIAL MESSAGE AT 0(R1)                          *         
*                                                                     *         
* 1. IF THE FIRST CHARACTER OF THE MESSAGE IS AN 'X' JUST PUT OUT THE *         
* MESSAGE                                                             *         
*                                                                     *         
* 2. IF IT IS A NUMBER THIS IS THE # OF CHARACTERS FOR THE RESPONSE   *         
* RESPONSE IS RETURNED IN 'REPLY' - "DUMP" AND "EOJ" HANDLED IN HERE  *         
*                                                                     *         
* 3. ELSE THIS IS A MULTILINE MESSAGE AND IT LOOPS UNTIL (1) OR (2)   *         
***********************************************************************         
SYSMESS  NTR1  BASE=*,LABEL=*                                                   
         LR    R8,R1                                                            
         AHI   R0,-1                                                            
         BM    SYM02               R0 == 0 MEANS R1=A(MESSAGE)                  
*                                                                               
         MHI   R0,L'SYSMSGS                                                     
         L     R8,ASYSMSGS                                                      
         AR    R8,R0               R8=A(SYSMSGS ENTRY)                          
*                                                                               
SYM02    MVC   MESSAGE,SPACES      BUILD MESSAGE                                
         MVC   MESSAGE(8),=CL08'EDPQSCAN'                                       
         MVC   MESSAGE+9(L'SYSMSGS-1),1(R8)                                     
*                                                                               
         LA    R0,MESSAGE          NOW REPLACE SUBSTITUTE CHARS                 
         LA    R1,MESSAGE+L'MESSAGE-1                                           
SYM04    CR    R0,R1                                                            
         BE    SYM10                                                            
         CLC   0(18,R1),=18C'X'                                                 
         BE    SYM08                                                            
         CLC   0(8,R1),=18C'X'                                                  
         BE    SYM06                                                            
         BCT   R1,SYM04                                                         
         DC    H'0'                                                             
*                                                                               
SYM06    MVC   0(8,R1),XTRAMESS                                                 
         B     SYM10                                                            
*                                                                               
SYM08    MVC   0(18,R1),XTRAMES2                                                
         B     SYM10                                                            
*                                                                               
SYM10    CLI   0(R8),C'0'                                                       
         BH    SYM12                                                            
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'MESSAGE,MESSAGE)                      
         CLI   0(R8),C'X'                                                       
         BE    SYM16                                                            
         AHI   R8,L'SYSMSGS        SPECIAL MULTILINE MESSAGE                    
         B     SYM10                                                            
*                                                                               
SYM12    GOTO1 VLOGIO,DMCB,1,(L'MESSAGE,MESSAGE)                                
                                                                                
         XR    R0,R0                                                            
         ICM   R0,1,0(R8)                                                       
         N     R0,=X'0000000F'                                                  
         GOTO1 VLOGIO,DMCB,0,((R0),REPLY)                                       
*                                                                               
         CLC   REPLY(4),=C'DUMP'   CHECK FOR DUMP REPLY                         
         BNE   SYM14                                                            
         ABEND 666,DUMP                                                         
*                                                                               
SYM14    CLC   REPLY(3),=C'EOJ'    CHECK FOR EOJ REPLY                          
         BNE   SYM16                                                            
         ABEND 666                                                              
*                                                                               
SYM16    MVC   XTRAMESS,SPACES     CLEAR THESE OUT                              
         MVC   XTRAMES2,SPACES                                                  
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OPEN CONTROL FILE IF REQUIRED                            *         
***********************************************************************         
OPENCTFL NTR1  BASE=*,LABEL=*                                                   
         CLI   CTOPEN,YES                                                       
         BE    EXITOK                                                           
         MVI   CTOPEN,YES                                                       
         GOTO1 VDMGR,DMCB,DMOPEN,CONTROL,CTFLIST,AIO,0                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK FOR SPECIAL TEST CARDS                             *         
* INPUT: CARDIO                                                       *         
***********************************************************************         
CHKTCRD  NTR1  BASE=*,LABEL=*                                                   
*                                  TEST PQ STATUS FILTER                        
         CLC   =C'TEST:PQSCAN',CARDIO                                           
         BNE   CKTC20                                                           
         CLC   =C'TEST:PQSCAN=ALL',CARDIO                                       
         BNE   *+8                                                              
         MVI   TSTPQALL,YES                                                     
         B     CKTCX                                                            
*                                                                               
*                                  TEST PQ PURGE OPTION                         
CKTC20   CLC   =C'TEST:PQPURGE',CARDIO                                          
         BNE   CKTC40                                                           
         CLC   =C'TEST:PQPURGE=YES',CARDIO                                      
         BNE   *+8                                                              
         MVI   TSTPQPUR,YES                                                     
         CLC   =C'TEST:PQPURGE=NO',CARDIO                                       
         BNE   *+8                                                              
         MVI   TSTPQPUR,NO                                                      
         B     CKTCX                                                            
*                                                                               
*                                  TEST MQ Q MGR                                
CKTC40   CLC   =C'TEST:MQMGR',CARDIO                                            
         BNE   CKTC60                                                           
         MVC   TSTMQMGR,CARDIO+11                                               
         B     CKTCX                                                            
*                                                                               
*                                  TEST MQ QUEUE                                
CKTC60   CLC   =C'TEST:MQQUEUE',CARDIO                                          
         BNE   CKTC80                                                           
         MVC   TSTMQQUE,CARDIO+13                                               
         B     CKTCX                                                            
*                                                                               
CKTC80   EQU   *                                                                
         B     EXIT                                                             
CKTCX    EQU   *                                                                
         OI    TSTMODE,X'80'       REMEMBER WE ARE IN TEST MODE                 
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'       *         
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.      *         
***********************************************************************         
CHKOPER  NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,19                                                            
         BRAS  RE,SYSMESS          OPERATOR COMMAND MESSAGE                     
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CH02                                                             
         MVI   OPERSTOP,YES        YES -- SET STOP FLAG                         
         LHI   R0,20                                                            
         BRAS  RE,SYSMESS          OUTPUT OPERATOR STOP MESSAGE                 
         B     CHX                                                              
*                                                                               
CH02     CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT DID THE OPERATOR DO?                    
*                                                                               
         LA    R4,COMMTAB                                                       
         USING COMMTABD,R4                                                      
         XR    RF,RF                                                            
CH04     CLI   0(R4),X'FF'         EOT                                          
         BE    CHBAD               BAD COMMAND                                  
*                                                                               
         IC    RF,COMMLEN          GET MINIMUM LENGTH                           
         CH    RF,CIBDATLN         CHECK STRING LENGTH                          
         BL    CH06                                                             
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8              MATCH COMMAND IN TABLE                       
         BE    CH08                PASS                                         
         CLC   COMMCMD(0),CIBDATA                                               
*                                                                               
CH06     AHI   R4,COMMTABL         NEXT ENTRY                                   
         B     CH04                                                             
*                                                                               
CH08     ICM   RF,15,COMMRTN       GET PROCESSING ROUTINE                       
         BASR  RE,RF                                                            
         B     CHX                                                              
*                                                                               
CHBAD    LHI   R0,21                                                            
         BRAS  RE,SYSMESS          OUTPUT unknown command message               
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
COMMTAB  DC    CL8'????????',AL1(8,0,0,0),AL4(OPDUMMY)                          
         DC    X'FF'                                                            
*                                                                               
OPDUMMY  BR    RE                                                               
*                                                                               
COMMTABD DSECT                     COVERS COMMTAB ABOVE                         
COMMCMD  DS    CL8                 INPUT COMMAND                                
COMMLEN  DS    X                   MINIMUM LENGTH                               
         DS    XL3                                                              
COMMRTN  DS    AL4                 A(PROCESSING ROUTINE)                        
COMMTABL EQU   *-COMMTABD                                                       
*                                                                               
EDPQSCAN CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMON STORAGE/ROUTINES                                             *         
***********************************************************************         
         DROP  RB                                                               
         DS    0D                                                               
COMMON   DC    CL8'*COMMON*'                                                    
SAVERD   DC    F'0'                                                             
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
SKIP     EQU   C'S'                                                             
PM360Q   EQU   C'3'                                                             
OPTICAQ  EQU   C'O'                                                             
MAXPQS   EQU   16                  MAXIMUM OF 16 PRINT QUEUES                   
NUMRPTSQ EQU   6000                MAX NUMBER OF CLASS G RPTS PER PQ            
USRIDSMX EQU   400                 MAX NUMBER OF USERID FILTERS                 
BDESOVMX EQU   501                 MAX NUMBER OF USERID FILTERS                 
*                                                                               
SRTTBMAX DC    A(20000)            XMITTBL MAX NO. OF ENTRIES                   
*                                                                               
ARZERO   DC    16F'0'                                                           
AMSGBUFF DC    A(0)                MESSAGE BUFFER                               
AMSGBUFX DC    A(0)                END OF MESSAGE BUFFER                        
MAXMSGLN DC    A(2*K*K)            MAX MESSAGE LENGTH (2M)                      
MAXRPTLN DC    A(1*K*K)                                                         
STIMER1  DS    XL4                 FOR TIMER POPS                               
*                                                                               
         LTORG                                                                  
*                                                                               
VCPRINT  DC    V(CPRINT)                                                        
VCARDS   DC    V(CARDS)                                                         
VDDSIO   DC    V(DDSIO)                                                         
VPRINTER DC    V(PRINTER)                                                       
VDYNALOC DC    V(DYNALLOC)                                                      
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXO31  DC    V(HEXO31)                                                        
VLOGIO   DC    V(LOGIO)                                                         
VDMGR    DC    V(DATAMGR)                                                       
VENQDEQ  DC    V(DMENQDEQ)                                                      
VPRNTBL  DC    V(PRNTBL)                                                        
VSCANNER DC    V(SCANNER)                                                       
VNUMVAL  DC    V(NUMVAL)                                                        
*                                                                               
ASYSMSGS DC    A(SYSMSGS)                                                       
ASCANTAB DC    A(SCANTAB)                                                       
AERRMSG  DC    A(ERRMSGS)                                                       
ACARDTAB DC    A(CARDTAB)                                                       
AWHY     DC    A(MQ_REASON_CODE_TABLE)                                          
ASSB     DC    A(SSB)                                                           
AUTL     DC    A(UTL)                                                           
AIO      DC    A(IO)                                                            
ACXREC   DC    A(CXREC)                                                         
ARPTNUMS DC    A(RPTNUMS)                                                       
ARPTNUMX DC    A(RPTNUMSX)                                                      
ABDESOV  DC    A(BDESOV)                                                        
AUSRIDS  DC    A(USRIDS)                                                        
AEXUSIDS DC    A(EXUSIDS)                                                       
ACITABL  DC    A(CITABLE)                                                       
MESSAGE  DS    CL60                                                             
XHDR     DC    AL1(NO)             Extended message header?                     
XTRAMESS DS    XL8'00'                                                          
XTRAMES2 DS    XL18'00'                                                         
PTYPE    DS    CL8' '                                                           
*                                                                               
SEQ      DC    CL4'SEQ '                                                        
READ     DC    CL8'READ    '                                                    
GLIST    DC    CL8'GLIST   '                                                    
RANDOM   DC    CL8'RANDOM  '                                                    
INDEX    DC    CL8'INDEX   '                                                    
PRINTED  DC    CL8'PRINTED '                                                    
RETAIN   DC    CL8'RETAIN  '                                                    
CLARET   DC    CL8'CLARET  '                                                    
CTOPEN   DC    AL1(NO)                                                          
DMOPEN   DC    CL8'DMOPEN  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
CONTROL  DC    CL8'CONTROL '                                                    
BUFFER   DC    CL8'BUFFER  '                                                    
PRTQUE   DC    CL8'PRTQUE  '                                                    
CTFLIST  DC    C'NCTFILE NGENDIR NGENFIL X'                                     
*                                                                               
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
ACOMM    DC    A(0)                A(COMMUNICATIONS PARAMETER LIST)             
*                                                                               
PONE     DC    P'1'                                                             
P99      DC    P'99'                                                            
*                                                                               
SUBFILT  DC    CL3'   '            OPTIONAL SUB-ID FILTER                       
*                                                                               
WAITSECS DC    A(120*100)          DEFAULT SCAN RATE - EVERY 2 MINS             
WAITSEC2 DC    F'0'                                                             
LASTTIME DC    F'0'                BIN TIME FOR LAST PQ SCAN                    
*                                                                               
         DS    0D                                                               
         DC    CL16'ECBLIST*ECBLIST*'                                           
ECBLST   DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    X'80',AL3(TIMERECB) TIMER POP                                    
         EJECT                                                                  
***********************************************************************         
* TEST CONTROL OPTIONS                                                *         
***********************************************************************         
TSTMODE  DS    X           TEST MODE (X'80' ON)                                 
TSTPQALL DS    C           TEST PQ SCAN ALL OPTION                              
TSTPQPUR DS    C           TEST PQ PURGE OPTION (Y/N)                           
TSTMQMGR DS    CL48        TEST MQ Q MGR                                        
TSTMQQUE DS    CL48        TEST MQ QUEUE                                        
***********************************************************************         
* MQ STUFF                                                            *         
***********************************************************************         
QMGR     DC    CL48'MQ1P'                                                       
QMASTER  DC    CL48' '                                                          
*                                                                               
         DC    CL8'HCONN==>'                                                    
HCONN    DC    F'0'                MQ QMGR CONNECTION HANDLE                    
*                                                                               
**QMASTERP DC    CL48'DDS.BDEFTP.REP.LOCALQ'                                    
**QMASTERT DC    CL48'DDS.BDEFTP.TST.LOCALQ'                                    
QMASTERP DC    CL48'DDS.BROKER.LOCALQ'                                          
QMASTERT DC    CL48'DDS.BROKER.TEST.LOCALQ'                                     
QMASTERC DC    CL48'DDS.BROKER.CSC.LOCALQ'                                      
QMASTERQ DC    CL48'DDS.BROKER.FQA.LOCALQ'                                      
*QMASTERT DC    CL48'MQDEV01.MSWA.QREMOTE'                                      
*                                                                               
         DS    0D                                                               
         DC    CL16'ENTRYPTSENTRYPTS'                                           
ENTRYPTS DC    Y((ENTRYLSQ-ENTRYSTQ)/60) NUMBER OF TABLE ENTRIES                
         DC    H'60'                     MUST REMAIN AS 60                      
ENTRYSTQ EQU   *                                                                
CSQBCLOS DC    CL8'CSQBCLOS'                                                    
         DC    XL52'00'                                                         
CSQBCOMM DC    CL8'CSQBCOMM'                                                    
         DC    XL52'00'                                                         
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
         DC    XL52'00'                                                         
CSQBPUT  DC    CL8'CSQBPUT'                                                     
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
*                                                                               
         CMQA    LIST=YES,EQUONLY=NO                                            
***********************************************************************         
* PARAMETER LISTS TO FACILITATE MQ CALLS                              *         
*                                                                     *         
*  CL16 EBCDIC ROUTINE NAME                                           *         
*  F    A(ROUTINE)                                                    *         
*  XL1  FLAGS                                                         *         
*       X'80': ROUTINE IS SYNCHRONOUS ONLY                            *         
*       X'40': ROUTINE CAN BE FOLLOWED BY EXTRACT_ERROR CALL          *         
*  XL3  SPARE                                                         *         
*  PARAMETERS (STANDARD IBM FORMAT)                                   *         
***********************************************************************         
         DS    0D                                                               
MQCT     DC    CL16'MQ QMGR connect'                                            
AMQCT    DC    A(0)                                                             
         DC    A(QMGR)                                                          
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQOPENQ  DC    CL16'MQ Open queue'                                              
AMQOPENQ DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MAINQ)            CMQODA                                       
         DC    A(OPN_OPTS)                                                      
         DC    A(MAINQHOB)         HANDLE TO OBJECT (RETURNED)                  
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQCMIT   DC    CL16'MQ Commit'                                                  
AMQCMIT  DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQCLOSE  DC    CL16'MQ Close Queue'                                             
AMQCLOSE DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MAINQHOB)                                                      
         DC    A(CLS_OPTS)                                                      
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQDISC   DC    CL16'MQ Disconnect'                                              
AMQDISC  DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
MQPUT    DC    CL16'MQ Put'                                                     
AMQPUT   DC    A(0)                                                             
         DC    A(HCONN)                                                         
         DC    A(MAINQHOB)         QUEUE NAME                                   
         DC    A(MSGDESC)                                                       
         DC    A(PUTOPTS)                                                       
         DC    A(DATALEN)                                                       
MQPUTBUF DC    A(0)                                                             
         DC    A(MQ_CC)                                                         
         DC    X'80',AL3(MQ_RC)                                                 
*                                                                               
OPN_OPTS DS    F                   MQOPEN OPTIONS                               
CLS_OPTS DS    F                   MQCLOSE OPTIONS                              
MQ_CC    DS    F                   COMPLETION CODE                              
MQ_RC    DS    F                   QUALIFIES COMPLETION CODE                    
DATALEN  DS    F                   LENGTH OF THE MESSAGE                        
*                                                                               
         DS    0D                                                               
         DC    CL8'MAINQHOB'                                                    
MAINQHOB DC    F'0'                                                             
         DC    F'0'                                                             
*                                                                               
         DC    CL8'MAINQ==>'                                                    
MAINQ    CMQODA  LIST=YES          OBJECT DESCRIPTOR                            
*                                                                               
MSGDESC  CMQMDA  LIST=YES          MESSAGE DESCRIPTOR                           
PUTOPTS  CMQPMOA DSECT=NO,LIST=YES PUT MESSAGE OPTIONS                          
         EJECT                                                                  
***********************************************************************         
* NON-ADDRESSIBLE STORAGE AREAS                                       *         
***********************************************************************         
         DS    0D                                                               
         DC    CL16'SCANTAB+SCANTAB+'                                           
SCANTAB  DS    (PARMCNTQ)CL(SCBLKLQ)                                            
         DC    CL16'SCANTAB-SCANTAB-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'SSB+SSB+SSB+SSB+'                                           
SSB      DC    XL2'0000',X'FF',1021X'00'                                        
         DC    CL16'SSB-SSB-SSB-SSB-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'UTL+UTL+UTL+UTL+'                                           
UTL      DC    F'0',X'0A',251X'00'                                              
         DC    CL16'UTL-UTL-UTL-UTL-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'CITABLE+CITABLE+'                                           
CITABLE  DC    (MAXPQS*CITBLLNQ)X'FF'                                           
         DC    X'FF',XL15'00'      END OF TABLE MARKER                          
         DC    CL16'CITABLE-CITABLE-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'+IO++IO++IO++IO+'                                           
IO       DC    4096X'00'           CTFILE/GENFIL I/O AREA                       
         DC    CL16'-IO--IO--IO--IO-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'RPTNUMS+RPTNUMS+'                                           
RPTNUMS  DS    (NUMRPTSQ)XL2       REPORT NUMBERS TO EXAMINE                    
RPTNUMSX DC    X'0000'                                                          
         DC    CL16'RPTNUMS-RPTNUMS-'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'+CXREC+++CXREC++'                                           
CXREC    DC    14336X'00'          PRINT QUEUE BUFFER                           
         DC    CL16'-CXREC---CXREC--'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'BDESOV++BDESOV++'                                           
BDESOV   DC    (BDESOVMX)XL9'00'   OPTIONAL BDE SENDER OVERRIDES                
         DC    H'0'                         (XL1'#', CL8'USERID')               
         DC    CL16'BDESOV--BDESOV--'                                           
*                                                                               
         DS    0D                                                               
         DC    CL16'USRIDS++USRIDS++'                                           
USRIDS   DC    (USRIDSMX)H'0'      OPTIONAL HEX USERID FILTERS                  
         DC    H'0'                                                             
         DC    CL16'USRIDS--USRIDS--'                                           
*                                                                               
         DC    CL16'EXUSIDS+EXUSIDS+'                                           
EXUSIDS  DC    (USRIDSMX)H'0'      OPTIONAL HEX EXUSERID FILTERS                
         DC    H'0'                                                             
         DC    CL16'EXUSIDS-EXUSIDS-'                                           
*                                                                               
       ++INCLUDE DDMQREASON                                                     
         EJECT                                                                  
***********************************************************************         
* VALID INPUT CARD TABLE                                              *         
***********************************************************************         
         DS    0D                                                               
CARDTAB  DC    CL10'DSPACE    ',AL1(5,5,0,0),AL4(VCDSPACE)                      
         DC    CL10'DDSIO     ',AL1(4,4,0,0),AL4(VCDDSIO)                       
         DC    CL10'TRACE     ',AL1(4,4,0,0),AL4(VCTRACE)                       
         DC    CL10'QMGR      ',AL1(3,3,0,0),AL4(VCQMGR)                        
         DC    CL10'MQQMGRNAME',AL1(9,9,0,0),AL4(VCQNAME)                       
         DC    CL10'LOGGING   ',AL1(6,6,0,0),AL4(VCLOG)                         
         DC    CL10'WAITSECS  ',AL1(7,7,0,0),AL4(VCWAIT)                        
         DC    CL10'USERID    ',AL1(3,5,0,0),AL4(VCUSER)                        
         DC    CL10'EXUSERID  ',AL1(5,7,0,0),AL4(VCEXUSER)                      
         DC    CL10'BDESNDROV ',AL1(8,8,0,0),AL4(VCBDESOV)                      
         DC    CL10'SUBID     ',AL1(4,4,0,0),AL4(VCSUBID)                       
         DC    CL10'BDE       ',AL1(2,2,0,0),AL4(VCBDEGO)                       
         DC    CL10'FAX       ',AL1(2,2,0,0),AL4(VCFAXGO)                       
         DC    CL10'PDF       ',AL1(2,2,0,0),AL4(VCPDFGO)                       
         DC    CL10'BDESNDRS  ',AL1(7,7,0,0),AL4(VCBDESND)                      
         DC    X'FF'                                                            
*                                                                               
CARDTABD DSECT ,                   INPUT CARD PARAMETERS                        
CARDTXT  DS    CL10                TEXT                                         
CMIN     DS    X                   MIN LEN FOR EXECUTE (-1)                     
CMAX     DS    X                   MAX LEN FOR EXECUTE (-1)                     
         DS    XL2                 N/D                                          
CARDVAL  DS    AL4                 A(VALIDATION ROUTINE)                        
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
EDPQSCAN CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                       *         
***********************************************************************         
EMSGL    EQU   45                                                               
ERRMSGS  DS    0CL(EMSGL)                                                       
EMSG01   DC    CL45'Undefined parameter card                    '               
EMSG02   DC    CL45'DSPACE= parameter can only be 1 byte long   '               
EMSG03   DC    CL45'DDSIO= parameter cannot be more than 8 long '               
EMSG04   DC    CL45'Input line is not valid for SCANNER         '               
EMSG05   DC    CL45'Too many parameters in total                '               
EMSG06   DC    CL45'The parameter must be a number              '               
EMSG07   DC    CL45'This userid is not valid                    '               
EMSG08   DC    CL45'This userid has no 02 element - this is bad '               
EMSG09   DC    CL45'Too many filters - this one is ignored      '               
EMSG10   DC    CL45'This user has no EDICT record               '               
EMSG11   DC    CL45'Too many logical reports                    '               
EMSG12   DC    CL45'Logical report too big                      '               
EMSG13   DC    CL45'BDESNDRS= parameter must be 1 to 9          '               
EMSG14   DC    CL45'Too many BDE sender overrides               '               
         EJECT                                                                  
***********************************************************************         
* CONSOLE MESSAGE TABLE - NEED NOT BE ADDRESSIBLE                     *         
***********************************************************************         
SYSMSGS  DS    0CL50                                                            
SMSG01   DC    CL50'XBeginning initialisation                      '            
SMSG02   DC    CL50'XCompleted initialisation                      '            
SMSG03   DC    CL50'XBeginning reading input cards                 '            
SMSG04   DC    CL50'XCompleted reading input cards                 '            
SMSG05   DC    CL50'XBeginning validating input cards              '            
SMSG06   DC    CL50'XCompleted validating input cards              '            
SMSG07   DC    CL50'XBeginning setting operator comms              '            
SMSG08   DC    CL50'XCompleted setting operator comms              '            
SMSG09   DC    CL50'XBeginning MQ Initialisation                   '            
SMSG10   DC    CL50'XCompleted MQ Initialisation                   '            
SMSG11   DC    CL50'XObtained  MQ QMGR handle                      '            
SMSG12   DC    CL50'XOpened    MQ Master Queue                     '            
SMSG13   DC    CL50'XOpened    MQ Log Queue                        '            
SMSG14   DC    CL50'XBeginning MQ Deallocation                     '            
SMSG15   DC    CL50'XCompleted MQ Deallocation                     '            
SMSG16   DC    CL50'XClosed    MQ Master Queue                     '            
SMSG17   DC    CL50'XClosed    MQ Log Queue                        '            
SMSG18   DC    CL50'X** WARNING ** Logging Suppressed              '            
SMSG19   DC    CL50'XIncoming operator command                     '            
SMSG20   DC    CL50'X** WARNING ** Operator requested "STOP"       '            
SMSG21   DC    CL50'X** WARNING ** Unknown operator command ignored'            
SMSG22   DC    CL50'XOpened all subsidiary MQ Queues               '            
SMSG23   DC    CL50'XClosed all subsidiary MQ Queues               '            
SMSG24   DC    CL50'XNUMPRTS inadequate - abending call programmer '            
SMSG25   DC    CL50'X** ERROR ** Unknown userid ?????????????????? '            
SMSG26   DC    CL50'XBeginning building PQ list                    '            
SMSG27   DC    CL50'XCompleted building PQ list                    '            
SMSG28   DC    CL50'1MQPUT FAILED!  JUST RECYCLE THIS JOB.         '            
         EJECT                                                                  
***********************************************************************         
* W/S AREA                                                            *         
***********************************************************************         
         DS    0D                                                               
WORKD    DC    CL8'*WORKD**'                                                    
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BITS     DS    X                                                                
PLIST    DS    8F                                                               
DMCB     DS    8F                                                               
PM1      DS    F                                                                
PM2      DS    F                                                                
PM3      DS    F                                                                
PM4      DS    F                                                                
PM5      DS    F                                                                
PM6      DS    F                                                                
*                                                                               
XMLPARM  DS    3F                                                               
*                                                                               
APQSAVE  DS    A                   A(PRINT QUEUE SAVE AREA)                     
SVRE     DS    A                                                                
*                                                                               
BDESNDRS DS    F                   INPUT CARD, # BDE SENDERS                    
BDESN    DS    C                   BDE SENDER # FOR THIS USER ID                
*                                                                               
ERR#     DS    X                                                                
CONOK    DS    X                                                                
OPERSTOP DS    X                                                                
ISEMAIL  DS    C                                                                
ISDARE   DS    C                                                                
ISBDE    DS    C                   'Y' = BDE reporting                          
ISECN    DS    C                   'Y' = ECN faxing                             
ISELINK  DS    C                   'Y' = Easylink faxing                        
ISPDF    DS    C                   'Y' = PDF reporting                          
IS360    DS    C                   'Y' = Send to PM360                          
HASBXF   DS    C                   'Y' = Report has BXF formatting              
ISXRR    DS    C                   'Y' = Extreme reach                          
ISDIRTY  DS    C                   'Y' = Report has data                        
HAVEHDR  DS    C                   'Y' = Processing HDR now                     
SKIPBXF  DS    C                   'Y' = Remove BXF section                     
DIDPUT   DS    C                   'Y' = CALLED MQ TO PUT (MUST COMMIT)         
ENDRPT   DS    C                   'Y' = END OF THIS PHYSICAL RPT               
ENDLRPT  DS    C                   'Y' = END OF THIS LOGICAL RPT                
EMPTYRPT DS    C                   'Y' = PQ REPORT HAS NO DATA TO SEND          
ANYFAX   DC    AL1(NO)                                                          
LOG      DC    AL1(YES)                                                         
BDE      DC    AL1(NO)                                                          
FAX      DC    AL1(NO)                                                          
PDF      DC    AL1(NO)                                                          
DESTINAT DS    CL25                EASYLINK DESTINATION                         
DESTFMT  DS    CL16                FORMATTED DESTINATION                        
PQS      DS    CL3                 PQ CLASS/STATUS                              
PLINE    DS    CL100               OUTPUT PRINT LINE                            
REPLY    DS    CL8                                                              
CARDIO   DS    CL80                                                             
FERN     DS    X                                                                
*TYPE    DS    X                                                                
DSPACE   DS    C                                                                
TRACE    DS    X                                                                
GOTERR   DS    X                                                                
PARMCNT  DS    H                   NO OF I/P CARDS                              
PARMCNTQ EQU   500                 MAX NUMBER OF I/P CARDS                      
WORK     DS    XL64                                                             
KEY      DS    XL25                FOR CTFILE READS                             
KEY2     DS    XL48                FOR GENDIR/FILE READS                        
DESTNTRY DS    XL(DESTTBLQ)        TEMP STORAGE FOR 1 DESTTBL  ENTRY            
*                                                                               
RLEN     DS    H                   PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
*                                                                               
PQINDEX  DS    XL40                PRINT QUEUE INDEX ENTRY                      
RPTUID   DS    CL8                 REPORT USERID (ALPHA)                        
RPTUIDNO DS    XL2                 REPORT USERID                                
RPTSUBID DS    CL3                 REPORT SUB-ID                                
RPTREFNO DS    XL2                 REPORT REFERENCE NUMBER                      
RPTDTTM  DS    0XL4                                                             
RPTCRDAT DS    XL2                 REPORT CREATION DATE - CMPRSD                
RPTCRTIM DS    XL2                 REPORT CREATION TIME                         
*                                                                               
RPTLOGNO DS    H                   LOGICAL REPORT SEQ. WITHIN PHYSICAL          
RPTLDSTS DS    H                   LOG. REP. DEST. SEQ. WITHIN PHYSICAL         
RPTMETH  DS    C                   LOGICAL REPORT TRANSMISSION METHOD           
RPTERROR DS    X                   ERROR REASON CODE                            
RPTFLAGS DS    X                   VARIOUS FLAGS                                
RPTPQTYP DS    X                   PQ REPORT TYPE                               
RPTSECAG DS    CL2                 PQ REPORT Security agency                    
RPTPIDNO DS    XL2                 PQ REPORT Person id number                   
*                                                                               
ASRTTBL  DS    A                   A(TABLE OF REPORTS)                          
ASRTPTR  DS    A                   A(CURRENT TABLE ENTRY)                       
COUNTER  DS    F                   XMITTBL ENTRY COUNTER                        
TBLNTRY  DS    XL(SRTTBLQ)         XMIT TABLE ENTRY HOLDER                      
*                                                                               
AGYALPHA DS    CL2                                                              
SVAGOPTS DS    X                   SAVED AGY OPTS FROM X'B4'(CTAGDELQ)          
SSSSSTHB DS    F                   Seconds in 100th of a sec./day               
SAVGRPID DS    D                   Save Group Id                                
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* RD CHAIN CSECT                                                      *         
***********************************************************************         
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* CTGENEDICT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DMPRTQK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQK                                                        
         PRINT ON                                                               
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
* DDEDICTWRK                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDEDICTWRK                                                     
         PRINT ON                                                               
* DMPRTQS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQS                                                        
         PRINT ON                                                               
* IEZCIB                                                                        
         PRINT OFF                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         PRINT ON                                                               
* IEZCOM                                                                        
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
* EDIDESTD                                                                      
EDIHDRD  DSECT                                                                  
         DC    X'00'               CONTROL CHARACTER                            
         PRINT OFF                                                              
       ++INCLUDE EDIDESTD                                                       
         PRINT ON                                                               
* EDISCANHDR                                                                    
         PRINT OFF                                                              
       ++INCLUDE EDISCANHDR                                                     
         PRINT ON                                                               
* IHASDWA                                                                       
         PRINT OFF                                                              
         IHASDWA                                                                
         PRINT ON                                                               
* IEFZB4D2                                                                      
         PRINT OFF                                                              
         IEFZB4D2                                                               
         PRINT ON                                                               
* IHAPSA                                                                        
         PRINT OFF                                                              
         IHAPSA                                                                 
         PRINT ON                                                               
*                                                                               
SRTTABLD DSECT                                                                  
SRTCRDTM DS    0XL4                PQ REPORT CREATION DATE/TIME                 
SRTCRDAT DS    XL2                 PQ REPORT CREATION DATE - CMPRSD             
SRTCRTIM DS    XL2                 PQ REPORT CREATION TIME                      
SRTPRTQ  DS    X                   PQ NUMBER                                    
SRTPQKEY DS    0XL7                PQ REPORT KEY                                
SRTUSRID DS    XL2                 PQ REPORT SENDING USERID                     
SRTSUBID DS    CL3                 PQ REPORT SUB-ID                             
SRTREFNO DS    XL2                 PQ REPORT REFERENCE NUMBER                   
SRTTBLQ  EQU   *-SRTTABLD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053EDIPQSCAN 05/27/20'                                      
         END                                                                    
