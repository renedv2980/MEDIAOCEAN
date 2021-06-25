*          DATA SET SPREPIQ02  AT LEVEL 037 AS OF 07/27/12                      
*PHASE SPIQ02A                                                                  
*INCLUDE PRTREC                                                                 
                                                                                
* WHEN THE TABLE OF EXISTING BUYS IS BUILT, THE LINE NUMBERS OF DELETED         
* BUYS ARE ACCOUNTED FOR, BUT THE LINES ARE NOT REUSED, BECAUSE THERE           
* REALLY SHOULD NOT BE ANY UPLOAD OF A MONTH THAT IS BEING CHANGED FOR          
* CLEARANCES. IF IT'S AN ISSUE, CHANGE IT TO REUSE THE DELETIONS, WHICH         
* WILL GO AWAY ANYWAY AFTER THE FILE IS RELOADED.                               
                                                                                
SPIQ02   TITLE 'IQ/DDS - UPLOAD DATA TO DDS'                                    
SPIQ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPIQ02                                                       
*                                                                               
         L     RC,=A(SPIQWK)                                                    
         USING SPIQWK,RC                                                        
*                                                                               
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
         J     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
*=====================================================================*         
* RUNFRST FIRST PROCESSING                                            *         
*=====================================================================*         
RUNF     DS    0H                                                               
         L     R4,VMASTC                                                        
         USING MASTD,R4                                                         
         MVC   MCDUB,SPACES          LOAD DEMOVAL                               
         MVC   MCDUB(4),=C'T00A'                                                
         MVC   MCDUB+4(2),=C'D9'                                                
         GOTO1 MCVLOADM,DMCB,0                                                  
         MVC   DEMOVAL,4(R1)                                                    
*                                                                               
         L     RE,SSB                                                           
         USING SSBD,RE                                                          
         MVI   SSORPRG,C'Q'        SET PROGRAM ID FOR RCVRHDR                   
         OI    SSOSTAT2,SSOSROLC   RECOVER COPIES AND CHANGES                   
         DROP  RE                                                               
*                                                                               
         J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*=====================================================================*         
* RUNLAST PROCESSING                                                  *         
*=====================================================================*         
                                                                                
RUNL     MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTERS                                                      
         LHI   R5,(COUNTERX-COUNTERS)/L'COUNTERS                                
*                                                                               
RUNL2    OI    3(R4),X'0F'                                                      
         UNPK  P(6),0(4,R4)                                                     
         MVC   P+8(24),4(R4)                                                    
         GOTO1 REPORT                                                           
*                                                                               
         AHI   R4,L'COUNTERS                                                    
         BCT   R5,RUNL2                                                         
                                                                                
*============================================================                   
* SEND EMAIL IF ANY PROCESSING ERRORS                                           
*============================================================                   
                                                                                
         CLI   SVQOPT5,C'N'        TEST NO EMAIL                                
         JE    EXIT                                                             
*                                                                               
         LA    R4,ERRCNT                                                        
         LHI   R5,(ERRCNTX-ERRCNT)/L'COUNTERS                                   
*                                                                               
RUNL10   CP    0(4,R4),=P'0'                                                    
         BH    RUNL20                                                           
         AHI   R4,L'COUNTERS                                                    
         BCT   R5,RUNL10                                                        
         J     EXIT                                                             
*                                                                               
RUNL20   L     RE,VMASTC           DIG OUT REPORT ID AND SAVE IT !              
         USING MASTD,RE            BEFORE EMAIL REPORT OVERWRITES IT            
*                                                                               
         L     R7,MCVREMOT                                                      
         USING REMOTED,R7                                                       
         DROP  RE                                                               
*                                                                               
         XC    SVJOBID,SVJOBID                                                  
         OC    REMOTJID,REMOTJID   ARE WE GOING TO PRTQUE                       
         BZ    RUNL22                                                           
         MVC   SVJOBID,REMOTJID    SAVE JOB ID                                  
         MVC   SVRNO,REMOTRNO      AND JOB NUMBER                               
         DROP  R7                                                               
*                                                                               
RUNL22   WTO   'SIQ - NON-ZERO ERROR COUNT'                                     
         CLI   SVQOPT5,C'1'        TEST NO EMAIL TO CLIENT                      
         JE    EXIT                YES - SO NO EMAIL AT ALL                     
*                                                                               
         BRAS  RE,OPNPQ            OPEN PRINT QUEUE FOR CLASS G REPT            
         BRAS  RE,PUTPQ                                                         
         BRAS  RE,CLSPQ            CLOSE PRINT QUEUE                            
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* REQUEST FIRST PROCESSING                                            *         
*=====================================================================*         
                                                                                
REQF     LHI   R0,1                                                             
         STH   R0,PAGE                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVQOPT5,QOPT5       SAVE EMAIL OPTION FOR RUNLAST                
*                                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,FILEGET                                                       
*                                                                               
         CLC   IQTYPE,=C'HDR*'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVIQHDR,IQTYPE                                                   
         BAS   RE,PROCHDR                                                       
*                                                                               
         BRAS  RE,FILEGET                                                       
*                                                                               
         CLC   IQTYPE,=C'EOF*'                                                  
         BE    ENDIN                                                            
*                                                                               
         CLC   IQTYPE,=C'BUY*'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,PROCB                                                         
         EJECT                                                                  
*=============================================================                  
* END OF INPUT FILE - DELETE ALL STATIONS NOT RECEIVED                          
*=============================================================                  
                                                                                
ENDIN    DS    0H                                                               
         MVC   P(13),=C'CLIENT TOTALS'                                          
         L     R0,CLTTOTG                                                       
         EDIT  (R0),(10,P+14),2,FLOAT=$                                         
         GOTO1 REPORT                                                           
*                                                                               
         L     R4,ASTALIST                                                      
         ICM   R5,15,STALPAR3      ENTRY COUNT                                  
         BZ    ENDINX                                                           
*                                                                               
ENDIN2   CLI   5(R4),C'Y'          TEST STATION RECEIVED THIS TIME              
         BE    *+8                                                              
         BRAS  RE,DELBUYS                                                       
*                                                                               
         LA    R4,L'STALIST(R4)                                                 
         BCT   R5,ENDIN2                                                        
*                                                                               
ENDINX   DS    0H                                                               
         GOTO1 AENDREQ                                                          
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* PROCESS HEADER RECORD                                                         
*===============================================================                
*                                                                               
PROCHDR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
* PUT FILEID IN PQDESC BEFORE PRINTING FIRST LINE                               
                                                                                
         L     RE,VMASTC                                                        
         L     RE,MCVREMOT-MASTD(RE)   GET REMOTC ADDRESS                       
         USING REMOTED,RE                                                       
*                                                                               
         MVC   REMOTDSC(1),QMED                                                 
         MVC   REMOTDSC+1(3),QCLT                                               
         MVC   REMOTDSC+4(3),IQEST                                              
         MVC   REMOTDSC+7(4),IQEND-IQTYPE+SVIQHDR                               
         DROP  RE                                                               
*                                                                               
         MVC   P(17),=C'PROCESSING HEADER'                                      
         MVC   P+18(36),IQREC                                                   
         GOTO1 REPORT                                                           
*                                                                               
         CLI   IQSYS,C'S'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IQUPLD,=C'PEX'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   IQMED,C'T'                                                       
         BE    PH10                                                             
         CLI   IQMED,C'R'                                                       
         BE    PH10                                                             
         DC    H'0'                                                             
*                                                                               
PH10     MVC   QMED,IQMED                                                       
         GOTO1 MEDGET,DMCB,(IQMED,SVAGY),DATAMGR,WORK                           
         CLI   DMCB+8,X'FF'        TEST MEDIA NOT VALID                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVAGYMD,WORK        BINARY A/M                                   
         MVC   BAGYMD,WORK                                                      
         PACK  BAGY,BAGYMD                                                      
         NI    BAGY,X'0F'          1 BYTE AGY/RIGHT ALIGNED                     
         MVC   MED,IQMED           EBCDIC                                       
         MVC   MEDNM,WORK+1                                                     
*                                                                               
         MVC   QCLT,IQCLT                                                       
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLT                                                           
                                                                                
* GET IQ DATES IN BINARY AND BACK TO DDS YYMMDD FORMAT                          
                                                                                
         GOTO1 DATCON,DMCB,IQSTART,(3,DUB)                                      
         GOTO1 (RF),(R1),(3,DUB),IQSTART                                        
*                                                                               
         GOTO1 (RF),(R1),IQEND,(3,DUB+3)                                        
         GOTO1 (RF),(R1),(3,DUB+3),IQEND                                        
*                                                                               
         MVC   BUSTART,DUB         SAVE BINARY START/END DATES                  
         MVC   BUEND,DUB+3                                                      
                                                                                
* MAKE SURE THEY SENT BROADCAST MONTH DATES                                     
                                                                                
         GOTO1 GETBROAD,DMCB,IQSTART,WORK                                       
         MVC   BUWKS,0(R1)         SAVE WEEKS IN BDCST MONTH                    
         CLC   IQSTART,WORK                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IQEND,WORK+6                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* BUILD LIST OF WEEK DATES                                                      
                                                                                
         XC    BUWKLIST(24),BUWKLIST                                            
         LA    R4,BUWKLIST                                                      
         SR    R5,R5                                                            
         IC    R5,BUWKS                                                         
*                                                                               
PH12     GOTO1 DATCON,DMCB,WORK,(2,(R4))                                        
*                                                                               
         LHI   R0,6                SET TO GET WEEK END DATE                     
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,WORK,(2,2(R4))                                       
*                                                                               
         LHI   R0,1                GET START DATE OF NEXT WEEK                  
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         MVC   WORK(6),WORK+6                                                   
*                                                                               
         LA    R4,4(R4)                                                         
         BCT   R5,PH12                                                          
         EJECT                                                                  
*===============================================================                
* READ MASTER ESTIMATE                                                          
*===============================================================                
                                                                                
         LA    RE,IQEST+2          DEAL WITH LEFT-ALIGNED FIELD                 
         LHI   RF,3                                                             
*                                                                               
PH14     CLI   0(RE),C' '                                                       
         BH    PH16                                                             
         BCTR  RE,0                                                             
         BCT   RF,PH14                                                          
         DC    H'0'                                                             
*                                                                               
PH16     BCTR  RF,0                SET FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,IQEST(0)                                                     
*                                                                               
         UNPK  IQEST(3),DUB        EXPAND TO 3 BYTES                            
*                                                                               
         CVB   R0,DUB                                                           
         MVC   KEY+4(3),=C'POL'                                                 
         STC   R0,BEST                                                          
         STC   R0,KEY+7                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADMAST                                                          
*                                                                               
         GOTO1 GETEST                                                           
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   IQEND,ESTART        TEST DATA PRIOR TO ESTIMATE START            
         BH    PH20                NO - SO NO REDIRECTION                       
*                                                                               
         CLI   EREQLO,0            TEST ANYTHING IN REQ RANGE                   
         BE    BADREDIR            DATA SHOULD BE REDIRECTED                    
                                                                                
*===================================================================            
* EREQLO AND EREQHI ARE USED TO REDIRECT DATA TO A                              
* DIFFERENT MASTER ESTIMATE. WHEN THE DATA IS FOR A BROADCAST                   
* MONTH PRIOR TO THE MASTER ESTIMATE START.                                     
* WHICHEVER FIELD (LO/HI) DOES NOT MATCH THE CURRENT ESTIMATE                   
* IS THE REAL MASTER ESTIMATE NUMBER                                            
*===================================================================            
                                                                                
         IC    R0,EREQLO                                                        
         CLC   EREQLO,KEY+7        MATCH LOW RANGE                              
         BNE   *+8                                                              
         IC    R0,EREQHI                                                        
         STC   R0,BEST             SAVE AS REQUESTED ESTIMATE                   
         STC   R0,KEY+7                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADREDIR                                                         
*                                                                               
         MVC   P(36),=C'DATA FOR T/PT6/011 REDIRECTED TO 012'                   
         MVC   P+11(3),QCLT                                                     
         MVC   P+15(3),IQEST                                                    
         OC    P+15(3),=3C'0'                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+33(3),DUB                                                      
         GOTO1 REPORT                                                           
         EJECT                                                                  
*=========================================================                      
* READ CSO MASTER ESTIMATE LIST RECORDS                                         
*=========================================================                      
                                                                                
PH20     XC    KEY,KEY                                                          
K        USING MASRECD,KEY                                                      
         MVI   K.MASKTYPE,X'0D'                                                 
         MVI   K.MASKSTYP,X'6F'                                                 
         MVC   K.MASKAM,BAGYMD                                                  
         MVC   K.MASKCLT,BCLT                                                   
         GOTO1 HIGH                                                             
         B     PH24                                                             
*                                                                               
PH22     GOTO1 SEQ                                                              
*                                                                               
PH24     CLC   KEY(5),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   K.MASKDATE,IQSTART  DATE PRIOR TO UPLOAD START                   
         BH    PH22                NO                                           
         DROP  K                                                                
*                                                                               
PH26     GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY            USE BUYREC I/O AREA                          
         LA    R6,24(R6)                                                        
         CLI   0(R6),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,2(R6)            POINT TO FIRST LIST ENTRY                    
*                                                                               
PH28     CLC   BEST,0(R6)          MATCH MASTER ESTIMATE                        
         BE    PH30                                                             
         LA    R6,9(R6)            NEXT MASTER EST ENTRY                        
         CLI   0(R6),0                                                          
         BNE   PH28                                                             
         B     PH22                READ ANOTHER MASTER ESTIMATE                 
*                                                                               
PH30     MVC   SUBLIST,3(R6)       MOVE SUB-EST LIST                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),=C'POL'                                                 
                                                                                
         LA    R4,SUBLIST                                                       
*                                                                               
PH32     MVC   KEY+7(1),0(R4)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETEST                                                           
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   EEND,IQSTART        EST END BEFORE UPLOAD START                  
         BNL   PH34                                                             
         LA    R4,1(R4)            YES - TRY NEXT                               
         CLI   0(R4),0                                                          
         BE    BADMAST                                                          
         B     PH32                                                             
*                                                                               
PH34     CLC   ESTART,IQSTART      BDCST MONTH MUST BE IN EST PER               
         BH    BADMAST                                                          
         CLC   IQEND,EEND                                                       
         BH    BADMAST                                                          
*                                                                               
         MVC   BEST,EKEYEST        SAVE ACTUAL ESTIMATE NUMBER                  
         SR    R0,R0                                                            
         IC    R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EST,DUB                                                          
         MVC   QEST,EST            SET IT HERE TOO                              
* GET DAYPART MENU                                                              
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB(3),QAGY        A-M                                          
         MVC   DMCB+3(1),EDAYMENU                                               
         GOTO1 DPTRD,DMCB,,(C'P',ADDPTTAB)                                      
         DROP  R6                                                               
                                                                                
* NOW BUILD A LIST OF ALL BRANDS WITH THIS ESTIMATE OPEN                        
                                                                                
         L     R6,ADCLT                                                         
         AHI   R6,CLIST-CLTHDRD                                                 
         XC    ESTLST,ESTLST                                                    
*                                                                               
PH40     XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),0(R6)                                                   
         MVC   KEY+7(1),BEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PH42                                                             
         SR    RE,RE                                                            
         IC    RE,3(R6)                                                         
         LA    RE,ESTLST(RE)                                                    
         MVC   0(1,RE),3(R6)       SET PRD NUMBER IN EST TABLE                  
*                                                                               
PH42     AHI   R6,4                                                             
         CLI   0(R6),C'A'                                                       
         BL    PH44                                                             
         CLI   3(R6),X'FF'                                                      
         BE    PH42                                                             
         B     PH40                                                             
*                                                                               
PH44     BRAS  RE,BLDSTLST         BUILD LIST OF ALL ACTIVE STATIONS            
         EJECT                                                                  
*=============================================================                  
* TRANSLATE DEMO CODES TO 3-BYTE FORMAT                                         
*=============================================================                  
                                                                                
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,QAGY                                                    
         DROP  RE                                                               
*                                                                               
         LA    R4,IQDEMOS                                                       
         LA    R5,DEMOLIST                                                      
         LHI   R0,40                                                            
*                                                                               
PH50     XC    WORK,WORK                                                        
         MVI   WORK,14             SET FLDHDR LEN                               
         MVI   WORK+5,6            SET INPUT FIELD LEN                          
         MVC   WORK+8(6),0(R4)                                                  
         GOTO1 DEMOVAL,DMCB,(1,WORK),(1,(R5)),(C'S',ADBLOCK),0                  
         CLI   4(R1),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,6(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNH   PH52                                                             
         LA    R5,3(R5)                                                         
         BCT   R0,PH50                                                          
*                                                                               
PH52     LHI   RE,41                                                            
         SR    RE,R0                                                            
         STH   RE,NDEMOS           SAVE NUMBER OF DEMOS                         
                                                                                
*==========================================================                     
* SEE WHICH DEMOS ARE IN THE ESTIMATE HEADER                                    
*==========================================================                     
                                                                                
         LA    R1,DEMOLIST                                                      
         LH    R0,NDEMOS                                                        
*                                                                               
PH54     BAS   RE,CHKDEM                                                        
         BE    *+10                                                             
         MVC   0(3,R1),=X'FFFFFF'  SET TO IGNORE THIS DEMO                      
         LA    R1,3(R1)                                                         
         BCT   R0,PH54                                                          
         J     EQXIT                                                            
*                                                                               
CHKDEM   NTR1                                                                   
         L     RE,ADEST                                                         
         AHI   RE,EDEMOS-ESTHDR                                                 
         LA    RF,20                                                            
*                                                                               
CHKDEM2  CLC   0(3,R1),0(RE)       MATCH DEMO                                   
         JE    EQXIT                                                            
         LA    RE,3(RE)                                                         
         BCT   RF,CHKDEM2                                                       
         J     NEQXIT                                                           
*                                                                               
BADMAST  AP    MASTERRS,=P'1'                                                   
         MVC   P(44),=C'** ERROR ** BAD MASTER ESTIMATE. RUN ABORTED'           
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
BADREDIR AP    MASTERRS,=P'1'                                                   
         MVC   P(44),=C'** ERROR ** BAD EST REDIRECTION. RUN ABORTED'           
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*=============================================================                  
* PROCESS BUY* AND ASSOCIATED OBJECTS                                           
*=============================================================                  
                                                                                
PROCB    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
PB2      MVC   P(19),=C'PROCESSING DATA FOR'                                    
         MVC   P+20(1),QMED                                                     
         MVC   P+22(3),QCLT                                                     
         MVC   P+26(3),=C'POL'                                                  
         MVC   P+30(3),QEST                                                     
         MVC   P+34(8),IQSTA                                                    
*                                                                               
         CLI   IQSTA+3,C'+'                                                     
         BNE   *+8                                                              
         MVI   IQSTA+3,C' '                                                     
*                                                                               
         CLI   IQSTA+4,C'+'                                                     
         BNE   *+8                                                              
         MVI   IQSTA+4,C' '                                                     
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),IQSTA                                                   
*                                                                               
         CLI   KEY+6,C'-'          IF INPUT IS XXXX-Y                           
         BNE   *+10                                                             
         MVC   KEY+6(1),IQSTA+5    THEN MOVE THE Y IN                           
*                                                                               
         CLI   KEY+6,C' '                                                       
         BH    *+8                                                              
         MVI   KEY+6,C'T'                                                       
*                                                                               
         CLI   KEY+6,C'C'          GET RID OF -C FUNNIES                        
         BNE   *+8                                                              
         MVI   KEY+6,C'T'                                                       
*                                                                               
         MVC   DDSSTA,SPACES                                                    
         MVC   DDSSTA(5),KEY+2     SAVE THE STATION WE READ FOR                 
         CLI   DDSSTA,C'0'                                                      
         BL    *+10                                                             
         MVC   DDSSTA+5(3),IQSTA+5  MOVE CABLE NETWORK                          
*                                                                               
         MVC   KEY+7(2),QAGY                                                    
         MVC   KEY+9(3),QCLT                                                    
*                                                                               
         GOTO1 HIGHSTA                                                          
         L     R6,ADSTAT                                                        
         CLC   KEY(12),0(R6)                                                    
         BE    PB4                                                              
*                                                                               
         MVC   KEY+9(3),=C'000'                                                 
         GOTO1 HIGHSTA                                                          
         CLC   KEY(12),0(R6)                                                    
         BNE   PBNOSTA                                                          
*                                                                               
         USING STARECD,R6                                                       
PB4      MVC   MKT,SMKT                                                         
         MVC   STA(5),STAKCALL                                                  
         MVC   BUTAX,SNEWTAX       SAVE TAX RATE                                
         DROP  R6                                                               
* USE STAPACK, SO CAN GIVE ERROR IF STATION NOT VALID                           
         XC    STAPWORK,STAPWORK                                                
S        USING STAPACKD,STAPWORK                                                
         MVI   S.STAPACT,C'P'                                                   
         MVC   S.STAPACOM,ACOMFACS                                              
         MVC   S.STAPAGY,QAGY                                                   
         MVC   S.STAPMED,QMED                                                   
         MVI   S.STAPCTRY,C'U'                                                  
         MVC   S.STAPQMKT,MKT                                                   
         MVC   S.STAPQSTA(8),DDSSTA                                             
*                                                                               
         GOTO1 VSTAPACK,STAPWORK                                                
         CLI   S.STAPERR,0                                                      
         BNE   PBNOSTA                                                          
*                                                                               
         MVC   SVIQSTA,IQSTA       SAVE CURRENT STATION                         
         MVC   BMKTSTA,S.STAPMKST                                               
         DROP  S                                                                
* FLAG THE STALIST ENTRY THAT THIS STATION RECEIVED                             
         GOTO1 BINSRCH,STALPARS,BMKTSTA                                         
         CLI   0(R1),0                                                          
         BNE   PB6                                                              
         L     RE,0(R1)                                                         
         MVI   5(RE),C'Y'          SET STATION RECEIVED FLAG                    
*                                                                               
PB6      L     R0,ABUYTAB          CLEAR THE BUY TABLE                          
         LHI   R1,BUYTABX-BUYTAB                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    BUYCNT,BUYCNT                                                    
         XC    BUYLINES,BUYLINES                                                
         XC    STATOTG,STATOTG                                                  
         MVI   ALLUSED,C'N'                                                     
                                                                                
*============================================================                   
* READ ALL THE EXISTING BUYS ON THIS STATION                                    
* FOR ALL BROADCAST MONTHS                                                      
*============================================================                   
                                                                                
         L     R4,ABUYTAB                                                       
         USING BUYTABD,R4                                                       
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         NI    DMOUTBTS,X'FD'                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),BAGYMD                                                    
         MVC   KEY+1(2),BCLT                                                    
         MVI   KEY+3,X'FF'                                                      
         MVC   KEY+4(5),BMKTSTA                                                 
         MVC   KEY+9(1),BEST                                                    
         MVI   KEY+11,1            START AT LINE 1 STUPIDO                      
         MVC   SVBUYKEY,KEY                                                     
         GOTO1 HIGH                                                             
         B     PB12                                                             
*                                                                               
PB10     GOTO1 SEQ                                                              
*                                                                               
PB12     CLC   KEY(10),KEYSAVE     A-M/CLT/PRD/MKT/STA/EST                      
         BNE   PB30                                                             
         CLI   KEY+10,0            ACTIVE POINTERS ONLY                         
         BNE   PB10                                                             
*                                                                               
         SR    RE,RE               FLAG NUMBER USED IN LIST                     
         IC    RE,KEY+11                                                        
         LA    RE,BUYLINES(RE)                                                  
         MVC   0(1,RE),KEY+11                                                   
*                                                                               
         TM    KEY+13,X'80'        TEST DELETED                                 
         BO    PB10                YES - DON'T REUSE                            
*                                                                               
         L     R0,BUYCNT                                                        
         AHI   R0,1                                                             
         ST    R0,BUYCNT                                                        
*                                                                               
         MVC   BUYTABID+7(1),KEY+11  SET LINE NUMBER SO NO DUP IDS              
         MVC   BUYTABLN,KEY+11                                                  
         MVC   BUYTABDA,KEY+14                                                  
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
PB14     BRAS  RE,NEXTEL                                                        
         BNE   PB20                                                             
         OC    4(2,R6),4(R6)       TEST SPOT PAID                               
         BZ    *+8                                                              
         OI    BUYTABIN,X'80'                                                   
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AR    RE,R6               LOOK AT NEXT ELEM                            
         CLI   0(RE),X'10'         TEST MATCHED - YOU SFI!                      
         BNE   *+8                                                              
         OI    BUYTABIN,X'40'      SET MATCHED FLAG                             
         B     PB14                                                             
*                                                                               
PB20     LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'95'                                                     
         MVI   ELCDHI,X'95'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   PB22                                                             
* SAVE UID                                                                      
         USING BUPELEM,R6                                                       
         MVC   BUYTABID,BUPUID     SAVE UNIQUEID                                
         MVC   BUYTABMO,BDEND+1    SAVE BROADCAST MONTH END DATE                
         DROP  R6                                                               
*                                                                               
PB22     LA    R4,L'BUYTAB(R4)                                                  
         B     PB10                                                             
         EJECT                                                                  
*=============================================================                  
* SORT BUYTAB ON UNIQUEID                                                       
*=============================================================                  
                                                                                
PB30     LA    R1,DMCB                                                          
         MVC   0(4,R1),ABUYTAB                                                  
         ICM   R0,15,BUYCNT                                                     
         ST    R0,BUYPAR3          SET NUMRECS IN BINSRCH PARMS                 
         BZ    PB40                                                             
         ST    R0,4(R1)                                                         
         LA    R0,L'BUYTAB         REC LEN                                      
         ST    R0,8(R1)                                                         
         LHI   R0,BUYTABKL         KEY LEN                                      
         ST    R0,12(R1)                                                        
         SR    R0,R0                                                            
         ST    R0,16(R1)           KEY DSPL                                     
         GOTO1 XSORT,(R1)                                                       
                                                                                
*=============================================================                  
* FIND UID IN TABLE                                                             
*=============================================================                  
                                                                                
PB40     XC    SVUNIQ,SVUNIQ                                                    
         MVC   SVUNIQ(8),IQUNIQ                                                 
         MVC   SVUNIQ+8(1),BUEND+1 SET BDCST MONTH                              
*                                                                               
         GOTO1 BINSRCH,BUYPARS,(2,SVUNIQ)                                       
         CLI   0(R1),1             TEST NOT FOUND                               
         BE    PB41                                                             
*                                                                               
         L     RE,0(R1)                                                         
         CLC   SVUNIQ(9),0(RE)                                                  
         BE    PB42                FOUND IT                                     
*                                                                               
PB41     XC    BUYPAR1,BUYPAR1     SET FLAG NO ENTRY FOUND                      
         B     PB44                                                             
*                                                                               
PB42     ICM   R4,15,0(R1)                                                      
         USING BUYTABD,R4                                                       
         MVC   SVBUYKEY+11(1),BUYTABLN  SET LINE NUMBER                         
*                                                                               
         MVI   BUYTABUS,C'Y'       SET FLAG RECORD USED                         
         MVI   PAIDBUY,C'N'                                                     
*                                                                               
         CLC   =C'SJ',QAGY         ALLOW TESTING ON SJR                         
         BE    PB44                                                             
         CLI   BUYTABIN,0          TEST PAID OR MATCHED                         
         BE    PB44                                                             
         MVI   PAIDBUY,C'Y'        SET ERROR FLAG                               
         AP    PAIDERRS,=P'1'                                                   
         MVC   P(26),=C'** ERROR ** PAID BUY FOUND'                             
         GOTO1 REPORT                                                           
*                                                                               
PB44     XC    BUYTOTG,BUYTOTG                                                  
         BRAS  RE,BLDBUY           CREATE A NEW BUY RECORD                      
*                                                                               
         BRAS  RE,FILEGET                                                       
         CLC   IQTYPE,=C'DEM*'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,BLDDEM                                                        
*                                                                               
         BRAS  RE,FILEGET                                                       
         CLC   =C'COM*',IQTYPE                                                  
         BNE   PB46                                                             
*                                                                               
         XC    ELEM,ELEM           CLEAR FOR ASCENDING COMMENT NUMBERS          
         MVI   ELEM,X'66'                                                       
*                                                                               
PB45     BRAS  RE,BLDCOM                                                        
         BRAS  RE,FILEGET                                                       
         CLC   =C'COM*',IQTYPE                                                  
         BE    PB45                                                             
*                                                                               
PB46     CLC   IQTYPE,=C'DEM*'                                                  
         BNE   *+8                                                              
         BRAS  RE,FILEGET                                                       
*                                                                               
         CLC   IQTYPE,=C'ROT*'                                                  
         BE    PB47                                                             
         CLC   IQTYPE,=C'BUY*'     SOMETIMES THEY DON'T SEND SPOTS              
         BE    PB50                SO THERE'S NOT MUCH TO DO                    
         J     PB80                                                             
*                                                                               
PB47     MVI   OVERFLOW,C'N'                                                    
*                                                                               
PB48     BRAS  RE,BLDROT           IGNORE ERROR ON RETURN                       
*                                                                               
         BRAS  RE,FILEGET                                                       
         CLC   IQTYPE,=C'ROT*'                                                  
         BE    PB48                                                             
         CLI   QOPT1,C'Y'          TEST PRINTING DETAILS                        
         BNE   PB49                                                             
*                                                                               
         MVC   P(14),=C'BUY TOTALS ID='                                         
         MVC   P+14(8),SVUNIQ                                                   
         L     R0,BUYTOTG                                                       
         EDIT  (R0),(10,P+23),2,FLOAT=$                                         
         GOTO1 REPORT                                                           
*                                                                               
PB49     XC    BUYTOTG,BUYTOTG                                                  
         CLI   PAIDBUY,C'Y'        TEST BUY PAID                                
         BE    PB80                YES -DON'T CHANGE IT!                        
         CLI   OVERFLOW,C'Y'                                                    
         BE    PB80                IF ERRORS - DON'T ADD BUY                    
*                                                                               
         BRAS  RE,SETNPW           FIND MOST FREQ NPW VALUE                     
*                                                                               
PB50     OC    BUYPAR1,BUYPAR1     TEST OVERWRITING EXISTING                    
         BNZ   PB70                YES                                          
* FIND NEXT AVAILABLE LINE NUMBER                                               
         CLI   ALLUSED,C'Y'                                                     
         BE    PB80                                                             
         LA    RE,BUYLINES+1       A(START)                                     
         LA    RF,BUYLINES+255     A(END)                                       
         SR    R0,R0               X'00' IS WHAT WE LOOKIN FO                   
PB52     SRST  RF,RE                                                            
         BC    1,PB52              CONTINUE SEARCH                              
         BC    4,PB54              FOUND IT                                     
         AP    NOLINES,=P'1'                                                    
         MVI   ALLUSED,C'Y'                                                     
         MVC   P(34),=C'*** ERROR *** NO MORE BUYLINE NUMSBERS'                 
         GOTO1 REPORT                                                           
         B     PB80                                                             
*                                                                               
PB54     LR    RE,RF               POINT RE TO NEXT FREE SLOT                   
         LA    R0,BUYLINES         GIVES LINE NUMBER                            
         SR    RF,R0                                                            
         STC   RF,0(RE)            MARK THIS ENTRY USED                         
*                                                                               
         L     R8,ADBUY                                                         
         STC   RF,10(R8)           SET LINE NUMBER IN BUY                       
         MVI   11(R8),1            SET X'01' IN BUYREC+11!                      
         MVC   AREC,ADBUY                                                       
         GOTO1 ADD                                                              
         AP    ADDCNT,=P'1'                                                     
         MVC   KEY+14(4),KEY       MOVE DISK ADDRESS ADDED                      
         MVC   KEY(4),0(R8)        RESTORE PART OF KEY DESTROYED                
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         B     PB56  <<<<< NOP >>>>>>>                                          
         BNE   PB56                                                             
         MVC   P(6),=C'ADDING'                                                  
         GOTO1 HEXOUT,DMCB,ADBUY,P+10,24,=C'TOG'                                
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,KEY,P+10,18                                          
         GOTO1 REPORT                                                           
**NOP**  GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(24,13),PRINT,HEXOUT                
                                                                                
*============================================================                   
* NEED TO ADD SPTDIR RECORDS FOR PRDS                                           
* ADDREC CODE IN DMDAPTRS ONLY ADDS MASPRD POINTERS                             
*============================================================                   
                                                                                
PB56     XC    NEWPRDL,NEWPRDL                                                  
         L     R8,ADBUY                                                         
         LA    R7,NEWPRDL                                                       
         BAS   RE,BLDPRDL                                                       
*                                                                               
         LHI   R0,254              EXCLUDE X'00'/X'FF'                          
         LA    R7,NEWPRDL+1                                                     
*                                                                               
PB60     CLI   0(R7),0             TEST FOR PRODUCT                             
         BE    PB62                                                             
         MVC   KEY(10),0(R8)       A-M/CLT/XX/MKT/STA/EST                       
         MVC   KEY+3(1),0(R7)      PRD                                          
         MVI   KEY+10,X'FF'        PASSIVE FLAG                                 
         MVI   KEY+11,0                                                         
         MVC   KEY+12(1),10(R8)    LINE NUMBER                                  
         GOTO1 ADDIR                                                            
*&&DO                                                                           
         GOTO1 HEXOUT,DMCB,KEY,P+10,18                                          
         GOTO1 REPORT                                                           
*&&                                                                             
*                                                                               
PB62     AHI   R7,1                                                             
         BCT   R0,PB60                                                          
         B     PB80                                                             
         EJECT                                                                  
*===========================================================                    
* OVERWRITE EXISTING BUY RECORD                                                 
*===========================================================                    
                                                                                
PB70     XC    KEY,KEY                                                          
         MVC   KEY,SVBUYKEY                                                     
         MVC   KEY+14(4),BUYTABDA                                               
*                                                                               
         L     RE,ADBUY                                                         
         MVC   10(1,RE),SVBUYKEY+11 SET LINE NUMBER IN NEW RECORD               
*                                                                               
         L     R0,=A(IQIO)                                                      
         ST    R0,AREC                                                          
         GOTO1 GET                 READ EXISTING BUY RECORD                     
                                                                                
* TEST IF RECORD HAS CHANGED                                                    
         L     RE,=A(IQIO)                                                      
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)                                                      
         AHI   R1,-BUPLENQ         DON'T COMPARE UPLOAD ELEMENTS                
         LR    R0,RE                                                            
*                                                                               
         L     RE,ADBUY                                                         
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         AHI   RF,-BUPLENQ         DON'T COMPARE UPLOAD ELEMENTS                
         CLCL  R0,RE                                                            
         BNE   PB72                                                             
         AP    UNCHGCNT,=P'1'                                                   
         CLI   QOPT1,C'Y'                                                       
         B     PB80  <<<<<<  NOP >>>>>>                                         
         BNE   PB80                                                             
         MVC   P(9),=C'UNCHANGED'                                               
         B     PB78                                                             
*                                                                               
PB72     XC    OLDPRDL,OLDPRDL     CLEAR OLD PRD LIST                           
         XC    NEWPRDL,NEWPRDL     CLEAR NEW PRD LIST                           
*                                                                               
         L     R8,=A(IQIO)                                                      
         USING BUYRECD,R8                                                       
*                                                                               
         LA    R7,OLDPRDL                                                       
         BAS   RE,BLDPRDL                                                       
*                                                                               
         L     R8,ADBUY                                                         
         LA    R7,NEWPRDL                                                       
         BAS   RE,BLDPRDL                                                       
*                                                                               
         LHI   R0,254              EXCLUDE X'00'/X'FF'                          
         L     R1,PRDLIST          POINT TO ADDED PRD LIST                      
         XC    0(256,R1),0(R1)                                                  
         LA    RE,OLDPRDL+1                                                     
         LA    RF,NEWPRDL+1                                                     
*                                                                               
PB74     CLC   0(1,RE),0(RF)       TEST SAME IN BOTH LISTS                      
         BE    PB76                                                             
         CLI   0(RF),0             TEST NEW ENTRY IS NULL                       
         BE    PB76                                                             
         MVC   0(1,R1),0(RF)       THEN PUT NEW ENTRY IN LIST                   
         LA    R1,1(R1)                                                         
*                                                                               
PB76     LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PB74                                                          
         MVC   DMCB+20(4),PRDLIST  SET PRDLIST ADDRESS IN DMCB                  
         GOTO1 PUTBUY                                                           
         AP    REPLCNT,=P'1'                                                    
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         B     PB80   <<<<<< NOP >>>>>>                                         
         BNE   PB80                                                             
         MVC   P(9),=C'REPLACING'                                               
*                                                                               
PB78     GOTO1 HEXOUT,DMCB,ADBUY,P+10,24,=C'TOG'                                
         GOTO1 REPORT                                                           
**NOP**  GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(24,13),PRINT,HEXOUT                
         EJECT                                                                  
PB80     CLC   IQTYPE,=C'EOF*'                                                  
         BE    PB82                                                             
         CLC   IQTYPE,=C'BUY*'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   IQSTA+3,C'+'                                                     
         BNE   *+8                                                              
         MVI   IQSTA+3,C' '                                                     
*                                                                               
         CLI   IQSTA+4,C'+'                                                     
         BNE   *+8                                                              
         MVI   IQSTA+4,C' '                                                     
*                                                                               
         CLC   IQSTA,SVIQSTA       TEST CHANGE OF STATION                       
         BE    PB40                NO - GO PROCESS                              
*                                                                               
PB82     MVC   P(10),=C'TOTALS FOR'                                             
         MVC   P+11(7),SVIQSTA                                                  
         L     R0,STATOTG                                                       
         EDIT  (R0),(10,P+20),2,FLOAT=$                                         
         GOTO1 REPORT                                                           
         XC    STATOTG,STATOTG                                                  
                                                                                
*==========================================================                     
* DELETE ALL BUY RECORDS FOR THIS BROACAST MONTH NOT REUSED                     
* EXCEPT FOR THOSE ADDED BY DDS, WHICH WILL NOT HAVE A UID                      
* UPLOADED RECORDS HAVE LEADING SPACES IN BUYID FIELD                           
*===========================================================                    
                                                                                
PB90     L     R4,ABUYTAB                                                       
         USING BUYTABD,R4                                                       
*                                                                               
PB92     CLI   BUYTABUS,C'Y'                                                    
         BE    PB94                                                             
         CLI   BUYTABIN,0          TEST BUY PAID/MATCHED                        
         BNE   PB94                YES - NEVER DELETE                           
*                                                                               
         CLI   BUYTABID,C' '       TEST NO ID                                   
         BL    PB94                YES - SKIP                                   
*                                                                               
         CLC   BUYTABMO,BUEND+1    MATCH UPLOAD BROADCAST MONTH                 
         BNE   PB94                NO - SKIP                                    
*                                                                               
         XC    KEY,KEY             DELETE THIS UNUSED BUYLINE                   
         MVC   KEY(13),SVBUYKEY                                                 
         MVC   KEY+11(1),BUYTABLN  SET LINE NUMBER                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         MVC   PMSG(7),=C'DELETED'                                              
         BRAS  RE,PRTBUY                                                        
         AP    DELCNT,=P'1'                                                     
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PB94                                                             
*                                                                               
         L     R8,ADBUY                                                         
         OI    15(R8),X'80'        DELETE BUY                                   
*                                                                               
         GOTO1 PUTBUY                                                           
         OI    KEY+13,X'80'        AND POL POINTER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
*                                                                               
PB94     LA    R4,L'BUYTAB(R4)                                                  
         OC    BUYTABID,BUYTABID   TEST ENTRY PRESENT                           
         BNZ   PB92                                                             
         CLC   IQTYPE,=C'EOF*'                                                  
         JE    EXIT                                                             
         B     PB2                 GO PROCESS NEW STATION                       
*                                                                               
PBNOSTA  MVC   SVIQSTA,IQSTA       SAVE CURRENT STATION                         
         MVC   P(19),=C'** ERROR ** STATION'                                    
         MVC   P+20(1),QMED                                                     
         MVC   P+22(6),IQSTA                                                    
         MVC   P+29(9),=C'NOT FOUND'                                            
         GOTO1 REPORT                                                           
         AP    STAERRS,=P'1'                                                    
*                                                                               
PBNOSTA2 BRAS  RE,FILEGET          SKIP TO NEXT STATION                         
*                                                                               
         CLC   IQTYPE,=C'EOF*'                                                  
         JE    EXIT                                                             
*                                                                               
         CLC   IQTYPE,=C'BUY*'                                                  
         BNE   PBNOSTA2                                                         
         CLC   IQSTA,SVIQSTA       TEST CHANGE OF STATION                       
         BE    PBNOSTA2                                                         
         B     PB2                 YES - TRY NEXT STATION                       
         EJECT                                                                  
*==============================================================                 
* FLAG ALL PRDLIST ENTRIES IN TABLE AT 0(R7)                                    
*==============================================================                 
                                                                                
BLDPRDL  NTR1                                                                   
         LA    R6,24(R8)                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
BLDPRD2  BRAS  RE,NEXTEL                                                        
         BNE   BLDPRDX                                                          
         CLI   1(R6),10                                                         
         BNH   BLDPRD2                                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AHI   R0,-10                                                           
         SRL   R0,2                                                             
         LA    R1,10(R6)                                                        
*                                                                               
BLDPRD4  SR    RE,RE                                                            
         IC    RE,0(R1)            GET PRD CODE                                 
         AR    RE,R7               POINT TO SLOT IN TABLE                       
         MVC   0(1,RE),0(R1)       SET PRD CODE IN ITS OWN ENTRY                
         LA    R1,4(R1)                                                         
         BCT   R0,BLDPRD4                                                       
         B     BLDPRD2                                                          
*                                                                               
BLDPRDX  XIT1                                                                   
         LTORG                                                                  
*                                                                               
FILEGET  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,IQREC                                                         
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,IQREC                                                         
         GET   FILEIN,(0)                                                       
         AP    INCNT,=P'1'                                                      
         CP    INCNT,INMAX                                                      
         JH    NEQXIT                                                           
         CLI   QOPT3,C'Y'          TEST TO PRINT INPUT                          
         BNE   FILEGETX                                                         
         MVC   P(110),IQREC                                                     
         GOTO1 REPORT                                                           
*                                                                               
FILEGETX J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* BUILD A BUY RECORD                                                            
*===============================================================                
                                                                                
BLDBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         L     R0,ADBUY            ALWAYS CREATE A NEW BUY RECORD               
         LHI   R1,4096                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   0(10,R8),SVBUYKEY   SET A-M/CLT/PRD/MKT-STA/EST                  
         MVC   BUYALPHA,QAGY                                                    
         MVI   24(R8),1            SET BDELEM CODE                              
         MVI   25(R8),BDELEMX-BDELEM  AND LENGTH                                
         LHI   R0,24                                                            
         AHI   R0,BDELEMX-BDELEM                                                
         STCM  R0,3,13(R8)         SET RECORD LEN                               
*                                                                               
         MVC   BDSTART(6),BUSTART   SET BDCST MONTH AS BUY DATES                
         MVC   BDWKS,BUWKS          SET WEEKS IN MONTH                          
         MVI   BDINPUT,1                                                        
         MVI   BDWKIND,C'O'                                                     
         MVC   BDDAYPT,IQDPT                                                    
* VALIDATE DAYPART                                                              
         L     RE,ADDPTTAB                                                      
         SR    RF,RF                                                            
*                                                                               
BLDB2    CLC   0(1,RE),IQDPT                                                    
         BE    BLDB4                                                            
         LA    RE,5(RE)                                                         
         AHI   RF,1                                                             
         CHI   RF,36                                                            
         BL    BLDB2                                                            
         B     BLDB6                                                            
*                                                                               
BLDB4    CLI   IQDPT,C'Z'          Z IS NOT VALID                               
         BNE   BLDB10                                                           
*                                                                               
BLDB6    B     *+8                 NOP INVALID DAYPART MESSAGE                  
         BAS   RE,BADDPT                                                        
*                                                                               
BLDB10   PACK  DUB,IQROT                                                        
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R0,BDDAY                                                         
* FIND HIGHEST DAY BIT THAT IS ON                                               
         LHI   RE,1                                                             
         LA    RF,X'40'                                                         
BLDB20   EX    RF,*+8                                                           
         B     *+8                                                              
         TM    BDDAY,0                                                          
         BO    BLDB22                                                           
         AHI   RE,1                                                             
         SRL   RF,1                                                             
         B     BLDB20                                                           
*                                                                               
BLDB22   STC   RE,BDSEDAY                                                       
* NOW FIND THE LOWEST                                                           
         LHI   RE,7                                                             
         LA    RF,X'01'                                                         
*                                                                               
BLDB24   EX    RF,*+8                                                           
         B     *+8                                                              
         TM    BDDAY,0                                                          
         BO    BLDB26                                                           
         BCTR  RE,0                                                             
         SLL   RF,1                                                             
         B     BLDB24                                                           
*                                                                               
BLDB26   IC    RF,BDSEDAY          RETRIEVE START DAY                           
         SLL   RF,4                LEFT ALIGN IN BYTE                           
         OR    RE,RF               OR IN END DAY                                
         STC   RE,BDSEDAY                                                       
                                                                                
* NOW ADJUST START/END DATES TO AGREE WITH START DAY                            
         SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         SRL   R0,4                                                             
         AHI   R0,-1               GET NUMBER OF DAYS TO ADJUST                 
         BNP   BLDB28                                                           
*                                                                               
         LA    R4,BDSTART                                                       
         BAS   RE,ADJDATE                                                       
*                                                                               
BLDB28   SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         N     R0,=X'0000000F'     ISOLATE END DAY                              
         AHI   R0,-7                                                            
         BZ    BLDB30                                                           
*                                                                               
         LA    R4,BDEND                                                         
         BAS   RE,ADJDATE                                                       
         B     BLDB30                                                           
*                                                                               
ADJDATE  NTR1                                                                   
         GOTO1 DATCON,DMCB,(3,(R4)),WORK                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,DMCB,WORK+6,(3,(R4))                                      
         J     EXIT                                                             
*                                                                               
BLDB30   PACK  DUB,IQSTIM                                                       
         CVB   R0,DUB                                                           
         CHI   R0,2400                                                          
         BNH   *+8                                                              
         AHI   R0,-2400                                                         
         STCM  R0,3,BDTIMST                                                     
*                                                                               
         PACK  DUB,IQETIM                                                       
         CVB   R0,DUB                                                           
         STCM  R0,3,BDTIMEND                                                    
         CHI   R0,2400                                                          
         BNH   *+8                                                              
         AHI   R0,-2400                                                         
         CLC   BDTIMST,BDTIMEND                                                 
         BNE   *+10                                                             
         XC    BDTIMEND,BDTIMEND   DO NOT SET END TIME=START TIME               
*                                                                               
         MVC   BDDAYPT,IQDPT                                                    
         PACK  DUB,IQSLN                                                        
         CVB   R0,DUB                                                           
         STC   R0,BDSEC                                                         
         MVC   BDPROGRM(17),IQPROG                                              
         MVI   BDPROGRM+17,C' '                                                 
*                                                                               
         MVI   BDCIND,BDCGROSQ                                                  
         MVC   BDNTAX,BUTAX                                                     
         MVC   SVCOSTY,IQCOSTY                                                  
         CLI   IQCOSTY,C'P'        TEST NTP (TRADE)                             
         BNE   BLDBX                                                            
         MVI   BDCIND,BDCNTPQ                                                   
*                                                                               
         LA    RE,BDPROGRM+14                                                   
         LHI   RF,15                                                            
*                                                                               
BLDB32   CLI   0(RE),C' '          FIND LAST NON-BLANK                          
         BH    BLDB34                                                           
         BCTR  RE,0                                                             
         BCT   RF,BLDB32                                                        
*                                                                               
BLDB34   MVC   1(2,RE),=C'-T'                                                   
*                                                                               
BLDBX    J     EXIT                                                             
                                                                                
*==========================================================                     
* PRINT INVALID DAYPART MESSAGE (AND CONTINUE PROCESSING)                       
*==========================================================                     
                                                                                
BADDPT   NTR1                                                                   
         AP    DPTERRS,=P'1'                                                    
         MVC   P(27),=C'** ERROR ** INVALID DAYPART'                            
         MVC   P+30(IQBUYX-IQBUY),IQBUY                                         
         GOTO1 REPORT                                                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* PROCESS DEMO OBJECT                                                           
*===========================================================                    
                                                                                
BLDDEM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'02'                                                       
*                                                                               
         LA    R1,ELEM+24                                                       
         LA    R2,DEMOLIST                                                      
         LA    RE,IQDMVALS                                                      
         LH    RF,NDEMOS                                                        
*                                                                               
BLDDEM2  CLC   0(3,R2),=X'FFFFFF' TEST NOT IN ESTHDR                            
         BE    BLDDEM4                                                          
*                                                                               
         MVC   0(3,R1),0(R2)       MOVE DEMO TYPE                               
         MVI   3(R1),100           SET SVI                                      
         SR    R0,R0                                                            
         CLC   0(6,RE),SPACES      TREAT SPACES AS 0                            
         BE    BLDDEM2A                                                         
         PACK  DUB,0(6,RE)                                                      
         SRP   DUB,64-1,5          ROUND TO 1 DECIMAL PRECISION                 
         CVB   R0,DUB                                                           
BLDDEM2A ST    R0,4(R1)                                                         
         OI    4(R1),X'80'         SET DEMO OVRD                                
         LA    R1,8(R1)                                                         
*                                                                               
BLDDEM4  LA    R2,3(R2)                                                         
         LA    RE,6(RE)                                                         
         BCT   RF,BLDDEM2                                                       
*                                                                               
         LA    R0,ELEM                                                          
         SR    R1,R0                                                            
         STC   R1,ELEM+1           SET ELEMENT LENGTH                           
*                                                                               
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         AR    R6,R0               ADD DEMEL AT EOR                             
         BRAS  RE,ADDEL                                                         
         JNE   EXIT                                                             
                                                                                
B        USING BUPELEM,ELEM                                                     
*                                                                               
         XC    ELEM,ELEM           ADD UPLOAD ELEMENT                           
         MVI   B.BUPCODE,X'95'                                                  
         MVI   B.BUPLEN,BUPLENQ                                                 
         MVC   B.BUPDAT,TODAYB                                                  
         MVC   B.BUPUID,SVUNIQ                                                  
         DROP  B                                                                
*                                                                               
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         AR    R6,R0               ADD AT EOR                                   
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* PROCESS COMMENT OBJECT                                                        
*===========================================================                    
                                                                                
BLDCOM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         IC    R0,ELEM+2                                                        
         AHI   R0,1                                                             
         STC   R0,ELEM+2           SET COMMENT NUMBER                           
*                                                                               
BLDCOM2  LA    R4,IQCOMS+70        POINT TO END OF DATA                         
         LHI   R5,70                                                            
*                                                                               
BLDCOM4  CLI   0(R4),C' '                                                       
         BH    BLDCOM6                                                          
         BCTR  R4,0                                                             
         BCT   R5,BLDCOM4                                                       
         J     EXIT                NO MORE COMMENTS                             
*                                                                               
BLDCOM6  BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+3(0),IQCOMS                                                 
         AHI   R5,4                                                             
         STC   R5,ELEM+1                                                        
*                                                                               
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         AR    R6,R0               ADD AT EOR                                   
         BRAS  RE,ADDEL                                                         
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* PROCESS  ROT* OBJECT                                                          
*===========================================================                    
                                                                                
BLDROT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         PACK  DUB,IQRCOST                                                      
         CVB   R0,DUB                                                           
         ST    R0,BUCOST                                                        
         CLI   BUCOST,0            IF FIRST BYTE 0, RATE IS IN PENNIES          
         BE    BLDR2                                                            
         SRDL  R0,32               ELSE CONVERT COST TO DOLLARS                 
         D     R0,=F'100'                                                       
         ST    R1,BUCOST                                                        
         OI    BDCIND2,BDCNBRDQ    SET RATE IN DOLLARS                          
*                                                                               
BLDR2    MVC   BDCOST,BUCOST+1                                                  
*                                                                               
         CLC   IQPRD,=C'POL'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,ADCLT                                                         
         LA    RE,CLIST-CLTHDR(RE)                                              
*                                                                               
BLDR10   CLC   IQRPRD,0(RE)                                                     
         BE    BLDR12                                                           
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   BLDR10                                                           
         CLC   IQRPRD,SVIQRPRD     TEST SAME ERROR AS LAST TIME                 
         BE    BLDR10X                                                          
         MVC   SVIQRPRD,IQRPRD     SAVE CURRENT PRODUCT                         
         MVC   P(19),=C'** ERROR ** PRODUCT'                                    
         MVC   P+20(1),QMED                                                     
         MVC   P+22(3),QCLT                                                     
         MVC   P+26(3),IQRPRD                                                   
         MVC   P+30(9),=C'NOT FOUND'                                            
         GOTO1 REPORT                                                           
BLDR10X  AP    PRDERRS,=P'1'                                                    
         J     EXIT                                                             
*                                                                               
BLDR12   MVC   BPRD,3(RE)          SAVE BINARY PRD CODE                         
*                                                                               
         SR    RF,RF               MAKE SURE BRAND ESTIMATE OPEN                
         IC    RF,3(RE)                                                         
         LA    RF,ESTLST(RF)                                                    
         CLI   0(RF),0             ZERO MEANS NO ESTIMATE OPEN                  
         BNE   BLDR14                                                           
*                                                                               
         CLC   ESTNOPRD,IQRPRD     TEST SAME ERROR AS LAST TIME                 
         BE    BLDR12X                                                          
         MVC   ESTNOPRD,IQRPRD     SAVE CURRENT PRODUCT                         
         MVC   P(20),=C'** ERROR ** ESTIMATE'                                   
         MVC   P+21(1),QMED                                                     
         MVC   P+23(3),QCLT                                                     
         MVC   P+27(3),IQRPRD                                                   
         MVC   P+31(3),QEST                                                     
         MVC   P+35(9),=C'NOT FOUND'                                            
         GOTO1 REPORT                                                           
BLDR12X  AP    ESTERRS,=P'1'                                                    
         J     EXIT                                                             
*                                                                               
BLDR14   LA    R3,IQRWK1           POINT TO SPOT COUNT LIST                     
         LA    R4,BUWKLIST                                                      
         SR    R5,R5                                                            
         IC    R5,BUWKS                                                         
*                                                                               
BLDR16   XC    ELEM,ELEM                                                        
         MVI   ELEM,X'0B'                                                       
         MVI   ELEM+1,14                                                        
         MVC   ELEM+2(2),0(R4)     SET WEEK DATE                                
         MVC   ELEM+10(1),BPRD                                                  
         MVC   ELEM+11(1),BDSEC                                                 
*                                                                               
         CLC   BDCOST,BUCOST+1     TEST COST MATCHES BUY COST                   
         BE    BLDR18                                                           
         MVC   ELEM+7(3),BUCOST+1  NO - SET AS COST OVERRIDE                    
         OI    ELEM+6,X'20'                                                     
*                                                                               
BLDR18   SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         SRL   R0,4                                                             
         AHI   R0,-1               GIVES DAYS TO ADVANCE                        
         BNP   BLDR20              NONE IF MONDAY                               
         GOTO1 DATCON,DMCB,(2,ELEM+2),WORK                                      
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,DMCB,WORK+6,(2,ELEM+2)                                    
*                                                                               
BLDR20   SR    R0,R0                                                            
         LA    R6,BDELEM                                                        
*                                                                               
BLDR22   ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),X'0B'                                                      
         BL    BLDR22                                                           
         BH    BLDR24              INSERT BEFORE ELEM > X'0B'                   
         CLC   2(2,R6),0(R4)       SPOT PRIOR TO THIS WEEK ?                    
         BL    BLDR22                                                           
*                                                                               
BLDR24   L     RF,BUCOST           GET COST/SPOT                                
         TM    BDCIND2,BDCNBRDQ                                                 
         BZ    *+8                                                              
         MHI   RF,100                                                           
         CLI   SVCOSTY,C'P'                                                     
         BNE   BLDR25                                                           
         AR    RF,RF               X 2                                          
         LHI   RE,570              GMI NTP FACTOR                               
         CLI   BPRD,X'40'          GMI BRANDS HAVE VALUES < X'40'               
         BL    BLDR24A                                                          
         CLI   BPRD,X'7F'          GMI BRANDS HAVE VALUES > X'7F'               
         BH    BLDR24A                                                          
         LHI   RE,1700                                                          
BLDR24A  MR    RE,RE               COST X NTP FACTOR                            
         D     RE,=F'10000'                                                     
         AHI   RF,1                                                             
         SRL   RF,1                                                             
*                                                                               
BLDR25   CLC   0(2,R3),=C'  '      TEST FOR SPACES (IQ HAS A BUG)               
         BNH   BLDR30                                                           
         PACK  DUB,0(2,R3)                                                      
         CVB   R0,DUB              NUMBER OF SPOTS                              
         LTR   R0,R0                                                            
         BZ    BLDR30                                                           
         MR    RE,R0               SPOTS X COST/SPOT TO RF                      
*                                                                               
         L     RE,BUYTOTG                                                       
         AR    RE,RF                                                            
         ST    RE,BUYTOTG                                                       
         L     RE,STATOTG                                                       
         AR    RE,RF                                                            
         ST    RE,STATOTG                                                       
         L     RE,CLTTOTG                                                       
         AR    RE,RF                                                            
         ST    RE,CLTTOTG                                                       
*                                                                               
BLDR26   BRAS  RE,ADDEL                                                         
         BNE   BLDRERR                                                          
         BCT   R0,BLDR26                                                        
*                                                                               
BLDR30   LA    R3,2(R3)            NEXT SPOTS/WEEK                              
         LA    R4,4(R4)            NEXT MONDAY DATE                             
         BCT   R5,BLDR16                                                        
         J     EXIT                                                             
*                                                                               
BLDRERR  MVI   OVERFLOW,C'Y'                                                    
         J     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* CALCULATE MOST FREQUENT NPW VALUE AND SET IN BDELEM                           
*=============================================================                  
                                                                                
SETNPW   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
                                                                                
* FIRST COUNT THE SPOTS IN EACH WEEK                                            
                                                                                
         LA    R1,BUWKLIST                                                      
         LA    R4,SPOTTAB                                                       
         XC    SPOTTAB,SPOTTAB                                                  
*                                                                               
SETNPW2  SR    R5,R5                                                            
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
*                                                                               
SETNPW4  BRAS  RE,NEXTEL                                                        
         BNE   SETNPW6                                                          
         CLC   2(2,R6),0(R1)       COMPARE TO WEEK START                        
         BL    SETNPW4             PRIOR TO START - SKIP                        
         CLC   2(2,R6),2(R1)       COMPARE TO WEEK END                          
         BH    SETNPW6             HIGH - DONE                                  
         AHI   R5,1                BUMP COUNTER                                 
         B     SETNPW4                                                          
*                                                                               
SETNPW6  STH   R5,0(R4)            SAVE SPOTS/WEEK                              
*                                                                               
         LA    R1,4(R1)            NEXT WEEK                                    
         OC    0(2,R1),0(R1)       TEST EOL                                     
         BZ    SETNPW10                                                         
         LA    R4,2(R4)                                                         
         B     SETNPW2                                                          
                                                                                
* FIND THE MOST FREQUENT NUMBER OF SPOTS (OTHER THAN 0)                         
                                                                                
SETNPW10 GOTO1 XSORT,DMCB,(C'D',SPOTTAB),5,2,2,0                                
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         LA    R2,SPOTTAB                                                       
         B     SETNPW22                                                         
*                                                                               
SETNPW20 OC    0(2,R2),0(R2)       TEST DONE                                    
         BZ    SETNPW28            0 IS EOL                                     
*                                                                               
         LA    R1,3(R1)            NEXT COUNTER                                 
*                                                                               
SETNPW22 MVC   1(2,R1),0(R2)       MOVE VALUE                                   
         MVI   0(R1),1             SET COUNTER TO 1                             
*                                                                               
SETNPW24 CLC   0(2,R2),2(R2)       TEST NEXT WEEK SAME VALUE                    
         BE    SETNPW26                                                         
         LA    R2,2(R2)            POINT TO NEW VALUE                           
         B     SETNPW20            YES                                          
*                                                                               
SETNPW26 IC    R0,0(R1)            BUMP COUNT                                   
         AHI   R0,1                                                             
         STC   R0,0(R1)                                                         
         LA    R2,2(R2)                                                         
         B     SETNPW24                                                         
*                                                                               
SETNPW28 GOTO1 XSORT,DMCB,(C'D',WORK),14,3,2,0                                  
         MVC   BDNOWK,WORK+2       THIS IS MOST FREQ NUM                        
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
ADDEL    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECUP,DMCB,ADBUY,ELEM,(C'R',(R6))                                
         CLI   8(R1),C'R'          TEST ELEMENT FIT                             
         JE    EQXIT                                                            
         AP    RECOVFL,=P'1'                                                    
         MVC   P(27),=C'** ERROR ** RECORD OVERFLOW'                            
         MVC   P+30(1),QMED                                                     
         MVC   P+32(3),QCLT                                                     
         MVC   P+36(3),IQRPRD                                                   
         MVC   P+42(8),SVUNIQ                                                   
         GOTO1 REPORT                                                           
         J     NEQXIT                                                           
*                                                                               
DELEL    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
         J     EXIT                                                             
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* BUILD A LIST OF ALL MARKET/STATIONS FOR THIS ESTIMATE                         
*===============================================================                
                                                                                
BLDSTLST NTR1  BASE=*,LABEL=*                                                   
         MVC   P(23),=C'ACTIVE STATION LIST FOR'                                
         MVC   P+24(1),QMED                                                     
         MVC   P+26(3),QCLT                                                     
         MVC   P+30(3),=C'POL'                                                  
         MVC   P+34(3),QEST                                                     
         LA    R0,IQEND-IQTYPE+SVIQHDR  POINT TO MONTH END DATE                 
         GOTO1 DATCON,DMCB,(R0),(6,P+38)  GET MMM/YY                            
         GOTO1 REPORT                                                           
*                                                                               
         L     R0,ASTALIST                                                      
         LHI   R1,STALISTX-STALIST                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         SR    R0,R0               CLEAR COUNTER                                
         L     R4,ASTALIST                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),BAGYMD                                                    
         MVC   KEY+1(2),BCLT                                                    
         MVI   KEY+3,X'FF'                                                      
*                                                                               
BLDST2   GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   BLDSTX                                                           
         CLC   KEY+9(1),BEST       MATCH ESTIMATE                               
         BE    BLDST10             YES - ADD TO LIST                            
         BH    BLDST12             HIGH- NEXT STATION                           
         MVC   KEY+9(1),BEST       ELSE READ FOR ESTIMATE                       
         XC    KEY+10(3),KEY+10                                                 
         B     BLDST2                                                           
*                                                                               
BLDST10  MVC   0(5,R4),KEY+4       SAVE MARKET STATION                          
*                                                                               
         LA    R4,6(R4)                                                         
         AHI   R0,1                                                             
         C     R0,STALPAR6         COMPARE TO MAX ENTRIES                       
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT1,C'Y'          TEST PRINTING DETAILS                        
         BNE   BLDST12                                                          
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),P+2,P+8                                
         GOTO1 REPORT                                                           
*                                                                               
BLDST12  MVC   KEY+9(4),=X'FFFFFFFF' FORCE NEXT STATION                         
         B     BLDST2                                                           
*                                                                               
BLDSTX   ST    R0,STALPAR3         SAVE RECORD COUNT                            
         MVI   FORCEHED,C'Y'                                                    
         J     EXIT                                                             
         LTORG                                                                  
*===============================================================                
* DELETE ALL BUYS FOR THIS BROADCAST MONTH FOR MKT-STA AT 0(R4)                 
*===============================================================                
                                                                                
DELBUYS  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(1),BAGYMD                                                    
         MVC   KEY+1(2),BCLT                                                    
         MVI   KEY+3,X'FF'                                                      
         MVC   KEY+4(5),0(R4)                                                   
         MVC   KEY+9(1),BEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(21),=C'DELETING BUYS FOR MKT'                                  
         GOTO1 MSUNPK,DMCB,(X'80',(R4)),WORK,WORK+4                             
         MVC   P+22(4),WORK                                                     
         MVC   P+27(7),=C'STATION'                                              
         MVC   P+35(8),WORK+4                                                   
         MVC   P+45(12),=C'NOT RECEIVED'                                        
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QOPT2,C'Y'          TEST SUPPRESS DELETION                       
         BNE   DELB4                                                            
         MVC   P+40(30),=C'** DO NOT DELETE OPTION SET **'                      
         GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
DELB2    GOTO1 SEQ                                                              
*                                                                               
DELB4    CLC   KEY(10),KEYSAVE                                                  
         JNE   EXIT                                                             
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         CLC   BDEND+1(1),BUEND+1  MATCH BROADCAST MONTH                        
         BNE   DELB2                                                            
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
DELB6    BRAS  RE,NEXTEL                                                        
         BNE   DELB8                                                            
         USING REGELEM,R6                                                       
         OC    RPAY,RPAY                                                        
         BZ    DELB6                                                            
         B     DELB2               IF PAID -SKIP                                
*                                                                               
DELB8    MVC   PMSG(7),=C'DELETED'                                              
         BRAS  RE,PRTBUY                                                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   DELB10                                                           
*                                                                               
         OI    BUYKEY+15,X'80'                                                  
         GOTO1 PUTBUY                                                           
*                                                                               
         OI    KEY+13,X'80'                                                     
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
*                                                                               
DELB10   OI    DMINBTS,X'08'       PASS DELETES                                 
         NI    DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                RESTORE DIR FOR SEQ                          
         NI    DMINBTS,X'F7'                                                    
         B     DELB2                                                            
         LTORG                                                                  
         EJECT                                                                  
PRTBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         MVC   PAGY(2),BUYALPHA                                                 
*                                                                               
         MVC   PMED,QMED                                                        
*                                                                               
         L     RE,ADCLT                                                         
         SR    R0,R0                                                            
         IC    R0,CPROF+6-CLTHDR(RE)                                            
         GOTO1 CLUNPK,DMCB,((R0),BUYKCLT),PCLT                                  
*                                                                               
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
*                                                                               
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BNE   *+10                                                             
         MVC   PSTA+5(2),=C'TV'                                                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         MVI   ELCDLO,X'95'                                                     
         MVI   ELCDHI,X'95'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   PRTB10                                                           
         USING BUPELEM,R6                                                       
         MVC   PUNIQID,BUPUID                                                   
         TM    BUPIND,BUPIDDS                                                   
         BZ    *+8                                                              
         MVI   PUNIQID+8,C'D'                                                   
         DROP  R6                                                               
*                                                                               
PRTB10   GOTO1 DATCON,DMCB,(3,BDSTART),(5,PDATES)                               
         MVI   PDATES+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,BDEND),(5,PDATES+9)                                 
*                                                                               
         GOTO1 REPORT                                                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* OPEN PRINT QUEUE                                                              
*=====================================================================          
                                                                                
OPNPQ    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,VMASTC                                                        
         USING MASTD,RE                                                         
*                                                                               
         L     R7,MCVREMOT                                                      
         USING REMOTED,R7                                                       
*                                                                               
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,DATAMGR                                                 
         DROP  RE                                                               
*                                                                               
         MVC   REMOTDST,=AL2(8093)    DDSEMAIL ID RECORD                        
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(3),=C'SIQ'                                              
         MVC   REMOTSYS+3(2),QAGY                                               
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'SIQ'                                                 
         J     EXIT                                                             
         DROP  R7                                                               
         LTORG                                                                  
                                                                                
*=====================================================================          
* CLOSE PRINT QUEUE                                                             
*=====================================================================          
                                                                                
CLSPQ    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* OUTPUT PQ LINES TO MAKE UP A NOTE                                             
*=====================================================================          
         SPACE 1                                                                
PUTPQ    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   LINE,0              SUPPRESS PAGING                              
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         MVI   FORCEFUT,C'N'                                                    
*                                                                               
         MVC   P(L'HDRREC),HDRREC        HEADER                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(L'TRNREC),TRNREC        TRN                                    
         GOTO1 REPORT                                                           
*                                                                               
         LA    R1,PXLIST                                                        
         MVC   RCPREC+15(60),0(R1)                                              
         MVC   P(L'RCPREC),RCPREC        TO                                     
         GOTO1 REPORT                                                           
         B     PUTPQ4                                                           
*                                                                               
PUTPQ2   MVC   CCRREC+15(60),0(R1)                                              
         MVC   P(L'CCRREC),CCRREC        CC                                     
         GOTO1 REPORT                                                           
*                                                                               
PUTPQ4   LA    R1,60(R1)                                                        
         CLI   0(R1),X'FF'         TEST EOL                                     
         BNE   PUTPQ2                                                           
*                                                                               
         MVC   SUBJLINE+3(2),AGY                                                
         MVC   SUBREC+15(L'SUBJLINE),SUBJLINE                                   
         MVC   P(L'SUBREC),SUBREC                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(L'ERRLINE),ERRLINE                                             
* GET JOB/STEP NAMES                                                            
         OC    SVJOBID,SVJOBID     TEST RUNNING REMOTE                          
         BNZ   PUTPQ6                                                           
*                                                                               
         LA    R4,FULL                                                          
         EXTRACT (R4),FIELDS=TIOT                                               
*                                                                               
         L     R4,FULL                                                          
         USING TIOTD,R4                                                         
         MVC   P+32(8),TIOCNJOB                                                 
         MVI   P+40,C'/'                                                        
         MVC   P+41(8),TIOCPSTN     PROC STEP NAME                              
         DROP  R4                                                               
         B     PUTPQ10                                                          
*                                                                               
PUTPQ6   MVC   P+32(3),SVJOBID                                                  
         MVI   P+35,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,SVRNO                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+36(5),DUB                                                      
*                                                                               
PUTPQ10  GOTO1 REPORT                                                           
         J     EXIT                                                             
         LTORG                                                                  
HDRREC   DC    C'    *HDR*                          P                  +        
                              M'                                                
*                                                                               
TRNREC   DC    C'++DDS X XXXTRN'                                                
*                                                                               
RCPREC   DC    CL80'++DDS      RCP '                                            
*                                                                               
CCRREC   DC    CL80'++DDS      CCR '                                            
*                                                                               
BCCREC   DC    CL80'++DDS      BCC '                                            
*                                                                               
SUBREC   DC    CL80'++DDS      SUB '                                            
*                                                                               
ERRLINE  DC    C'PLEASE CHECK OUTPUT FOR ERRORS'                                
SUBJLINE DC    CL50'SIQDF UPLOAD ERRORS FOUND'                                  
*                                                                               
PXLIST   DC    CL60'MARY.MCQUAID@PROGRAMEXCHANGE.COM'                           
         DC    CL60'JOSEPHINE.HO@PROGRAMEXCHANGE.COM'                           
         DC    CL60'CHRISTINE.NOTO@PROGRAMEXCHANGE.COM'                         
         DC    CL60'DRAYSIA.NOEL@PROGRAMEXCHANGE.COM'                           
PXLISTX  DC    X'FF'                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*SPIQWK*'                                                    
SPIQWK   DS    0D                                                               
                                                                                
DEMOVAL  DS    A                                                                
ABUYTAB  DC    A(BUYTAB)                                                        
ASTALIST DC    A(STALIST)                                                       
                                                                                
ELEM     DS    XL256                                                            
*                                                                               
SVUNIQ   DS    CL10                UNIQUEID/BRDMON/LINE                         
SVIQSTA  DS    CL8                                                              
DDSSTA   DS    CL8                 ACTUAL STATION WE READ FOR                   
SVIQRPRD DS    CL3                 LAST IQ PRD IN ERROR                         
SVIQHDR  DS    CL32                                                             
*                                                                               
         DC    CL8'SUBLIST'                                                     
SUBLIST  DS    XL6                                                              
         DC    X'00'               EOL FLAG                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'DEMOLIST'                                                    
DEMOLIST DS    XL120               ALLOW 40 DEMOS                               
         DC    XL3'00'             EOL FLAG                                     
NDEMOS   DS    H                   NUMBER OF DEMOS                              
*                                                                               
         DS    0D                                                               
INMAX    DC    PL8'9999999999'                                                  
         DS    0D                                                               
COUNTERS DS    0XL28                                                            
ERRCNT   EQU   *                                                                
RECOVFL  DC    PL4'0',CL24'REC OVERFLOW ERRORS'                                 
STAERRS  DC    PL4'0',CL24'STA NOT FOUND ERRORS'                                
PRDERRS  DC    PL4'0',CL24'PRD NOT FOUND ERRORS'                                
ESTERRS  DC    PL4'0',CL24'EST NOT FOUND ERRORS'                                
DPTERRS  DC    PL4'0',CL24'INVALID DAYPARTS'                                    
MASTERRS DC    PL4'0',CL24'BAD MASTER ESTIMATES'                                
PAIDERRS DC    PL4'0',CL24'PAID BUYS'                                           
NOLINES  DC    PL4'0',CL24'NO MORE BUYLINE NUMBERS'                             
ERRCNTX  EQU   *                                                                
INCNT    DC    PL4'0',CL24'INPUT RECORD COUNT'                                  
ADDCNT   DC    PL4'0',CL24'BUY RECORDS ADDED'                                   
REPLCNT  DC    PL4'0',CL24'BUY RECORDS REPLACED'                                
DELCNT   DC    PL4'0',CL24'BUY RECORDS DELETED'                                 
UNCHGCNT DC    PL4'0',CL24'BUY RECORDS UNCHANGED'                               
COUNTERX EQU   *                                                                
*                                                                               
         DS    0D                                                               
SPOTTAB  DS    XL16                2 BYTE COUNTERS                              
BUWKLIST DS    6XL4                5 START/END DATES + X'0000'                  
BUYTOTG  DS    F                                                                
STATOTG  DS    F                                                                
CLTTOTG  DS    F                                                                
*                                                                               
STAPWORK DS    XL32                                                             
*                                                                               
BUSTART  DS    XL3                                                              
BUEND    DS    XL3                                                              
BUWKS    DS    XL1                                                              
SVCOSTY  DS    C                                                                
OVERFLOW DS    C                                                                
PAIDBUY  DS    C                                                                
ALLUSED  DS    C                                                                
ESTNOPRD DS    CL3                                                              
BUTAX    DS    H                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
BUCOST   DS    F                                                                
BUCOVRD  DS    F                                                                
BUYCNT   DS    F                                                                
SVJOBID  DS    CL3                                                              
SVRNO    DS    XL2                                                              
SVQOPT5  DS    C                                                                
         DS    0D                                                               
BUYLINES DS    XL256               BUYLINE NUMBER TABLE                         
*                                                                               
OLDPRDL  DS    XL256                                                            
NEWPRDL  DS    XL256                                                            
*                                                                               
BUYPARS  DS    0D                   BINSRCH PARMS FOR BUYTAB                    
BUYPAR1  DC    A(0)                                                             
BUYPAR2  DC    A(BUYTAB)            TABLE ADDRESS                               
BUYPAR3  DC    F'0'                 NUMBER OF RECORDS IN TABLE                  
BUYPAR4  DC    AL4(L'BUYTAB)        RECLEN                                      
BUYPAR5  DC    AL1(0),AL3(BUYTABKL) KEYLEN                                      
BUYPAR6  DC    F'256'               MAX RECS                                    
*                                                                               
STALPARS DS    0D                  BINSRCH PARMS FOR STALIST                    
STALPAR1 DC    A(0)                                                             
STALPAR2 DC    A(STALIST)          TABLE ADDRESS                                
STALPAR3 DC    F'0'                NUMBER OF RECORDS IN TABLE                   
STALPAR4 DC    AL4(6)              RECLEN                                       
STALPAR5 DC    AL1(0),AL3(5)       KEYLEN                                       
STALPAR6 DC    AL4((STALISTX-STALIST)/6)  MAX RECS                              
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,LRECL=1024,MACRF=GM,    X        
               EODAD=FILEGETX                                                   
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*IQREC**'                                                    
IQREC    DS    XL1024                                                           
*                                                                               
IQLEN    DS    XL2                                                              
         DS    XL2                                                              
         ORG   IQREC+4                                                          
IQTYPE   DS    CL4                 HDR*/BUY*/DEM*/COM*/ROT*/EOF*                
*                                                                               
         ORG   IQTYPE                                                           
         DS    CL4'HDR*'                                                        
IQSYS    DS    CL1'S'              SYSTEM CODE                                  
IQUPLD   DS    CL3'PEX'            UPLOAD TYPE                                  
IQAGY    DS    CL2                 AGENCY                                       
IQMED    DS    CL1                 MEDIA                                        
IQCLT    DS    CL3                 CLIENT                                       
IQPRD    DS    CL3'POL'                                                         
IQEST    DS    CL3                 MASTER ESTIMATE NUMBER                       
IQSTART  DS    CL6                 BROADCAST MONTH YYMMDD                       
IQEND    DS    CL6                 BROADCAST MONTH YYMMDD                       
IQDEMOS  DS    20CL6               DEMO CODES                                   
*                                                                               
         ORG   IQTYPE                                                           
IQBUY    DS    CL4'BUY*'                                                        
IQSTA    DS    CL8                                                              
IQUNIQ   DS    CL8                 IQ UNIQUEID                                  
IQROT    DS    CL3                 ROT DAYS (MO=64,TU=32,WE=16,..)              
IQROTST  DS    CL2                 ROT START DAY                                
IQSTIM   DS    CL4                 START TIME - MILITARY                        
IQETIM   DS    CL4                 END TIME - MILITARY                          
IQDPT    DS    CL1                 DAYPART CODE                                 
IQSLN    DS    CL3                 SLN                                          
IQSLNTY  DS    CL1                 'S' FOR SECONDS                              
IQPROG   DS    CL17                PROGRAM NAME                                 
IQCOSTY  DS    CL1                 'P' FOR TRADE                                
IQBUYX   EQU   *                                                                
*                                                                               
         ORG   IQTYPE                                                           
         DS    CL4'DEM*'                                                        
IQDMVALS DS    40CL6                                                            
*                                                                               
         ORG   IQTYPE                                                           
         DS    CL4'COM*'                                                        
IQCOMS   DS    5CL70                                                            
*                                                                               
         ORG   IQTYPE                                                           
         DS    CL4'ROT*'                                                        
IQRPRD   DS    CL3                                                              
IQRCOST  DS    CL9                                                              
IQRWK1   DS    CL2                 WEEK 1 SPOTS                                 
IQRWK2   DS    CL2                 WEEK 2 SPOTS                                 
IQRWK3   DS    CL2                 WEEK 3 SPOTS                                 
IQRWK4   DS    CL2                 WEEK 4 SPOTS                                 
IQRWK5   DS    CL2                 WEEK 5 SPOTS                                 
*                                                                               
         ORG   IQTYPE                                                           
         DS    CL4'EOF*'                                                        
         ORG                                                                    
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*BUYTAB*'                                                    
BUYTAB   DS    256XL24                                                          
         DS    XL24                ENTRY 257 IS ALWAYS X'00'                    
BUYTABX  EQU   *                                                                
         DS    0D                                                               
         DC    CL8'*STALIST'                                                    
STALIST  DS    1000XL6             MKT/STA/FLAG                                 
STALISTX EQU   *                                                                
         DS    0D                                                               
         DC    CL8'**IQIO**'                                                    
IQIO     DS    XL4096                                                           
*                                                                               
BUYTABD  DSECT                                                                  
BUYTABID DS    CL8                                                              
BUYTABMO DS    XL1                 BROADCAST MONTH OF DATA                      
BUYTABKL EQU   *-BUYTABID          BUYTAB KEYLEN                                
*                                                                               
BUYTABLN DS    XL1                 BUYLINE NUMBER                               
BUYTABDD DS    CL1                 C'D' IF DDS UNIQUEID                         
BUYTABIN DS    XL1                 X'80'=PAID,X'40'=MATCHED                     
BUYTABDA DS    XL4                                                              
BUYTABUS DS    CL1                 C'Y' IF BUYREC USED                          
         DS    CL7                 SPARE                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
TIOTD    DSECT                                                                  
         IEFTIOT1                                                               
         PRINT ON                                                               
SPWORKD  DSECT                                                                  
* DSECT FOR PRINT LINE                                                          
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PSTA     DS    CL8                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PUNIQID  DS    CL8                                                              
         DS    CL1                                                              
PDATES   DS    CL17                                                             
         DS    CL1                                                              
PMSG     DS    CL12                                                             
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037SPREPIQ02 07/27/12'                                      
         END                                                                    
