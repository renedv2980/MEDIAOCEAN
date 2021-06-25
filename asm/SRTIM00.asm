*          DATA SET SRTIM00    AT LEVEL 010 AS OF 01/15/20                      
*PHASE T10400A                                                                  
*INCLUDE WKSCAN                                                                 
*INCLUDE BINSR31                                                                
T10400   TITLE 'SRTIM00 ($TIM) - FACPAK TIMER EXPIRATION ROUTINES'              
         PRINT NOGEN                                                            
TIMERS   CSECT                                                                  
         NMODL WORKL,**$TIM**,CLEAR=YES,RR=RE                                   
         USING WORKD,RC            RC=A(TEMP W/S)                               
         LARL  RA,COMMON                                                        
         USING COMMON,RA                                                        
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
         ST    R1,SAVER1                                                        
         MVC   IPARMS,0(R1)                                                     
*                                                                               
         L     R8,AMYUTL           R8=A(UTL)                                    
         USING UTLD,R8                                                          
         MVI   MODE,0                                                           
         SAM31                                                                  
         BRAS  RE,INIT                                                          
*                                                                               
         L     R9,ADSSB                                                         
         USING SSBD,R9                                                          
         OC    SSBTPOPC,SSBTPOPC   IS THIS THE FIRST SRTIM                      
         BNZ   T2                                                               
         BRAS  RE,FSTTIME          INIT FACPAK TABLES                           
         B     T2                                                               
         EJECT                                                                  
***********************************************************************         
* TIMER 2 - FACPAK MAJOR TIMER                                        *         
***********************************************************************         
T2       TM    SSBSTAT4,SSBSAOR    AOR HAS NO T2 PROCESSING                     
         BO    TIMEXIT                                                          
         CLI   TAGYB,2             TEST IF TIMER 2 HAS EXPIRED                  
         BNE   T3                  NO                                           
*                                                                               
         LHI   R0,1                                                             
         BRAS  RE,CPUREP                                                        
*                                                                               
         L     R1,SSBTPOPC         BUMP NUMBER OF TIMER POPS IN SSB             
         AHI   R1,1                                                             
         ST    R1,SSBTPOPC                                                      
         ST    R1,FULL             SAVE NUMBER OF POPS                          
*                                                                               
         MVC   TIMEHMSL,SSBTPOPT   SAVE TIME OF LAST POP                        
         MVC   SSBTPOPT,TIMEHMS    SET TIME OF THIS POP IN SSB                  
*&&US*&& OI    SSBDARFL,SSBDRSDR   CALL $SPTDARE                                
         DROP  R9                                                               
*                                                                               
         TM    FULL+3,X'03'        TEST MULTIPLE OF 4                           
         BNO   *+8                                                              
         OI    MODE,X'01'          YES SET EVERY FORTH TIME IN                  
         TM    FULL+3,X'01'        TEST MULTIPLE OF 2                           
         BO    *+8                                                              
         OI    MODE,X'02'                                                       
         OI    MODE,X'80'          YES SET EXTENDED PQ MODE                     
*                                                                               
         BRAS  RE,LOGGER           CALL LOGGER FOR T2 POP                       
*                                                                               
         BRAS  RE,LOGCPU           LOG SYSTEM CPU USAGE                         
*                                                                               
         LHI   R0,2                                                             
         BRAS  RE,CPUREP                                                        
*                                                                               
         BRAS  RE,BLDJOBS          BUILD JOB TABLE COPY IN CORE                 
*                                                                               
         LHI   R0,3                                                             
         BRAS  RE,CPUREP                                                        
*                                                                               
         BRAS  RE,BLDBC            BUILD LIST OF BROADCAST MSGS                 
*                                                                               
         LHI   R0,4                                                             
         BRAS  RE,CPUREP                                                        
*                                                                               
         BRAS  RE,SETBC            SET ANY TERMINALS THAT NOW QUALIFY           
*                                                                               
         LHI   R0,5                                                             
         BRAS  RE,CPUREP                                                        
*                                                                               
         TM    MODE,X'01'                                                       
         BZ    *+8                                                              
         BRAS  RE,ESSTIME          CHECK ESS TIMEOUTS EVERY 4TH POP             
*                                                                               
         LHI   R0,6                                                             
         BRAS  RE,CPUREP                                                        
*                                                                               
*&&US*&& BRAS  RE,SETDAR           CHECK AND SET ANY DARE TERMS                 
*                                                                               
         LHI   R0,7                                                             
         BRAS  RE,CPUREP                                                        
*                                                                               
*&&UK*&& BRAS  RE,CHKMSG           CHECK ON MESSAGE BUFFER                      
*&&UK*&& BRAS  RE,CHKCOM           CHECK COMMAND BLOCK                          
*                                                                               
         LHI   R0,8                                                             
         BRAS  RE,CPUREP                                                        
*                                                                               
         CLI   RSTRDONE,C'Y'       TEST TO RESTART PRINTERS                     
         BE    *+8                                                              
         BRAS  RE,RSTRPRNS         RESTART REMOTE PRINTERS                      
*                                                                               
         LHI   R0,9                                                             
         BRAS  RE,CPUREP                                                        
*                                                                               
         BRAS  RE,PQFIX                                                         
*                                                                               
         LHI   R0,10                                                            
         BRAS  RE,CPUREP                                                        
*                                                                               
         OC    PRTNUM,PRTNUM       ANY PRINTERS QUALIFY?                        
         BNZ   T2M010              YES-SEARCH PQ INDEX                          
         OC    JNUM,JNUM           ANY JOBS PENDING NOTIFY?                     
         BNZ   T2M010              YES-SEARCH PQ INDEX                          
         TM    MODE,X'02'          EVERY OTHER POP OR EVEN NUMBER               
         BZ    T2M020              NO-DON'T SEARCH                              
*                                                                               
         L     RE,ADSSB            SSB DATA                                     
         USING SSBD,RE                                                          
         TM    SSBSYSFL,X'80'      TEST SYSTEMS?                                
         BZ    T2M020              NO-SO KEEP OLD LOGIC                         
         DROP  RE                                                               
*                                                                               
T2M010   BRAS  RE,RPTMCH                                                        
*                                                                               
         LHI   R0,11                                                            
         BRAS  RE,CPUREP                                                        
*                                                                               
         BRAS  RE,PRTSTRT                                                       
*                                                                               
         LHI   R0,12                                                            
         BRAS  RE,CPUREP                                                        
*                                                                               
         BRAS  RE,JOBS             UPDATE S/R SAVE PAGES                        
*                                                                               
         LHI   R0,13                                                            
         BRAS  RE,CPUREP                                                        
*                                                                               
*NOP*    BRAS  RE,FIXJTAB          FIX UP JOB TABLE IN DATASPACE                
*                                                                               
*                                                                               
         LHI   R0,14                                                            
         BRAS  RE,CPUREP                                                        
*                                                                               
T2M020   BRAS  RE,WKRUPDT          SCAN FACWRK FOR UPDATE PENDING               
*                                                                               
         LHI   R0,15                                                            
         BRAS  RE,CPUREP                                                        
*                                                                               
         BRAS  RE,WKFSCAN                                                       
*                                                                               
         LHI   R0,16                                                            
         BRAS  RE,CPUREP                                                        
*                                                                               
         L     RE,ADSSB                                                         
         USING SSBD,RE                                                          
         TM    SSBSTAT6,SSB6MQWI                                                
         BZ    T2M030                                                           
*AH3     NI    SSBSTAT6,X'FF'-SSB6MQWI         <--LOOK AT THIS A.HYDE           
         GOTO1 AMQIO,DMCB,=CL8'SYSOPEN ',0,0,0                                  
         DROP  RE                                                               
*                                                                               
         LHI   R0,17                                                            
         BRAS  RE,CPUREP                                                        
*                                                                               
T2M030   B     T3                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER TIMER SUPPORT                                                 *         
***********************************************************************         
T3       B     TIMEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF UPDATIVE TYPE JOBS THAT HAVE COMPLETED                *         
***********************************************************************         
*&&DO                                                                           
         PUSH  USING                                                            
         USING DMSPACED,DSPHD                                                   
UPDJOBS  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'20'           SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DUB        GET JOBTAB HEADER                            
         ICM   RF,15,4(R1)                                                      
         MVC   DSPHD,0(RF)                                                      
         NI    DSPTFRST,X'3F'      TURN OFF X'40' BIT                           
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBALET                                                   
         ICM   R2,15,DSPTFRST                                                   
         SAC   512                                                              
         USING TABJOBS,R2                                                       
         MVC   JOBHDRL,TBJLHEAD    SET NEW SOFT LENGTHS                         
         MVC   JOBTABL,TBJLNTRY                                                 
         OC    JOBHDRL,JOBHDRL     IF NOT SET                                   
         BNZ   *+16                                                             
         MVC   JOBHDRL,=H'1024'    USE OLD HARD DEFAULTS                        
         MVC   JOBTABL,=H'64'                                                   
*                                                                               
         XC    UPJOB#,UPJOB#       CLEAR N'ENTRIES IN CORE TABLE                
         L     R0,AUPJOBS          AND TABLE ITSELF                             
         LHI   R1,UPJOBSQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,AUPJOBS          R3=A(UPDATIVE JOBS ENTRIES)                  
         USING UPJOBSD,R3                                                       
         XR    R4,R4               R4=NUM OF JTAB ENTRIES                       
         ICM   R0,15,0(R2)         R0=NUM JOBS IN DSPACE TABLE                  
         BZ    UPJOBXIT            NOTHING TO PROCESS                           
         AH    R2,JOBHDRL                                                       
         USING TBJOBTAB,R2         R2=A(FIRST REAL JOB TABLE ENTRY)             
*                                                                               
UPJOB10  CLM   R2,15,DSPTEND       TEST PAST END OF TABLE                       
         BH    UPSJOBXIT           YES-THEN DONE                                
         OC    TBJNTRY(4),TBJNTRY  TEST USED ENTRY                              
         BZ    UPJOB50                                                          
*                                                                               
         MVC   BYTE,TBJADV         ONLY JOBS FOR THIS FACPAK                    
         NI    BYTE,X'0F'                                                       
         CLC   BYTE,SYSID                                                       
         BNE   UPJOB50                                                          
*                                                                               
         OC    TBJETIME,TBJETIME   ONLY COMPLETED JOBS                          
         BZ    UPJOB50                                                          
*                                                                               
         TM    TBJSTAT,TBJUPDT     UPDATIVE JOBS ONLY                           
         BZ    UPJOB50                                                          
*                                                                               
UPJOB20  MVC   UPJOBUSR,TBJPQUSR   SAVE USER ID                                 
         MVC   UPJOBREP,TBJPQSEQ                                                
         AHI   R4,1                UP NUMBER OF ENTIRES                         
         AHI   R3,UPJOBQ                                                        
*                                                                               
UPJOB50  AH    R2,JOBTABL          NEXT JOB                                     
         B     UPJOB10                                                          
*                                                                               
UPJOBXIT ST    R4,UPJOB#           TOTAL # OF UPDATIVE JOBS FOR ADV             
         SAC   0                                                                
         J     EXIT                                                             
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF SUBMITTED JOBS WHICH ARE COMPLETE FOR NOTIFICATION    *         
***********************************************************************         
         PUSH  USING                                                            
         USING DMSPACED,DSPHD                                                   
BLDJOBS  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'20'           SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DUB        GET JOBTAB HEADER                            
         ICM   RF,15,4(R1)                                                      
         MVC   DSPHD,0(RF)                                                      
         NI    DSPTFRST,X'3F'      TURN OFF X'40' BIT                           
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBALET                                                   
         ICM   R2,15,DSPTFRST                                                   
         SAC   512                                                              
         USING TABJOBS,R2                                                       
         MVC   JOBHDRL,TBJLHEAD    SET NEW SOFT LENGTHS                         
         MVC   JOBTABL,TBJLNTRY                                                 
         OC    JOBHDRL,JOBHDRL     IF NOT SET                                   
         BNZ   *+16                                                             
         MVC   JOBHDRL,=H'1024'    USE OLD HARD DEFAULTS                        
         MVC   JOBTABL,=H'64'                                                   
*                                                                               
         XC    JNUM,JNUM           CLEAR N'ENTRIES IN CORE TABLE                
         L     R0,AJTAB            AND TABLE ITSELF                             
         LHI   R1,JTABLQ                                                        
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LAM   AR2,AR2,TBALET                                                   
         ICM   R2,15,DSPTFRST                                                   
         SAC   512                                                              
*                                                                               
         L     R3,AJTAB            R3=A(JOB TABLE ENTRY)                        
         USING JTABD,R3                                                         
         XR    R4,R4               R4=NUM OF JTAB ENTRIES                       
         ICM   R0,15,0(R2)         R0=NUM OF JOBS IN DSPACE TABLE               
         BZ    BLDJOBX                                                          
         AH    R2,JOBHDRL                                                       
         USING TBJOBTAB,R2         R2=A(FIRST REAL JOB TABLE ENTRY)             
*                                                                               
BJOB02   CLM   R2,15,DSPTEND       TEST PAST END OF TABLE                       
         BH    BJOB06              YES                                          
         OC    TBJNTRY(4),TBJNTRY  TEST USED ENTRY                              
         BNZ   *+12                                                             
         AH    R2,JOBTABL                                                       
         B     BJOB02                                                           
*                                                                               
         MVC   BYTE,TBJADV         ONLY JOBS OF MY FACPAK GROUP                 
         NI    BYTE,X'0F'                                                       
         CLC   BYTE,SYSID                                                       
         BNE   BJOB04                                                           
*                                                                               
         OC    TBJETIME,TBJETIME   ONLY COMPLETED JOBS                          
         BZ    BJOB04                                                           
         CLI   TBJMONS,C' '        SHOULD HAVE MONS ASID                        
         BNH   BJOB04               -LIKELY, B/C WRONG MONSTER RAN IT           
*                                                                               
BJOB02A  MVC   JTERM,TBJTERM                                                    
         MVC   JPQKEY,TBJPQKEY                                                  
         MVC   JPQID,TBJPQID                                                    
         OI    JSTAT,JSOUT                                                      
         STCM  R2,15,JJOBADDR      SAVE A(ENTRY)                                
         OI    NOTIFY,JSOUT                                                     
*                                                                               
BJOB03   AHI   R3,JTABL                                                         
         AHI   R4,1                                                             
         CHI   R4,JMAX                                                          
         JNL   *+2                 DIE IF LOCAL JOBTAB IS FULL                  
*                                                                               
BJOB04   AH    R2,JOBTABL                                                       
         BCT   R0,BJOB02                                                        
*                                                                               
BJOB06   BRAS  RE,ARSOFF                                                        
         MVC   JTERM,EFFS          SET E-O-L                                    
         ST    R4,JNUM             SET N'ENTRIES IN JTAB                        
         LTR   R4,R4                                                            
         BZ    BLDJOBX                                                          
*                                                                               
         LA    R0,1(R4)            SORT JTAB INTO TNUM SEQUENCE                 
         ST    R0,DMCB+04                                                       
         LA    R0,JTABL                                                         
         ST    R0,DMCB+08                                                       
         LHI   R0,L'JTERM                                                       
         ST    R0,DMCB+12                                                       
         LHI   R0,JTERM-JTABD                                                   
         ST    R0,DMCB+16                                                       
         GOTO1 AXSORT,DMCB,(0,AJTAB)                                            
*                                                                               
BLDJOBX  BRAS  RE,ARSOFF           CLEAN UP AFTER YOURSELF                      
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* READ GENDIR TO SEE IF ANY BRDCST MESSAGES SINCE LAST TIME           *         
* IF ANY NEW MESSAGES MOVE TO BCTAB                                   *         
***********************************************************************         
BLDBC    NTR1  BASE=*,LABEL=*                                                   
         ICM   R5,15,ABCTAB                                                     
         BZ    EXIT                R5=A(BROADCAST TABLE)                        
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         TIME  BIN                                                              
*                                                                               
         A     R0,TIMEADJ                                                       
         STM   R0,R1,DUB           R0=TIME(BIN 1/100 SECS),R1=DATE              
         SR    R0,R0                                                            
         ICM   R0,3,DATEC                                                       
         SRDL  R0,9                R0=THIS YEAR                                 
         STC   R0,YEARB                                                         
         SRL   R1,32-4             R1=THIS MONTH                                
         STC   R1,MONTHB                                                        
         SLL   R0,9                                                             
         STH   R0,YEARC                                                         
         LLC   R1,YEARB            COMPUTE YEAR+1                               
         LA    R1,1(R1)                                                         
         SLL   R1,9                                                             
         STH   R1,YEARCP1                                                       
         LLC   R1,YEARB            COMPUTE YEAR-1                               
         BCTR  R1,0                                                             
         SLL   R1,9                                                             
         STH   R1,YEARCM1                                                       
*                                                                               
         SR    R0,R0                                                            
         L     R1,DUB              CALCULATE REPORT SCAN TIME                   
         D     R0,=F'100'                                                       
         SH    R1,=Y(32*60)        NOW-32 MINTES                                
         MHI   R1,3                                                             
         SRL   R1,2                                                             
         STH   R1,TIMEC                                                         
*                                                                               
         L     R1,DUB              CONVERT TIME TO BINARY HHMMSS                
         SR    R0,R0                                                            
         D     R0,=F'100'          CONVERT TO SECONDS                           
         SR    R0,R0                                                            
         D     R0,=F'60'           CONVERT TO MINUTES                           
         STC   R0,TIMEB+2                                                       
         SR    R0,R0                                                            
         D     R0,=F'60'           CONVERT TO HOURS                             
         STC   R0,TIMEB+1                                                       
         STC   R1,TIMEB+0                                                       
*                                                                               
         LA    R5,6(R5)                                                         
         USING BCTABD,R5                                                        
*                                                                               
         LA    R3,DIR              SET GENDIR KEY FOR FIRST MESSAGE             
         USING BRDKEYD,R3                                                       
         XC    BRDKEY,BRDKEY                                                    
         MVI   BRDKSYS,BRDKSYSQ                                                 
         MVI   BRDKSTYP,BRDKSTYQ                                                
         MVI   BRDKTYPE,BRDKTEMQ                                                
         MVC   BRDKMSGN,BCTLDNUM   SET LAST DISK MSG NUM FROM BCTAB             
         MVC   KEY,BRDKEY                                                       
         GOTO1 ADMGR,DMCB,(X'08',DMRDHI),GENDIR,(R3),(R3)                       
         TM    8(R1),X'02'                                                      
         BO    *+12                INCLUDE DELETED RECORDS                      
         CLI   8(R1),0                                                          
         BNE   EXIT                                                             
*                                                                               
         CLC   KEY(BRDKMSGN-BRDKEYD),BRDKEY                                     
         BNE   EXIT                EXIT IF NO BC MESSAGES                       
*                                                                               
         MVC   FULL(2),BCTLCNUM    SAVE NUMBER OF FIRST CORE MESSAGE            
         MVC   FULL+2(2),FULL      SAVE NUMBER OF LAST CORE MESSAGE             
         MVC   FUL1(2),BCTLDNUM    SAVE NUMBER OF FIRST DISK MESSAGE            
         MVC   FUL1+2(2),FUL1      SAVE NUMBER OF LAST DISK MESSAGE             
*                                                                               
BBCT02   MVC   KEY(32),BRDKEY      READ NEXT GENDIR BRDCST RECORD               
         GOTO1 ADMGR,DMCB,(X'08',DMRSEQ),GENDIR,(R3),(R3)                       
         TM    8(R1),X'02'                                                      
         BO    *+12                INCLUDE DELETED RECORDS                      
         CLI   8(R1),0                                                          
         BNE   BBCT18                                                           
*                                                                               
         CLC   KEY(BRDKMSGN-BRDKEYD),BRDKEY                                     
         BNE   BBCT18                                                           
         MVC   FUL1+2(2),BRDKMSGN  SAVE NUMBER OF NEW LAST DISK MSG             
         TM    BRDKSTAT,X'80'                                                   
         BO    BBCT02              IGNORE DELETED RECORDS                       
*                                                                               
BBCT04   SR    R0,R0               UNPACK GENDIR DIR STATUS BYTES               
         ICM   R0,7,BRDKSTAT+1                                                  
         SRDL  R0,18               R0=FACPAKID,R1=DATES                         
         STC   R0,DUB                                                           
*                                                                               
* IF THIS IS A TEST SYSTEM DO NOT DISPLAY 'ALL' MSGS                            
*                                                                               
         L     RF,ADSSB            GET A(FACIDTAB) FROM SSB                     
         L     RF,SSBAFID-SSBD(RF)                                              
BBCT06   CLI   0(RF),X'FF'                                                      
         BE    BBCT02              IF NOT FOUND, IGNORE MESSAGE                 
         CLC   SYSID,4(RF)                                                      
         BE    *+12                                                             
         AHI   RF,L'FACITAB                                                     
         B     BBCT06                                                           
*                                                                               
         TM    5(RF),X'80'         IS THIS A TEST SYSTEM?                       
         BNZ   *+12                YES-SKIP 'ALL' CHECK                         
         CLI   DUB,0               VALID FOR ALL SYSTEMS                        
         BE    *+14                YES                                          
         CLC   DUB(1),SYSID                                                     
         BNE   BBCT02              IGNORE IF NOT THIS FACPAK                    
*&&US                                                                           
         SLL   R1,2                SHIFT OUT ALL BUT END DATE                   
         CLM   R1,B'1100',DATEC    DID MESSAGE EXPIRE?                          
         BL    BBCT02              YES-IGNORE IT                                
         B     BBCT14                                                           
*&&                                                                             
         SR    R0,R0                                                            
         SLDL  R0,9                R0=START MMDD                                
         SRL   R1,32-9             R1=END MMDD                                  
         STH   R0,STRDC                                                         
         STH   R1,ENDDC                                                         
         CR    R0,R1               IF START GT END HAVE CROSSED YEAR            
         BH    BBCT08                                                           
         OC    STRDC,YEARC         ELSE ASSUME THIS YEAR                        
         OC    ENDDC,YEARC                                                      
         B     BBCT12                                                           
*                                                                               
BBCT08   CLI   MONTHB,3                                                         
         BH    BBCT10                                                           
         OC    STRDC,YEARCM1       IF IN FIRST QTR ASSUME LAST YEAR             
         OC    ENDDC,YEARC                                                      
         B     BBCT12                                                           
*                                                                               
BBCT10   OC    STRDC,YEARC         ELSE ASSUME NEXT YEAR                        
         OC    ENDDC,YEARCP1                                                    
*                                                                               
BBCT12   CLC   DATEC,STRDC         TEST IF BRDCST MSG VALID FOR TODAY           
         BL    BBCT02                                                           
         CLC   DATEC,ENDDC                                                      
         BH    BBCT02                                                           
*                                                                               
BBCT14   MVC   DSKADR,BRDDA        READ FULL MESSAGE DETAILS                    
         GOTO1 ADMGR,DMCB,(X'00',GETREC),GENFIL,DSKADR,AIO,DMWORK               
         CLI   DMCB+8,0                                                         
         BNE   BBCT02              IGNORE FUNNY RECORDS                         
*                                                                               
         L     R4,AIO                                                           
         AHI   R4,BRDFSTEL-BRDKEYD                                              
         USING BRDFLTD,R4                                                       
         XR    RF,RF                                                            
BBCT16   CLI   BRDFLTC,0           SEARCH FOR FILTER ELEMENT                    
         BE    BBCT02                                                           
         CLI   BRDFLTC,BRDFLTCQ                                                 
         BE    *+12                                                             
         IC    RF,BRDFLTL                                                       
         BXH   R4,RF,BBCT16                                                     
*                                                                               
         CLC   DATEC,BRDFSTDT      CHECK TODAY WITH FULL DATES                  
         BL    BBCT02                                                           
         CLC   DATEC,BRDFENDT                                                   
         BH    BBCT02                                                           
*                                                                               
* CHECK IF IT WILL BE SENT TODAY                                                
*                                                                               
         CLI   BRDFDAYS,X'00'      PRE-DAY RECORD                               
         BNE   *+12                                                             
         MVI   DUB,X'7F'           ASSUME ALL DAYS                              
         B     *+10                                                             
         MVC   DUB(1),BRDFDAYS     CHECK IF TODAY IS SET                        
         NC    DUB(1),DAY          DAY=TODAY                                    
         BZ    BBCT02              WON'T BE SENT TODAY                          
*                                                                               
         L     R5,ABCTAB           POINT TO BCTAB                               
         LH    R1,FULL+2           GET LAST ENTRY NUMBER IN BCTAB               
         LA    R1,1(R1)                                                         
         LR    R0,R1               SAVE NEW NUMBER                              
         MH    R1,0(R5)                                                         
         LA    R1,6(R5,R1)                                                      
         C     R1,2(R5)            TEST END OF BCTAB                            
         BH    BBCT02                                                           
*                                                                               
         STH   R0,FULL+2           SET NEW BC NUMBER                            
         LR    R5,R1               R5=A(NEXT AVAILABLE ENTRY)                   
         XC    0(BCTABL,R5),0(R5)                                               
         MVC   BCTNUM,FULL+2       ENTRY NUMBER                                 
         MVC   BCTDNUM,FUL1+2      DISK MESSAGE NUMBER                          
         MVC   BCTDADR,DSKADR      DISK MESSAGE DISK ADDRESS                    
         MVC   BCTNAME,BRDFNAME    MESSAGE NAME                                 
*                                                                               
         TM    BRDFLAGS,BRDFLLST   LUID IS A LIST                               
         BZ    *+8                                                              
         OI    BCTFLAG,BCTFLST                                                  
         TM    BRDFLAGS,BRDFLMST   MUST DISPLAY FLAG                            
         BZ    *+8                                                              
         OI    BCTFLAG,BCTFMST                                                  
         MVC   BCTCTRY,BRDFCTRY    COUNTRY CODE                                 
         MVC   BCTSTTM,BRDFSTTM    START TIME                                   
         MVC   BCTENTM,BRDFENTM    END TIME                                     
         MVC   BCTOVSYS,BRDFOVSY   OVERLAY SYSTEM                               
         MVC   BCTSYS,BRDFSNUM     SE NUMBER                                    
         MVC   BCTPRG,BRDFPROG     PROGRAM NUMBER                               
         MVC   BCTLUID,BRDFLUID    LUID                                         
         B     BBCT02              BACK FOR NEXT                                
*                                                                               
BBCT18   L     R5,ABCTAB           POINT TO START OF BROADCAST TABLE            
         LA    R5,6(R5)                                                         
         CLC   FULL(2),FULL+2      TEST IF NEW CORE ENTRY                       
         BNE   *+14                                                             
         CLC   FUL1(2),FUL1+2                                                   
         BE    EXIT                                                             
*                                                                               
         MVC   BCTLCNUM,FULL+2     SET NEW LAST CORE                            
         MVC   BCTLDNUM,FUL1+2     SET NEW LAST DISK                            
         MVC   BCTPCNUM,FULL       SET PREV LAST CORE                           
         MVC   BCTPDNUM,FUL1       SET PREV LAST DISK                           
         MVC   BCTLTIME,TIMEB      SET TIME THIS TABLE WAS UPDATED              
         L     RE,ADSSB                                                         
         OI    SSBSTAT1-SSBD(RE),SSBSCHK1                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SEARCH BROADCAST TABLE FOR RELEVENT MESSAGES.                       *         
* SCAN UTL AND FLAG TERMINALS THAT MATCH THE FILTERS ON THE MESSAGES  *         
***********************************************************************         
SETBC    NTR1  BASE=*,LABEL=*                                                   
         ICM   R5,15,ABCTAB        R5=A(BROADCAST TABLE)                        
         BZ    EXIT                                                             
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         AHI   R5,6                FIRST ENTRY IS RESERVED                      
         B     SETBC04                                                          
*                                                                               
         USING BCTABD,R5                                                        
SETBC02  OC    BCTNUM,BCTNUM       EXIT IF NO MORE BRDCST MESSAGES              
         BZ    EXIT                                                             
         TM    BCTFLAG,BCTFDEL+BCTFPRC IGNORE DELETED/PROCESSED MSGS            
         BNZ   SETBC04                                                          
         OC    BCTSTTM,BCTSTTM     TEST IF MSG HAS START TIME                   
         BZ    *+14                                                             
         CLC   TIMEB(2),BCTSTTM                                                 
         BL    SETBC04                                                          
         OC    BCTENTM,BCTENTM     TEST IF MSG HAS END TIME                     
         BZ    SETBC06                                                          
         CLC   TIMEB(2),BCTENTM                                                 
         BNH   SETBC06                                                          
*                                                                               
SETBC04  BXLE  R5,R6,SETBC02                                                    
         B     EXIT                EXIT IF BCTAB FULL CUNT HEAD                 
*                                                                               
SETBC06  OI    BCTFLAG,BCTFPRC     SET HAVE PROCESSED THIS MESSAGE              
*                                                                               
         L     R3,AUTL                                                          
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         AHI   R3,6                                                             
X        USING UTLD,R3             SET TO SEARCH UTL FOR MATCHES                
*                                                                               
SETBC08  OC    X.TXPRNT,X.TXPRNT   IGNORE PRINTERS                              
         BNZ   SETBC24                                                          
*                                                                               
         CLI   BCTCTRY,X'FF'       FILTER ON COUNTRY                            
         BE    SETBC10                                                          
         CLC   BCTCTRY,X.TCTRY                                                  
         BE    SETBC10                                                          
         CLI   BCTCTRY,0           USE DEFAULT CTRY IF NOT DEFINED              
         BNE   SETBC24                                                          
         CLC   BCTCTRY,DEFCTRY                                                  
         BNE   SETBC24                                                          
*                                                                               
SETBC10  CLI   X.TSYS,0            IGNORE DISCONNECTED TERMINALS                
         BE    SETBC24                                                          
         CLI   BCTSYS,0            FILTER ON SPECIFIC SYSTEM                    
         BE    SETBC12                                                          
         CLC   BCTSYS,X.TSYS                                                    
         BNE   SETBC24                                                          
         B     SETBC14                                                          
*                                                                               
SETBC12  CLI   BCTOVSYS,0          FILTER ON MAJOR SYSTEM                       
         BE    SETBC16                                                          
         CLC   BCTOVSYS,X.TOVSYS                                                
         BNE   SETBC24                                                          
*                                                                               
SETBC14  CLI   BCTPRG,0            FILTER ON PROGRAM (WITHIN SYSTEM)            
         BE    SETBC16                                                          
         CLC   BCTPRG,X.TPRG                                                    
         BNE   SETBC24                                                          
*                                                                               
SETBC16  CLI   BCTLUID,0           FILTER ON LUID                               
         BE    SETBC22                                                          
         MVC   DUB,X.TSYM          SET TERMINAL LUID IN DUB                     
         TM    BCTFLAG,BCTFLST                                                  
         BO    SETBC22                                                          
*                                                                               
         LHI   R0,8                PROCESS FIRST 8 CHRS                         
         LA    R1,BCTLUID                                                       
         LA    R2,DUB                                                           
SETBC18  CLI   0(R1),C'*'          ALLOW WILD CARD CHRS                         
         BE    SETBC20                                                          
         CLI   0(R1),C' '                                                       
         BNH   SETBC20                                                          
         CLC   0(1,R1),0(R2)                                                    
         BNE   SETBC24                                                          
SETBC20  LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,SETBC18                                                       
*                                                                               
SETBC22  EQU   *                                                                
*&&UK*&& B     SETBC24             *NOP* BROADCAST                              
         OI    X.TSTAT2,TSTATBCP   SET BRDCST PENDING IN UTL ENTRY              
*                                                                               
SETBC24  BXLE  R3,R4,SETBC08                                                    
         B     SETBC04             BACK FOR NEXT BRDCST MESSAGE                 
         DROP  R5,X                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK ESS TIMEOUTS FOR PRESTO                                       *         
***********************************************************************         
ESSTIME  NTR1  BASE=*,LABEL=*                                                   
         TIME  TU                  TIME NOW                                     
         SL    R0,=A(5*60*38400)   LESS 5 MINUTES                               
         ST    R0,FULL                                                          
*                                                                               
         L     R2,AUTL                                                          
         LH    R3,0(R2)                                                         
         L     R4,2(R2)                                                         
         AHI   R2,6                R2=A(UTL ENTRY)                              
X        USING UTLD,R2                                                          
*                                                                               
ESS02    DS    0H                                                               
*&&US*&& CLC   X.TLUID+4(2),=C'ES' ESS SERVER HAS ES AS 5/6 CHARS               
*&&UK*&& CLI   X.TLUID+7,C'E'      ESS SERVER HAS E AS LAST CHAR                
         BNE   ESS04                                                            
         CLC   X.TTIME,FULL        WORKED IN LAST 5 MINUTES                     
         BH    ESS04                                                            
*                                                                               
         MVC   WORK1,SPACES                                                     
         MVC   WORK1,ESSMSG                                                     
         MVC   WORK1+19(8),X.TLUID                                              
         ICM   RF,15,=C'LVL2'      OUTPUT ERROR MESSAGE                         
         GOTO1 ADMOD000,DMCB,AWCTYPE,WORK1,L'WORK1,(RF)                         
*                                                                               
ESS04    BXLE  R2,R3,ESS02                                                      
         B     EXITOK                                                           
         DROP  X                                                                
*                                                                               
ESSMSG   DC    CL48'PRMON **ESS SERVER XXXXXXXX HAS TIMED OUT**'                
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* TEST TO SEE IF 5000 MESSAGES READ SINCE LAST CLEANUP                *         
* IF SO SCAN BUFFER AND REMOVE ALL MESSAGES WITH LOW USEAGE           *         
***********************************************************************         
CHKMSG   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ACALLOV,DMCB,0,X'D9000AA0'                                       
         L     R1,0(R1)            GET A(MSGBUFF IN R1)                         
         ST    R1,AMSGBUFF                                                      
         LR    R5,R1               POINT R5 TO MESSAGE BUFFER                   
         USING MSGBUFFD,R5                                                      
         CLI   MSGBSTAT,C'+'       BUFFER MUST BE ENABLED                       
         BNE   CHKX                                                             
         L     R1,MSGBTOTL                                                      
         S     R1,MSGBPREV                                                      
         CH    R1,=H'5000'         ONLY WHEN 5000+ CALLS HAVE GONE              
         BL    CHKX                                                             
         MVI   MSGBSTAT,C'*'       FLAG REORG IN PROCESS                        
         MVC   MSGBPREV,MSGBTOTL                                                
         MVC   FULL(1),MSGBCUT                                                  
         LH    R6,MSGBNUM          R6=NUMBER OF MESSAGES                        
         LA    R5,MSGBNDX          R5=A(INDEX)                                  
         LTR   R6,R6                                                            
         BZ    CHKX                NO MESSAGES                                  
*                                                                               
CHK010   CLC   5(1,R5),FULL        DELETE MSGS LOWER THAN CUT OFF               
         MVI   5(R5),0             RESET COUNTER FOR NEXT TIME                  
         BH    CHK020                                                           
         LR    R1,R5                                                            
         BRAS  RE,DELMSG                                                        
         B     *+8                 DON'T BUMP INDEX                             
CHK020   LA    R5,8(R5)            NEXT INDEX ENTRY                             
         BCT   R6,CHK010                                                        
         L     R5,AMSGBUFF         POINT R5 TO MESSAGE BUFFER                   
         MVI   MSGBSTAT,C'+'       RE ENABLE BUFFER                             
         B     CHKX                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE ENTRY FROM MESSAGE TABLE   R1=A(INDEX ENTRY)                 *         
***********************************************************************         
DELMSG   NTR1  BASE=*,LABEL=*                                                   
         L     R5,AMSGBUFF         POINT R5 TO MESSAGE BUFFER                   
         USING MSGBUFFD,R5                                                      
         LR    R2,R1               SAVE A(INDEX ENTRY) IN R2                    
         SR    RF,RF                                                            
         ICM   RF,3,6(R1)          RF=DISPLACEMENT TO MESSAGE ELEMENT           
         LA    RF,MSGBUFFS(RF)                                                  
         LLC   R1,1(RF)            GET ELEMENT LEN                              
         LR    R4,R1               SAVE IT IN R4                                
         LR    R0,RF               R0=A(ELEMENT)                                
         LA    RE,0(RF,R1)         RE=A(NEXT ELEMENT)                           
         L     RF,MSGBNXT                                                       
         LR    R1,RF                                                            
         SR    RF,RE               CALCULATE LENGTHS FOR MOVE                   
         SR    R1,R0                                                            
         BRAS  R9,CHECKIT                                                       
         MVCL  R0,RE               MOVE TEXT UP                                 
         L     RF,MSGBNXT                                                       
         SR    RF,R4               RESET END OF TEXT POINTER                    
         ST    RF,MSGBNXT                                                       
*                                                                               
         LA    RE,8(R2)            RE=A(NEXT INDEX)                             
         LR    R0,R2               R0=A(THIS INDEX)                             
         LH    RF,MSGBNUM                                                       
         BCTR  RF,0                                                             
         STH   RF,MSGBNUM          REMOVE 1 FROM INDEX                          
         LA    RF,1(RF)                                                         
         SLL   RF,3                RF=TOTAL * 8                                 
         LA    R1,MSGBNDX                                                       
         AR    RF,R1               RF=A(INDEX END)                              
         SR    RF,RE               RF=LEN SOURCE                                
         LA    R1,8(RF)            R1=LEN DEST (SOURCE+8)                       
         BRAS  R9,CHECKIT                                                       
         MVCL  R0,RE                                                            
         SR    RF,RF                                                            
         LR    R1,R2               SCAN LOWER INDEX ENTRIES                     
DEL1     OC    0(8,R1),0(R1)       TEST FOR INDEX END                           
         BZ    CHKX                                                             
         ICM   RF,3,6(R1)          GET DISPLACEMENT                             
         SR    RF,R4               SUBTRACT L'DELETED ELEMENT                   
         STCM  RF,3,6(R1)          SAVE IT                                      
         LA    R1,8(R1)            NEXT INDEX                                   
         B     DEL1                                                             
         EJECT                                                                  
CHECKIT  STM   RE,R1,REGSAVE                                                    
         SR    R1,RF                                                            
         CLM   R1,1,=X'53'         TEST OFFSET < 84                             
         BH    FOFF                                                             
         C     R0,AMSGBUFF         TEST A(DEST)   > A(BUFFER)                   
         BNH   FOFF                                                             
         C     RE,AMSGBUFF         TEST A(SOURCE) > A(BUFFER)                   
         BNH   FOFF                                                             
         AR    RF,R0                                                            
         AR    RF,R1                                                            
         L     R1,AMSGBUFF                                                      
         L     R1,MSGBEND-MSGBUFFD(R1)                                          
         CR    RF,R1               TEST DEST+LEN  < A(BUFFERX)                  
         BH    FOFF                                                             
         LM    RE,R1,REGSAVE       OK DO IT!!                                   
         BR    R9                                                               
FOFF     DC    H'0'                DON'T DO THAT MVCL                           
*                                  LEAVE TABLE DISABLED                         
CHKX     B     EXITOK                                                           
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* RESTART ALL PRINTERS WITH AUTO RESTART PENDING                      *         
* LINE MUST BE UP AND OPEN AND REPORT AVAIL FOR PRINTING              *         
***********************************************************************         
RSTRPRNS NTR1  BASE=*,LABEL=*                                                   
         MVI   RSTRDONE,C'Y'       SET RESTART DONE                             
         L     R9,ADSSB            AOR HAS NO PRINT QUEUES                      
         USING SSBD,R9                                                          
         TM    SSBSTAT4,SSBSAOR                                                 
         BO    EXIT                                                             
*                                                                               
         L     R4,APRTLST          R4=A(LIST OF RESTARTABLE PRINTERS)           
         L     R8,AUTL             SEARCH UTL FOR RESTARTABLE PRINTERS          
         LH    R6,0(R8)                                                         
         L     R7,2(R8)                                                         
         AHI   R8,6                R8=A(TERMINAL UTL ENTRY)                     
         USING UTLD,R8                                                          
         MVI   BYTE,C'N'           SET NO RESTARTS PENDING                      
*                                                                               
RSTR02   ICM   R5,15,TXPRNT        R5=A(PRINTER QUEUE ENTRY)                    
         BZ    RSTR04                                                           
         USING PRQD,R5                                                          
         TM    PRSTAT3,X'04'       RETRY                                        
         BO    RSTR03                                                           
         TM    PRSTAT1,PRS1ARS     TEST RESTART PENDING                         
         BZ    RSTR04                                                           
         MVI   BYTE,C'Y'           SET RESTART PENDING                          
         CLI   PRSTAT,0            TEST PRINTER STILL INACTIVE                  
         BNE   RSTR04                                                           
         OC    PRCIADDR,PRCIADDR                                                
         BNZ   RSTR04                                                           
*                                                                               
RSTR03   ST    R5,0(R4)            SET A(PRQ) IN LIST                           
         TM    PRSTAT3,X'04'       RETRY                                        
         BZ    PSTR03B                                                          
         NI    PRSTAT3,X'FF'-X'04' TURN OF RETRY NOW                            
         OI    0(R4),X'80'         SET TO RESTART                               
*                                                                               
PSTR03B  AHI   R4,4                                                             
*                                                                               
RSTR04   BXLE  R8,R6,RSTR02        BUMP TO NEXT TERMINAL IN UTL                 
*                                                                               
         S     R4,APRTLST          END OF PRINTER QUEUES                        
         SRL   R4,2                                                             
         ST    R4,PRTNUM           SET # OF PRINTERS IN PRTLST                  
         LTR   R4,R4                                                            
         BNZ   RSTR06              CONTINUE IF PRINTERS IN PRTLST               
*                                                                               
         CLI   BYTE,C'Y'           TEST RESTARTS STILL PENDING                  
         BE    EXITOK              YES-EXIT                                     
         OI    SSBSTAT1,SSBSRSPR                                                
         B     EXITOK              SET ALL RESTARTS DONE AND EXIT               
*                                                                               
RSTR06   GOTO1 ADMGR,PQDMCB,(0,GLIST),PRTQUE,NDX,,CXREC                         
         L     RE,NDX+32                                                        
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST         SAVE ADR OF FIRST PRTQ FILE ENTRY            
         MVC   PRTQID+4(1),1(RE)   SET PRTQ ID FROM LIST ENTRY                  
*                                                                               
RSTR08   GOTO1 ADMGR,PQDMCB,(0,BUFFER),PRTQID,,,CXREC                           
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   CIDATA,CXREC+12     SET CI DATA FOR THIS PRTQ FILE               
         USING PQRECD,R5                                                        
*                                                                               
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET ADDRESSES FOR THIS RESOURCE              
         BNE   EXIT                INVALID PRTQ                                 
*                                                                               
         L     R3,PRTNUM           R3=NUMBER OF PRINTERS IN PRTLST              
         L     R4,APRTLST          R4=A(PRTLST ENTRY)                           
*                                                                               
RSTR10   L     R6,0(R4)            R6=A(PRINTER QUEUE)                          
         USING PRQD,R6                                                          
         CLC   PRTQID+4(1),PRPRTQA TEST IF SAME PRTQ FILE AS THIS ONE           
         BNE   RSTR18              NO IGNORE THIS TIME                          
         MVC   CIADDR,PR1CIFST                                                  
*                                                                               
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRC1            CHECK VALID CI ADDRESS                       
         JL    *+2                 NO-INVALID CI ADDRESS                        
         BRAS  RE,FIRCN            A(CI) TO A(INDEX NODE) AND REP#              
         L     R5,FIWNDA                                                        
         LA    R5,SI1NDX-SI1PAR(R5)                                             
*                                                                               
         CLC   PR1KEY,PQKEY        TEST IF KEYS MATCH                           
         BNE   RSTR18                                                           
         TM    PQSTAT,PQSTAC       TEST IF REPORT ACTIVE                        
         BZ    RSTR18                                                           
         TM    PQSTAT,PQSTIN       TEST IF REPORT INVISIBLE                     
         BO    RSTR18                                                           
         TM    PQSTAT,PQSTPG       TEST IF ALREADY PRINTING                     
         BO    RSTR18                                                           
*                                                                               
         BRAS  RE,FIRRLOCK                                                      
         OI    PQSTAT,PQSTPG       SET REPORT PRINTING                          
         MVI   PQAGERT,X'FE'       SET VALUE TO SHOW SYSID IN INDEX             
         ICM   R8,15,PRQAUTL                                                    
         MVC   PQAGEDD(2),TNUM     SET SYSID/TRMNUM IN INDEX                    
         OC    PQAGEDD(1),SYSIDHOB                                              
         OI    0(R4),X'80'         SET THIS PRINTER OK TO START                 
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
*                                                                               
RSTR18   LA    R4,4(R4)            BUMP TO NEXT PRINTER LIST ENTRY              
         BCT   R3,RSTR10                                                        
*                                                                               
         L     RE,APRTQLST                                                      
         LA    RE,8(RE)            BUMP TO NEXT PRTQ FILE                       
         ST    RE,APRTQLST                                                      
         CLI   0(RE),0             END OF LIST                                  
         BE    RSTR24                                                           
         MVC   PRTQID+4(1),1(RE)                                                
         B     RSTR08              BACK TO PROCESS NEXT PRTQ FILE               
*                                                                               
RSTR24   L     R0,PRTNUM           R3=NUMBER OF PRINTERS IN PRTLST              
         L     R4,APRTLST          R4=A(PRTLST ENTRY)                           
*                                                                               
RSTR26   TM    0(R4),X'80'         TEST PRINTER OK TO START                     
         BZ    RSTR28              NO                                           
         L     R6,0(R4)            R6=A(PRINTER QUEUE)                          
         MVI   PRSTAT,PRSACTV                                                   
         ICM   R8,15,PRQAUTL       R8=A(XA PRINTER UTL ENTRY)                   
         BRAS  RE,STRT             PHYSICALLY START PRINTER                     
         B     RSTR30                                                           
*                                                                               
RSTR28   L     R6,0(R4)            REPORT NOT AVAIL FOR RESTART                 
         MVI   PRSTAT1,0                                                        
         XC    PRHDR1,PRHDR1       RESET AUTO RESTART PENDING                   
*                                                                               
RSTR30   AHI   R4,4                BUMP TO NEXT PRINTER IN LIST                 
         BCT   R0,RSTR26                                                        
         B     EXIT                                                             
         DROP  R5,R6,R9                                                         
         EJECT                                                                  
***********************************************************************         
* PHYSICALLY START PRINTER DEVICE - R8=A(UTL ENTRY)                   *         
***********************************************************************         
STRT     NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,VTPRSTRT         CALL LCM TO START PRINTER                    
         GOTO1 ALCM,DMCB,(R0),(R8)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE S/R SAVE PAGES FOR JOB NOTIFICATION & SET FLAGS IN UTL       *         
***********************************************************************         
JOBS     NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF           MAKE SURE ARS ARE GOOD                       
*                                                                               
         L     R9,ADSSB                                                         
         USING SSBD,R9                                                          
         L     R2,ATIA             R2=A(SPECIAL S/R PAGE)                       
         USING SRSD,R2                                                          
         L     R3,AJTAB            R3=A(JOB NOTIFY TABLE)                       
         USING JTABD,R3                                                         
         ICM   R6,15,JNUM          R6=N'ENTRIES IN JOB TABLE                    
         BZ    EXIT                                                             
*                                                                               
         AHI   R6,1                ADD ONE ENTRY FOR E-O-L                      
         XC    DUB,DUB             DUB+0(2)=TNUM, DUB+2(1)=STATUS               
*                                                                               
JOBS02   CLC   JTERM,DUB           TEST CHANGE OF TERMINAL                      
         BE    JOBS16              NO                                           
*                                                                               
         TM    DUB+2,TWAR+TWAU     TEST S/R PAGE READ/UPDATED                   
         BZ    JOBS14              NOT READ NOR UPDATED                         
         BM    JOBS12              READ BUT NOT UPDATED                         
*                                                                               
         LHI   RF,SRPAGENO         WRITE BACK UPDATED SAVE PAGE                 
         SLL   RF,32-8                                                          
         ICM   RF,3,DUB                                                         
         GOTO1 ADMGR,DMCB,DMWRT,TEMPSTR,(RF),ATIA                               
*                                                                               
         L     RF,AUTL             GET UTL ENTRY                                
         LH    RE,DUB                                                           
         BCTR  RE,0                                                             
         MH    RE,0(RF)            INDEX INTO UTL LIST                          
         LA    RF,6(RE,RF)                                                      
X        USING UTLD,RF                                                          
*                                                                               
         CLI   SSBJESIO,C' '                                                    
         BNE   JOBS08                                                           
         XC    X.TJOBSINQ,X.TJOBSINQ                                            
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ       R0=NUM ENTRIES IN JOB Q                      
         BNZ   *+14                                                             
         XC    X.TJOBSINQ,X.TJOBSINQ                                            
         B     JOBS08                                                           
*                                                                               
         LA    R4,SRJOBQ                                                        
         USING SRJOBQ,R4           R4=A(JOB QUEUE)                              
         XR    R1,R1               R1=NUM OF COMPLETED JOBS                     
*                                                                               
JOBS04   TM    SRJOBSTA,(SRJOBOUT+SRJOBNOT+SRJOBINV)                            
         BZ    *+8                                                              
         AHI   R1,1                INCREMENT COMPLETED JOB COUNTER              
         AHI   R4,SRJOBQLN                                                      
         BCT   R0,JOBS04                                                        
*                                                                               
         IC    R0,SRJOBINQ                                                      
         SR    R0,R1               R0=NUM OF USED ENTRIES                       
         STC   R0,X.TJOBSINQ                                                    
*                                                                               
JOBS08   XR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ       R0=N'ENTRIES IN JOB Q                        
         BZ    JOBS10                                                           
*                                                                               
         LA    R4,SRJOBQ           R4=A(JOB QUEUE)                              
         TM    SRJOBSTA,(SRJOBPUT+SRJOBOUT)                                     
         BNO   JOBS10                                                           
         AHI   R4,SRJOBQLN                                                      
         BCT   R0,*-12                                                          
         OI    DUB+2,ALLD          SET ALL JOBS COMPLETED                       
         DROP  R4                                                               
*                                                                               
JOBS10   TM    DUB+2,ALLD          TEST ALL JOBS COMPLETE                       
         BZ    *+8                                                              
         NI    X.TJOBFLAG,255-TJOBFSUB                                          
         TM    DUB+2,NOTP          TEST NOTIFY PENDING                          
         BZ    *+8                                                              
         OI    X.TJOBFLAG,TJOBFOUT                                              
         TM    DUB+2,EXTP          TEST EXTRACT UPDATE PENDING                  
         BZ    *+12                                                             
         OI    X.TJOBFLAG,TJOBFEXT                                              
         OI    SSBJFLAG,SSBJFEXT   SET SYSTEM EXTRACT UPDATE PENDING            
         DROP  X                                                                
*                                                                               
JOBS12   GOTO1 ADMGR,DMCB,DMUNLK,TEMPSTR                                        
*                                                                               
JOBS14   MVC   DUB(2),JTERM        SET CURRENT TERMINAL NUMBER                  
         MVI   DUB+2,0             SET PAGE NOT READ/UPDATED                    
*                                                                               
JOBS16   TM    JSTAT,JSOUT+JSUPD                                                
         BZ    JOBS34                                                           
         TM    DUB+2,TWAR          TEST SAVE PAGE IN CORE                       
         BNZ   JOBS18              YES                                          
*                                                                               
         LHI   RF,SRPAGENO         READ S/R SAVE PAGE                           
         SLL   RF,32-8                                                          
         ICM   RF,3,DUB            RF=PAGE/TERMINAL                             
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADMGR,DMCB,(X'80',DMREAD),TEMPSTR,(RF),ATIA                      
         OI    DUB+2,TWAR          SET SAVE PAGE IN CORE                        
*                                                                               
JOBS18   ICM   R7,15,JJOBADDR                                                   
         BZ    JOBS19                                                           
*                                                                               
         LAM   AR7,AR7,TBALET      EXTRACT JOB LUID INTO DUB1                   
         SAC   512                                                              
         MVC   DUB1,TBJLUID-TBJOBTAB(R7)                                        
         SAC   0                                                                
         CLC   DUB1,SRSLUID        IS TERMINAL STILL IN THIS SLOT               
         BE    JOBS19                                                           
         SAC   512                                                              
         CLC   JPQKEY,TBJPQKEY-TBJOBTAB(R7)   STILL THE SAME JOB?               
         BNE   JOBS33                                                           
         MVI   TBJSTAT-TBJOBTAB(R7),0    JUST DELETE IT THEN                    
         B     JOBS33                                                           
*                                                                               
JOBS19   LAM   AR7,AR7,ARZERO                                                   
         XR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ       R0=N'ENTRIES IN JOB Q                        
         JNZ   JOBS19X                                                          
         LAM   AR7,AR7,TBALET      IF NO JOBS IN TERMINAL AREA                  
         SAC   512                                                              
         USING TBJOBTAB,R7                                                      
         MVI   TBJSTAT,0           FLAG OK TO DELETE NOW                        
         J     JOBS33                                                           
*                                                                               
JOBS19X  EQU   *                                                                
*                                                                               
         LA    R4,SRJOBQ           R4=A(JOB QUEUE)                              
         USING SRJOBQ,R4                                                        
JOBS20   CLC   JPQUSR(JPQCIA-JPQUSR),SRJOBUSR                                   
         BNE   JOBS30                                                           
         TM    SRJOBSTA,SRJOBOUT   TEST COMPLETED                               
         BNZ   JOBS30                                                           
*                                                                               
         OI    DUB+2,TWAU          SET SAVE PAGE UPDATED                        
         TM    JSTAT,JSINV         TEST REPORT IS INVALID                       
         BZ    JOBS22                                                           
         OI    SRJOBSTA,SRJOBOUT+SRJOBINV                                       
         OI    DUB+2,NOTP          SET JOB NOTIFY PENDING                       
*                                                                               
JOBS22   TM    JSTAT,JSUPD         TEST EXTRACT UPDATE PENDING                  
         BNZ   JOBS24                                                           
         OI    SRJOBSTA,SRJOBOUT                                                
         OI    DUB+2,NOTP          SET JOB NOTIFY PENDING                       
         B     JOBS32                                                           
*                                                                               
JOBS24   LA    R5,SRSD             R5=A(EXTRACT UPDATE QUEUE)                   
         AHI   R5,SRUPDQ-SRSD      CREATE EXTRACT UPDATE Q ENTRY                
         USING SRUPDQ,R5                                                        
         LHI   RF,SRUPDMAX         RF=NUM OF QUEUE ENTRIES                      
*                                                                               
JOBS26   CLI   SRUPDSTA,SRUPDAVA   FIND A FREE SRUPDQ ENTRY                     
         BE    JOBS28                                                           
         CLI   SRUPDSTA,SRUPDERR   ERROR ENTRIES CAN BE RE-USED                 
         BE    JOBS28                                                           
         AHI   R5,SRUPDQLN                                                      
         BCT   RF,JOBS26                                                        
         B     JOBS34              NO SLOTS AVAILABLE - DO NEXT TIME            
*                                                                               
JOBS28   LA    RE,NDX                                                           
         USING PQUKRECD,RE                                                      
         XC    NDX,NDX             FIND WHICH PRTQ FILE FROM USER ID            
         XC    SRUPDQ(SRUPDQLN),SRUPDQ                                          
         MVC   PQUKSRCID,SRJOBUSR                                               
         MVC   PQUKREPNO,SRJOBREP                                               
         MVI   PQUKFLAG,PQUKFLNUM+PQUKFLCIA+PQUKFLCIR                           
         MVC   PRTQID,PRTQUE                                                    
         DROP  RE                                                               
*                                                                               
         GOTO1 ADMGR,PQDMCB,(0,=C'INDEX'),PRTQID,NDX,CXREC,ACIREC,0             
         CLI   8(R1),0             TEST FOR ERRORS                              
         BNE   JOBS32                                                           
*&&DO                                                                           
         MVC   CIADDR+0(2),SRJOBCIA                                             
         MVC   CIADDR+2(2),=X'0100'                                             
         XC    NDX,NDX             FIND WHICH PRTQ FILE FROM USER ID            
         MVC   NDX(2),SRJOBUSR                                                  
         GOTO1 ADMGR,PQDMCB,GFILE,PRTQID,NDX,,ACIREC                            
         MVC   PRTQID,NDX+32                                                    
         GOTO1 ADMGR,PQDMCB,DMREAD,PRTQID,CIADDR,ACIREC                         
         CLI   8(R1),0             TEST FOR ERRORS                              
         BNE   JOBS32                                                           
*&&                                                                             
         L     RF,ACIREC                                                        
X        USING PQRECD,RF                                                        
         MVC   SRUPDUSR,X.PQSRCID                                               
         MVC   SRUPDKEY,X.PQUSRINF                                              
         MVC   SRUPDREP,SRJOBREP   SET REPORT NUMBER                            
         OI    SRJOBSTA,SRJOBUPD   SET UPDATE PENDING IN JOB Q                  
         MVI   SRUPDSTA,SRUPDSCH   SET UPDATE PENDING IN UPDATE Q               
         OI    DUB+2,EXTP          SET UPDATE PENDING FOR TERMINAL              
         B     JOBS32                                                           
         DROP  X                                                                
*                                                                               
JOBS30   AHI   R4,SRJOBQLN         BUMP TO NEXT QUEUE ENTRY                     
         BCT   R0,JOBS20                                                        
*                                                                               
JOBS32   ICM   R7,15,JJOBADDR                                                   
         BZ    JOBS34                                                           
         LAM   AR7,AR7,TBALET                                                   
         SAC   512                                                              
         USING TBJOBTAB,R7                                                      
         CLC   JPQKEY,TBJPQKEY                                                  
         BNE   JOBS33                                                           
         MVI   TBJSTAT,0           FLAG OK TO DELETE NOW                        
*                                                                               
JOBS33   SAC   0                                                                
         LAM   AR7,AR7,ARZERO                                                   
         DROP  R7                                                               
*                                                                               
JOBS34   AHI   R3,JTABL            BUMP TO NEXT JOB ENTRY                       
         BCT   R6,JOBS02                                                        
         B     EXITOK                                                           
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* CHECK JOBTAB FOR FREE ENTRIES. FILE AND REMOVE + UPDATE COUNT       *         
***********************************************************************         
*&&DO                                                                           
         PUSH  USING                                                            
         USING DMSPACED,DSPHD                                                   
FIXJTAB  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ADSSB            ONLY 1 SYSTEM NEEDS TO DO THIS               
         CLI   SSBPGMUP-SSBD(RE),C'Y'                                           
         BNE   EXITOK                                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         GOTO1 ALOCKSPC,DUB                                                     
         L     RF,4(R1)                                                         
         MVC   DSPHD,0(RF)                                                      
         NI    DSPTFRST,X'3F'                                                   
*                                                                               
         TIME  BIN                 R0 USED BELOW                                
*                                                                               
         L     R2,DSPTFRST         GET A(JOB TABLE)                             
         LAM   AR2,AR2,TBALET                                                   
         SAC   512                                                              
         USING TBJOBHDR,R2                                                      
*                                                                               
         S     R0,TBJTIME                                                       
         C     R0,=AL4(5*60*100)   5 MINS IN 100TH SEC                          
         BNL   FJT02               IF NOT CLEANED UP GO DO IT                   
         XC    FULL,FULL                                                        
         B     FJT14               OTHERWISE RCVR JOB HAS DONE IT               
         EJECT                                                                  
***********************************************************************         
* NOW SCAN,LOG,UPDATE CLASS AND REMOVE JOB ENTRIES                    *         
***********************************************************************         
FJT02    SR    R3,R3               R3=NEW ENTRY COUNT                           
         ICM   R0,15,TBJNUM        SET R0 TO CURRENT ENTRY COUNT                
         BNZ   *+12                                                             
         ST    R0,FULL             NOTHING TO FIX UP                            
         B     FJT14                                                            
*                                                                               
         AH    R2,JOBHDRL          BUMP PAST HEADER                             
         USING TBJOBTAB,R2                                                      
*                                                                               
FJT04    CLI   TBJSTAT,X'FF'       TEST FOR NOT FOUND                           
         BNE   *+14                                                             
         OC    TBJETIME,TBJETIME   CLEAR IF JOB RAN TO COMPLETION               
         BNZ   FJT06                                                            
*                                                                               
         CLI   TBJSTAT,0           TEST FOR CLEARED ENTRY                       
         BE    FJT06                                                            
         AH    R2,JOBTABL          BUMP TO NEXT                                 
         AHI   R3,1                                                             
         B     FJT10                                                            
*                                                                               
FJT06    OC    0(4,R2),0(R2)                                                    
         BNZ   FJT08                                                            
         AH    R2,JOBTABL          BUMP TO NEXT                                 
         C     R2,DSPTEND                                                       
         BL    FJT04                                                            
         B     FJT12                                                            
*                                                                               
FJT08    MVI   WORK,49                                                          
         MVC   WORK+1(48),TBJNTRY                                               
         BRAS  RE,ARSOFF                                                        
         GOTO1 ATEMPTRC,DMCB,TSOONTRC,WORK,XA=Y                                 
         LAM   AR2,AR2,TBALET                                                   
         SAC   512                                                              
*                                                                               
         LH    R1,JOBTABL          CLEAR JOB ENTRY                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    TBJNTRY(0),TBJNTRY                                               
         AH    R2,JOBTABL          BUMP TO NEXT                                 
*                                                                               
FJT10    C     R2,DSPTEND                                                       
         BNL   FJT12                                                            
         BCT   R0,FJT04                                                         
*                                                                               
FJT12    L     R2,DSPTFRST                                                      
         USING TBJOBHDR,R2                                                      
         ST    R3,TBJNUM           SAVE NEW COUNT                               
         ST    R0,FULL             R0 SHOULD BE ZERO                            
*                                                                               
FJT14    BRAS  RE,ARSOFF           UNLOCK JOB TABLE                             
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         OI    DUB,X'10'                                                        
         GOTO1 ALOCKSPC,DUB                                                     
*                                                                               
         OC    FULL,FULL           WAS THE COUNTER CORRECT                      
         BZ    FJT16                                                            
*                                                                               
         MVC   WORK1,SPACES                                                     
         MVC   WORK1,FJTMSG                                                     
         MVC   WORK1+4(4),SYSNAM4                                               
         ICM   RF,15,=C'LVL2'      OUTPUT ERROR MESSAGE                         
         GOTO1 ADMOD000,DMCB,AWCTYPE,WORK1,L'WORK1,(RF)                         
*                                                                               
FJT16    BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
*                                                                               
FJTMSG   DC    CL48'*FACPAK* =RUN COUNTER WAS RESET'                            
         POP   USING                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* SCAN FACWRK TO SEE IF ANY ACTIVE RECOVERY FILES FOR UPDATE PENDING  *         
***********************************************************************         
WKRUPDT  NTR1  BASE=*,LABEL=*                                                   
         L     R9,ADSSB                                                         
         USING SSBD,R9                                                          
*                                                                               
         GOTO1 ADMGR,DMCB,(X'00',DTFAD),FACWRK                                  
         XR    RE,RE                                                            
         ICM   RE,7,DMCB+13                                                     
         TM    36(RE),X'40'        IS FACWRK FILE NO-OP?                        
         BO    EXIT                YES-CAN'T READ IT                            
*                                                                               
         TM    SSBJFLAG,SSBJFWKR+SSBJFNUP                                       
         BNZ   EXIT                WORK PENDING - WAIT UNTIL DONE               
*                                                                               
         LA    R2,FWNDX            SET TO READ FACWRK INDEX                     
         USING UKRECD,R2                                                        
         XC    UKINDEX,UKINDEX                                                  
*                                                                               
         LAY   RE,FACWRK           SET SPECIAL DMCB FOR FACWRK I/OS             
         ST    RE,FWAFILE                                                       
         ST    R2,FWANDX                                                        
         LAY   R3,FWREC                                                         
         ST    R3,FWAREC                                                        
         USING FWRECD,R3                                                        
*                                                                               
         LAY   R4,FWBUF                                                         
         ST    R4,FWABUF                                                        
         GOTO1 ADMGR,FWDMCB,(X'00',=C'BUF')                                     
*                                                                               
         XR    R0,R0               R0 IS A LOOP COUNTER                         
WKU02    CHI   R0,3                WE SET FLAGS FOR 3 GOOD ENTRIES              
         BH    EXIT                                                             
         GOTO1 ADMGR,FWDMCB,(X'08',=C'IND')                                     
         CLI   8(R1),0                                                          
         BE    WKU04                                                            
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BO    EXIT                                                             
         DC    H'0'                DIE IF DISK ERROR                            
*                                                                               
WKU04    TM    UKSTAT,WKSTAC       FILE MUST BE ACTIVE                          
         BZ    WKU06                                                            
         TM    UKSTAT,WKSTKE       AND NOT KEEP                                 
         BO    WKU02                                                            
         CLI   UKCLASS,C'R'        FILE MUST HAVE CORRECT CLASS                 
         BNE   WKU02                                                            
         CLI   UKSUBPRG,C' '       TEST FACPAK ID IN KEY                        
         BNH   WKU08               NO                                           
         CLC   UKSUBPRG,SSBSYSN1   YES-MUST MATCH ON FACPAK ID                  
         BNE   WKU02                                                            
         B     WKU08                                                            
*                                                                               
WKU06    TM    UKSTAT,WKSTHO       LOOK FOR HOLD FILES THAT DIDN'T GET          
         BZ    WKU02               PROCESSED, WE DON'T KNOW WHY!!               
         TM    UKSTAT,WKSTKE       TEST IF HOLD AND NOT KEEP                    
         BO    WKU02               MUST HAVE DIED OR INTERVENTION               
         CLI   UKCLASS,C'R'        FILE MUST HAVE CORRECT CLASS                 
         BNE   WKU02                                                            
         CLI   UKSUBPRG,C' '       TEST FACPAK ID IN KEY                        
         BNH   WKU02               NO                                           
         CLC   UKSUBPRG,SSBSYSN1   YES MUST MATCH ON FACPAK ID                  
         BNE   WKU02                                                            
         OI    SSBJFLAG,SSBJFWKR   FLAG UPDATE PENDING                          
         AHI   R0,1                                                             
         B     WKU02               BACK FOR ANOTHER GO                          
*                                                                               
         USING WKRECD,R6                                                        
WKU08    AHI   R0,1                                                             
         MVC   DMCB(24),FWDMCB                                                  
         LAY   RE,FWBUF2                                                        
         ST    RE,DMCB+16                                                       
         LHI   RF,BLKSZQ           CURRENTLY 6140 + 256                         
         L     R6,FWABUF                                                        
         LR    R7,RF                                                            
         MVCL  RE,R6               COPY TO 2ND BUFFER TO READ IN                
         LAY   RE,FWBUF2                                                        
         L     RF,FWABUF                                                        
*                                                                               
         GOTO1 ADMGR,DMCB,(X'00',=CL8'READ')                                    
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         LAY   R6,FWBUF2           R6=A(FIRST BLOCK OF FAWRK REPORT)            
         OC    WKPQCI,WKPQCI       ANY PQ JOB ATTACHED                          
         BZ    WKU09               NO                                           
         TM    WKIND1,WKIREP#                                                   
         BZ    WKU09               DON'T CHECK FOR NOW                          
         MVC   PRTQID,PRTQUE       PRIME FOR DATAMGR                            
*&&DO                                                                           
         L     R3,AUPJOBS                                                       
         ICM   RF,15,UPJOB#                                                     
         JZ    *+2                                                              
*                                                                               
WKU08A   CLC   UPJOBUSR,WKPQUSER                                                
         BNE   WKU08B                                                           
         CLC   UPJOBREP,WKPQREP#                                                
         BE    WKU08C                                                           
WKU08B   AHI   R3,UPJOBQ           NEXT ENTRY                                   
         BCTR  RF,WKU08A                                                        
         DC    H'0'                                                             
*&&                                                                             
WKU08C   LA    R5,NDX              GET REPORT                                   
         USING PQUKRECD,R5                                                      
         XC    NDX,NDX                                                          
         MVC   PQUKSRCID,WKPQUSER  USER ID                                      
         MVC   PQUKREPNO,WKPQREP#  REPORT NUM                                   
         OI    PQUKFLAG,PQUKFLNUM  LOCATE BY REF (REP) NUMBER                   
         GOTO1 ADMGR,PQDMCB,(0,=C'INDEX'),PRTQID,NDX,CXREC,ACIREC,0             
         CLI   8(R1),0                                                          
         BE    WKU08D                                                           
         GOTO1 MSGERR              SEND EMAIL AND MOVE ON ANYWAY                
         B     WKU09               LET'S MOVE ON                                
*                                                                               
WKU08D   TM    PQUKATTB,PQUKATJOBI                                              
         BZ    WKU09               THIS IS OK THEN                              
         AHI   R0,-1               DECREMENT, WE CHANGED OUR MIND               
         B     WKU02               SKIP THIS ONE STILL RUNNING                  
         DROP  R5,R6                                                            
*                                                                               
WKU09    GOTO1 ADMGR,FWDMCB,(X'00',=C'HOL')                                     
         CLI   8(R1),0                                                          
         BNE   WKU10                                                            
         OI    SSBJFLAG,SSBJFWKR   FLAG UPDATE PENDING MORE TO DO               
         B     WKU02                                                            
*                                                                               
WKU10    MVC   WORK1,SPACES                                                     
         MVC   WORK1,BADWKR                                                     
         MVC   WORK1+4(4),SYSNAM4                                               
         GOTO1 AHEXOUT,DMCB,UKKEY,WORK1+L'BADWKR,L'UKKEY,0                      
         ICM   RF,15,=C'LVL2'      OUTPUT ERROR MESSAGE                         
         GOTO1 ADMOD000,DMCB,AWCTYPE,WORK1,L'WORK1,(RF)                         
         XR    R0,R0                                                            
         B     WKU02                                                            
*                                                                               
BADWKR   DC    CL31'*FACPAK* BAD WORKER FILE KEY - '                            
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* SEND OUT EMAIL TO INDICATE NO REPORT EVEN THOUGH WE THINK THERE IS  *         
***********************************************************************         
         USING WKRECD,R6                                                        
MSGERR   NTR1                                                                   
         MVC   DUB,8(R1)           ERROR CODE THAT GOT US HERE                  
         GOTO1 AHEXOUT,DMCB,DUB,MSGERRCC,1,0                                    
*                                                                               
         LAY   R6,FWBUF2           R6=A(FIRST BLOCK OF FAWRK REPORT)            
         LLH   RF,WKUSRID                                                       
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGFWCID,DUB                                                     
         MVC   MSGFWSUB,WKSYSPRG                                                
         LLH   RF,WKFILNO                                                       
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGFWRP#,DUB                                                     
         DROP  R6                                                               
*                                                                               
PKNDX    USING PQUKRECD,NDX                                                     
         LLH   RF,PKNDX.PQUKSRCID                                               
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGFPCID,DUB                                                     
         MVC   MSGFPSUB,PKNDX.PQUKSUBID                                         
         LLH   RF,PKNDX.PQUKREPNO                                               
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MSGFPRP#,DUB                                                     
         DROP  PKNDX                                                            
*                                                                               
         GOTO1 ADMGR,DMCB,=C'OPMSG',('MSGFWLNQ',MSGFW1)                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD DARTAB - R5=A(PQREC)                                          *         
***********************************************************************         
*&&US                                                                           
         USING PQRECD,R5                                                        
XTODTAB  NTR1  BASE=*,LABEL=*                                                   
         L     R9,ADSSB                                                         
         USING SSBD,R9                                                          
         L     R1,SSBDARPQ                                                      
         LH    RE,0(R1)            RE=L(PQ ENTRY)                               
         L     RF,2(R1)            RF=A(LAST BYTE OF TABLE)                     
         LA    R1,6(R1)            R1=A(FIRST SAVED PQ ENTRY)                   
*                                                                               
XTOD02   CLC   0(8,R1),EFFS        FOUND AN COMPLETED ENTRY?                    
         BE    XTOD06              YES                                          
         OC    0(8,R1),0(R1)       FOUND AN EMPTY ENTRY?                        
         BZ    XTOD06              YES                                          
*                                                                               
TD1      USING PQKEY,R1            SAME ENTRY AS BEFORE?                        
         CLC   TD1.PQSRCID,PQSRCID                                              
         BNE   XTOD04                                                           
         CLC   TD1.PQSUBID,PQSUBID                                              
         BNE   XTOD04                                                           
         CLC   TD1.PQREPNO,PQREPNO                                              
         BE    XTOD06              YES                                          
*                                                                               
XTOD04   BXLE  R1,RE,XTOD02        NO-GO TO THE NEXT ENTRY                      
         B     XTODX               HIT THE END, BUILD ON NEXT TIMER POP         
*                                                                               
XTOD06   MVC   TD1.PQSRCID,PQSRCID                                              
         MVC   TD1.PQSUBID,PQSUBID                                              
         MVC   TD1.PQREPNO,PQREPNO                                              
         DROP  TD1                                                              
*                                                                               
         CLC   PQSUBID,=C'DAR'     TEST DARE                                    
         BNE   *+8                                                              
         OI    SSBDARFL,SSBDRRPQ   YES                                          
*NOP*    B     *+8                                                              
*NOP*    OI    SSBDARFL,SSBDRSPQ   HAVE AN ENTRY FOR $MKGDARE  (SPOT)           
*                                                                               
         L     R2,AIO2                                                          
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,PQSRCID                                                  
         MVC   HALF,PQSRCID        USE HALF FOR ID NUMBER                       
         MVC   KEY,CTIKEY                                                       
*                                                                               
         GOTO1 ADMGR,DMCB,(0,DMREAD),CTFILE,(R2),(R2)                           
         CLI   0(R1),0                                                          
         BNE   XTOD14              DON'T DIE EVERY POP                          
*                                                                               
         LA    R1,CTIDATA                                                       
         USING CTAGYD,R1                                                        
         XR    RF,RF                                                            
XTOD08   CLI   CTAGYEL,0           FIND AGENCY CODE                             
         BE    XTOD14              BAIL OUT                                     
         CLI   CTAGYEL,CTAGYELQ                                                 
         BE    *+12                                                             
         IC    RF,CTAGYLEN                                                      
         BXH   R1,RF,XTOD08                                                     
*                                                                               
         LA    RF,DARXAGY          EXCLUDED AGENCY LIST                         
XTOD10   CLC   CTAGYID,0(RF)       AGENCY CODE MATCH?                           
         BE    XTODX               YES-NOTHING TO ADD                           
         AHI   RF,L'DARXAGY                                                     
         CLI   0(RF),0             END OF LIST?                                 
         BNE   XTOD10                                                           
*                                                                               
         LA    R1,CTIDATA                                                       
         USING CTDSCD,R1                                                        
         XR    RF,RF                                                            
XTOD12   CLI   CTDSCEL,0                                                        
         BE    XTOD14                                                           
         CLI   CTDSCEL,CTDSCELQ    FIND ALPHA ID                                
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R1,RF,XTOD12                                                     
*                                                                               
         XC    CTIKEY,CTIKEY       POINT TO ALPHA ID RECORD                     
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,CTDSC                                                     
         MVC   KEY,CTIKEY                                                       
         DROP  R1                                                               
*                                                                               
         GOTO1 ADMGR,DMCB,(0,DMRDHI),CTFILE,(R2),(R2)                           
         CLI   8(R1),0                                                          
         BNE   XTOD14              DON'T DIE EVERY POP - ITS DARE'S             
*                                                                               
         CLC   CTIKEY,KEY                                                       
         BE    XTOD16                                                           
         CLC   PQSUBID,=C'DAR'     HAVE AN ENTRY FOR $REPDARE? (REP)            
         BE    XTOD16              YES-DO MULTIPLE ID'S                         
*                                                                               
XTOD14   XC    CTIKEY,CTIKEY       CLEAR ID INFORMATION                         
*                                                                               
XTOD16   L     RE,SSBDARTB         EXTRACT SSB DATA                             
*                                                                               
XTOD18   CLC   0(4,RE),EFFS                                                     
         BE    XTODX               NO ROOM LEFT                                 
         OC    0(4,RE),0(RE)       EMPTY SLOT?                                  
         BZ    XTOD20              YES                                          
         CLC   HALF,0(RE)          USER ID MATCH?                               
         BE    XTOD20              YES                                          
         LA    RE,4(RE)                                                         
         B     XTOD18                                                           
*                                                                               
XTOD20   MVC   0(2,RE),HALF                                                     
         ZAP   DUB(4),=P'0'                                                     
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ADDJUST FOR DDS TIME                         
         AP    DUB(4),TIMEHMS                                                   
         MVO   FULL,DUB(4)         PWOS HHMM?                                   
         MVC   2(2,RE),FULL                                                     
*                                                                               
* REPDARE IS SOMETIMES ACROSS MULTIPLE IDS, SO CHECK FOR MORE IDS               
*                                                                               
         OC    CTIKEY,CTIKEY                                                    
         BZ    XTODX               NO-JUST EXIT                                 
*                                                                               
XTOD22   GOTO1 ADMGR,DMCB,(0,DMRSEQ),CTFILE,AIO2,AIO2                           
         CLI   DMCB+8,0                                                         
         BNE   XTODX               DON'T DIE EVERY POP - ITS DARE'S             
*                                                                               
* PARSE ALPHA ID                                                                
* ADDITIONAL IDS WILL BE ALPHA ID PLUS OTHER CHARACTERS WITH SAME LIMIT         
* ACCESS (E.G. SELNY (O=NY) HAS ADDITIONAL IDS, SELNY1 AND SELNY3)              
*                                                                               
         LA    R1,KEY+((CTIKID-CTIKEY)+L'CTIKID-1)                              
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
*                                                                               
         LA    R0,KEY                                                           
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         BNE   XTODX                                                            
         CLC   CTIKEY,KEY                                                       
*                                                                               
         AHI   R1,-1                                                            
         BNP   XTODX                                                            
         LA    R1,KEY(R1)                                                       
         MVC   FULL(2),0(R1)                                                    
*                                                                               
         LA    R1,CTIDATA                                                       
         USING CTSYSD,R1                                                        
         XR    RF,RF                                                            
XTOD24   CLI   CTSYSEL,0                                                        
         BE    XTOD22              NEXT RECORD                                  
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   *+12                                                             
         CLI   CTSYSNUM,8          REP LIMIT ACCES?                             
         BE    *+12                                                             
         IC    RF,CTSYSLEN                                                      
         BXH   R1,RF,XTOD24                                                     
*                                                                               
         CLC   CTSYSLMT(2),=C'O='                                               
         BNE   XTOD22                                                           
         CLC   CTSYSLMT+2(2),FULL                                               
         BNE   XTOD22                                                           
         DROP  R1                                                               
*                                                                               
         LA    R1,CTIDATA                                                       
         USING CTDSCD,R1                                                        
         XR    RF,RF                                                            
XTOD26   CLI   CTDSCEL,0                                                        
         BE    XTOD22              NEXT RECORD                                  
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+12                                                             
         IC    RF,CTDSCLEN                                                      
         BXH   R1,RF,XTOD26                                                     
*                                                                               
         MVC   HALF,CTDSC                                                       
         B     XTOD16                                                           
         DROP  R1                                                               
*                                                                               
*NOP*    BRAS  RE,SETDAR                                                        
XTODX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* AGENCIES EXCLUDED FROM DARE NOTIFIES                                *         
***********************************************************************         
DARXAGY  DS    0CL2                                                             
         DC    C'WI'               WESTERN                                      
         DC    C'WJ'               WITEST                                       
         DC    X'00'                                                            
         DROP  R5,R9                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* SET DARE MESSAGE FLAG FROM DARETAB IN UTLS                          *         
*                                                                     *         
* DARETAB IN TABS DATASPACE HOLDS 2 TABLES, ONE SORTED, ONE NOT       *         
* SORTED. THE PROGRAM ATTEMPTS TO MERGE ALL UNSORTED (I.E. NEW)       *         
* ENTRIES INTO THE SORTED TABLE, BEFORE RESETTING THE POINTER         *         
* FOR THE UNSORTED TABLE TO THE START OF THE TABLE.                   *         
*                                                                     *         
* NEW POSTINGS COME IN THROUGH DDMQIO AND ARE PLACED INTO THE         *         
* UNSORTED TABLE. IT IS NOT POSSIBLE TO LOCK THIS TABLE DUE TO        *         
* THE INLINE NATURE OF THE MQIO EXECUTION.                            *         
*                                                                     *         
* AFTER MERGING THE TABLES AND RESETTING THE UNSORTED COUNT,          *         
* APPLICATION BXLES AROUND ALL THE UTL ENTRIES.                       *         
* FOR EACH ENTRY AN ATTEMPT TO FIND ANY SORTED DARETAB ENTRIES        *         
* WHICH MATCH USERID AND ARE MORE RECENT IS MADE IN THE               *         
* FOLLOWING SEQUENCE.                                                 *         
*                                                                     *         
* 1. RDHI FOR USERID ONLY                                             *         
*    NO ENTRIES FOR THIS USERID          - NEXT UTL ENTRY             *         
*    USERID (NO INITIALS)                - SET TIME IF REQUIRED       *         
*                                                                     *         
* 2. TEST INITIALS IN USE                                             *         
*    NO INITIALS                         - NEXT UTL ENTRY             *         
*                                                                     *         
* 3. EXACT READ FOR EACH USERID/INITIALS COMBO IN LIST                *         
*    MATCH COMBO                         - SET TIME IF REQUIRED       *         
*                                                                     *         
* 4. NEXT UTL ENTRY                                                   *         
***********************************************************************         
         PUSH  USING                                                            
         USING DMSPACED,DSPHD                                                   
SETDAR   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
         L     R8,ADSSB                                                         
         USING SSBD,R8                                                          
*                                                                               
         XC    DUB,DUB             MOVE & SORT ALL UNSORTED RECORDS             
         MVC   DUB(4),=AL4(DTDARE)                                              
         MVI   DUB,X'80'           THIS WILL BE A LONG ALLOCATE                 
         GOTO1 ALOCKSPC,DUB        LOCK DARE TABLE ENTRY                        
         L     RF,4(R1)                                                         
         MVC   DSPHD,0(RF)                                                      
         NI    DSPTFRST,X'3F'                                                   
*                                                                               
         BRAS  RE,ARSOFF                                                        
         L     R2,DSPTFRST                                                      
         LAM   AR2,AR2,SSBTBLET                                                 
         SAC   512                                                              
         USING TABDARED,R2         R2=BLOCK HEADER INFORMATION                  
         MVC   PLIST(L'TBDBSP),TBDBSP                                           
*                                                                               
         ICM   R3,15,TBDFRST       R3=A(UNSORTED TABLE)                         
         CPYA  AR3,AR2                                                          
USRT     USING TBDDARD,R3                                                       
*                                                                               
         ICM   R0,15,TBDNOW        GET CURRENT COUNT                            
         BZ    SDAR08              NO UNSORTED RECORDS                          
         ST    R0,FULL             SAVE COUNT FOR LATER                         
         B     SDAR04                                                           
*                                                                               
SDAR02   L     R1,FULL             ENTRIES PROCESSED ALREADY                    
         ST    R0,FULL             NEW PROCESSED ENTRY COUNT                    
         SR    R0,R1               R0=REMAINDER REQUIRING PROCESSING            
         JNP   *+2                                                              
*                                                                               
UNCOPY   USING TBDDARD,WORK1       COPY UNSORTED ENTRY LOCALLY                  
SDAR04   MVC   UNCOPY.TBDDARD(TBDDARL),USRT.TBDDARD                             
         LA    R1,PLIST                                                         
         USING BSPARA,R1                                                        
         MVI   BSPLENR,X'01'       SET ADD IF NOT FOUND                         
         LA    RF,UNCOPY.TBDDARD                                                
         ST    RF,BSPAREC          SET RECORD                                   
         MVI   BSPLENR+1,X'80'     SET USING ACCESS REGISTER                    
         STAM  AR2,AR2,BSPARS      SET ACCESS REGISTER                          
*                                                                               
         GOTO1 ABINSRCH,(R1)                                                    
         SAFE  CLEAR=ARZERO                                                     
         TM    0(R1),X'80'         WAS RECORD ADDED?                            
         BO    SDAR06              YES                                          
         ICM   R4,15,BSPAREC       PICK UP A(SORTED RECORD)                     
         CPYA  AR4,AR3             AND SEE IF THIS POSTING IS NEWER             
SORTED   USING TBDDARD,R4                                                       
*                                                                               
         CLC   SORTED.TBDTIME,UNCOPY.TBDTIME                                    
         BH    SDAR06                                                           
         MVC   SORTED.TBDTIME,UNCOPY.TBDTIME                                    
         MVC   SORTED.TBDTYPE,UNCOPY.TBDTYPE                                    
*                                                                               
SDAR06   AHI   R3,TBDDARL          NEXT UNSORTED RECORD                         
         BCT   R0,SDAR04                                                        
*                                                                               
* WE HAVE NOW PROCESSED ALL UNSORTED RECORDS.                                   
* SEE IF ANY NEW ONES CAME IN WHILST WE WERE PROCESSING THEM AND IF SO          
* PROCESS THESE NEW ONES ALSO                                                   
*                                                                               
         L     R0,FULL             MAKE SURE NO NEW ENTRIES HAVE BEEN           
         XR    R1,R1               ADDED WHILST PROCESSING SORT TABLE           
         CS    R0,R1,TBDNOW                                                     
         BNE   SDAR02              COUNT HAS CHANGED WHILST SORTING             
         DROP  USRT,UNCOPY,SORTED                                               
*                                                                               
SDAR08   LAM   AR3,AR4,ARZERO      CLEAR DOWN THE ACCESS REGISTERS              
*                                                                               
SDAR10   L     R5,AUTL             LOOP AROUND THE UTL'S                        
         USING UTLD,R5                                                          
         LH    R6,0(R5)            BXLE SETUP                                   
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
*                                                                               
SDAR12   OC    TXPRNT,TXPRNT       UTL MUST BE A CONNECTED TERMINAL             
         BNZ   SDAR42                                                           
         OC    TUSER,TUSER                                                      
         BZ    SDAR42                                                           
*                                                                               
         XC    WORK1,WORK1         SEE IF POSTINGS FOR THIS USERID              
USERID   USING TBDDARD,WORK1                                                    
         MVC   USERID.TBDUSER,TUSER                                             
*                                                                               
         TM    SSBSYSFL,FACITST    FOR THE TEST SYSTEM                          
         BZ    SDAR14              NEED TO DISTINGUISH REP OR ADV USER          
         CLC   TAGY,=C'SJ'         BASED ON AGENCY                              
         BE    SDAR16              ADV USES SJ AND REP USES B3                  
         B     SDAR18                                                           
*                                                                               
SDAR14   TM    SSBSYSFL,FACIREP    REP SYSTEMS USE USERID IN BINARY             
         BO    SDAR18                                                           
*                                                                               
SDAR16   MVC   USERID.TBDAGY,TAGY  WHILE ADV SYSTEMS USE EBCDIC                 
*                                                                               
SDAR18   LA    R1,PLIST                                                         
         USING BSPARA,R1                                                        
         MVI   BSPLENR,X'02'       READ HIGH                                    
         LA    RF,WORK1                                                         
         ST    RF,BSPAREC          A(RECORD)                                    
         MVI   BSPLENR+1,X'80'     USING ARS                                    
         STAM  AR2,AR2,BSPARS                                                   
*                                                                               
         GOTO1 ABINSRCH,(R1)                                                    
         SAFE  CLEAR=ARZERO                                                     
         TM    0(R1),X'80'                                                      
         BO    SDAR42              NO ENTRIES FOR THIS USERID                   
*                                                                               
         ICM   R3,15,BSPAREC       PICK UP A(SORTED TABLE ENTRY)                
         CPYA  AR3,AR2                                                          
SORTED   USING TBDDARD,R3                                                       
*                                                                               
SDAR20   TM    SSBSYSFL,FACITST    FOR THE TEST SYSTEM                          
         BZ    SDAR22              NEED TO DISTINGUISH REP OR ADV USER          
         CLC   TAGY,=C'SJ'         BASED ON AGENCY                              
         BE    SDAR24              ADV USES SJ AND REP USES B3                  
         B     SDAR26                                                           
*                                                                               
SDAR22   TM    SSBSYSFL,FACIREP    REP SYSTEMS USE USERID IN BINARY             
         BO    SDAR26                                                           
*                                                                               
SDAR24   CLC   SORTED.TBDAGY,TAGY  ADV SYSTEMS USER USERID IN EBCDIC            
         BE    SDAR28                                                           
         B     SDAR42              NO RECORDS FOR THIS USERID                   
*                                                                               
SDAR26   CLC   SORTED.TBDUSER,TUSER                                             
         BNE   SDAR42              NO RECORDS FOR THIS USERID                   
*                                                                               
SDAR28   OC    SORTED.TBDINIT,SORTED.TBDINIT                                    
         BNZ   SDAR30              NO GLOBAL USERID POSTING                     
         CLC   TTIMEDAR,SORTED.TBDTIME                                          
         BNL   SDAR30              POSTING IS OLDER THAN UTL SETTING            
*                                                                               
         MVC   TTIMEDAR,SORTED.TBDTIME                                          
         MVC   TDARETYP,SORTED.TBDTYPE                                          
         OI    TSTATU,TSTATD4U     COPY TIME AND SET FLAG                       
         MVC   TDAREUSR,TUSER      FOR =CT                                      
         DROP  USERID,SORTED                                                    
*                                                                               
SDAR30   ICM   R4,15,TAINITS       ARE WE USING INITIALS SCHEME?                
         BZ    SDAR42              NO-NEXT UTL ENTRY                            
*                                                                               
         CPYA  AR4,AR2                                                          
         USING ASSISTD,R4          R4=INITIALS SCHEME ENTRY                     
         XR    R0,R0                                                            
         ICM   R0,1,ASSCNT         COUNT OF INITIAL SETS                        
         BZ    SDAR42              NONE CURRENTLY                               
         LA    R4,ASSINITS         R4=FIRST INITIAL SET IN LIST                 
*                                                                               
COMBO    USING TBDDARD,WORK1                                                    
SDAR32   XC    WORK1,WORK1         SEE IF POSTING FOR USERID/INITIALS           
         MVC   COMBO.TBDUSER,TUSER                                              
*                                                                               
         TM    SSBSYSFL,FACITST    FOR THE TEST SYSTEM                          
         BZ    SDAR34              NEED TO DISTINGUISH REP OR ADV USER          
         CLC   TAGY,=C'SJ'         BASED ON AGENCY                              
         BE    SDAR36              ADV USES SJ AND REP USES B3                  
         B     SDAR38                                                           
*                                                                               
SDAR34   TM    SSBSYSFL,FACIREP    REP SYSTEMS USE USERID IN BINARY             
         BO    SDAR38                                                           
*                                                                               
SDAR36   MVC   COMBO.TBDAGY,TAGY   WHILE ADV SYSTEMS USE EBCDIC                 
*                                                                               
SDAR38   MVC   COMBO.TBDINIT,0(R4)                                              
*                                                                               
         LA    R1,PLIST                                                         
         USING BSPARA,R1                                                        
         MVI   BSPLENR,X'00'       READ EXACT                                   
         LA    RF,COMBO.TBDDARD                                                 
         ST    RF,BSPAREC          A(RECORD)                                    
         MVI   BSPLENR+1,X'80'     USING ARS                                    
         STAM  AR2,AR2,BSPARS                                                   
*                                                                               
         GOTO1 ABINSRCH,(R1)                                                    
         SAFE  CLEAR=ARZERO                                                     
         TM    0(R1),X'80'         NO ENTRIES FOR THIS COMBINATION              
         BO    SDAR40              YES                                          
*                                                                               
         ICM   R3,15,BSPAREC       PICK UP A(SORT TABLE ENTRY)                  
         CPYA  AR3,AR2                                                          
SORTED   USING TBDDARD,R3                                                       
*                                                                               
         CLC   TTIMEDAR,SORTED.TBDTIME                                          
         BNL   SDAR40              POSTING IS OLDER THAN UTL SETTING            
         MVC   TTIMEDAR,SORTED.TBDTIME                                          
         MVC   TDARETYP,SORTED.TBDTYPE                                          
         OI    TSTATU,TSTATD4U     COPY TIME AND SET FLAG                       
         MVC   TDAREUSR,TUSER      FOR =CT                                      
*                                                                               
SDAR40   AHI   R4,L'ASSINITS       NEXT SET OF INITIALS                         
         BCT   R0,SDAR32                                                        
*                                                                               
SDAR42   BXLE  R5,R6,SDAR12        NEXT UTL ENTRY                               
         DROP  R4,R5,SORTED,COMBO                                               
         MVC   TBDBSP,PLIST        SAVE OFF NEW BINSRCH PLIST                   
         DROP  R2                                                               
*                                                                               
         BRAS  RE,ARSOFF                                                        
         XC    DUB,DUB             FREE DATASPACE                               
         MVC   DUB(4),=AL4(DTDARE)                                              
         MVI   DUB,X'10'                                                        
         GOTO1 ALOCKSPC,DUB        UNLOCK DARE TABLE ENTRY                      
*                                                                               
SDARX    BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         DROP  R1,R8                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK COMMAND TABLE FOR UN-PROCESSED COMMANDS                       *         
***********************************************************************         
CHKCOM   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
*                                                                               
         BRAS  RE,ARSOFF                                                        
         L     RE,ADSSB                                                         
         LAM   AR2,AR2,SSBALET-SSBD(RE)                                         
         SR    R2,R2                                                            
         USING DMDHDR,R2                                                        
         SAC   512                                                              
         L     R2,DHACOMM          SET R2 TO COMMS BLOCK                        
         AHI   R2,4096             SKIP HEADER PAGE                             
         LA    R0,512                                                           
         USING DSCOMM,R2                                                        
*                                                                               
CHKCO010 OC    DSCDEST,DSCDEST     IS THERE A COMMAND                           
         BZ    CHKCO090                                                         
         LA    R1,WORK             BUILD A LIST IN WORK                         
CHKCO020 CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),DSCDEST     ADD THIS ENTRY                               
         B     CHKCO090                                                         
         CLC   0(1,R1),DSCDEST     IGNORE IF ALREADY IN LIST                    
         BE    CHKCO090                                                         
         LA    R1,1(R1)                                                         
         B     CHKCO020                                                         
*                                                                               
CHKCO090 LA    R2,32(,R2)          POINT TO NEXT COMM ENTRY                     
         BCT   R0,CHKCO010         NEXT COMMAND                                 
*                                                                               
CHKCOX   SR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         L     R2,DHAADVS                                                       
         USING DSJOBD,R2                                                        
         LA    R0,DSJOBMXQ         MAX JOBS                                     
CHKCO100 CLI   0(R2),0                                                          
         BE    CHKCO190                                                         
         LA    R1,WORK                                                          
*                                                                               
CHKCO110 CLI   0(R1),0                                                          
         BE    CHKCO190                                                         
         CLC   DSJADV,0(R1)                                                     
         BNE   *+18                                                             
         MVC   WORK1(DSJOBLNQ),DSJOBD                                           
         SAC   0                                                                
         BAS   RE,POSTIT                                                        
         SAC   512                                                              
         LA    R1,1(R1)                                                         
         B     CHKCO110                                                         
*                                                                               
CHKCO190 LA    R2,DSJOBLNQ(,R2)                                                 
         BCT   R0,CHKCO100                                                      
         SAC   0                                                                
         XIT1                                                                   
         DROP  R2                                                               
***********************************************************************         
* DO X MEMORY POST TO FACPAK (DETAILS IN CARD)                        *         
***********************************************************************         
POSTIT   NTR1  BASE=*,LABEL=*                                                   
         LH    R4,WORK1+12         ASID IS AT +12                               
         L     R2,WORK1+16         SSBOPECB IS AT +16                           
         LOCASCB ASID=(R4)                                                      
         LR    R3,R1                                                            
         LTR   RF,RF                                                            
         BNZ   NOPOST                                                           
         USING ASCB,R3                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   NOPOST                                                           
         L     R4,ASCBASSB                                                      
*                                                                               
         L     R4,ASSBJSAB-ASSB(R4) R4=A(JSAB)                                  
         USING JSAB,R4                                                          
         CLC   WORK1(8),JSABJBNM                                                
         BNE   NOPOST                                                           
*                                                                               
         LHI   R5,99               SET COMPLETION CODE                          
         POST  (R2),(R5),ASCB=(R3),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTX)         
         B     POSTXX                                                           
*                                                                               
NOPOST   MVC   WORK1+10(8),WORK1                                                
         MVC   WORK1+2(8),WORK1+10                                              
         MVC   WORK1+10(18),=C' SYSTEM NOT POSTED'                              
         MVC   WORK1+0(2),=H'26'                                                
         WTO   TEXT=WORK1                                                       
         B     POSTXX                                                           
*                                                                               
POSTX    POST  ERRET=DEAD,ECBKEY=YES,MF=L                                       
DEAD     DC    H'0'                                                             
         DROP  R3,R4                                                            
*                                                                               
POSTXX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DO SOME FIXUP WORK ON THE PRINTERS                                  *         
***********************************************************************         
PQFIX    NTR1  BASE=*,LABEL=*                                                   
         L     R8,AUTL             SEARCH UTL LOOKING FOR PRINTERS              
         LH    R6,0(R8)                                                         
         L     R7,2(R8)                                                         
         AHI   R8,6                R8=A(UTL ENTRY)                              
         USING UTLD,R8                                                          
*                                                                               
         L     R4,APRTLST          R4=A(LIST OF PRINTER ADDRS)                  
PQFX02   ICM   R5,15,TXPRNT        R5=A(PRINTER QUEUE ENTRY)                    
         BZ    PQFX28                                                           
         USING PRQD,R5                                                          
*                                                                               
         XC    PR1PEND,PR1PEND     CLEAR PENDING COUNT                          
         OC    PRCIADDR,PRCIADDR   SET TO 1 IF A PRINTER IS BUSY                
         BZ    *+8                                                              
         MVI   PR1PEND,X'01'       THIS ONE SHOULD BE PRINTING                  
*                                                                               
         TM    PRSTAT,PRSERR       TEST IF POOR LITTLE LOST PRINTER             
         BZ    PQFX04                                                           
         TM    PRSTAT3,X'04'       SIMLOGON FAILED                              
         BZ    PQFX03                                                           
         NI    PRSTAT3,X'FB'       TURN OFF X'04'                               
         GOTO1 ALCM,DMCB,VTPRSTRT,(R8),0                                        
         B     PQFX28                                                           
*                                                                               
PQFX03   TM    PRSTAT2,PRS2PATH    THIS HAPPENS WHEN ITS LOST ITS PATH          
         BZ    PQFX04                                                           
         GOTO1 ALCM,DMCB,VTGETCID,(R8),0                                        
         OC    12(2,R1),12(R1)                                                  
         BZ    *+10                                                             
         XC    TCID,TCID           CLEAR CID IF NO SESSION ESTABLISHED          
         GOTO1 ALCM,DMCB,VTPRSTRT,(R8),0                                        
         B     PQFX28                                                           
*                                                                               
PQFX04   TM    PRQMODE,X'80'       IGNORE NON PERM QUEUE PRINTERS               
         BZ    PQFX26                                                           
         TM    PRSTAT1,PRS1ARS     IGNORE RESTART PENDING PRINTERS              
         BO    PQFX26                                                           
         CLI   PRQNE,0             IS PRINTER QUEUE ALREADY INITIALIZED         
         BNE   PQFX26              YES                                          
*                                                                               
         NI    PRQMODE,X'7F'       TURN OFF PERM ENTRY BIT                      
         L     R3,AIO                                                           
         USING CTTREC,R3           R3=A(TERMINAL RECORD)                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,C'T'                                                     
         MVC   CTTKLINE(8),TSYM                                                 
         MVC   KEY(25),CTTKEY                                                   
         GOTO1 ADMGR,DMCB,(X'00',DMREAD),CTFILE,CTTKEY,CTTKEY                   
         CLI   DMCB+8,0                                                         
         BNE   PQFX28                                                           
*                                                                               
PQFX06   LA    R3,CTTDATA          POINT TO FIRST ELEMENT                       
*                                                                               
PQFX08   CLI   0(R3),0             END OF TERMINAL RECORD                       
         BE    PQFX22                                                           
         CLI   0(R3),CTPRTELQ      PRINTER ID ELEMENT                           
         BE    PQFX12                                                           
         CLI   0(R3),CTPRQELQ      PRINTER QUEUE ELEMENT                        
         BE    PQFX14                                                           
*                                                                               
PQFX10   LLC   R1,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         B     PQFX08                                                           
*                                                                               
         USING CTPRTD,R3           MOVE PRINTER ID TO QUEUE HEADER              
PQFX12   OC    PRID,PRID           DONT SET PRINTER-ID MORE THAN ONCE           
         BNZ   PQFX10                                                           
         MVC   PRID,CTPRTID                                                     
         MVC   PRNUM,CTPRTNUM                                                   
         B     PQFX10                                                           
*                                                                               
         USING CTPRQD,R3           MOVE ENTRY TO PRINTER QUEUE                  
PQFX14   LA    RF,WORK1            BUILD A PERM QUEUE ENTRY                     
X        USING PNTRY,RF                                                         
         XC    X.PNTRY,X.PNTRY                                                  
         MVC   X.PNSRCID,CTPRQUSR                                               
         MVC   X.PNSUBID,CTPRQSUB                                               
         MVC   X.PNCLASS,CTPRQCLS                                               
         CLI   CTPRQLEN,8          TEST NEW STYLE SHORT ELEMENT                 
         BE    *+10                YES                                          
         MVC   X.PNCLASS,CTPRQDTA+7                                             
         MVI   X.PNEX,PREXPERM                                                  
         CLI   PRQNE,0             IS THIS THE FIRST QUEUE ENTRY                
         BNE   PQFX16              NO                                           
         MVC   PNTRY,0(RF)         YES MOVE TO FIRST SLOT                       
         XC    PNLAST,PNLAST                                                    
         B     PQFX20                                                           
         DROP  X                                                                
*                                                                               
PQFX16   GOTO1 ALCM,DMCB,VTADDPRQ,(RF),0                                        
         ICM   R1,15,8(R1)                                                      
         BZ    PQFX24              PRINTER ENTRY POOL FULL                      
         MVC   FULL(2),PNLAST      SAVE OLD LAST ENTRY NUMBER                   
         STH   R1,PNLAST           R1=NEW LAST ENTRY NUMBER                     
         LA    RE,PNTRY                                                         
         SR    RF,RF                                                            
         ICM   RF,3,FULL                                                        
         BZ    PQFX18              OLD LAST ENTRY WAS THE FIRST                 
         BCTR  RF,0                                                             
         MHI   RF,L'PNTRY                                                       
         LA    RE,6(RF)                                                         
         A     RE,APRQENTS         RE=A(OLD LAST ENTRY)                         
PQFX18   STH   R1,PNNEXT-PNTRY(RE)                                              
*                                                                               
PQFX20   OI    PRQMODE,X'80'       SET PRINTER MODE TO AUTO                     
         LLC   RE,PRQNE                                                         
         LA    RE,1(RE)                                                         
         STC   RE,PRQNE            BUMP NUMBER OF PRQ ENTRIES                   
         CLC   PRQNE,PRQNEMAX      TEST IF PRINTER QUEUE FULL                   
         BE    PQFX24                                                           
         B     PQFX10                                                           
*                                                                               
PQFX22   L     R3,AIO              R3=A(TERMINAL RECORD)                        
         USING CTTREC,R3                                                        
         GOTO1 ADMGR,DMCB,(X'00',DMRSEQ),CTFILE,CTTKEY,CTTKEY                   
         CLI   DMCB+8,0                                                         
         BNE   PQFX24                                                           
         CLC   CTTKEY(CTTKPASS-CTTKEY),KEY                                      
         BNE   PQFX24                                                           
         CLI   CTTKPASS,C'1'       ENSURE PRINTER QUEUE PAGE RECORD             
         BL    PQFX22                                                           
         CLI   CTTKPASS+1,C' '                                                  
         BNE   PQFX22                                                           
         CLC   CTTKPASS+2(L'CTTKPASS-2),CTTKPASS+1                              
         BNE   PQFX22                                                           
         B     PQFX06                                                           
*                                                                               
PQFX24   CLI   PRQNE,0             ANY ELEMENTS FOUND                           
         BE    PQFX28              NO-IGNORE THIS PRINTER                       
*                                                                               
PQFX26   ST    R5,0(R4)            SET A(PRQ) IN LIST                           
         LA    R4,4(R4)                                                         
         L     R1,APRTLSTX         TEST FOR TABLE FULL                          
         CR    R4,R1                                                            
         JNL   *+2                 PRTLST TOO SMALL                             
*                                                                               
PQFX28   BXLE  R8,R6,PQFX02        BUMP TO NEXT TERMINAL IN UTL                 
*                                                                               
PQFX30   L     RE,APRTLST          END OF PRINTER QUEUE                         
         SR    R4,RE                                                            
         SRL   R4,2                                                             
         ST    R4,PRTNUM           SET NUMBER OF PRINTERS IN PRTLST             
         XC    MTCHNUM,MTCHNUM                                                  
         MVI   NOTIFY,0                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ PRTQ FILES INDICIES AND MATCH REPORTS TO PRINTERS IN PTRLST    *         
***********************************************************************         
RPTMCH   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ADMGR,PQDMCB,(0,GLIST),PRTQUE,NDX,,CXREC                         
         L     RE,NDX+32                                                        
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST         SAVE ADR OF FIRST PRTQ FILE ENTRY            
         MVC   PRTQID+4(1),1(RE)   SET PRTQ ID FROM LIST ENTRY                  
*                                                                               
RPM02    GOTO1 ADMGR,PQDMCB,(0,BUFFER),PRTQID,,,CXREC                           
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         MVC   CIDATA,CXREC+12     SET CI DATA FOR THIS PRTQ FILE               
         XC    DUB,DUB             SET TO READ FIRST INDEX PAGE                 
         MVC   DUB(2),EFFS                                                      
*                                                                               
         LA    R5,FIWNDX                                                        
         USING PQRECD,R5                                                        
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET ADDRESSES FOR THIS RESOURCE              
         JNE   *+2                 INVALID PRTQ                                 
         MVC   FIWNDA,FIWP1A       A(START OF PART1 INDEXES)                    
         B     RPM05                                                            
*                                                                               
RPM04    BRAS  RE,FIRNSN           GET NEXT INDEX ENTRY                         
         BNE   RPM76                                                            
RPM05    L     R1,FIWNDA                                                        
         MVC   FIWNDX(L'PQINDEX),SI1NDX-SI1PARD(R1)                             
         BRAS  RE,FIRNC            A(NODE) TO A(CI)                             
         MVC   CIADDR,FIWCIA                                                    
*                                                                               
RPM06    CLI   PQSEQ,1             LOW ORDER CI'S ONLY                          
         BH    RPM74                                                            
*&&DO                                                                           
*&&US                                                                           
RPM08    CLI   PQCLASS,C'N'        DARE - CLASS N ACTIVE REPORT                 
         BNE   RPM14                                                            
         TM    PQSTAT,PQSTAC                                                    
         BZ    RPM14                                                            
         CLC   PQSUBID,=C'DAR'     DAR REPORT?                                  
         BNE   RPM14                                                            
         CLI   SYSID,1             TST OR FQA SYSTEM?                           
         BE    RPM12                                                            
         CLI   SYSID,15                                                         
         BE    RPM12               YES-PUT INTO DARTAB                          
*                                                                               
         CLI   SYSID,6             MEL SYSTEM?                                  
         BE    RPM14               YES-DON'T PROCESS - STOP CONFLICTS           
*                                                                               
         L     RF,ADSSB            GET A(FACIDTAB) FROM SSB                     
         USING FACITABD,RF                                                      
         L     RF,SSBAFID-SSBD(RF)                                              
RPM10    CLI   0(RF),X'FF'                                                      
         JE    *+2                 THIS BETTER EXIST ON FACIDTAB                
         CLC   FACIID,SYSID                                                     
         BE    *+12                                                             
         LA    RF,L'FACITAB(RF)                                                 
         B     RPM10                                                            
         TM    FACIFL,FACIREP      ARE WE ON THE REP SYSTEM?                    
         BZ    RPM14               NO-DONE                                      
         DROP  RF                                                               
*                                                                               
RPM12    BRAS  RE,XTODTAB          PUT ENTRY INTO DARPQTAB                      
         B     RPM14                                                            
*&&                                                                             
*&&                                                                             
*                                                                               
RPM14    TM    MODE,X'01'          TEST EXTENDED CHECKING MODE                  
         BO    RPM24                                                            
*                                                                               
* MODE 0 PROCESS ONLY THOSE REPORTS FLAGGED AS COMPLETED JOBS                   
* MATCH ON PRTQUE KEY TO CORRESPONDING JOB TAB ENTRY                            
*                                                                               
RPM16    TM    PQATTB,PQATJOBI     IGNORE SCHEDULED JOBS                        
         BO    RPM74                                                            
         TM    PQSTAT,PQSTIN       IGNORE INVISIBLE                             
         BO    RPM74                                                            
         TM    PQATTB,PQATJOBO     ONLY WANT COMPLETED JOBS                     
         BZ    RPM38                                                            
*                                                                               
RPM18    ICM   R0,15,JNUM          R0=N'JOB NOTIFIES PENDING                    
         BZ    RPM38                                                            
         L     R1,AJTAB                                                         
         USING JTABD,R1            R1=A(JOB NOTIFY TABLE)                       
RPM20    CLC   PQKEY,JPQKEY                                                     
         BE    RPM32               PRTQUE KEY MATCHES JOBTAB KEY                
         AHI   R1,JTABL                                                         
         BCT   R0,RPM20                                                         
         B     RPM38               THIS COMPLETED JOB NOT IN JOBTAB             
*                                                                               
* MODE 1 TEST EVERY PQ REPORT AGAINST EVERY JOB TAB ENTRY                       
* CHECK FOR CHANGE OF KEY IN PRTQUE FROM THAT IN JOB TAB                        
*                                                                               
RPM24    OC    JNUM,JNUM           TEST IF ANY JOBS TO CHECK                    
         BZ    RPM38               NO                                           
RPM26    ICM   R0,15,JNUM          R0=N'JOB NOTIFIES PENDING                    
         BZ    RPM38                                                            
         L     R1,AJTAB                                                         
         USING JTABD,R1            R1=A(JOB NOTIFY TABLE)                       
RPM28    CLC   PRTQID+4(1),JPQID   TEST IF SAME PRTQUE FILE                     
         BNE   RPM30                                                            
         CLC   CIADDR(2),JPQCIA    TEST IF SAME CIADDR                          
         BNE   RPM30                                                            
         CLC   PQKEY,JPQKEY        TEST IF KEYS HAVE CHANGED                    
         BNE   RPM34                                                            
         TM    PQATTB,PQATJOBI     IGNORE SCHEDULED JOBS                        
         BO    RPM74                                                            
         TM    PQSTAT,PQSTIN       IGNORE INVISIBLE                             
         BO    RPM74                                                            
         TM    PQATTB,PQATJOBO     TEST IF JOB HAS COMPLETED                    
         BO    RPM32                                                            
         B     RPM38                                                            
*                                                                               
RPM30    LA    R1,JTABL(R1)                                                     
         BCT   R0,RPM28                                                         
         B     RPM38               THIS REPORT NOT IN JOB TABLE                 
*                                                                               
* THIS COMPLETED PRTQUE REPORT MATCHES ENTRY IN JOBTAB                          
*                                                                               
RPM32    OI    JSTAT,JSOUT         SET TO NOTIFY THIS JOB HAS COMPLETED         
         LH    RE,OUTCNT           BUMP JOBS OUT COUNTER                        
         LA    RE,1(RE)                                                         
         STH   RE,OUTCNT                                                        
         TM    PQATTB,PQATERR      TEST THIS JOB ABENDED                        
         BZ    *+16                                                             
         OI    JSTAT,JSINV         YES-SET JOB INVALID                          
         OI    NOTIFY,JSOUT                                                     
         B     RPM36                                                            
         TM    PQATTB,PQATUSRW     TEST WORKER KEY PRESENT                      
         BO    *+12                                                             
         OI    NOTIFY,JSOUT                                                     
         B     RPM36                                                            
         OI    JSTAT,JSUPD         KEY PRESENT - SET UPDATE PENDING             
         OI    NOTIFY,JSUPD                                                     
         B     RPM36                                                            
*                                                                               
* THIS JOBTAB ENTRY HAS A CHANGE IN THE PRTQUE KEY                              
*                                                                               
RPM34    OI    JSTAT,JSOUT         SET TO NOTIFY THIS JOB HAS VANISHED          
         LH    RE,OUTCNT           BUMP JOBS OUT COUNTER                        
         LA    RE,1(RE)                                                         
         STH   RE,OUTCNT                                                        
         OI    NOTIFY,JSOUT+JSNPQ                                               
         OI    JSTAT,JSNPQ         SET JOB NO LONGER IN PRTQUE                  
         B     RPM36                                                            
         DROP  R1                                                               
*                                                                               
* RECORD THE COMPLETION OF A JOB EVENT IF YOU WANT TO                           
*                                                                               
RPM36    DS    0H                                                               
*                                                                               
* PROCESS ACTIVE PRTQUE REPORTS AGAINST PRINTER WORK LISTS                      
*                                                                               
RPM38    TM    PQATTB,PQATJOBI     IGNORE SCHEDULED JOBS                        
         BO    RPM74                                                            
         TM    PQSTAT,PQSTAC       REPORT MUST BE ACTIVE                        
         BZ    RPM74                                                            
         TM    PQSTAT,PQSTIN       AND NOT INVISIBLE                            
         BO    RPM74                                                            
         TM    PQSTAT,PQSTPG       AND NOT PRINTING                             
         BO    RPM74                                                            
         TM    MODE,X'80'          IF MODE NOT 80                               
         BO    RPM40                                                            
         LA    RF,DATEN                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,DATEO                                                         
         CLC   PQAGELD,0(RF)       TODAYS REPORTS ONLY                          
         BNE   RPM74                                                            
         CLC   PQAGELT,TIMEC       AND WITHIN LAST HALF HOUR                    
         BL    RPM74                                                            
RPM40    ICM   R3,15,PRTNUM        R3=NUMBER OF PRINTERS IN PRTLST              
         BZ    RPM74                                                            
         L     R4,APRTLST          R4=A(A(PRINTER QUEUE))                       
*                                                                               
RPM42    L     R6,0(R4)            R6=A(PRINTER QUEUE)                          
         USING PRQD,R6                                                          
         SR    R7,R7                                                            
         ICM   R7,1,PRQNE          R7=NUMBER OF ENTRIES IN QUEUE                
         BZ    RPM72                                                            
         LA    R6,PNTRY            R6=A(NEXT PRQ ENTRY)                         
         USING PNTRY,R6                                                         
         B     RPM46                                                            
*                                                                               
RPM44    SR    RF,RF               BUMP TO NEXT ENTRY                           
         ICM   RF,3,PNNEXT                                                      
         BZ    RPM72               C'EST LA VIE                                 
         BCTR  RF,0                                                             
         MHI   RF,L'PNTRY                                                       
         LA    R6,6(RF)                                                         
         A     R6,APRQENTS         LOCATE PRQ ENTRY                             
RPM46    OC    0(8,R6),0(R6)                                                    
         BZ    RPM72               HONI SOI QUI MAL Y PENSE                     
*&&US                                                                           
         CLC   PNSRCID,=X'0406'    GRAFNET ONLY HAS CLASS G REPORTS             
         BNE   *+16                                                             
         CLI   PQCLASS,C'G'                                                     
         BNE   RPM70                                                            
         B     RPM64                                                            
         CLI   PQCLASS,C'G'        CLASS G RESERVED FOR GRAFNET                 
         BE    RPM70                                                            
         CLI   PQCLASS,C'N'        DO NOT PRINT CLASS N REPORTS                 
         BE    RPM70                                                            
*&&                                                                             
RPM48    TM    PNSRCID,X'80'       TEST GENERIC USER-ID                         
         BZ    RPM52               NO                                           
         OC    AGENIDS,AGENIDS     TEST A(GENIDS) SET                           
         BZ    RPM52               NO-IGNORE                                    
*                                                                               
RPM50    GOTO1 AGENIDS,DMCB,PNSRCID,ADMGR,XA=Y                                  
         BNE   RPM52                                                            
         LM    RE,RF,0(R1)         RE=N'ENTRIES, RF=A(ENTRIES)                  
         CLC   PQSRCID,0(RF)       MATCH SOURCE ID                              
         BE    RPM54                                                            
         LA    RF,2(RF)            BUMP TO NEXT                                 
         BCT   RE,*-14                                                          
         B     RPM70                                                            
*                                                                               
RPM52    CLC   PNSRCID,PQSRCID     MATCH SOURCE ID                              
         BNE   RPM70                                                            
RPM54    TM    PNCOPYS,PNCENDP     TEST IF SUBID CONTAINS SUBID                 
         BO    RPM60                                                            
         CLC   PNSUBID,=C'ALL'     MATCH REPORT SUB-ID                          
         BE    RPM60                                                            
         CLI   PNSUBID+1,C'*'      GENERIC SUB-ID X**                           
         BNE   RPM56                                                            
         CLC   PNSUBID(1),PQSUBID  MATCH ON FIRST CHR ONLY                      
         BNE   RPM70                                                            
         B     RPM60                                                            
*                                                                               
RPM56    CLI   PNSUBID+2,C'*'      GENERIC SUB-ID XX*                           
         BNE   RPM58                                                            
         CLC   PNSUBID(2),PQSUBID  MATCH ON FIRST TWO CHRS ONLY                 
         BNE   RPM70                                                            
         B     RPM60                                                            
*                                                                               
RPM58    CLC   PNSUBID,PQSUBID                                                  
         BNE   RPM70                                                            
*                                                                               
RPM60    CLI   PNCLASS,0           MATCH REPORT CLASS                           
         BE    RPM64                                                            
         MVC   FLAG,PNCLASS                                                     
         TM    FLAG,X'40'                                                       
         BZ    RPM62                                                            
         CLC   FLAG,PQCLASS        POSITIVE CLASS FILTER                        
         BNE   RPM70                                                            
         B     RPM64                                                            
*                                                                               
RPM62    OI    FLAG,X'40'                                                       
         CLC   FLAG,PQCLASS        NEGATIVE CLASS FILTER                        
         BE    RPM70                                                            
*                                                                               
RPM64    TM    PNCOPYS,PNCTIME     TEST IF SEQNUM CONTAINS SEQ NUM              
         BZ    RPM66                                                            
         LA    RF,DATEN                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,DATEO                                                         
         CLC   PQAGELD,0(RF)       BEFORE TODAY ALWAYS QUALIFIES                
         BL    RPM68                                                            
         BH    RPM70                                                            
         CLC   PQAGELT,PNSEQN                                                   
         BNH   RPM68                                                            
         B     RPM70                                                            
*                                                                               
RPM66    OC    PNSEQN,PNSEQN       TEST ALL SEQUENCE NUMBERS                    
         BZ    RPM68                                                            
         CLC   PQREPNO,PNSEQN      MATCH REPORT SEQUENCE NUMBER                 
         BE    RPM68                                                            
         B     RPM70                                                            
*                                                                               
RPM68    OI    0(R4),X'80'         SET PRINTER/REPORT MATCHED                   
         L     R1,0(R4)                                                         
         LLC   RE,PR1PEND-PRQD(R1)                                              
         LA    RE,1(RE)            BUMP PENDING COUNT                           
         STC   RE,PR1PEND-PRQD(R1)                                              
         TM    PNEX,PREXACTV       IS QUEUE ENTRY ACTIVE                        
         BO    RPM72               YES-TRY ANOTHER PRINTER                      
         OI    PNEX,PREXACTV                                                    
         L     RE,MTCHNUM          BUMP PRINTER/REPORT MATCH COUNT              
         LA    RE,1(RE)                                                         
         ST    RE,MTCHNUM                                                       
         B     RPM74                                                            
*                                                                               
RPM70    BCT   R7,RPM44            BUMP TO NEXT PRINTER QUEUE ENTRY             
*                                                                               
RPM72    LA    R4,4(R4)            BUMP TO NEXT PRINTER                         
         BCT   R3,RPM42                                                         
*                                                                               
RPM74    B     RPM04               BUMP TO NEXT REPORT                          
*                                                                               
RPM76    ICM   RE,15,APRTQLST                                                   
         BZ    EXIT                                                             
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST         BUMP TO NEXT PRTQ FILE                       
         CLI   0(RE),0                                                          
         BE    EXIT                EXIT IF END OF PRTQ FILE LIST                
         MVC   PRTQID+4(1),1(RE)   SET NEXT PRTQ FILE ID                        
         B     RPM02               BACK TO PROCESS NEXT PRTQ FILE               
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ALL INDEX RECORDS READ - NOW START PRINTERS THAT HAVE ANY WORK      *         
***********************************************************************         
PRTSTRT  NTR1  BASE=*,LABEL=*                                                   
         ICM   R3,15,PRTNUM        R3=NUMBER OF PRINTERS IN PRTLST              
         BZ    EXITOK                                                           
*                                                                               
         L     R4,APRTLST          R4=A(A(PRINTER QUEUE))                       
PSTRT02  TM    0(R4),X'80'         MATCHED PRINTERS ONLY                        
         BZ    PSTRT04                                                          
*                                                                               
         L     R6,0(R4)                                                         
         USING PRQD,R6                                                          
         TM    PRQMODE,X'80'       IGNORE NON PERM QUEUE PRINTERS               
         BZ    PSTRT04                                                          
         TM    PRSTAT1,PRS1ARS     IGNORE RESTART PENDING PRINTERS              
         BO    PSTRT04                                                          
         CLI   PRSTAT,0            IGNORE IF PRINTER ALREADY ACTIVE             
         BNE   PSTRT04                                                          
         OC    PRCIADDR,PRCIADDR                                                
         BNZ   PSTRT04                                                          
*                                                                               
         MVI   PRSTAT,PRSACTV      SET STATUS TO ACTIVE                         
         MVI   PRSTAT1,PRS1SOS     SET START OF SESSION                         
         ICM   R8,15,PRQAUTL       R8=A(PRINTER UTL ENTRY)                      
         BRAS  RE,STRT             PHYSICALLY START PRINTER                     
*                                                                               
         XC    LOGREC,LOGREC       SET TIMER 2 LOG DATA                         
         MVC   LOGID,MYLOGID                                                    
         MVI   LOGID+3,C'U'        $PQU FOR START PRINTER ON POP                
         ICM   RF,15,PRQAUTL                                                    
         MVC   LOGLUID,TSYM-UTLD(RF)                                            
         MVC   LOGTIME,TIMEHMS                                                  
         GOTO1 ALOGGER,LOGREC                                                   
*                                                                               
PSTRT04  LA    R4,4(R4)            BUMP TO NEXT PRINTER                         
         BCT   R3,PSTRT02                                                       
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SCAN WRKF FOR ACTIVE SCRIPT FILES                                   *         
***********************************************************************         
WKFSCAN  NTR1  BASE=*,LABEL=*                                                   
         L     R9,ADSSB                                                         
         USING SSBD,R9                                                          
         LA    R1,ERLYSTRT         START/END PAIR                               
*&&US                                                                           
         CLI   SSBDAYNO,6          TEST SAT/SUN                                 
         BH    EXITOK              SUNDAY-EXIT                                  
         BNE   *+8                                                              
         LA    R1,ERLYSTR6                                                      
*&&                                                                             
         TM    SSBSTAT1,SSBUII     NO SCAN IF SSB STOP                          
         BO    EXITOK                                                           
         CLC   SSBTPOPT,4(R1)      NO SCRIPTS > LATE STOP                       
         BH    EXITOK                                                           
         CLC   SSBTPOPT,0(R1)      NO SCRIPTS < EARLY START                     
         BL    EXITOK                                                           
*                                                                               
         GOTO1 ADMGR,DMCB,(X'00',DTFAD),WRKF1,0                                 
         XR    RE,RE                                                            
         ICM   RE,7,DMCB+13                                                     
         TM    36(RE),X'40'        IS FIRST WRKF FILE NOP?                      
         BO    EXITOK              YES-CAN'T READ IT                            
*                                                                               
         LA    R2,WORK1            SCAN WRKF FOR ACTIVE SCRIPT FILES            
         USING WSBLOCKD,R2                                                      
         XC    WSBLOCK,WSBLOCK                                                  
         MVI   WSFILE,X'FF'        ALL FILES                                    
         MVI   WSSORT,2            SORT BY CREATION                             
         MVI   WSFLAGS,WSFXUSER    IGNORE USER IN SORT                          
         OI    WSFLAGS,WSFREFNO    RETURN REF# IN CIADDR                        
         MVI   WSSTAT,X'80'        FIND ACTIVE FILES                            
         MVI   WSSTATN,X'23'       EXCLUDE TEMP AND PROCESSED FILES             
         MVI   WSTYPE,C'A'         FIND TYPE A (AUTO SCRIPT)                    
         MVC   WSSMAX,=PL2'100'    SET MAX 100 ENTRIES                          
         MVC   WSSUBPRG,SSBSYSN1   SET SYSTEM 1 CHR NAME                        
*                                                                               
         LA    R1,LATERUN                                                       
         CLI   SSBDAYNO,6          TEST SATURDAY                                
         BH    WFS02                                                            
         BNE   *+8                                                              
         LA    R1,LATERUN6         SATURDAY LATERUN TIME                        
*                                                                               
         CLC   SSBTPOPT,0(R1)      LATERUN=5 O CLOCK                            
         BL    *+8                                                              
         MVI   WSTYPE+1,C'L'       ALSO FIND TYPE L (LATE AUTO SCRIPT)          
*                                                                               
WFS02    GOTO1 AWKSCAN,DMCB,(R2),ACIREC,AIO,CXREC,ACOMFACS,XA=Y                 
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         L     R2,AIO              POINT TO TABLE                               
         USING WSEDATAD,R2                                                      
         CLC   WSEDATA,EFFS        TEST FOR END OF TABLE                        
         BE    EXITOK                                                           
         OI    SSBJFLG2,SSBJFSCP                                                
         B     EXITOK                                                           
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* START TIMES MUST BE 4 BYTES FOLLOWED BY END TIMES                   *         
***********************************************************************         
*&&US                                                                           
ERLYSTRT DC    PL4'013000'         DO NOT RUN BEFORE 07.30 AM                   
LATESTOP DC    PL4'151500'         DO NOT RUN AFTER 9.15 PM                     
*                                                                               
ERLYSTR6 DC    PL4'044500'         DO NOT RUN BEFORE 10.45 AM SAT               
LATESTP6 DC    PL4'110000'         DO NOT RUN AFTER 5PM SAT                     
*                                                                               
LATERUN  DC    PL4'110000'         START TIME FOR LATE SCRIPTS                  
LATERUN6 DC    PL4'090000'         START TIME FOR LATE SATURDAY                 
*&&                                                                             
*                                                                               
*&&UK                                                                           
ERLYSTRT DC    PL4'073000'         DO NOT RUN BEFORE 07.30 AM                   
LATESTOP DC    PL4'230000'         DO NOT RUN AFTER 11PM                        
*                                                                               
LATERUN  DC    PL4'170000'         START TIME FOR LATE SCRIPTS                  
LATERUN6 DC    PL4'170000'         START TIME FOR LATE SATURDAY                 
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         L     RF,=V(BINSRCH)                                                   
         A     RF,RELO                                                          
         ST    RF,ABINSRCH                                                      
         L     RF,=V(WKSCAN)                                                    
         A     RF,RELO                                                          
         ST    RF,AWKSCAN                                                       
*                                                                               
         L     RF,ASYSFAC                                                       
         USING SYSFACD,RF                                                       
         MVC   ADMGR,VDATAMGR                                                   
         MVC   ATICTOC,VTICTOC                                                  
         MVC   ALOCKSPC,VLOCKSPC                                                
         MVC   AISGENQ,VISGENQ                                                  
         MVC   AENQDEQ,VENQDEQ                                                  
         MVC   ALOGGER,VLOGGER                                                  
         MVC   APRQENTS,VPRQENTS                                                
         MVC   ACALLOV,VCALLOV                                                  
         MVC   ATEMPTRC,VTEMPTRC                                                
         MVC   ADMOD000,VDMOD000                                                
         MVC   ADSSB,VSSB                                                       
         MVC   ALCM,VLCM                                                        
         MVC   AUTL,VUTL                                                        
         MVC   AWCTYPE,VWCTYPE                                                  
         MVC   ATCB,VTCB                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   ADATCON,CDATCON                                                  
         MVC   AGETDAY,CGETDAY                                                  
         MVC   ADEJAVU,CDEJAVU                                                  
         MVC   ASWITCH,CSWITCH                                                  
         MVC   AXSORT,CXSORT                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   AMQIO,CMQIO                                                      
         DROP  RF                                                               
*                                                                               
         L     RE,=A(CIREC-WORKD)                                               
         AR    RE,RC                                                            
         ST    RE,ACIREC                                                        
         L     RE,=A(IO-WORKD)                                                  
         AR    RE,RC                                                            
         ST    RE,AIO              SET A(AIO) IN W/S                            
         L     RE,=A(IO2-WORKD)                                                 
         AR    RE,RC                                                            
         ST    RE,AIO2             SET A(AIO2) IN W/S                           
         L     RE,=A(PRTLST-WORKD)                                              
         AR    RE,RC                                                            
         ST    RE,APRTLST                                                       
         L     RE,=A(PRTLSTX-WORKD)                                             
         AR    RE,RC                                                            
         ST    RE,APRTLSTX                                                      
         L     RE,=A(JTAB-WORKD)                                                
         AR    RE,RC                                                            
         ST    RE,AJTAB            SET A(JTAB) IN W/S                           
*        L     RE,=A(UPJOBS-WORKD)                                              
*        AR    RE,RC                                                            
*        ST    RE,AUPJOBS          A(UPDATIVE JOBS)                             
*                                                                               
         XC    AGENIDS,AGENIDS                                                  
         GOTO1 ACALLOV,DMCB,0,X'D9000AFC',0                                     
         XR    R0,R0                                                            
         ICM   R0,7,1(R1)                                                       
         CLI   4(R1),X'FF'         TEST GENIDS FOUND                            
         BE    *+8                 GIVE UP                                      
         ST    R0,AGENIDS                                                       
*                                                                               
         GOTO1 ATICTOC,DUB,C'SGET'    SAVE TIME AS P'0HHMMSS+'                  
         MVC   TIMEHMS,DUB                                                      
         GOTO1 ATICTOC,DUB,C'1SET',0  DISABLE LOOP TIMER                        
         TIME  BIN                                                              
         STM   R0,R1,DUB           R0=TIME(BIN 1/100 SECS),R1=DATE              
         ST    R0,TIMEBIN                                                       
*                                                                               
         GOTO1 ADATCON,DMCB,(6,DUB+4),(30,DATEN) NEW CMPRSD DATE                
         MVC   DATEO,DATEN                                                      
         OI    DATEO,X'80'                       OLD CMPRSD DATE                
         MVC   DATEC,DATEO                       SET DATE TO OLD CMPRSD         
         GOTO1 (RF),(R1),(14,DATEN),(0,DUB)      DUB=YYMMDD                     
         GOTO1 AGETDAY,DMCB,DUB,DUB1             DUB1=DDD                       
         GOTO1 ADEJAVU,DMCB,(3,DUB1),(X'10',DAY),0                              
*                                                                               
         LA    RE,L'PQINDEX        SET PRTQUE CONTROL INTERVAL DATA             
         STH   RE,CINDXLN                                                       
         L     RE,AISGENQ                                                       
         ST    RE,FIWENQ                                                        
         MVC   PRTQID,PRTQUE                                                    
         XC    CIDATA,CIDATA                                                    
*                                                                               
         L     RE,ADSSB            EXTRACT SSB DATA                             
         USING SSBD,RE                                                          
         OI    SSBSTAT3,SSBSRTIM                                                
         MVC   DMALET,SSBALET                                                   
         MVC   TBALET,SSBTBLET                                                  
         MVC   RECLEN,SSBTWAL      SET TEMPSTR RECORD LENGTH                    
         MVC   ABCTAB,SSBABC                                                    
*                                                                               
         IC    R0,SSBSYSID                                                      
         STC   R0,SYSID                                                         
         SLL   R0,4                                                             
         STC   R0,SYSIDHOB         SET FACPAK SYSTEM ID AS HOB                  
         MVC   SYSNAME,SSBSYSNA    SET FACPAK 3 CHR NAME                        
         LLC   R1,SSBSYSIX         ADD 1 CHR FOR AORS                           
         SRL   R1,4                                                             
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         AHI   R1,X'C0'                                                         
         STC   R1,SYSNAME+3                                                     
         MVC   SYSNAM4,SSBSYSN4                                                 
         MVC   SYSDATE,SSBDATE     SET FACPAK START DATE                        
         MVI   RSTRDONE,C'N'                                                    
         TM    SSBSTAT1,SSBSRSPR                                                
         BZ    *+8                                                              
         MVI   RSTRDONE,C'Y'       SET PRINTER RESTART DONE                     
         DROP  RE                                                               
*                                                                               
         MVI   FRST,C'N'           TEST IF FIRST TIME FLAG SET                  
*                                                                               
         GOTO1 ADMGR,DMCB,SHMUSS,ATTACH,MEMORY,0,0                              
         ICM   R1,15,DMCB+12       A(SHARED MEMORY)                             
         JZ    *+2                                                              
         ST    R1,FIWSHA                                                        
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME SYSTEM INITIALISATION CODE                               *         
***********************************************************************         
FSTTIME  NTR1  BASE=*,LABEL=*                                                   
         MVI   FRST,C'Y'                                                        
*                                                                               
         L     R9,ADSSB            DISABLE MULTI-TASKING WAITS                  
         USING SSBD,R9                                                          
         MVI   SSBMTIND,0                                                       
         MVC   SSBTPOPC,=F'1'      SET TO INITIAL                               
*                                                                               
         CLI   RSTRDONE,C'Y'                                                    
         BE    *+8                                                              
         BRAS  RE,RSTRPRNS         RESTART REMOTE PRINTERS                      
*                                                                               
         XC    LOGREC,LOGREC       SET TIMER 2 LOG DATA                         
         MVC   LOGID,MYLOGID                                                    
         MVI   LOGID+3,C'S'        $PQS FOR START OF FACPAK                     
         MVC   LOGLUID,SPACES                                                   
         MVC   LOGINFO,=C'STRT1'                                                
         MVC   LOGINFO+4(1),SSBRCNT                                             
         OI    LOGINFO+4,X'F0'                                                  
         MVC   LOGTIME,TIMEHMS                                                  
         MVC   LOGNAME,SYSNAME                                                  
         MVC   LOGSYSID,SSBSYSIX                                                
         MVC   LOGDATE,SYSDATE                                                  
         TIME                                                                   
         ST    R1,LOGJLND                                                       
         GOTO1 ALOGGER,LOGREC                                                   
*                                                                               
         MVI   SSBMTIND,C'M'       ENABLE MULTI-TASKING WAITS                   
         CLI   TAGYB,0             TEST IF ANY OTHER TIMER EXPIRED              
         BNE   EXIT                YES-PROCESS                                  
         B     TIMEXIT             NO                                           
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* CALL LOGGER FOR T2 TIMER POP                                        *         
***********************************************************************         
LOGGER   NTR1  BASE=*,LABEL=*                                                   
         TIME                                                                   
         ST    R1,LOGJLND                                                       
         MVC   LOGID,MYLOGID                                                    
         MVI   LOGID+3,C'T'        $PQT FOR BASIC TIMER POP                     
         MVC   LOGLUID,SPACES                                                   
         MVC   LOGTIME,TIMEHMS                                                  
         MVC   LOGNAME,SYSNAM4                                                  
         MVC   LOGSYSID,SYSID                                                   
         MVC   LOGDATE,SYSDATE                                                  
         GOTO1 ALOGGER,LOGREC                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CALL LOGGER TO LOG SYSTEM CPU                                       *         
***********************************************************************         
LOGCPU   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,SYSWAIT          DON'T WRITE OVERHEAD IF TASKS BUSY           
         JNE   EXITOK                                                           
*                                                                               
         TIME  TU                                                               
         ST    R0,FULL                                                          
*                                                                               
         L     R9,ADSSB                                                         
         USING SSBD,R9                                                          
         ICM   R3,15,SSBTKADR                                                   
         MVC   BYTE,TCBTASK-TCBD(R3)                                            
*                                                                               
         LG    GR1,SSBCPUSE        GR1 = CPU TOTAL AT END OF PREV TASK          
         LG    GRF,SSBCPUTK        GRF = CPU TASKS SO FAR                       
*NOP     AG    GRF,DUB             GRF = GRF + ACCUMULATED                      
         SGR   GR1,GRF             GR1 = CPU OVERHEAD                           
         LG    GRF,SSBCPUDF        GRF = PREVIOUS CPU OVERHEAD                  
         STG   GR1,SSBCPUDF        SAVE GR1 AS NEW PREVIOUS OVERHEAD            
         SGR   GR1,GRF             PRV-CPU TO GET CURRENT SYS USED              
         JNM   *+12                IF NEGATIVE                                  
         XR    R1,R1               REPORT NOTHING                               
         STG   GRF,SSBCPUDF        AND SAVE GRF AS OVERHEAD AGAIN               
*                                                                               
         LA    RF,LOGREC                                                        
         USING ADRRECD,RF                                                       
         XC    ADRREC,ADRREC                                                    
         MVI   ADRSYSNO,1                                                       
         MVI   ADROVSYS,1                                                       
         MVI   ADRPRGNO,4                                                       
         MVC   ADRTASK,BYTE                                                     
         MVC   ADRDAYNO,SSBDAYNO                                                
         MVC   ADRSYSIX,SSBSYSIX                                                
         MVC   ADRINTM,FULL                                                     
         MVC   ADRSTTM,FULL                                                     
         MVC   ADRNDTM,FULL                                                     
         MVC   ADRSYM,=C'OVERHEAD'                                              
         ST    R1,ADRCPUTK                                                      
         GOTO1 ALOGGER,LOGREC                                                   
         B     EXITOK                                                           
         DROP  R9,RF                                                            
         EJECT                                                                  
***********************************************************************         
* LOOP UNTIL ALL NON SERVICE TASKS COMPLETED                          *         
***********************************************************************         
SYSWAIT  NTR1                                                                   
         L     R3,=F'100'          LOOP COUNT                                   
*                                                                               
SWAIT1   L     R6,ATCB                                                          
         LH    R4,0(R6)                                                         
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         USING TCBD,R6                                                          
*                                                                               
SWAIT2   CLI   TCBSEN,1            SERVICE IS OK                                
         JE    SWAIT7                                                           
         CLI   TCBSEN,0            ELSE MUST BE INACTIVE                        
         JNE   SWAIT8                                                           
*                                                                               
SWAIT7   JXLE  R6,R4,SWAIT2                                                     
         J     SWAIT9                                                           
*                                                                               
* WAIT FOR 1/10 SEC. AFTER 10 SECONDS GIVE UP                                   
*                                                                               
SWAIT8   GOTO1 ATICTOC,DMCB,C'WAIT',F'3840'                                     
*                                                                               
         JCT   R3,SWAIT1                                                        
         J     EXITH                                                            
*                                                                               
SWAIT9   J     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* USEFUL ROUTINES                                                     *         
***********************************************************************         
COMMON   DS    0D                                                               
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
TIMEXIT  L     RE,ADSSB                                                         
         USING SSBD,RE                                                          
         NI    SSBSTAT3,255-SSBSRTIM                                            
         L     RD,SAVERD                                                        
         B     EXIT                                                             
         DROP  RE                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
CPUREP   NTR1                      ROUTINE NUM IS IN R0                         
         L     R9,ADSSB                                                         
         USING SSBD,R9                                                          
         CLI   SSBCPU,C'Y'                                                      
         BNE   EXIT                                                             
         ICM   R3,15,SSBTKADR                                                   
         USING TCBD,R3                                                          
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REPRTN,DUB                                                       
         GOTO1 AHEXOUT,DMCB,TCBCPUTM,REPCPU,4,0                                 
         GOTO1 AHEXOUT,DMCB,TCBIOCNT,REPPIO,3,0                                 
         LA    R2,REPMSGH                                                       
         WTO   TEXT=(R2)                                                        
         B     EXIT                                                             
         DROP  R3,R9                                                            
*                                                                               
REPMSGH  DC    AL2(80)                                                          
REPMSG   DC    80C' '                                                           
         ORG   REPMSG                                                           
         DC    C'RTN='                                                          
REPRTN   DC    CL4'0000'                                                        
         DC    C', CPU='                                                        
REPCPU   DC    CL8'00000000'                                                    
         DC    C', I/O='                                                        
REPPIO   DC    CL6'00000000'                                                    
         ORG                                                                    
* DDSHFIR                                                                       
       ++INCLUDE DDSHFIR                                                        
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
ARZERO   DC    16F'0'                                                           
*                                                                               
EFFS     DC    16X'FF'                                                          
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
DTFAD    DC    CL8'DTFAD   '                                                    
DMUNLK   DC    CL8'DMUNLK  '                                                    
BUFFER   DC    CL8'BUFFER  '                                                    
SHMUSS   DC    CL8'SHMUSS  '                                                    
ATTACH   DC    CL8'ATTACH  '                                                    
GLIST    DC    CL8'GLIST   '                                                    
GFILE    DC    CL8'GFILE   '                                                    
PRTQUE   DC    CL8'PRTQU   '                                                    
GENDIR   DC    CL8'GENDIR  '                                                    
GENFIL   DC    CL8'GENFIL  '                                                    
TEMPSTR  DC    CL8'TEMPSTR '                                                    
FACWRK   DC    CL8'FACWRK  '                                                    
MEMORY   DC    CL8'PRTQ    '                                                    
WRKF1    DC    CL8'WRKF1   '                                                    
*                                                                               
TIMEADJ  DS    0F                                                               
*&&UK*&& DC    A(0)                                                             
*&&US*&& DC    A(6*60*60*100)      DDSTIME=YES                                  
*                                                                               
MYLOGID  DC    CL3'$PQ '                                                        
SRTXT    DC    X'0144'                                                          
DEFCTRY  DS    0X                                                               
*&&UK*&& DC    AL1(1)                                                           
*&&US*&& DC    AL1(2)                                                           
*                                                                               
TWAR     EQU   X'80'               S/R SAVE PAGE READ                           
TWAU     EQU   X'40'               S/R SAVE PAGE UPDATED                        
NOTP     EQU   X'20'               NOTIFY PENDING FOR TERMINAL                  
EXTP     EQU   X'10'               EXTRACT UPDATE PENDING FOR TERMINAL          
ALLD     EQU   X'01'               ALL JOBS COMPLETE FOR TERMINAL               
*                                                                               
SPACES   DC    CL64' '                                                          
         EJECT                                                                  
MSGFW1   DS    0C                                                               
*&&US*&& DC    C'AUTONOTE*AHYDN:'                                               
***US*&& DC    C'AUTONOTE*US-MF_FAC_TEAM_NY:'                                   
*&&UK*&& DC    C'AUTONOTE*US-MF_FAC_TEAM_UK:'                                   
         DC    C'SRTIM ERR#='                                                   
MSGERRCC DC    C'  '               CC SET FROM CALL                             
         DC    C' '                                                             
         DC    C'WK='                                                           
MSGFWCID DC    C'NNNNN'                                                         
         DC    C','                                                             
MSGFWSUB DC    C'XXX'                                                           
         DC    C','                                                             
MSGFWRP# DC    C'NNNNN'                                                         
         DC    C' '                                                             
         DC    C'PQ='                                                           
MSGFPCID DC    C'NNNNN'                                                         
         DC    C','                                                             
MSGFPSUB DC    C'XXX'                                                           
         DC    C','                                                             
MSGFPRP# DC    C'NNNNN'                                                         
MSGFWLNQ EQU   *-MSGFW1                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TEMP W/S                                             *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
*                                                                               
IPARMS   DS    0XL32                                                            
ASYSFAC  DS    A                   A(SYSFACS)                                   
ATIA     DS    A                   A(TIA)                                       
AMYUTL   DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMFACS)                                   
ASELIST  DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TIOB)                                      
*                                                                               
ABINSRCH DS    A                                                                
AWKSCAN  DS    A                                                                
*                                                                               
FULL     DS    F                                                                
FUL1     DS    F                                                                
DMCB     DS    6F                                                               
PQDMCB   DS    6F                                                               
PLIST    DS    8F                                                               
RELO     DS    A                                                                
APRTLST  DS    A                                                                
APRTLSTX DS    A                                                                
SVREGS   DS    2A                                                               
SAVER1   DS    A                                                                
SAVERD   DS    A                                                                
ASRTXT   DS    A                                                                
JNUM     DS    F                   N'ENTRIES IN JTAB                            
*PJOB#   DS    F                   N'ENTRIES IN UPJOBS                          
AJTAB    DS    A                   A(JTAB)                                      
*UPJOBS  DS    A                   A(UPDATIVE JOBS)                             
AJCLASS  DS    A                   A(JOB CLASS TABLE)                           
ACIREC   DS    A                                                                
AIO      DS    A                                                                
AIO2     DS    A                                                                
JOBHDRL  DS    H                                                                
JOBTABL  DS    H                                                                
*                                                                               
ADMGR    DS    A                                                                
ADMOD000 DS    A                                                                
AWCTYPE  DS    A                                                                
ATICTOC  DS    A                                                                
ACALLOV  DS    A                                                                
AGENIDS  DS    A                                                                
ALOCKSPC DS    A                                                                
APRQENTS DS    A                                                                
ALOGGER  DS    A                                                                
ADSSB    DS    A                                                                
ALCM     DS    A                                                                
AUTL     DS    A                                                                
ATCB     DS    A                                                                
*                                                                               
AXSORT   DS    A                                                                
ASWITCH  DS    A                                                                
AISGENQ  DS    A                                                                
AENQDEQ  DS    A                                                                
AHEXOUT  DS    A                                                                
AMQIO    DS    A                                                                
ADATCON  DS    A                                                                
AGETDAY  DS    A                                                                
ADEJAVU  DS    A                                                                
ATEMPTRC DS    A                                                                
*                                                                               
TIMEBIN  DS    F                                                                
TIMEHMSL DS    F                                                                
TIMEHMS  DS    F                                                                
MTCHNUM  DS    F                                                                
PRTNUM   DS    F                                                                
OUTCNT   DS    H                                                                
NOTIFY   DS    X                                                                
FLAG     DS    X                                                                
FRST     DS    X                                                                
MODE     DS    X                                                                
RSTRDONE DS    X                                                                
SYSID    DS    X                                                                
SYSIDHOB DS    X                                                                
BYTE     DS    X                                                                
         DS    XL2                                                              
SYSNAME  DS    CL4                                                              
SYSNAM4  DS    CL4                                                              
SYSDATE  DS    CL8                                                              
RECLEN   DS    H                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
*                                                                               
ABCTAB   DS    A                                                                
ABCTABX  DS    A                                                                
ABCNXT   DS    A                                                                
NEWBC    DS    XL1                                                              
TIMEB    DS    XL3                                                              
YEARB    DS    XL1                                                              
MONTHB   DS    XL1                                                              
         DS    XL2                                                              
DAY      DS    X                   DEJAVU TYPE DAY VALUE (TODAY)                
         DS    X                   SPARE (TO FORCE ALIGNMENT)                   
DATEC    DS    XL2                                                              
YEARC    DS    XL2                                                              
YEARCM1  DS    XL2                                                              
YEARCP1  DS    XL2                                                              
STRDC    DS    XL2                                                              
ENDDC    DS    XL2                                                              
TIMEC    DS    H                                                                
*                                                                               
DATEN    DS    XL2                                                              
DATEO    DS    XL2                                                              
*                                                                               
DMALET   DS    F                                                                
TBALET   DS    F                                                                
AMSGBUFF DS    A                                                                
REGSAVE  DS    4F                                                               
*                                                                               
DMWORK   DS    10F                                                              
DSKADR   DS    F                                                                
KEY      DS    CL32                                                             
DIR      DS    CL40                I/O AREA FOR GENDIR READS                    
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
*                                                                               
LOGREC   DS    0CL80               TIMER POP AND START PRINTER LOG REC          
LOGHDR   DS    0CL40                                                            
LOGID    DS    CL4                 $PQS/$PQT/$PQU                               
LOGLUID  DS    CL8                                                              
LOGTIME  DS    PL4                                                              
LOGNUM   DS    CL4                                                              
         DS    CL4                                                              
LOGREPU  DS    CL6                                                              
LOGREPI  DS    CL3                                                              
LOGRENO  DS    CL5                                                              
         DS    CL2                                                              
*                                                                               
LOGMISC  DS    0CL24                                                            
LOGFLAGS DS    XL1                                                              
LOGSYSID DS    XL1                                                              
LOGNAME  DS    CL4                                                              
LOGDATE  DS    CL8                                                              
LOGINFO  DS    CL5                                                              
         DS    CL1                                                              
LOGJLND  DS    PL4                                                              
*                                                                               
LOGTRACE DS    CL16                                                             
LOGRECX  EQU   *                                                                
*                                                                               
APRTQLST DS    A                                                                
PRTQID   DS    CL8                                                              
NDX      DS    CL40                                                             
* DDSHFIW                                                                       
       ++INCLUDE DDSHFIW                                                        
* DMPRTQW                                                                       
       ++INCLUDE DMPRTQW                                                        
DSPHD    DS    XL64                                                             
*                                                                               
CXREC    DS    14336C              PRTQ BUFFERS                                 
CIREC    DS    14336C                                                           
         ORG   CXREC                                                            
FWDMCB   DS    0XL24               FACWRK DMCB                                  
FWAACTN  DS    A                                                                
FWAFILE  DS    A                                                                
FWANDX   DS    A                                                                
FWAREC   DS    A                                                                
FWABUF   DS    A                                                                
         DS    A                                                                
*                                                                               
FWNDX    DS    XL32                FACWRK INDEX                                 
FWREC    DS    2076C               FACWRK RECORD                                
FWBUF    DS    (BLKSZQ)C           FACWRK BUFFER                                
FWBUF2   DS    (BLKSZQ)C           FACWRK BUFFER 2                              
BLKSZQ   EQU   6140+256            256 IS FOR SAVED BUFFER AREA                 
         ORG                                                                    
PRTLST   DS    2000F                                                            
PRTLSTX  DS    F                                                                
*                                                                               
JTAB     DS    (JMAX+1)XL(JTABL)                                                
JTABLQ   EQU   *-JTAB                                                           
*&&DO                                                                           
UPJOBS   DS    (JMAX)XL(UPJOBD)                                                 
UPJOBSQ  EQU   *-UPJOBS                                                         
*&&                                                                             
IO       DS    2048C               I/O AREA FOR CTFILE/GENFILE READS            
IO2      DS    2048C               I/O AREA FOR CTFILE/GENFILE READS            
*                                                                               
WORKL    EQU   *-WORKD             END OF TOTAL W/S                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER JOB NOTIFY TABLE                                     *         
***********************************************************************         
JTABD    DSECT                                                                  
JSTAT    DS    XL1                 JOB STATUS                                   
JTERM    DS    XL2                 TERMINAL NUMBER                              
JSOUT    EQU   X'80'               JOB COMPLETE                                 
JSINV    EQU   X'40'               JOB COMPLETE BUT INVALID (ABEND)             
JSUPD    EQU   X'20'               JOB COMPLETE EXTRACT UPDATE PENDING          
JSNPQ    EQU   X'10'               JOB NOT IN PRTQUE ANY MORE                   
JPQKEY   DS    0XL7                PRTQUE KEY                                   
JPQUSR   DS    XL2                 USER-ID                                      
JPQSUB   DS    CL3                 REPORT SUB-ID                                
JPQSEQ   DS    XL2                 REPORT SEQUENCE NUMBER                       
JPQCIA   DS    XL2                 REPORT C/I ADDRESS                           
JPQID    DS    CL1                 PRTQUE FILE ID                               
JJESNO   DS    XL2                 JES JOB NUMBER                               
JJOBADDR DS    XL4                                                              
JTABL    EQU   *-JTABD             L'TABLE ENTRY                                
JMAX     EQU   1250                MAXIMUM N'ENTRIES IN TABLE                   
*                                                                               
QTABD    DSECT                                                                  
QCLASS   DS    XL2                 CLASS                                        
QUSER    DS    XL2                 USER ID                                      
QTIME    DS    XL3                 SUBMIT TIME                                  
QSPARE   DS    XL1                 SPARE                                        
QOFFS    DS    XL4                 OFFSET                                       
QTABL    EQU   *-QTABD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER UPDATIVE TYPE JOB INFORMATION                        *         
***********************************************************************         
*&&DO                                                                           
UPJOBD   DSECT                                                                  
UPJOBUSR DS    XL2                 USER ID (COMPANY ID)                         
UPJOBRPT DS    XL2                 THIS SHOULD BE USE INSTEAD OF CIT#           
UPJOBQ   EQU   *-UPJOBD                                                         
*&&                                                                             
***********************************************************************         
* DSECT TO COVER FACWRK RECORD                                        *         
***********************************************************************         
FWRECD   DSECT                                                                  
FWRLEN   DS    XL2                 RECORD LENGTH  (INCLUDES HDR DATA)           
         DS    XL2                                                              
* DMRCVRHDR                        RECORD RECOVERY HEADER                       
       ++INCLUDE DMRCVRHDR                                                      
FWRDATA  DS    X                   RECORD DATA                                  
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* FABCTAB                                                                       
       ++INCLUDE FABCTAB                                                        
         EJECT                                                                  
* CTGENBRD                                                                      
       ++INCLUDE CTGENBRD                                                       
         EJECT                                                                  
SRTXTFFD DSECT                                                                  
         DS    CL64                                                             
* SRTXTFFD                                                                      
       ++INCLUDE SRTXTFFD                                                       
         EJECT                                                                  
* DMDSHDR                                                                       
       ++INCLUDE DMDSHDR                                                        
* DDDICTBLDD                                                                    
       ++INCLUDE DDDICTBLDD                                                     
* FAPRQ                                                                         
       ++INCLUDE FAPRQ                                                          
* DMPRTQD                                                                       
       ++INCLUDE DMPRTQD                                                        
* DDSHFID                                                                       
       ++INCLUDE DDSHFID                                                        
* DMPRTQK                                                                       
*PREFIX=PQ                                                                      
       ++INCLUDE DMPRTQK                                                        
*PREFIX=                                                                        
* DMWRKRK                                                                       
       ++INCLUDE DMWRKRK                                                        
* DMWRKRD                                                                       
       ++INCLUDE DMWRKRD                                                        
* DDWKSCAND                                                                     
       ++INCLUDE DDWKSCAND                                                      
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* FACIDTABD                                                                     
       ++INCLUDE FACIDTABD                                                      
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
* FATABSDAR                                                                     
       ++INCLUDE FATABSDAR                                                      
* DDASSISTD                                                                     
       ++INCLUDE DDASSISTD                                                      
* DMSPACED                                                                      
       ++INCLUDE DMSPACED                                                       
* DDBSPARA                                                                      
       ++INCLUDE DDBSPARA                                                       
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
* FAMSGBUFFD                                                                    
       ++INCLUDE FAMSGBUFFD                                                     
* FATABSJOB                                                                     
       ++INCLUDE FATABSJOB                                                      
* DDTEMPREC                                                                     
       ++INCLUDE DDTEMPREC                                                      
*                                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SRTIM00   01/15/20'                                      
         END                                                                    
