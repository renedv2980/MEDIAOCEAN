*          DATA SET ACREP6602  AT LEVEL 114 AS OF 07/23/13                      
*PHASE AC6602C                                                                  
*INCLUDE ACCDIV                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'CLIENT BUDGET REPORT'                                           
*                                                                               
*              CONTROL PROFILES/REQUEST OPTIONS                                 
*                                                                               
*              HISTORY                                                          
*                                                                               
*        6/93  OPTION 1=3, MEDIA/12 BYT JOB SORT                                
*              OPTION 7=A, A64 LIKE COLS                                        
*                                                                               
*        8/93  OPTION 9=Y, SHOW HIGHTEST REVISION                               
*              OPTION 10=Y SUPPRESS BALANCE FOR CLOSED JOBS                     
*                                                                               
*        5/94  CLEAR XPTHIRD AND FOURTH IN PUTDOWN AS THE DOWNLOAD              
*              CAN'T HANDLE STACKED NAMES                                       
*                                                                               
*        1/98  YEAR 2000 FIXES                                                  
*                                                                               
         EJECT ,                                                                
AC6602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC66**,R9                                                    
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA                                                       
*                                                                               
         LA    RC,SPACEND                                                       
*                                                                               
         USING AC66D,RC                                                         
*                                                                               
         L     R8,VBIGPRNT                                                      
*                                                                               
         USING BIGPRNTD,R8                                                      
         EJECT ,                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   CB10                                                             
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         USING BOXD,R2                                                          
         LA    R3,PAGEWDTH                                                      
         ST    R3,BOXWIDTH                                                      
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)                                                         
*                                                                               
         L     R3,=A(HDHOOK)                                                    
         ST    R3,HEADHOOK                                                      
         B     XIT                                                              
         EJECT ,                                                                
CB10     CLI   MODE,REQFRST                                                     
         BNE   CB20                                                             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         MVI   RCSUBPRG,0                                                       
         BAS   RE,INIT                                                          
*                                                                               
         XC    LEVSTAT,LEVSTAT                                                  
         XC    ACTIVITY,ACTIVITY                                                
         MVC   LEVEL,DETLEVEL                                                   
         BAS   RE,CLEAR                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
CB20     CLI   MODE,LEVAFRST                                                    
         BNE   CB30                                                             
         NI    LEVSTAT,X'FF'-GOTCLI                                             
         NI    ACTIVITY,X'FF'-ACTCLI                                            
         B     XIT                                                              
*                                                                               
CB30     CLI   MODE,LEVBFRST                                                    
         BNE   CB40                                                             
         NI    LEVSTAT,X'FF'-GOTPRO                                             
         NI    ACTIVITY,X'FF'-ACTPRO                                            
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PROCACC                                                                
*----------------------------------------------------------------------         
*                                                                               
CB40     CLI   MODE,PROCACC                                                     
         BNE   CB60                                                             
         NI    LEVSTAT,X'FF'-GOTJOB                                             
         NI    ACTIVITY,X'FF'-ACTJOB                                            
         L     R4,ADACC                                                         
         MVC   THISACC,3(R4)                                                    
*                                                                               
         BAS   RE,CHKMEDIA         CHECK STILL PROCESSING SAME MEDIA            
         BE    CB50D                                                            
         NI    LEVSTAT,X'FF'-GOTMED                                             
         NI    ACTIVITY,X'FF'-ACTMED                                            
*                                                                               
CB50D    MVI   FCRDTRNS,C'N'                                                    
         MVC   LASTACC,THISACC                                                  
         XC    INVSEQ,INVSEQ                                                    
         XC    INVFILL,INVFILL                                                  
         BAS   RE,FLTJOBS                                                       
         BNE   XIT                                                              
         MVI   FCRDTRNS,C'Y'                                                    
         BAS   RE,GETEST           EXTRACT ESTIMATE VALUES FROM JOBBER          
         ZAP   JOBBILLS,=P'0'      TOTAL BILLING ON JOB                         
         ZAP   JOBCBIL,=P'0'                                                    
         ZAP   JOBBTD,=P'0'                                                     
         B     XIT                                                              
         EJECT ,                                                                
CB60     CLI   MODE,PROCTRNS                                                    
         BNE   CB70                                                             
         MVI   LEVEL,LEVTRN                                                     
         BAS   RE,CLEAR                                                         
         BAS   RE,FLTTRNS          FILTER TRANSACTION                           
         BNE   XIT                 TRAN NOT WANTED                              
         OI    ACTIVITY,ACTJOB                                                  
*                                                                               
*        PLACE BILLING BY BILLING MONTH                                         
*                                                                               
         USING TRNELD,R2                                                        
*                                                                               
         BAS   RE,MONBUCK          GET BUCKET NUMBER I WANT                     
*                                                                               
         L     R2,ADTRANS                                                       
         LA    R3,TRNAMNT                                                       
         CLI   QOPT5,C'G'                                                       
         BNE   *+8                                                              
         LA    R3,TRNNARR+27                                                    
         ZAP   BILLAMNT,0(6,R3)                                                 
*                                                                               
         AP    JOBBILLS,BILLAMNT   SAVE BILLING FOR JOB                         
         SR    R3,R2                                                            
         STH   R3,PTTROFF                                                       
         MVI   COLTYPE,BLMO                                                     
         BAS   RE,CHKCOL                                                        
         BNE   *+8                                                              
         BAS   RE,PUTTRN           PUT TO DETLEVEL ACCUMS                       
*                                                                               
         MVC   PTBUCK,BILLBUCK     PUT TOTAL BILLING                            
         MVI   COLTYPE,BIL                                                      
         BAS   RE,CHKCOL                                                        
         BNE   *+8                                                              
         BAS   RE,PUTTRN                                                        
*                                                                               
         MVI   COLTYPE,CBIL                                                     
         BAS   RE,CHKCOL                                                        
         BNE   CB60J                                                            
         MVC   PTBUCK,CBILBUCK                                                  
         L     R2,ADTRANS                                                       
         CLC   CURMON,TRNDATE                                                   
         BNE   CB60J                                                            
         BAS   RE,PUTTRN                                                        
         AP    JOBCBIL,BILLAMNT                                                 
*                                                                               
CB60J    MVI   COLTYPE,BTD                                                      
         BAS   RE,CHKCOL                                                        
         BNE   CB60M                                                            
         MVC   PTBUCK,BTDBUCK                                                   
         L     R2,ADTRANS                                                       
         CLC   CURMON,TRNDATE                                                   
         BE    CB60M                                                            
         BAS   RE,PUTTRN                                                        
         AP    JOBBTD,BILLAMNT                                                  
*                                                                               
CB60M    MVI   COLTYPE,INO                                                      
         BAS   RE,CHKCOL                                                        
         BNE   XIT                                                              
*                                                                               
         MVC   PCOFF,INODATA      PUT INVOICE NUMBER                            
         L     R2,ADTRANS                                                       
         LA    R3,TRNREF                                                        
         SR    R3,R2                                                            
         STC   R3,PCTROFF                                                       
         LA    R1,L'TRNREF                                                      
         MVI   COLTYPE,INO                                                      
         BAS   RE,PUTCHR                                                        
*                                                                               
         ZIC   R1,INVSEQ                                                        
         LA    R1,1(R1)                                                         
         STC   R1,INVSEQ                                                        
         MVI   INVFILL,X'FF'                                                    
*                                                                               
         MVI   LEVEL,LEVINO                                                     
         BAS   RE,PUTSORT                                                       
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        LAST OF LEVELS, PUT SORT RECORDS FOR DETAIL LEVEL DATA                 
*----------------------------------------------------------------------         
*                                                                               
CB70     CLI   MODE,ACCLAST                                                     
         BNE   CB80                                                             
*                                                                               
         TM    ACTIVITY,ACTJOB     IS THIS ACCOUNT ACTIVE                       
         BNO   *+8                                                              
         OI    ACTIVITY,ACTMED+ACTPRO+ACTCLI+ACTREQ                             
*                                                                               
         CLI   DETLEVEL,LEVINO     SPECIAL FOR INVOICE DETAIL                   
         BNE   CB70A                                                            
         MVI   LEVEL,LEVINO                                                     
         BAS   RE,CLEAR            CLEAR DETAIL ACCUMS                          
         BAS   RE,PUTACC           PUT EST/                                     
         BAS   RE,GETACCUM                                                      
         LA    R3,1(R3)                                                         
         MVI   COLTYPE,BIL                                                      
*                                                                               
         BAS   RE,CHKCOL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    R1,R1                                                            
         IC    R1,BILLBUCK                                                      
         MH    R1,=Y(BUCKLN)                                                    
         LA    R4,0(R1,R3)                                                      
         ZAP   0(BUCKLN,R4),JOBBILLS                                            
*                                                                               
         IC    R1,CBILBUCK                                                      
         MH    R1,=Y(BUCKLN)                                                    
         LA    R4,0(R1,R3)                                                      
         ZAP   0(BUCKLN,R4),JOBCBIL                                             
*                                                                               
         IC    R1,BTDBUCK                                                       
         MH    R1,=Y(BUCKLN)                                                    
         LA    R4,0(R1,R3)                                                      
         ZAP   0(BUCKLN,R4),JOBBTD                                              
*                                                                               
         XC    INVSEQ,INVSEQ                                                    
         MVI   INVFILL,X'FF'       MAKE SURE ITS AFTER THE JOB REC              
*                                                                               
         MVC   PCOFF,INODATA      PUT INVOICE NUMBER AS **                      
         L     R2,ADTRANS                                                       
*                                                                               
         USING TRNELD,R2                                                        
*                                                                               
         MVC   TRNREF,=CL6'  **  '                                              
         LA    R3,TRNREF                                                        
         SR    R3,R2                                                            
         STC   R3,PCTROFF                                                       
         LA    R1,L'TRNREF                                                      
         MVI   COLTYPE,INO                                                      
         BAS   RE,PUTCHR                                                        
*                                                                               
         BAS   RE,PUTSORT                                                       
         BAS   RE,PUTHIGH                                                       
         B     XIT                                                              
*                                                                               
CB70A    BAS   RE,PUTACC           SET ACCOUNT LEVEL ACCUMS                     
*                                                                               
         CLI   DETLEVEL,LEVJOB     JOB LEVEL REPORT                             
         BE    CB72                                                             
         CLI   DETLEVEL,LEVJOB12   JOB LEVEL REPORT                             
         BNE   CB75                                                             
*                                                                               
         BAS   RE,PUTHIGH          PUT HIGHER LEVEL SORT RECORDS                
         MVI   LEVEL,LEVJOB12                                                   
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
*                                                                               
CB72     BAS   RE,PUTHIGH          PUT HIGHER LEVEL SORT RECORDS                
         MVI   LEVEL,LEVJOB                                                     
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
*                                                                               
CB75     CLI   DETLEVEL,LEVMED     MEDIA LEVEL REPORT                           
         BNE   CB80                NO                                           
*                                                                               
         BAS   RE,PUTHIGH          PUT HIGHER LEVEL SORT RECORDS                
*                                                                               
         MVI   LEVEL,LEVMED        PUR RECORDS AT PROCACC                       
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
*                                                                               
CB80     CLI   MODE,LEVBLAST                                                    
         BNE   CB90                                                             
         CLI   DETLEVEL,LEVPRO                                                  
         BNE   XIT                                                              
*                                                                               
         BAS   RE,PUTHIGH          PUT HIGHER LEVEL SORT RECORDS                
*                                                                               
         MVI   LEVEL,LEVPRO                                                     
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
*                                                                               
CB90     CLI   MODE,LEVALAST                                                    
         BNE   CB100                                                            
         CLI   DETLEVEL,LEVCLI                                                  
         BNE   XIT                                                              
*                                                                               
         BAS   RE,PUTHIGH          PUT HIGHER LEVEL SORT RECORDS                
*                                                                               
         MVI   LEVEL,LEVCLI                                                     
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        REQ LAST, PROCESS SORTED RECORDS                                       
*----------------------------------------------------------------------         
*                                                                               
CB100    CLI   MODE,REQLAST                                                     
         BNE   CB200                                                            
         TM    ACTIVITY,ACTREQ     ANYTHING HAPPEN                              
         BNO   CB150               NO                                           
*                                                                               
         CLI   DETLEVEL,LEVREQ                                                  
         BNE   CB105                                                            
*                                                                               
         MVI   LEVEL,LEVREQ                                                     
         BAS   RE,PUTSORT                                                       
*                                                                               
CB105    XC    PREVSRT,PREVSRT                                                  
         XC    PREVDKEY,PREVDKEY                                                
         LA    R1,NACCUMS                                                       
         LA    R3,PREVBUCK                                                      
         BAS   RE,ZAPEM                                                         
         XC    HOLDSRT(SRTKEYLN),HOLDSRT                                        
         LA    R1,NACCUMS                                                       
         LA    R3,HOLDBUCK                                                      
         BAS   RE,ZAPEM                                                         
         XC    SORTFLAG,SORTFLAG                                                
*                                                                               
CB110    BAS   RE,GETSORT          GET A RECORD FROM SORTER                     
         BNE   CB140                                                            
*                                                                               
         BAS   RE,LAST             PRINT TOTALS AS NEEDED                       
*                                                                               
         CLC   SRTLEVEL,DETLEVEL   IS THIS A DETAIL SORT RECORD                 
         BE    CB120               YES                                          
*                                                                               
         BAS   RE,FIRST            PRINT THE FIRST                              
         B     CB110               GET NEXT                                     
*                                                                               
CB120    BAS   RE,REPORT           PRINT SORT RECORDS                           
*                                                                               
         OC    SRTSEQ,SRTSEQ       IS THIS AN INVOICE DETAIL                    
         BNZ   *+8                 YES, TOTALS INCLUDED                         
         BAS   RE,BUMP             BUMP SORT REC INTO ACCUMS                    
*                                                                               
         BAS   RE,SETNEED          SET PRINT TOTALS FLAGS                       
         B     CB110                                                            
*                                                                               
CB140    MVI   PREVSRT,X'FF'       FORCE TOTALS TO PRINT                        
         BAS   RE,SETRNEED         SET TOTNEED FOR REQUEST LEVEL                
         BAS   RE,LAST                                                          
*                                                                               
CB150    BAS   RE,CLOSSORT                                                      
*                                                                               
         CLI   PRTOPT,C'D'         DOWNLOADED                                   
         BNE   *+8                                                              
         BAS   RE,ENDDOWN                                                       
*                                                                               
         B     XIT                                                              
*                                                                               
CB200    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
INIT     NTR1                                                                   
         GOTO1 =A(SETDATES),DMCB,(RC)                                           
         BAS   RE,OPENSORT                                                      
         XC    REPLEVS,REPLEVS                                                  
         L     R2,=A(REPLVTB)      TABLE LINKING QOPT1 WITH REPORT LEVS         
         LA    R0,NREPLVTB         N'ENTRIES                                    
         MVC   HALF(1),QOPT1                                                    
         MVC   HALF+1(1),QOPT7                                                  
*                                                                               
INIT10   CLC   0(2,R2),HALF        IS OPTION FOUND                              
         BNE   INIT20                                                           
         MVC   DETLEVEL,2(R2)                                                   
         MVC   NLEVELS,3(R2)                                                    
         MVC   REPLEVS,4(R2)                                                    
         B     INIT21                                                           
INIT20   LA    R2,LREPLVTB(R2)                                                  
         BCT   R0,INIT10                                                        
*                                                                               
         MVC   XP+1(11),=C'BAD REQUEST'                                         
         BAS   RE,PRINTEM                                                       
*                                                                               
INIT21   MVI   MEDPOS,9            CULCCCPPPM                                   
*                                                                               
         MVI   FRSTCOL,35                                                       
         CLI   DETLEVEL,LEVINO                                                  
         BNE   *+8                                                              
         MVI   FRSTCOL,45                                                       
*                                                                               
         L     R1,=A(HDTAB)                                                     
         LA    R0,NHEADS                                                        
         LA    R2,HDDATA                                                        
INIT25   MVC   0(20,R2),0(R1)                                                   
         LA    R1,20(R1)                                                        
         LA    R2,20(R2)                                                        
         BCT   R0,INIT25                                                        
*                                                                               
         CLI   QOPT9,C'Y'          SHOW HIGHEST REVISION OPTION                 
         BNE   INIT27                                                           
*                                                                               
         MVC   HDEST,=CL20' HIGHEST   REVISION'                                 
*                                                                               
INIT27   CLI   QOPT7,C'I'          FORMAT OVERRIDE                              
         BE    INIT60                                                           
*                                                                               
         LA    R2,BUCKMTHS                                                      
         LA    R3,CMONTAB                                                       
         XC    BUCKMTHS,BUCKMTHS                                                
*                                                                               
         MVC   YMD,START3                             PACKED    DATE            
         MVI   YMD+2,X'01'                            SET  DAY  TO   01         
         GOTO1 DATCON,DMCB,(1,YMD),(0,WORK)           SAVE C'YYMM01'            
*                                                                               
INIT30   MVI   YMD+2,X'FF'                            SET  DAY  TO   FF         
         MVC   0(DATELN,R2),YMD                       SAVE MAX  DATE            
*                                                                               
         MVI   YMD+2,X'01'                            SET  DAY  TO   01         
         MVC   0(CMONLN,R3),SPACES                                              
         GOTO1 DATCON,DMCB,(1,YMD),(6,4(R3))          GET  MMM/YY               
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',WORK),(0,WORK+6),F'1' GET  NEXT MONTH           
         MVC   WORK(6),WORK+6                         SAVE NEXT MONTH           
         GOTO1 DATCON,DMCB,(0,WORK),(1,YMD)           PACKED DATE               
*                                                                               
         CLC   YMD,END3                                                         
         BH    INIT60              ALL  DONE                                    
*                                                                               
         LA    R2,DATELN(,R2)      NEXT MONTH                                   
         LA    R3,CMONLN(,R3)      BUMP TABLE OF MMM/YY'S                       
         B     INIT30                                                           
*                                                                               
INIT60   MVI   REPTYPE,STD                                                      
         CLI   QOPT7,C'I'          REPORT FORMAT OVERRIDE                       
         BNE   *+8                 NO                                           
         MVI   REPTYPE,A64                                                      
*                                                                               
         BAS   RE,SETBUCKS         DEFINE BUCKETS                               
         GOTO1 =A(SETCOLS),DMCB,(RC)                                            
         BAS   RE,SETBOXES         SET BOXES BASED ON COLUMNS                   
*                                                                               
         MVI   PRTOPT,C'P'                                                      
*                                                                               
         CLI   QOPT8,C'Y'          DOWNLOAD OPTION                              
         BE    INIT65                                                           
*                                                                               
         USING MASTD,R2                                                         
*                                                                               
         L     R2,ADMASTC                                                       
         LA    R2,MCREMOTE                                                      
*                                                                               
         USING REMOTED,R2                                                       
*                                                                               
         CLC   =C'DOWN',REMOTFNO   DOWNLOADING                                  
         BNE   INIT70              NO                                           
*                                                                               
INIT65   MVI   PRTOPT,C'D'                                                      
         BAS   RE,SETDOWN          SET DLBUFF                                   
*                                                                               
INIT70   MVI   LEVEL,LEVOFF                                                     
         BAS   RE,CLEAR                                                         
         MVI   LEVEL,LEVREQ                                                     
         BAS   RE,CLEAR                                                         
*                                                                               
         BAS   RE,BLDTOT           SET TOTTAB, BASED ON REQUEST                 
         BAS   RE,SETINDNT         SET INDENT AMOUNTS IN TOTTAB                 
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        PUT SORT RECORDS WITH HIGHER LEVEL NAMES                               
*---------------------------------------------------------------------          
*                                                                               
PUTHIGH  NTR1                                                                   
*                                                                               
         LA    R1,LEVTAB                                                        
         LA    R0,NLEVTAB                                                       
PUTH10   MVC   BYTE,0(R1)          HAS THIS LEVEL BEEN PUTSORTED                
         NC    BYTE,LEVSTAT                                                     
         BNZ   PUTH40              YES                                          
*                                                                               
         MVC   LEVEL,1(R1)                                                      
         BAS   RE,CHKLEV           THIS LEVEL ON REPORT                         
         BNE   PUTH40              NO                                           
         CLC   DETLEVEL,LEVEL      THIS DETAIL LEVEL                            
         BE    PUTH40              YES, PUT AFTER ACCUMS ARE BUILT              
         BAS   RE,PUTSORT          PUT NAME.LEVEL SORTREC                       
         BNE   *+10                PUT WAS SUPPRESSED                           
         OC    LEVSTAT,0(R1)       SET LEVSTAT                                  
*                                                                               
PUTH40   LA    R1,LLEVTAB(R1)                                                   
         BCT   R0,PUTH10                                                        
*                                                                               
         B     XIT                                                              
*                                                                               
LEVTAB   DS    0C                                                               
         DC    AL1(GOTCLI,LEVCLI)                                               
LLEVTAB  EQU   *-LEVTAB                                                         
         DC    AL1(GOTPRO,LEVPRO)                                               
         DC    AL1(GOTMED,LEVMED)                                               
         DC    AL1(GOTJOB,LEVJOB)                                               
         DC    AL1(GOTJOB,LEVJOB12)                                             
NLEVTAB  EQU   (*-LEVTAB)/LLEVTAB                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        REFRESH HEADER INFO, ETC                                               
*---------------------------------------------------------------------          
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
FIRST    NTR1                                                                   
         L     R3,=A(TOTTAB)                                                    
         LA    R0,TOTTABNM                                                      
*                                                                               
FIR10    CLC   TOTLEVEL,SRTLEVEL                                                
         BE    FIR50                                                            
         LA    R3,TOTTABLN(R3)                                                  
         BCT   R0,FIR10                                                         
         DC    H'0'                LEVEL NOT FOUND IN TOTTAB                    
*                                                                               
FIR50    CLI   TOTHEAD,C'Y'        IS THIS IN THE HEADER                        
         BNE   FIR70                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     FIR100                                                           
*                                  NO                                           
FIR70    MVC   TOTACC,SRTACC                                                    
*                                                                               
         CLI   LINE,55             WILL THIS FIT                                
         BL    *+8                 YES                                          
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 =A(GETPIECE),DMCB,TOTACC,TOTLEVEL                                
         ZIC   R1,TOTINDNT                                                      
         LA    R2,XP+1(R1)                                                      
         MVC   0(L'PIECE,R2),PIECE                                              
         ZIC   RF,PIECELEN         LENGTH OF PIECE RETURNED FROM GETP           
         LA    R6,1(RF,R2)         ADDRESS TO CHOP NAME INTO                    
*                                                                               
         ZIC   R4,FRSTCOL          MAX WIDTH OF NAME COL -3                     
         SH    R4,=H'3'                                                         
         SR    R4,R1               LESS INDENT AMOUNT                           
         SR    R4,RF               AND L' ACCOUNT                               
*                                                                               
         GOTO1 ADSQUASH,DMCB,SRTNAME,L'SRTNAME                                  
         ZIC   R5,DMCB+7           SAVE LENGTH FROM SQUASHER (P2)               
*                                                                               
         CLI   DETLEVEL,LEVINO     INVOICE LEVEL DETAIL                         
         BNE   FIR75                                                            
         CLI   TOTLEVEL,LEVJOB     IS THIS THE JOB FIRST                        
         BE    FIR72                                                            
         CLI   TOTLEVEL,LEVJOB12                                                
         BNE   FIR75                                                            
*                                                                               
FIR72    BCTR  R4,0                DON'T CHOP JOB NAME WHEN INVDET              
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),SRTNAME                                                  
*                                                                               
         TM    SRTASTAT,RSTSACIC   IS JOB CLOSED                                
         BNO   FIR100              NO                                           
         LA    R6,1(R4,R6)         POINT TO LAST PRINT POSITION                 
         SH    R6,=H'7'            BACKUP 7 TO FIT ,ST=C AFTER NONSPACE         
FIR73    CLI   0(R6),C' '                                                       
         BNE   FIR74                                                            
         MVI   6(R6),C' '          CLEAR EXPOSED BYTE ON RIGHT                  
         BCTR  R6,0                SHIFT PRT POINTER TO THE LEFT                
         B     FIR73                                                            
FIR74    LA    R6,1(R6)            BUMP BACK TO NONSPACE OR LEN-6               
         MVC   0(6,R6),=CL6', ST=C'                                             
         B     FIR100                                                           
*                                                                               
FIR75    GOTO1 CHOPPER,DMCB,((R5),SRTNAME),((R4),(R6)),('LXP',4)                
*                                                                               
         LA    R2,L'XP(R2)         UNDERLINE CODE/NAME                          
         CLI   DMCB+11,2           2+ LINES BACK FROM CHOPPER (P3)              
         BL    *+8                 NO                                           
         LA    R2,L'XP(R2)         BUMP R2 1 MORE TIME                          
*                                                                               
         CR    R5,R4               IS L'FROM SQUASHER MORE THAN AVAIL.          
         BH    *+6                 YES,                                         
         LR    R4,R5               NO, UNDERLINE FOR L'SQUASHED STRING          
         ZIC   R1,PIECELEN                                                      
         LA    R4,1(R1,R4)         BUMP R4 TO INCLUDE L'CODE                    
*                                                                               
         MVI   0(R2),X'BF'         UNDERLINE CODE/LEN                           
         SH    R4,=H'2'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(R2)                                                    
*                                                                               
         BAS   RE,PRINTEM                                                       
*                                                                               
FIR100   MVC   TOTNAME,SRTNAME     FOR LASTS                                    
         MVC   TOTACC,SRTACC                                                    
         MVI   TOTNEED,C'N'                                                     
         MVC   LEVEL,SRTLEVEL                                                   
         BAS   RE,CLEAR                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
LXP      EQU   L'XP                                                             
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        DO TOTALS                                                              
*---------------------------------------------------------------------          
*                                                                               
LAST     NTR1                                                                   
*                                                                               
         MVI   FIRSTTOT,C'Y'                                                    
         MVI   MIDPEND,C'N'                                                     
*                                                                               
         OC    PREVSRT(SRTKEYLN),PREVSRT                                        
         BZ    LASTX                                                            
*                                                                               
         CLI   PREVSRT,X'FF'       IS THIS A FORCED LAST                        
         BE    LAS05               YES                                          
*                                                                               
         CLC   PREVLEV,DETLEVEL    IS PREV A DETAIL RECORD                      
         BNE   XIT                                                              
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
LAS05    L     R3,=A(TOTTAB)                                                    
         XR    R1,R1                                                            
*                                                                               
LAS10    CLI   0(R3),X'FF'         E-O-T                                        
         BE    LASTX                                                            
*                                                                               
         MVC   LEVEL,TOTLEVEL                                                   
         IC    R1,TOTLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRTREC(0),PREVSRT                                                
         BE    LASTX                                                            
*                                                                               
         BAS   RE,CHKLEV           USING THIS LEVEL                             
         BNE   LAS30                                                            
*                                                                               
         CLC   TOTLEVEL,DETLEVEL                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DETLEVEL,LEVINO     INVOICE                                      
         BNE   LAS12                                                            
         CLI   TOTLEVEL,LEVJOB                                                  
         BE    LAS27               JUST BOX                                     
         CLI   TOTLEVEL,LEVJOB12                                                
         BE    LAS27                                                            
*                                                                               
LAS12    CLI   TOTNEED,C'Y'        DO I NEED THIS TOTAL                         
         BNE   LAS30               NO                                           
*                                                                               
*                                                                               
LAS15    CLI   FIRSTTOT,C'Y'       IS THIS THE FIRST TOTAL I'M PRINTING         
         BE    LAS20               YES, NO MID                                  
*                                                                               
         BAS   RE,BOXMID                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
*                                                                               
LAS20    BAS   RE,PRTTOT                                                        
LAS27    MVI   MIDPEND,C'Y'                                                     
         MVI   FIRSTTOT,C'N'                                                    
*                                                                               
LAS30    CLI   LEVEL,LEVREQ        LEVREQ CLEARED AT INIT                       
         BE    *+8                                                              
         BAS   RE,CLEAR                                                         
*                                                                               
         LA    R3,TOTTABLN(R3)                                                  
         B     LAS10                                                            
*                                                                               
LASTX    CLI   MIDPEND,C'Y'        HAVE I PRINTED A TOTAL                       
         BNE   LASTXX              NO                                           
         CLI   PREVSRT,X'FF'       IS THIS A FORCED LAST                        
         BE    LASTXX              YES, END OF REPORT                           
         BAS   RE,BOXMID                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
LASTXX   B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        IF THE TOTAL FOR A LEVEL HAS COMPONENTS, SET WE NEED TO PRINT          
*---------------------------------------------------------------------          
*                                                                               
SETNEED  NTR1                                                                   
*                                                                               
         OC    PREVDKEY,PREVDKEY                                                
         BZ    SNX                                                              
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
         L     R3,=A(TOTTAB)                                                    
         XR    R1,R1                                                            
*                                                                               
SN10     CLI   0(R3),X'FF'         E-O-T                                        
         BE    SNX                                                              
*                                                                               
         MVI   TOTNEED,C'N'                                                     
         CLI   TOTLEVEL,LEVREQ     REQUEST LEVEL                                
         BE    SN50                SET AT END OF SORT                           
*                                                                               
         IC    R1,TOTLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRTREC(0),PREVDKEY                                               
         BNE   *+8                                                              
         MVI   TOTNEED,C'Y'                                                     
*                                                                               
SN50     LA    R3,TOTTABLN(R3)                                                  
         B     SN10                                                             
*                                                                               
SNX      B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET TOTNEED FOR REQUEST WHEN NO MORE SORT RECORDS                      
*---------------------------------------------------------------------          
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
SETRNEED NTR1                                                                   
         L     R3,=A(TOTTAB)                                                    
*                                                                               
SR10     CLI   0(R3),X'FF'         E-O-T                                        
         BE    SRX                                                              
*                                                                               
         CLI   TOTLEVEL,LEVREQ     REQUEST LEVEL                                
         BNE   *+8                 SET AT END OF SORT                           
         MVI   TOTNEED,C'Y'                                                     
*                                                                               
         LA    R3,TOTTABLN(R3)                                                  
         B     SR10                                                             
*                                                                               
SRX      B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        PRINT THE TOTAL FOR THE TOTAL AT 0(R3)                                 
*---------------------------------------------------------------------          
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
PRTTOT   NTR1                                                                   
         MVI   TOTNEED,C'N'        DON'T NEED TOTALS ANYMORE                    
         MVC   LEVEL,TOTLEVEL                                                   
         ZIC   R1,TOTINDNT                                                      
         LA    R2,XP+1(R1)                                                      
         MVC   0(10,R2),=C'TOTALS FOR'                                          
*                                                                               
         MVC   11(L'TOTTITLE,R2),TOTTITLE                                       
         CLI   TOTLEVEL,LEVREQ     TOTAL FOR REQUEST                            
         BE    PRTT40              NO ACCOUNT DATA                              
*                                                                               
         GOTO1 =A(GETPIECE),DMCB,TOTACC,TOTLEVEL  XTRACT ACCOUNT                
         MVC   XP+30(L'PIECE),PIECE                                             
*                                                                               
PRTT40   ZIC   R1,TOTINDNT                                                      
         LA    R2,XP+1(R1)                                                      
         LA    R4,33               MAX LEN                                      
         SR    R4,R1               LESS INDENTATION                             
         GOTO1 ADSQUASH,DMCB,0(R2),(R4)                                         
*                                                                               
         TM    TOTTYPE,TOTPN       PRINT NAME AFTER CODE                        
         BNO   PRTT60              NO, DONE                                     
*                                                                               
         ZIC   RF,DMCB+7           GET LENGTH SQUASHED ABOVE                    
         LA    R2,1(RF,R2)         SET OUTPUT ADDRESS                           
*                                                                               
         ZIC   R4,FRSTCOL          OFFSET TO FRSTCOL                            
         SH    R4,=H'4'            LESS COL'S AND XP+1 START                    
         SR    R4,RF               LESS L'STUFF PRINTED ABOVE                   
*                                                                               
         GOTO1 CHOPPER,DMCB,(L'TOTNAME,TOTNAME),((R4),0(R2)),('LXP',4)          
*                                                                               
PRTT60   BAS   RE,GETACCUM         GET ACCUMES FOR THIS LEVEL                   
         LA    R3,1(R3)            ADDRESS BUCKETTS                             
         BAS   RE,PRTBUCKS         PRINT ACCUMS                                 
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        PRINT THE RECORD IN SRTREC                                             
*---------------------------------------------------------------------          
*                                                                               
REPORT   NTR1                                                                   
*                                                                               
         MVC   LEVEL,DETLEVEL                                                   
         CLI   LEVEL,LEVINO        INVOICE NUMBER DETAIL                        
         BE    REP20               NO NAME/ACCOUNT INFO, PLEASE                 
*                                                                               
         GOTO1 =A(GETPIECE),DMCB,SRTACC,SRTLEVEL                                
*                                                                               
         ZIC   R1,DETINDNT                                                      
         LA    R2,XP+1(R1)                                                      
         MVC   0(L'PIECE,R2),PIECE                                              
         ZIC   RF,PIECELEN         LENGTH OF PIECE RETURNED FROM GETP           
         LA    R6,1(RF,R2)         ADDRESS TO CHOP NAME INTO                    
*                                                                               
         ZIC   R4,FRSTCOL          MAX WIDTH OF NAME COL -3                     
         SH    R4,=H'4'                                                         
         SR    R4,R1               LESS INDENT AMOUNT                           
         SR    R4,RF               AND L' ACCOUNT                               
*                                                                               
         GOTO1 ADSQUASH,DMCB,SRTNAME,L'SRTNAME                                  
         ZIC   R5,DMCB+7              SAVE SQUASSHED LENGTH                     
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTNAME),SRTNAME                                          
*                                                                               
         TM    SRTASTAT,RSTSACIC   IS ACCOUNT CLOSED                            
         BNO   REP05               NO                                           
         LA    R1,WORK                                                          
         LA    R1,0(R5,R1)         ADDRESS END OF NAME                          
         MVC   0(6,R1),=CL6', ST=C' FLAG CLOSED JOB                             
         LA    R5,6(R5)            ADJUST L'SOURCE                              
*                                                                               
REP05    GOTO1 CHOPPER,DMCB,((R5),WORK),((R4),(R6)),('LXP',4)                   
*                                                                               
REP10    LA    R3,SRTBUCKS                                                      
*                                                                               
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,PRTCHAR                                                       
         BAS   RE,PRINTEM                                                       
         BAS   RE,BOXMID                                                        
         BAS   RE,PRINTEM                                                       
         B     REPX                                                             
*                                                                               
REP20    LA    R3,SRTBUCKS                                                      
         BAS   RE,PRTCHAR          PRINT AL THE CHARACTER DATA                  
         CLI   PRTOPT,C'D'         DOWNLOADING?                                 
         BNE   REP25               NO,                                          
         CLI   SRTSEQ,0            IS THIS THE INVOICE HEADER                   
         BNE   REP22               NO                                           
*                                                                               
         MVI   COLTYPE,CBIL        ZAP CURRENT BILL AND BTD, BECAUSE            
         BAS   RE,GETCOL           THESE ARE DETAIL COLS                        
         BAS   RE,ZAPCOL                                                        
         MVI   COLTYPE,BTD                                                      
         BAS   RE,GETCOL                                                        
         BAS   RE,ZAPCOL                                                        
         B     REP23                                                            
*                                                                               
REP22    MVI   COLTYPE,ESTV        FOR INVOICE DETAIL, ZAP NON DETAIL           
         BAS   RE,GETCOL           ACCUMS                                       
         BAS   RE,ZAPCOL                                                        
         MVI   COLTYPE,BIL                                                      
         BAS   RE,GETCOL                                                        
         BAS   RE,ZAPCOL                                                        
         MVI   COLTYPE,BAL                                                      
         BAS   RE,GETCOL                                                        
         BAS   RE,ZAPCOL                                                        
*                                                                               
REP23    BAS   RE,PRTBUCKS         WHEN DOWNLOADING, PRINT ALL BUCKS            
*                                  (NEED 0'S AS PLACEHOLDER)                    
         B     REP40                                                            
*                                                                               
REP25    MVI   COLTYPE,CBIL                                                     
         BAS   RE,GETCOL           SET R5                                       
         BAS   RE,PRTCOL                                                        
*                                                                               
         MVI   COLTYPE,BTD                                                      
         BAS   RE,GETCOL                                                        
         BAS   RE,PRTCOL                                                        
*                                                                               
         CLI   SRTSEQ,0            IS THIS THE ACCOUNT LEVEL REC                
         BNE   REP40               NO, DONE                                     
*                                                                               
         MVI   COLTYPE,BIL         PRINT ACCOUNT LEVEL ACCUMS ONCE              
         BAS   RE,GETCOL                                                        
         BAS   RE,PRTCOL                                                        
*                                                                               
         MVI   COLTYPE,ESTV                                                     
         BAS   RE,GETCOL                                                        
         BAS   RE,PRTCOL                                                        
*                                                                               
         MVI   COLTYPE,BAL                                                      
         BAS   RE,GETCOL                                                        
         BAS   RE,PRTCOL                                                        
*                                                                               
REP40    BAS   RE,PRINTEM                                                       
*                                                                               
REPX     B     XIT                                                              
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        CLEAR THE SRTBUCK FOR THE COL PASSED IN THE COLTAB ENTRY               
*        IN 0(R5)                                                               
*---------------------------------------------------------------------          
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
ZAPCOL   CLI   CTEDIT,EDAMNT       IS THIS AN AMOUNT                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,CTFROM                                                      
         LA    RF,0(RF,R3)                                                      
         ZAP   0(BUCKLN,RF),=P'0'                                               
         BR    RE                                                               
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*                                                                               
*----------------------------------------------------------------------         
*        PRINT THE BUCKETS AT 0(R3) IN XP                                       
*----------------------------------------------------------------------         
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
PRTBUCKS NTR1                                                                   
         L     R5,=A(COLTAB)                                                    
PRTB10   CLI   0(R5),X'FF'                                                      
         BE    PRTBX                                                            
*                                                                               
         BAS   RE,PRTCOL                                                        
*                                                                               
PRTB60   LA    R5,COLTABLN(R5)                                                  
         B     PRTB10                                                           
*                                                                               
PRTBX    B     XIT                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
* ------------------------------------------------------                        
*        PRINT THE COL DEFINED BY THE COLTAB ENTRY A 0(R5)                      
* ------------------------------------------------------                        
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
PRTCOL   NTR1                                                                   
         XR    R1,R1                                                            
         XR    R4,R4                                                            
         XR    R6,R6                                                            
         ICM   R4,3,CTFROM                                                      
         LA    R4,0(R4,R3)         ADDRESS BUCKET TO PRINT                      
         ICM   R6,3,CTTO                                                        
         LA    R6,XP(R6)                                                        
*                                                                               
         CLI   CTEDIT,EDAMNT                                                    
         BNE   PCX                                                              
*                                                                               
         CLI   QOPT10,C'Y'         SUPPRESS BALANCE ON CLOSED JOBS              
         BNE   PC50                NO                                           
         CLI   CTID,BAL            IS THIS THE BALANCE COL                      
         BNE   PC50                NO                                           
         CLI   LEVEL,LEVINO        DETAIL LEVEL BUCKET COL                      
         BE    PC20                YES                                          
         CLI   LEVEL,LEVJOB12                                                   
         BE    PC20                YES                                          
         CLI   LEVEL,LEVJOB                                                     
         BNE   PC50                NO                                           
*                                                                               
PC20     TM    SRTASTAT,RSTSACIC   IS THIS ACCOUNT CLOSED                       
         BNO   PC50                NO PRINT BUCKET VALUE                        
         MVC   0(10,R6),=CL10'          '  JSUT TO BE SURE                      
         B     PCX                                                              
*                                                                               
PC50     ZAP   PL6,0(BUCKLN,R4)                                                 
         BAS   RE,PBUCK                                                         
*                                                                               
PCX      B     XIT                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
*----------------------------------------------------------------------         
*        PRINT THE CHARACTER DATA AS DEFINED ON COLTAB                          
*----------------------------------------------------------------------         
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
PRTCHAR  NTR1                                                                   
         L     R5,=A(COLTAB)                                                    
*                                                                               
PRTC10   CLI   0(R5),X'FF'                                                      
         BE    PRTCX                                                            
*                                                                               
         CLI   CTEDIT,CHARDATA                                                  
         BNE   PRTC60                                                           
*                                                                               
         XR    R1,R1                                                            
         XR    RF,RF                                                            
         XR    R4,R4                                                            
         XR    R6,R6                                                            
         ICM   R4,3,CTFROM                                                      
         LA    R4,SRTCHAR          ADDRESS BUCKET TO PRINT                      
         ICM   R6,3,CTTO                                                        
         LA    R6,XP(R6)                                                        
*                                                                               
         IC    RF,CTTLN            CENTER CHARACTER DATA                        
         IC    R1,CTFLN                                                         
         SR    RF,R1                                                            
         BNP   PRTC50                                                           
*                                                                               
         SRL   RF,2                                                             
         LA    R6,0(RF,R6)                                                      
*                                                                               
PRTC50   BCTR  R1,0                FROM LENGTH STILL IN R1                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R4)                                                    
         B     PRTC60                                                           
*                                                                               
PRTC60   LA    R5,COLTABLN(R5)                                                  
         B     PRTC10                                                           
*                                                                               
PRTCX    B     XIT                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        PRINT PL6 AT 0(R6)                                                     
*---------------------------------------------------------------------          
*                                                                               
PBUCK    SRP   PL6,64-2,5                                                       
         EDIT  PL6,(10,0(R6)),FLOAT=-,ZERO=NOBLANK                              
         BR    RE                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*                                                                               
*        ##### NEED TO SET MEDLEN   ####                                        
*                                                                               
*---------------------------------------------------------------------          
*                                                                               
CHKMEDIA LA    R1,6                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   THISACC(0),LASTACC                                               
         BR    RE                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        RETURN EQUAL IF LEVEL IS IN REPLEVS LIST                               
*-------------------------------------------------------------------            
*                                                                               
CHKLEV   NTR1                                                                   
         LA    RF,L'REPLEVS                                                     
         LA    R1,REPLEVS                                                       
*                                                                               
CHKLEV10 CLC   LEVEL,0(R1)                                                      
         BE    XIT                                                              
         LA    R1,1(R1)                                                         
         BCT   RF,CHKLEV10                                                      
         CR    RF,RE                                                            
         B     XIT                                                              
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        RETURN EQUAL IF COLTYPE IS IN REPTAB(REPTYPE)                          
*-------------------------------------------------------------------            
*                                                                               
CHKCOL   NTR1                                                                   
         L     R2,=A(REPTAB)                                                    
         XR    R1,R1                                                            
*                                                                               
CCOL05   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   REPTYPE,0(R2)                                                    
         BE    CCOL10                                                           
         IC    R1,1(R2)                                                         
         LA    R2,0(R1,R2)                                                      
         B     CCOL05                                                           
*                                                                               
CCOL10   ZIC   R1,1(R2)                                                         
         LA    R2,2(R2)                                                         
         SH    R1,=H'2'                                                         
*                                                                               
CCOL20   CLC   COLTYPE,0(R2)                                                    
         BE    XIT                                                              
         LA    R2,1(R2)                                                         
         BCT   R1,CCOL20                                                        
         CR    RF,RE                                                            
         B     XIT                                                              
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        RETURN A COLTAB ENTRY IN 0(R5)                                         
*-------------------------------------------------------------------            
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
GETCOL   L     R5,=A(COLTAB)                                                    
*                                                                               
GC10     CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   COLTYPE,CTID                                                     
         BER   RE                                                               
         LA    R5,COLTABLN(R5)                                                  
         B     GC10                                                             
*                                                                               
         DROP  R5                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        CLEAR THE ACCUMS PASSED IN LEVEL                                       
*---------------------------------------------------------------------          
*                                                                               
CLEAR    NTR1                                                                   
         BAS   RE,GETACCUM                                                      
*                                                                               
CL30     LA    R3,1(R3)            BUMP TO PACKED DATA                          
         LA    R1,NACCUMS                                                       
         BAS   RE,ZAPEM                                                         
         B     XIT                                                              
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET PTBUCK                                                             
*---------------------------------------------------------------------          
*                                                                               
         USING TRNELD,R4                                                        
*                                                                               
MONBUCK  NTR1                                                                   
         L     R4,ADTRANS                                                       
         MVI   PTBUCK,0                                                         
         LA    R2,BUCKMTHS                                                      
         ZIC   R0,NMTHS                                                         
         ZIC   R1,BLMOBUCK                                                      
*                                                                               
GETB10   CLC   TRNDATE,0(R2)                                                    
         BL    GETB20                                                           
*                                                                               
         LA    R1,1(R1)                                                         
         LA    R2,3(R2)                                                         
         BCT   R0,GETB10                                                        
*                                                                               
GETB20   STC   R1,PTBUCK                                                        
         B     XIT                                                              
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        AP THE DATA IN TRANS REC + PTTROFF TO THE BUCKET IN PTBUCK             
*---------------------------------------------------------------------          
*                                                                               
PUTTRN   NTR1                                                                   
*                                                                               
         MVC   LEVEL,DETLEVEL                                                   
         BAS   RE,GETACCUM                                                      
         LA    R3,1(R3)            SET O/P ADDRESS                              
         ZIC   R1,PTBUCK                                                        
         MH    R1,=Y(BUCKLN)                                                    
         LA    R3,0(R1,R3)                                                      
*                                                                               
         LA    R1,PL6                                                           
         OC    PTTROFF,PTTROFF     PASSED AN OFFSET INTO TRANSACTION            
         BZ    PUTT30              NO                                           
         L     R1,ADTRANS                                                       
         AH    R1,PTTROFF                                                       
*                                                                               
PUTT30   AP    0(BUCKLN,R3),0(BUCKLN,R1)                                        
         B     XIT                                                              
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        WRITE THE CHARACTER DATA A 0(R3) OUT TO THE BUCKET IN PTBUCK           
*---------------------------------------------------------------------          
*                                                                               
PUTCHR   NTR1                                                                   
         MVC   LEVEL,DETLEVEL                                                   
         LA    R3,SRTCHAR          SET O/P ADDRESS                              
         ZIC   RF,PCOFF                                                         
         LA    R3,0(RF,R3)                                                      
         L     R2,ADTRANS                                                       
         IC    RF,PCTROFF                                                       
         LA    R2,0(RF,R2)                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),0(R2)                                                    
         EJECT ,                                                                
*--------------------------------------------------------------------           
*        PUT BUCKETS WHICH ARE DEFINED AT THE ACCOUNT LEVEL                     
*--------------------------------------------------------------------           
*                                                                               
PUTACC   NTR1                                                                   
         MVC   LEVEL,DETLEVEL                                                   
         BAS   RE,GETACCUM                                                      
         LA    R3,1(R3)                                                         
*                                                                               
         XR    R1,R1                                                            
         MVI   COLTYPE,ESTV                                                     
         BAS   RE,CHKCOL                                                        
         BNE   PA30                                                             
         IC    R1,ESTVBUCK                                                      
         LA    R4,0(R1,R3)                                                      
         AP    0(BUCKLN,R4),ESTVALUE                                            
*                                                                               
PA30     MVI   COLTYPE,BAL                                                      
         BAS   RE,CHKCOL                                                        
         BNE   PA40                                                             
*                                                                               
         USING RSTELD,R4                                                        
*                                                                               
         L     R4,ADLVCSTA                                                      
         TM    RSTSTAT,RSTSACIC    IS THIS JOB CLOSED                           
         BNO   PA35                NO, CALCULATE BALANCE                        
         CLI   QOPT10,C'Y'         SUPPRESS BALANCE FOR CLOSED JOBS             
         BE    PA40                                                             
*                                                                               
PA35     IC    R1,BALBUCK                                                       
         MH    R1,=Y(BUCKLN)                                                    
         LA    R4,0(R1,R3)                                                      
         ZAP   DUB,ESTVALUE                                                     
         SP    DUB,JOBBILLS                                                     
         AP    0(BUCKLN,R4),DUB                                                 
*                                                                               
PA40     DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        RETURNS IN R3 THE A(THE ACCUM IN LEVEL)                                
*        OR NOT EQUAL IF THE ACCUM IS NOT FOUND                                 
*---------------------------------------------------------------------          
*                                                                               
GETACCUM L     R3,=A(ACCUMS)                                                    
*                                                                               
GETA10   CLI   0(R3),X'FF'         END OF ACCUMS                                
         BE    GETANO              ACCUM NOT FOUND                              
*                                                                               
         CLC   LEVEL,0(R3)         WANT THIS LEVEL                              
         BER   RE                  YES                                          
         LA    R3,LACCUMS(R3)      TRY NEXT                                     
         B     GETA10                                                           
*                                                                               
GETANO   DC    H'0'                DIE IF R3 IS BAD                             
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        BUILD THE KEY, AND PUT THE BUCKETS AT                                  
*        DETLEVEL TO SORT. ASSUMES LEVEL IS SET                                 
* --------------------------------------------------------------------          
*                                                                               
PUTSORT  NTR1                                                                   
         CLI   QOPT6,C'S'          SUPPRESS INACTIVE ACCOUNTS                   
         BNE   PUTS30                                                           
*                                                                               
         LA    R1,ACTTAB                                                        
         LA    R0,NACTTAB                                                       
*                                                                               
PUTS10   CLC   0(1,R1),LEVEL                                                    
         BE    PUTS20                                                           
         LA    R1,LACTTAB(R1)                                                   
         BCT   R0,PUTS10                                                        
         B     PUTS30                                                           
*                                                                               
PUTS20   MVC   BYTE,1(R1)                                                       
         NC    BYTE,ACTIVITY       IS THIS LEVEL ACTIVE                         
         BZ    PUTSNO              NO                                           
*                                                                               
PUTS30   L     R4,ADACC                                                         
         MVC   SRTACC,3(R4)        SAVE ACCOUNT                                 
         MVC   SRTSEQ,INVSEQ                                                    
         MVC   SRTFILL,INVFILL                                                  
         BAS   RE,BLDKEY                                                        
         BAS   RE,PUTNAME                                                       
         MVC   SRTLEVEL,LEVEL                                                   
         CLC   LEVEL,DETLEVEL                                                   
         BNE   PUTS50                                                           
         BAS   RE,GETACCUM                                                      
         LA    R3,1(R3)                                                         
         MVC   SRTBUCKS(LBUCKS),0(R3)                                           
*                                                                               
PUTS50   CLC   QUESTOR(4),=C'DUMP'                                              
         BNE   PUTS60                                                           
*                                                                               
         MVC   XP+1(3),=C'PUT'                                                  
         MVC   XP+5(L'XP-6),SRTREC                                              
         GOTO1 ACREPORT                                                         
*                                                                               
PUTS60   GOTO1 ADSORTER,DMCB,=C'PUT',SRTREC                                     
*                                                                               
PUTSX    BAS   RE,CLEAR            CLEAR DETAIL LEVEL ACCUMS                    
         CR    RB,RB               SET CC EQ                                    
         B     XIT                                                              
*                                                                               
PUTSNO   LTR   RB,RB               SET CC NEQ                                   
         B     XIT                                                              
*                                                                               
ACTTAB   DS    0C                                                               
         DC    AL1(LEVJOB,ACTJOB)                                               
LACTTAB  EQU   *-ACTTAB                                                         
         DC    AL1(LEVJOB12,ACTJOB)                                             
         DC    AL1(LEVMED,ACTMED)                                               
         DC    AL1(LEVPRO,ACTPRO)                                               
         DC    AL1(LEVCLI,ACTCLI)                                               
         DC    AL1(LEVINO,ACTJOB)                                               
NACTTAB  EQU   (*-ACTTAB)/LACTTAB                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        GET A RECORD FROM SORTER                                               
*        SUM DUPLICATE SORT RECORDS                                             
*        RETURNS NEQ IF NO MORE RECORDS                                         
*---------------------------------------------------------------------          
*                                                                               
GETSORT  NTR1                                                                   
         CLI   SORTFLAG,DONE                                                    
         BE    GETSNO                                                           
*                                                                               
GETS05   MVC   PREVSRT(SRTRECLN),SRTREC                                         
         CLC   SRTLEVEL,DETLEVEL         WAS THIS A DETAIL RECORD               
         BNE   *+10                                                             
         MVC   PREVDKEY,SRTKEY                                                  
*                                                                               
         MVC   PREVSRT(SRTRECLN),SRTREC                                         
         MVC   SRTREC(SRTRECLN),HOLDSRT                                         
*                                                                               
GETS10   GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BNZ   GETS15                                                           
         MVI   SORTFLAG,DONE                                                    
         B     GETS50                                                           
*                                                                               
GETS15   MVC   HOLDSRT(SRTRECLN),0(R1)                                          
         OC    SRTREC(SRTKEYLN),SRTREC  WAS THERE ONE HELD COMING IN            
         BZ    GETS05                   NO, PRIME SRTREC                        
*                                                                               
         CLC   HOLDSRT(SRTKEYLN),SRTREC  IS THIS A NEW KEY                      
         BNE   GETS50                    YES, PROCESS                           
*                                                                               
         CLI   SRTLEVEL,LEVINO     INVOICE LEVEL SRT REC                        
         BE    GETS50              YES, DON'T MERGE                             
*                                                                               
GETS20   MVC   SRTREC(SRTDATLN),HOLDSRT  SAVE KEY, NAME, LEVEL                  
*                                                                               
         CLC   SRTLEVEL,DETLEVEL         WAS THIS A DETAIL RECORD               
         BNE   GETS30              BUCKS ON NON DET LEVEL NOT NEEDED            
         LA    R1,NACCUMS                                                       
         LA    R2,HOLDBUCK         SAVE BUCKETS                                 
         LA    R3,SRTBUCKS                                                      
         BAS   RE,ADDEM                                                         
*                                                                               
GETS30   CLI   SORTFLAG,DONE       ANY MORE SORT RECS                           
         BNE   GETS10              YES GET ANOTHER                              
*                                                                               
GETS50   CLC   QUESTOR(4),=C'DUMP'                                              
         BNE   *+8                                                              
         BAS   RE,GETSDMP                                                       
*                                                                               
         CR    R1,R1                                                            
         B     GETSX                                                            
*                                                                               
GETSNO   DS    0H                                                               
         CR    R1,RB               SET CC NEQ                                   
*                                                                               
GETSX    B     XIT                                                              
*                                                                               
GETSDMP  ST    RE,SAVERE                                                        
         MVC   XP+1(3),=C'GET'                                                  
         MVC   XP+5(L'XP-6),SRTREC                                              
         GOTO1 ACREPORT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        BASED ON LEVELS NEEDED, BUILD A SORT KEY                               
*        STOP BUILDING WHEN YOU HAVE PROCESSED THE LEVEL IN "LEVEL"             
*----------------------------------------------------------------------         
*                                                                               
BLDKEY   NTR1                                                                   
         XC    SRTKEY,SRTKEY                                                    
         LA    R2,REPLEVS          LEVELS NEEDED                                
         LA    R4,SRTKEY                                                        
*                                                                               
BK10     LA    R3,KEYTAB                                                        
*                                                                               
         CLI   0(R2),LEVREQ        REQUEST LEVEL                                
         BE    BK40                YES, NOT NEEDED WHEN BUILDING KEY            
*                                                                               
BK20     CLI   0(R3),X'FF'        LEVEL NOT FOUND IN TABLE                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   0(1,R2),0(R3)       FIND THE KEYTAB ENTRY FOR THIS LEVEL         
         BE    BK30                                                             
*                                                                               
         LA    R3,KEYTABLN(R3)     NEXT KEYTAB ENRTRY                           
         B     BK20                                                             
*                                                                               
BK30     LA    R5,SRTACC           XTRACT KEY DATA FROM SRTACC                  
         ZIC   R1,1(R3)            OFFSET INTO SRTACC                           
         LA    R5,0(R1,R5)                                                      
         IC    R1,2(R3)            LENGTH                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R5)       MOVE A PIECE OF SRTACC INTO SRTKEY           
         LA    R4,1(R1,R4)         BUMP POINTER INTO SRTKEY                     
         CLC   LEVEL,0(R2)         IS THIS THE LEVEL I WANT TO PUTSORT          
         BE    BKX                 YES, ALL DONE                                
*                                                                               
BK40     LA    R2,1(R2)            GET NEXT REPORT LEVEL                        
         B     BK10                                                             
*                                                                               
BKX      B     XIT                                                              
         EJECT ,                                                                
*                                  LEVEL, OFFSET, LENGTH                        
KEYTAB   DS    0C                                                               
         DC    AL1(LEVREQ,0,0),CL11'REQUEST'                                    
KEYTABLN EQU   *-KEYTAB                                                         
         DC    AL1(LEVCLI,0,3),CL11'CLIENT'                                     
         DC    AL1(LEVPRO,3,3),CL11'PRODUCT'                                    
         DC    AL1(LEVMED,6,1),CL11'MEDIA'                                      
         DC    AL1(LEVJOB,6,6),CL11'JOB'                                        
         DC    AL1(LEVJOB12,0,12),CL11'JOB'                                     
         DC    AL1(LEVINO,12,2),CL11'INVOICE' OFFSET IS TO SRTSEQ/FILL          
         DC    XL1'FF'                                                          
*                                                                               
KEYTTOFF EQU   3                   OFFSET TO TITLE IN TABLE                     
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET BUCKETS BASED ON USER REQUEST                                      
*        FIND REPTYPE IN REPTAB, SET BUCKET NUMBERS                             
*                                                                               
*        R2 IS CURRENT BUCKET NUMBER                                            
*---------------------------------------------------------------------          
*                                                                               
SETBUCKS NTR1                                                                   
         L     R2,=A(REPTAB)                                                    
         MVC   ELCODE,REPTYPE                                                   
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R6,R2                                                            
*                                                                               
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         LA    R6,2(R6)                                                         
         XR    R2,R2               INIT BUCKET NUMBER                           
*                                                                               
SB20     XR    RF,RF                                                            
         IC    RF,0(R6)                                                         
         BCTR  RF,0                RF MINUS 1                                   
         SLL   RF,2                TIMES 4                                      
         B     SB30(RF)                                                         
*                                                                               
SB30     B     ASBEST                                                           
         B     ASBBLMO                                                          
         B     ASBBILL                                                          
         B     ASBBAL                                                           
         B     ASBCBIL                                                          
         B     ASBBTD                                                           
         B     ASBINO                                                           
         DC    H'0'                                                             
*                                                                               
SB40     LA    R6,1(R6)                                                         
         BCT   R1,SB20                                                          
         B     XIT                                                              
*                                                                               
ASBEST   STC   R2,ESTVBUCK                                                      
         B     ASBBUMP1                                                         
*                                                                               
ASBBLMO  STC   R2,BLMOBUCK                                                      
         ZIC   R3,NMTHS                                                         
         LA    R2,0(R3,R2)                                                      
         B     SB40                                                             
*                                                                               
ASBBILL  STC   R2,BILLBUCK                                                      
         B     ASBBUMP1                                                         
*                                                                               
ASBBAL   STC   R2,BALBUCK                                                       
         B     ASBBUMP1                                                         
*                                                                               
ASBCBIL  STC   R2,CBILBUCK                                                      
         B     ASBBUMP1                                                         
*                                                                               
ASBINO   XC    INODATA,INODATA     SET OFFSET INTO SRTCHAR                      
         B     ASBBUMP1            BUMP R2 TO SKIP THIS BUCKET                  
*                                                                               
ASBBTD   STC   R2,BTDBUCK                                                       
         B     ASBBUMP1                                                         
*                                                                               
ASBBUMP1 LA    R2,1(,R2)                                                        
         B     SB40                                                             
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET TOTTAB BASED ON USER REQUEST                                       
*---------------------------------------------------------------------          
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
BLDTOT   NTR1                                                                   
         L     R3,=A(TOTTAB)                                                    
*                                                                               
         ZIC   R1,NLEVELS          NUMBER OF LEVELS ON REPORT                   
         BCTR  R1,0                TURN INTO AN OFFSET                          
         LA    R2,REPLEVS                                                       
         LA    R2,0(R1,R2)         ADDRESS LOWEST LEVEL                         
         LA    R1,1(R1)            BUMP R1 FOR BCT                              
*                                                                               
BT10     CLC   0(1,R2),DETLEVEL    IS THIS THE DETAIL LEVEL                     
         BE    BT60                NEXT, PLEASE                                 
*                                                                               
         MVC   LEVEL,0(R2)                                                      
         BAS   RE,SETLEN           SET TOTLEN                                   
*                                                                               
         MVI   TOTNEED,C'N'                                                     
         MVC   TOTLEVEL,LEVEL                                                   
*                                                                               
         BAS   RE,GETTIT           SET RF TO A(TITLE OF THIS LEVEL)             
         MVC   TOTTITLE,0(RF)                                                   
*                                                                               
         XC    TOTACC,TOTACC                                                    
         MVC   TOTNAME,SPACES                                                   
*                                                                               
         BAS   RE,SETTHEAD         SET TOTHEAD BASED ON LEVEL AND OPT2          
*                                                                               
         BAS   RE,SETTYPE          SET TOTTYPE                                  
*                                                                               
         LA    R3,TOTTABLN(R3)                                                  
*                                                                               
BT60     BCTR  R2,0                ADDRESS HIGHER LEVEL IN REPLEV               
         BCT   R1,BT10                                                          
*                                                                               
         MVI   0(R3),X'FF'         MARK END OF TABLE                            
BTX      B     XIT                                                              
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        BLDTOT UTILITIES                                                       
*---------------------------------------------------------------------          
*        SET RF TO A(TITLE FOR THIS LEVEL)                                      
*---------------------------------------------------------------------          
*                                                                               
GETTIT   DS    0H                                                               
         LA    RF,KEYTAB                                                        
*                                                                               
GETT10   CLI   0(RF),X'FF'                                                      
         BER   RE                                                               
         CLC   LEVEL,0(RF)                                                      
         BE    GETTX                                                            
         LA    RF,KEYTABLN(RF)                                                  
         B     GETT10                                                           
*                                                                               
GETTX    LA    RF,KEYTTOFF(RF)    ADD OFFSET TO TITLE                           
         BR    RE                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
SETTYPE  DS    0H                                                               
         XC    TOTTYPE,TOTTYPE                                                  
         OI    TOTTYPE,TOTPC       ALWAYS PRINT CODE                            
         CLI   LEVEL,LEVMED                                                     
         BNER  RE                                                               
         CLI   QOPT1,C'3'          MEDIA/12 BYTE JOB REPORT                     
         BNER  RE                                                               
         OI    TOTTYPE,TOTPN       PRINT MEDIA CODE/NAME                        
         BR    RE                                                               
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET TOTLEN                                                             
*---------------------------------------------------------------------          
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
SETLEN   NTR1                                                                   
         LA    R2,REPLEVS                                                       
         LA    R0,L'REPLEVS                                                     
         XR    R4,R4                                                            
*                                                                               
SL40     CLI   0(R2),0             ANY MORE REPORT LEVELS LEFT                  
         BE    SLX                 NO                                           
*                                                                               
         MVC   BYTE,0(R2)          GET THE LENGTH OF A LEVEL IN R1              
         BAS   RE,GETLEN                                                        
         LA    R4,0(R1,R4)         ACCUMULATE LEN IN R4                         
         CLC   BYTE,LEVEL          IS THIS THE LEVEL I WANT                     
         BE    SLX                                                              
         LA    R2,1(R2)                                                         
         BCT   R0,SL40                                                          
SLX      STC   R4,TOTLEN                                                        
         B     XIT                                                              
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        GET LENGTH                                                             
*---------------------------------------------------------------------          
*                                                                               
GETLEN   DS    0H                                                               
         LA    RF,KEYTAB                                                        
*                                                                               
GL10     CLI   0(RF),X'FF'                                                      
         BER   RE                                                               
         CLC   BYTE,0(RF)                                                       
         BE    GL20                                                             
         LA    RF,KEYTABLN(RF)                                                  
         B     GL10                                                             
*                                                                               
GL20     ZIC   R1,2(RF)                                                         
         BR    RE                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET TOTHEAD                                                            
*---------------------------------------------------------------------          
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
SETTHEAD DS    0H                                                               
         MVI   TOTHEAD,C'N'        ASSUME NO                                    
         CLI   PRTOPT,C'D'                                                      
         BER   RE                  NO HEADER DATA WHEN DOWNLOADING              
*                                                                               
         CLI   LEVEL,LEVREQ        REQUEST LEVEL                                
         BNE   SETT10                                                           
         MVI   TOTHEAD,C'Y'        ALWAYS TOP O FORM                            
         B     SETT50                                                           
*                                                                               
SETT10   CLI   QOPT2,C'Y'          PAGE BREAK BY PRO                            
         BNE   SETT30              NO                                           
         CLI   LEVEL,LEVPRO        SETTING PRODUCT TOTTAB ENTRY                 
         BNE   SETT30                                                           
         MVI   TOTHEAD,C'Y'        SET TOF FOR PRODUCT                          
*                                                                               
SETT30   CLI   LEVEL,LEVCLI        CLIENT?                                      
         BNE   SETT50              NO                                           
         CLI   QOPT1,C'C'          CLIENT ONLY REPORT                           
         BE    SETT50                                                           
         CLI   QOPT1,C'1'          CLIENT/MEDIA REPORT                          
         BE    SETT50                                                           
         MVI   TOTHEAD,C'Y'        SET TOF FOR CLIENT                           
*                                                                               
SETT50   CLI   LEVEL,LEVMED        MEDIA LEVEL                                  
         BNE   SETTX                                                            
         CLI   QOPT1,C'3'          MEDIA/JOB REPORT                             
         BNE   SETTX                                                            
         MVI   TOTHEAD,C'Y'        MEDIA IN HEADER WHEN MEDIA/JOB               
*                                                                               
SETTX    BR    RE                                                               
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        BACK THRU TOTTAB, BUMPING THE INDENT AMOUNT FOR EACH                   
*        PRECEEDING LEVEL, WHEN DONE, SET INDENT AMOUNT FOR DETAIL              
*---------------------------------------------------------------------          
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
SETINDNT NTR1                                                                   
         L     R3,=A(TOTTAB)       GET TO END OF TOTTAB                         
         XR    R1,R1                                                            
         XR    RF,RF               INDENTATION ACCUMULATOR                      
         LA    R0,TOTTABNM                                                      
*                                                                               
SI10     CLI   0(R3),X'FF'                                                      
         BE    SI15                                                             
         LA    R3,TOTTABLN(R3)                                                  
         LA    R1,1(R1)            SAVE N'LEVELS IN THE TABLE                   
         BCT   R0,SI10                                                          
         DC    H'0'                                                             
*                                                                               
SI15     SH    R3,=Y(TOTTABLN)     BACK UP INTO TABLE                           
*                                                                               
SI20     XC    TOTINDNT,TOTINDNT                                                
         CLI   TOTHEAD,C'Y'        DID I SET TOTHEAD                            
         BE    SI30                YES                                          
*                                                                               
         STC   RF,TOTINDNT                                                      
         LA    RF,INDNTAMT(RF)     BUMP INDENT AMOUNT FOR NEXT NON HEAD         
*                                                                               
SI30     SH    R3,=Y(TOTTABLN)                                                  
         BCT   R1,SI20                                                          
         STC   RF,DETINDNT         AMOUNT TO INDENT DETAIL                      
         B     XIT                                                              
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET BOXTAB BASED ON COLTAB                                             
*        BOXTAB IS (OFFSET,BOX CHARACTER(L,C, OR R))                            
*---------------------------------------------------------------------          
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
SETBOXES NTR1                                                                   
         L     R4,ADBOX                                                         
         MVI   BOXINIT,0                                                        
         L     R3,=A(BOXTAB)       TABLE DEFINES WHERE TO PUT THE COLS          
         XC    0(L'BOXTAB,R3),0(R3)                                             
         XR    R1,R1                                                            
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
         L     R5,=A(COLTAB)                                                    
         MVI   0(R3),0             SET LEFT HAND SIDE                           
         MVI   1(R3),C'L'                                                       
         LA    R3,2(R3)                                                         
*                                                                               
SBX20    CLI   0(R5),X'FF'                                                      
         BE    SBXX                                                             
*                                                                               
         TM    CTTYPE,CTPRIM       IS THIS A PRIMARY COL                        
         BNO   SBX50               NO, DOSEN'T GET A BOX                        
*                                                                               
         ICM   R1,3,CTTO           SET BOXTAB ENTRY                             
         BCTR  R1,0                BOX IS BEFORE POSITION                       
         STC   R1,0(R3)                                                         
         MVI   1(R3),C'C'                                                       
         LA    R3,2(R3)                                                         
*                                                                               
SBX50    LA    R5,COLTABLN(R5)                                                  
         B     SBX20                                                            
*                                                                               
*              SHOULD SOFTEN THIS IN CASE LAST COL IS BIG                       
*                                                                               
SBXX     LA    R1,STDCOLLN+1(R1)   BUMP PAST LEFT COL FOR RIGHT                 
         STC   R1,0(R3)                                                         
         MVI   1(R3),C'R'                                                       
         LA    R3,2(R3)                                                         
         MVI   0(R3),X'FF'                                                      
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R4,R5               KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET SRT NAME FROM ADACCNAM, OR GET MEDIA NANE                          
*---------------------------------------------------------------------          
*                                                                               
PUTNAME  NTR1                                                                   
         MVC   SRTNAME,SPACES                                                   
         XC    SRTASTAT,SRTASTAT                                                
         CLI   LEVEL,LEVMED                                                     
         BNE   PUTN40                                                           
         BAS   RE,GETMEDNM                                                      
         MVC   SRTNAME,WORK                                                     
         B     PUTNX                                                            
*                                                                               
PUTN40   LA    R1,ANAMTAB                                                       
         LA    R0,ANAMTABN                                                      
*                                                                               
PUTN50   CLC   LEVEL,0(R1)                                                      
         BE    PUTN60                                                           
         LA    R1,ANAMTBLN(R1)                                                  
         BCT   R0,PUTN50                                                        
         B     PUTNX               CANT GET A NAME FOR THIS LEVEL               
*                                                                               
PUTN60   MVC   *+8(2),1(R1)                                                     
         L     R4,FULL             FULL IS A DUMMY, REPLACED BY SCON            
         LA    R3,SRTNAME                                                       
         BAS   RE,NAMOUT                                                        
*                                                                               
         MVC   *+8(2),3(R1)        GET STATUS ELEMENT                           
         L     R4,FULL                                                          
*                                                                               
         USING RSTELD,R4                                                        
*                                                                               
         MVC   SRTASTAT,RSTSTAT    SAVE STATUS BYTE IN SORT RECORD              
*                                                                               
         B     PUTNX                                                            
*                                                                               
PUTNX    B     XIT                                                              
*                                                                               
         DROP  R4                                                               
*                                                                               
ANAMTAB  DS    0C                                                               
         DC    AL1(LEVCLI),SL2(ADLVANAM),SL2(ADLVASTA)                          
ANAMTBLN EQU   *-ANAMTAB                                                        
         DC    AL1(LEVPRO),SL2(ADLVBNAM),SL2(ADLVBSTA)                          
         DC    AL1(LEVJOB),SL2(ADLVCNAM),SL2(ADLVCSTA)                          
         DC    AL1(LEVJOB12),SL2(ADLVCNAM),SL2(ADLVCSTA)                        
         DC    AL1(LEVINO),SL2(ADLVCNAM),SL2(ADLVCSTA)                          
ANAMTABN EQU   (*-ANAMTAB)/ANAMTBLN                                             
*                                                                               
         EJECT ,                                                                
*                                                                               
*---------------------------------------------------------------------          
*        FILTER JOBS BASED ON REQUEST OPTIONS                                   
*---------------------------------------------------------------------          
*                                                                               
         USING RSTELD,R4                                                        
*                                                                               
FLTJOBS  NTR1                                                                   
         L     R4,ADACCSTA                                                      
         CLI   QOPT3,C' '          INCLUDE LOCKED AND UNLOCKED ?                
         BE    FJ14                YES                                          
         CLI   QOPT3,C'S'          NO, SUPPRESS LOCKED ACCOUNTS ?               
         BNE   FJ12                NO, LOCKED ACCOUNTS ONLY                     
*                                                                               
* SUPPRESS LOCKED ACCOUNTS                                                      
         TM    RSTSTAT,X'20'       YES, IS THIS ACCOUNT LOCKED ?                
         BO    FJXNO               YES, REJECT JOB                              
         B     FJ14                NO, SEE ABOUT CLOSED                         
*                                                                               
* LOCKED ACCOUNTS                                                               
FJ12     TM    RSTSTAT,X'20'       IS THIS ACCOUNT LOCKED ?                     
         BNO   FJXNO               NO, EXIT                                     
*                                                                               
FJ14     MVI   CLOSED,C'N'                                                      
         TM    RSTSTAT,X'40'                                                    
         BZ    *+8                                                              
         MVI   CLOSED,C'Y'         SET FLAG IF CLOSED                           
         CLI   QOPT4,C' '          INCLUDE CLOSED AND OPEN JOBS ?               
         BE    XIT                 YES                                          
         CLI   QOPT4,C'S'          NO, SUPPRESS CLOSED JOBS ?                   
         BNE   FJ16                NO, CLOSED ACCOUNTS ONLY                     
*                                                                               
* SUPPRESS CLOSED JOBS                                                          
         CLI   CLOSED,C'Y'         IS THIS JOB CLOSED ?                         
         BE    FJXNO               YES, EXIT                                    
         B     FJXYES              NO                                           
*                                                                               
* CLOSED JOBS ONLY                                                              
FJ16     CLI   CLOSED,C'Y'         IS THIS JOB CLOSED ?                         
         BNE   XIT                 NO, REJECT                                   
*                                                                               
FJXYES   CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
FJXNO    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        FILTER TRANSACTIONS BASED ON REQUEST OPTIONS                           
*----------------------------------------------------------------------         
*                                                                               
         USING TRNELD,R4                                                        
*                                                                               
FLTTRNS  NTR1                                                                   
         L     R4,ADTRANS                                                       
         CLI   TRNEL,X'44'                                                      
         BNE   FTXIT                                                            
         CLC   TRNANAL,=C'99'      BILLS                                        
         BNE   FTXIT                                                            
*                                                                               
         CLC   TRNDATE,START3      WHICH ARE WITHIN DATE RANGE                  
         BL    FTXIT                                                            
         CLC   TRNDATE,END3                                                     
         BH    FTXIT                                                            
*                                                                               
         CR    RB,RB               TRN IS OK                                    
*                                                                               
FTXIT    B     XIT                                                              
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        EXTRACT ESTIMATE VALUES FROM JOBBER TABLES.                            
*        JOBCOL, DEFINING THE COLUMNS, IS CALLED VIA SPEC                       
*----------------------------------------------------------------------         
*                                                                               
         USING ACMD,R3             ADDRESS JOBBER TABLES                        
*                                                                               
GETEST   NTR1                                                                   
*                                                                               
         L     R3,AMONACC                                                       
*                                                                               
         L     R5,ACMAJOBB                                                      
         L     R3,ACMACOL                                                       
         USING JBLOCKD,R5                                                       
         ZAP   ESTVALUE,=P'0'                                                   
*                                                                               
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   GE20                NO                                           
*                                                                               
         USING MJETABD,R3                                                       
         LA    RE,MJETVAL          CURRENT ESTIMATES ARE FIRST 2 BUCKS          
         CLI   QOPT9,C'Y'          WANT HIGHEST REVISION                        
         BNE   *+8                                                              
         LA    RE,12(RE)           HIGHEST REVS ARE NEXT 2 BUCKS                
*                                                                               
         CLI   QOPT5,C'G'          USE GROSS AMOUNT ?                           
         BNE   *+8                 NO, USE NET                                  
         LA    RE,6(RE)            BUMP TO GROSS BUCKET                         
*                                                                               
         ZAP   ESTVALUE,0(6,RE)    SET ESTVALUE                                 
*                                                                               
         CP    ESTVALUE,=P'0'      ANY ESTIMATE                                 
         BE    *+8                 NO                                           
         OI    ACTIVITY,ACTJOB                                                  
         B     GEXIT                                                            
*                                                                               
         USING JBCOLD,R3                                                        
GE20     LH    R1,JBNROWS                                                       
*                                                                               
GE30     CLI   JBCOLTYP,JBCOLTJB   JOB TOTALS?                                  
         BNE   GE60                GET NEXT COL                                 
*                                                                               
         LA    RE,JBCOLVAL         CURRENT ESTIMATES ARE FIRST 2 BUCKS          
         CLI   QOPT9,C'Y'          WANT HIGHEST REVISION                        
         BNE   *+8                                                              
         LA    RE,12(RE)           HIGHEST REVS ARE NEXT 2 BUCKS                
*                                                                               
         CLI   QOPT5,C'G'          USE GROSS AMOUNT ?                           
         BNE   *+8                 NO, USE NET                                  
         LA    RE,6(RE)            BUMP TO GROSS BUCKET                         
*                                                                               
         ZAP   ESTVALUE,0(6,RE)    SET ESTVALUE                                 
*                                                                               
         CP    ESTVALUE,=P'0'      ANY ESTIMATE                                 
         BE    *+8                 NO                                           
         OI    ACTIVITY,ACTJOB                                                  
*                                                                               
         B     XIT                                                              
*                                                                               
GE60     AH    R3,JBLCOL                                                        
         BCT   R1,GE30                                                          
*                                                                               
GEXIT    B     XIT                                                              
*                                                                               
         DROP  R3,R5                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              DIG OUT AND PRINT MEDIA NAME                                     
*----------------------------------------------------------------------         
*                                                                               
GETMEDNM NTR1                                                                   
         MVC   WORK,SPACES                                                      
         L     R3,ADACC                                                         
         ZIC   R1,MEDPOS                                                        
         AR    R3,R1                                                            
         L     R2,ADCOMP                                                        
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PMDELD,R2                                                        
*                                                                               
GETMD2   CLC   PMDCODE,0(R3)                                                    
         BE    GETMD4                                                           
         BAS   RE,NEXTEL                                                        
         BE    GETMD2                                                           
         DC    H'0'                                                             
*                                                                               
GETMD4   MVC   WORK(L'PMDDESC),PMDDESC                                          
         B     XIT                                                              
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        R4 IS 20 ELEMENT, R3 IS A(OUTPUT AREA)                                 
*----------------------------------------------------------------------         
*                                                                               
         USING NAMELD,R4                                                        
*                                                                               
NAMOUT   ZIC   RF,NAMLN                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
*                                                                               
         MVC   0(0,R3),NAMEREC                                                  
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        ZAP THE R1 BUCKETS AT 0(R3) R1 TIMES                                   
*----------------------------------------------------------------------         
*                                                                               
ZAPEM    DS    0H                                                               
         ZAP   0(BUCKLN,R3),=P'0'                                               
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,ZAPEM                                                         
         BR    RE                                                               
*                                                                               
ADDEM    NTR1                                                                   
*                                                                               
ADDEM5   AP    0(BUCKLN,R3),0(BUCKLN,R2)                                        
         LA    R3,BUCKLN(R3)                                                    
         LA    R2,BUCKLN(R2)                                                    
         BCT   R1,ADDEM5                                                        
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        BUMP SRTREC TOTALS INTO ACCUMS                                         
*----------------------------------------------------------------------         
*                                                                               
BUMP     NTR1                                                                   
         L     R3,=A(ACCUMS)                                                    
*                                                                               
BUMP10   CLI   0(R3),X'FF'         END OF ACCUMS                                
         BE    BUMPX               ACCUM NOT FOUND                              
*                                                                               
         MVC   LEVEL,0(R3)                                                      
         BAS   RE,CHKLEV           THIS LEVEL ON REPORT                         
         BNE   BUMP30                                                           
*                                                                               
         LA    R1,NACCUMS                                                       
         LA    R2,SRTBUCKS                                                      
         LA    R3,1(R3)            FIRST BYTE IS LEVEL                          
*                                                                               
         BAS   RE,ADDEM                                                         
*                                                                               
         BCTR  R3,0                BACK UP A BYTE                               
*                                                                               
BUMP30   LA    R3,LACCUMS(R3)      TRY NEXT                                     
         B     BUMP10                                                           
*                                                                               
BUMPX    B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* DOWNLOADING ROUTINES                                                *         
***********************************************************************         
*                                                                               
SETDOWN  NTR1                                                                   
         L     RF,=V(DLFLD)                                                     
         ST    RF,DOWNLOAD                                                      
*                                                                               
         XC    HEADHOOK,HEADHOOK                                                
         MVI   RCSUBPRG,1                                                       
*                                                                               
         L     RF,=A(DLBUFF)                                                    
         ST    RF,ADLBUFF                                                       
*                                                                               
         L     R4,ADLBUFF                                                       
*                                  DO HEADERS                                   
         USING DLCBD,R4                                                         
*                                  DO HEADERS                                   
         MVI   DLCBACT,DLCBSOR     DOWN LOAD ACTION IS START                    
         SPACE 1                                                                
         LA    RE,XP                                                            
         ST    RE,DLCBAPL                                                       
         SPACE 1                                                                
         LA    RE,DLPRINT          SAVE A(HOOK)                                 
         ST    RE,DLCBAPR                                                       
         GOTO1 DOWNLOAD,(R4)                                                    
*                                  DO HEADERS                                   
         BAS   RE,DOWNHEAD                                                      
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------           
*        CONVERT THE DATA IN XP TO DOWNLOAD FIELDS                              
*--------------------------------------------------------------------           
*                                                                               
         USING COLTABD,R2                                                       
*                                                                               
PUTDOWN  NTR1                                                                   
         L     R2,=A(COLTAB)                                                    
*                                                                               
         MVC   DOWNDATA,XP         SAVE XP                                      
         MVC   DOWNDATA+L'XP,XPSECOND IN CASE WE ARE STACKING                   
         MVC   XP,XSPACES                                                       
         MVC   XPSECOND,XSPACES                                                 
         MVC   XPTHIRD,XSPACES                                                  
         MVC   XPFOURTH,XSPACES                                                 
*                                                                               
         USING DLCBD,R4                                                         
*                                                                               
         L     R4,ADLBUFF          DOWNLOAD CONTROL BUFFER                      
         LA    R3,DOWNDATA                                                      
*                                                                               
*                                                                               
         MVI   DLCBTYP,DLCBTXT     TYPE OF THIS FIELD                           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
*                                                                               
         ZIC   R6,FRSTCOL          OFFSET OF FIRST PRINTED COL                  
*                                  IS LENGTH OF THIS FIELD                      
         CH    R6,=Y(L'DLCBFLD)    CHECK SIZE TO DOWN LOAD                      
         BNH   *+8                                                              
         LA    R6,L'DLCBFLD                                                     
         STC   R6,DLCBLEN                                                       
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R3)                                                 
*                                                                               
         GOTO1 DOWNLOAD,(R4)                                                    
*                                                                               
PUTD10   CLI   0(R2),X'FF'         END OF COL TABLE                             
         BE    PUTD50                                                           
         XR    R5,R5               OFFSET INTO DOWNDATA                         
         ICM   R5,3,CTTO                                                        
         LA    R5,DOWNDATA(R5)                                                  
         MVC   DLCBLEN,CTTLN                                                    
                                                                                
         MVI   DLCBTYP,DLCBNUM     TYPE OF THIS FIELD                           
         CLI   CTEDIT,CHARDATA                                                  
         BNE   *+8                                                              
         MVI   DLCBTYP,DLCBTXT                                                  
                                                                                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
*                                                                               
         ZIC   R6,CTTLN            LENGTH OF THIS FIELD                         
         CH    R6,=Y(L'DLCBFLD)    CHECK SIZE TO DOWN LOAD                      
         BNH   *+8                                                              
         LA    R6,L'DLCBFLD                                                     
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R5)                                                 
*                                                                               
         GOTO1 DOWNLOAD,(R4)                                                    
*                                                                               
         LA    R2,COLTABLN(R2)                                                  
         B     PUTD10                                                           
*                                                                               
PUTD50   L     R4,ADLBUFF                                                       
         MVI   DLCBACT,DLCBEOL     MARK END-OF-LINE                             
         GOTO1 DOWNLOAD,(R4)                                                    
         B     XIT                                                              
*                                                                               
         DROP  R2,R4               KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------           
*        DOWN LOAD UTILITIES                                                    
*--------------------------------------------------------------------           
*                                                                               
         USING DLCBD,R4                                                         
*                                                                               
ENDDOWN  NTR1                                                                   
         L     R4,ADLBUFF          MARK END OF REPORT                           
         MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 DOWNLOAD,(R4)                                                    
         B     XIT                                                              
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
*                                                                               
*        HOOK FOR DOWNLOAD ROUTINE                                              
*                                                                               
DLPRINT  NTR1                                                                   
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         MVI   FORCEHED,C'N'       KEEP EVERY THING ON CURRENT PAGE             
         MVI   NEWPAGE,C'N'        TURN OFF SKIP TO CHANNEL 1 STUFF             
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
*                                                                               
*        HOOK FOR DOWNLOAD WITH TOF                                             
DLPRHEAD NTR1                                                                   
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
*        MVI   FORCEHED,C'Y'                                                    
*        MVI   NEWPAGE,C'Y'                                                     
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
*                                                                               
         EJECT ,                                                                
*                                                                               
         USING DLCBD,R4                                                         
*                                                                               
DOWNHEAD NTR1                                                                   
         L     R4,ADLBUFF          DOWNLOAD CONTROL BUFFER                      
*                                                                               
         LA    RE,DLPRHEAD         USE HEAD HOOK W/TOP OF FORM                  
         ST    RE,DLCBAPR                                                       
*                                                                               
         MVC   DLCBFLD,XSPACES                                                  
         MVC   DLCBFLD(20),=C'CLIENT BUDGET REPORT'                             
         MVI   DLCBLEN,20                                                       
*                                                                               
         MVI   DLCBTYP,DLCBTXT     HEADERS ARE TEXT                             
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
*                                                                               
         GOTO1 DOWNLOAD,(R4)                                                    
         BAS   RE,DOWNEOL                                                       
*                                                                               
         MVC   DLCBFLD,XSPACES                                                  
*                                                                               
         L     R4,ADCMPNAM                                                      
         MVC   WORK,SPACES                                                      
         MVC   WORK(7),=C'COMPANY'                                              
         LA    R3,WORK+8                                                        
         BAS   RE,NAMOUT                                                        
         L     R4,ADLBUFF                                                       
         MVC   DLCBFLD,XSPACES                                                  
         MVC   DLCBFLD,WORK                                                     
         MVI   DLCBLEN,L'DLCBFLD                                                
         MVI   DLCBTYP,DLCBTXT     TYPE OF THIS FIELD                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 DOWNLOAD,(R4)                                                    
         BAS   RE,DOWNEOL                                                       
*                                                                               
         LA    RE,DLPRINT          USE HEAD HOOK W/NO FORM FEED                 
         ST    RE,DLCBAPR                                                       
*                                                                               
         L     R3,=A(OPTTAB)       PRINT OPTION1/2 DATA                         
         MVC   DLCBFLD,XSPACES                                                  
*                                                                               
DH01     CLI   0(R3),X'FF'                                                      
         BE    DH05                IF NOT FOUND, DL A PAD                       
         CLC   0(2,R3),QOPT1                                                    
         BE    DH04                                                             
         LA    R3,LOPTTAB(R3)                                                   
         B     DH01                                                             
*                                                                               
DH04     MVC   DLCBFLD(LOPTTAB-2),2(R3)                                         
*                                                                               
DH05     MVI   DLCBLEN,LOPTTAB-1                                                
         MVI   DLCBTYP,DLCBTXT     TYPE OF THIS FIELD                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 DOWNLOAD,(R4)                                                    
*                                                                               
         USING COLTABD,R2                                                       
*                                                                               
         L     R2,=A(COLTAB)                                                    
*                                                                               
DH10     CLI   0(R2),X'FF'         END OF COL TABLE                             
         BE    DH50                                                             
         XR    R5,R5               OFFSET INTO DOWNDATA                         
         ICM   R5,3,CTTO                                                        
         AR    R5,R3                                                            
         MVC   DLCBLEN,CTTLN                                                    
*                                                                               
         MVI   DLCBTYP,DLCBTXT     HEADERS ARE TEXT                             
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,CTHEAD                                                      
         LA    R3,0(R3,RC)                                                      
         ZIC   R6,CTTLN            LENGTH OF THIS FIELD                         
         CH    R6,=Y(L'DLCBFLD)    CHECK SIZE TO DOWN LOAD                      
         BNH   *+8                                                              
         LA    R6,L'DLCBFLD                                                     
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R3)                                                 
*                                                                               
         GOTO1 DOWNLOAD,(R4)                                                    
*                                                                               
         LA    R2,COLTABLN(R2)                                                  
         B     DH10                                                             
*                                                                               
DH50     BAS   RE,DOWNEOL                                                       
*                                                                               
         MVC   DLCBFLD,XSPACES     DO SECOND LINE OF TITLES                     
         MVI   DLCBLEN,1           FIRST, A VLACE HOLDER FOR COL 1              
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 DOWNLOAD,(R4)                                                    
*                                                                               
         L     R2,=A(COLTAB)                                                    
*                                                                               
DH70     CLI   0(R2),X'FF'         END OF COL TABLE                             
         BE    DH100                                                            
         XR    R5,R5               OFFSET INTO DOWNDATA                         
         ICM   R5,3,CTTO                                                        
         AR    R5,R3                                                            
         MVC   DLCBLEN,CTTLN                                                    
*                                                                               
         MVI   DLCBTYP,DLCBTXT     HEADERS ARE TEXT                             
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,CTHEAD                                                      
         LA    R3,10(R3,RC)        SECOND HEADER LINE IS 10 BYTES               
*                                  INTO EACH FIELD                              
         ZIC   R6,CTTLN            LENGTH OF THIS FIELD                         
         CH    R6,=Y(L'DLCBFLD)    CHECK SIZE TO DOWN LOAD                      
         BNH   *+8                                                              
         LA    R6,L'DLCBFLD                                                     
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R3)                                                 
*                                                                               
         GOTO1 DOWNLOAD,(R4)                                                    
*                                                                               
         LA    R2,COLTABLN(R2)                                                  
         B     DH70                                                             
*                                                                               
DH100    BAS   RE,DOWNEOL                                                       
*                                                                               
         B     XIT                                                              
*                                                                               
DOWNEOL  NTR1                                                                   
         L     R4,ADLBUFF                                                       
         MVI   DLCBACT,DLCBEOL     MARK END-OF-LINE                             
         GOTO1 DOWNLOAD,(R4)                                                    
         B     XIT                                                              
*                                                                               
         DROP  R2,R4               KEEP IT CLEAN                                
         EJECT ,                                                                
*                                                                               
OPENSORT NTR1                                                                   
         LA    R1,SRTKEYLN                   SORT KEY LENGTH                    
         CVD   R1,DUB                        CONVERT KEY LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         L     RF,=A(SORTCARD)                                                  
         UNPK  15(3,RF),DUB+6(2)                                                
*                                                                               
         LA    R1,SRTRECLN                   SORT RECORD LENGTH                 
         CVD   R1,DUB                        CONVERT REC LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         L     RF,=A(RECCARD)                                                   
         UNPK  22(3,RF),DUB+6(2)                                                
         GOTO1 ADSORTER,DMCB,A(SORTCARD),A(RECCARD),0                           
         B     XIT                                                              
*                                                                               
CLOSSORT NTR1                                                                   
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT A MIDLINE, WHEREVER YOU ARE                                      
*----------------------------------------------------------------------         
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
BOXMID   NTR1                                                                   
         L     R4,ADBOX                                                         
         ZIC   R2,LINE                                                          
         CH    R2,=H'52'                                                        
         BL    BOXM20                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     BOXM50                                                           
*                                                                               
BOXM20   ZIC   R2,LINE                                                          
         LA    R3,BOXROWS(R2)                                                   
         BCTR  R3,0                                                             
         MVI   0(R3),C'M'                                                       
         MVI   BOXINIT,0                                                        
*                                                                               
BOXM50   B     XIT                                                              
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*                                                                               
PRINTEM  DS    0H                                                               
         ST    RE,SAVERE                                                        
         CLI   PRTOPT,C'D'         DOWNLOAD?                                    
         BE    PRINT10             YES                                          
*                                                                               
         GOTO1 ACREPORT                                                         
         B     PRINTX                                                           
*                                                                               
PRINT10  BAS   RE,PUTDOWN                                                       
*                                                                               
PRINTX   L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
HDTAB    DS    0C                                                               
         DC    CL20' CURRENT   ESTIMATE'                                        
         DC    CL20' BALANCE'                                                   
         DC    CL20' BILLING'                                                   
         DC    CL20' INVOICE    NUMBER'                                         
         DC    CL20' CURRENT   BILLING'                                         
         DC    CL20'  BILLING   TO DATE'                                        
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        TABLE DEFINING OPTION 1 OVERIDES OF DEFAULT REPORT LEVELS              
*        TABLE IS: OPTION1, DETAIL LEVEL, N'LEVELS , LEVELS ON REPORT           
*        WHERE OPTION SEVEN CHANGES REPORT LEVELS, IS IS NEEDED ALSO            
*----------------------------------------------------------------------         
*                                                                               
REPLVTB  DS    0C                                                               
         DC    CL2'C ',AL1(LEVCLI),AL1(2)                                       
         DC    AL1(LEVREQ,LEVCLI,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                   
*                                                                               
LREPLVTB EQU   *-REPLVTB                                                        
         DC    CL2'P ',AL1(LEVPRO),AL1(3)                                       
         DC    AL1(LEVREQ,LEVCLI,LEVPRO,0,0,0,0,0,0,0,0,0,0,0,0,0)              
*                                                                               
         DC    CL2'1 ',AL1(LEVMED),AL1(3)                                       
         DC    AL1(LEVREQ,LEVCLI,LEVMED,0,0,0,0,0,0,0,0,0,0,0,0,0)              
*                                                                               
         DC    CL2'2 ',AL1(LEVMED),AL1(4)                                       
         DC    AL1(LEVREQ,LEVCLI,LEVPRO,LEVMED,0,0,0,0,0,0,0,0,0,0,0,0)         
*                                                                               
         DC    CL2'3I',AL1(LEVINO),AL1(4)                                       
         DC    AL1(LEVREQ,LEVMED,LEVJOB12,LEVINO,0,0,0,0,0,0,0,0,0,0,0,X        
               0)                                                               
*                                                                               
         DC    CL2'3 ',AL1(LEVJOB12),AL1(3)                                     
         DC    AL1(LEVREQ,LEVMED,LEVJOB12,0,0,0,0,0,0,0,0,0,0,0,0,0)            
*                                                                               
         DC    CL2' I',AL1(LEVINO),AL1(6)                                       
         DC    AL1(LEVREQ,LEVCLI,LEVPRO,LEVMED,LEVJOB,LEVINO,0,0,0,0,0,X        
               0,0,0,0,0)                                                       
*                                                                               
         DC    CL2'  ',AL1(LEVJOB),AL1(5)                DEFAULT                
         DC    AL1(LEVREQ,LEVCLI,LEVPRO,LEVMED,LEVJOB,0,0,0,0,0,0,0,0,0X        
               ,0,0)                                                            
NREPLVTB EQU   (*-REPLVTB)/LREPLVTB                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        TABLE DEFINEING COLUMNS NEEDED FOR DIFFERNT REPTYPES                   
*        TABLE ENTRIES MUST BE IN ELEMENTAL FORM (CODE,LEN,DATA)                
*----------------------------------------------------------------------         
*                                                                               
REPTAB   DS    0C                                                               
         DC    AL1(STD,6,ESTV,BLMO,BIL,BAL)                                     
         DC    AL1(A64,8,ESTV,INO,CBIL,BTD,BIL,BAL)                             
         DC    AL1(0)                                                           
*                                                                               
*        REPTYPES                                                               
STD      EQU   1                                                                
A64      EQU   2                                                                
*                                                                               
*        COLTYPES                                                               
ESTV     EQU   1                   REPTYPE COLUMN EQUATES                       
BLMO     EQU   2                                                                
BIL      EQU   3                                                                
BAL      EQU   4                                                                
CBIL     EQU   5                                                                
BTD      EQU   6                                                                
INO      EQU   7                                                                
*                                                                               
NACCUMS  EQU   15                                                               
BUCKLN   EQU   6                                                                
COLLN    EQU   11                  WIDTH OF COL                                 
LACCUMS  EQU   (BUCKLN*NACCUMS)+1                                               
LBUCKS   EQU   (BUCKLN*NACCUMS)                                                 
PAGEWDTH EQU   L'XP                                                             
*                                                                               
*                                                                               
*                                  TABLE TO BUILD SORT KEYS                     
*                                  LENGTH TO MOVE IS SEEDED IN LATER            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              HEADHOOK - CALLED FROM ACPRINT                                   
*--------------------------------------------------------------------*          
*                                                                               
HDHOOK   NMOD1 0,**HDHK**                                                       
         L     R2,=A(BOXRC)                                                     
         ICM   RC,15,0(R2)                                                      
*                                                                               
         CLI   QOPT5,C'G'          GROSS                                        
         BNE   *+10                                                             
         MVC   XHEAD4+124(12),=C'GROSS OPTION'                                  
*                                                                               
         CLI   QOPT10,C'Y'          SUPPRESS CLOSED BALANCES                    
         BNE   *+10                                                             
         MVC   XHEAD2+124(34),=C'BALANCE SUPPRESSED FOR CLOSED JOBS'            
*                                                                               
         LA    R5,XHEAD4+1                                                      
         L     R3,=A(TOTTAB)       GET TO END OF TOTTAB                         
         LA    R0,TOTTABNM                                                      
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
HDT10    CLI   0(R3),X'FF'                                                      
         BE    HDT50                                                            
         LA    R3,TOTTABLN(R3)                                                  
         BCT   R0,HDT10                                                         
         DC    H'0'                LEVEL NOT FOUND IN TOTTAB                    
*                                                                               
HDT50    SH    R3,=Y(TOTTABLN)                                                  
         CLI   TOTHEAD,C'Y'        IS THIS IN THE HEADER                        
         BNE   HDT100              NO STOP AS SOON AS YOU GET A NONHEAD         
*                                                                               
         CLI   TOTLEVEL,LEVREQ     REQUEST TABLE ENTRY                          
         BE    HDT50               NO LITERAL DATA                              
*                                                                               
         GOTO1 =A(GETPIECE),DMCB,TOTACC,TOTLEVEL                                
         MVC   0(L'TOTTITLE,R5),TOTTITLE                                        
         MVC   L'TOTTITLE(L'PIECE,R5),PIECE                                     
         MVC   L'TOTTITLE+L'PIECE(L'TOTNAME,R5),TOTNAME                         
*                                                                               
         GOTO1 ADSQUASH,DMCB,L'TOTTITLE(R5),L'PIECE+L'TOTNAME                   
HDT60    LA    R5,L'XP(R5)                                                      
         B     HDT50                                                            
*                                                                               
HDT100   LA    R5,XHEAD10+1        PRINT "CONTINUED" FOR NON-HEADS              
         MVI   LEVEL,LEVCLI                                                     
         BAS   RE,HDCONT                                                        
         MVI   LEVEL,LEVPRO                                                     
         BAS   RE,HDCONT                                                        
         MVI   LEVEL,LEVMED                                                     
         BAS   RE,HDCONT                                                        
*                                                                               
         LA    R0,XHEAD10+1        ANY THING HAPPEN                             
         CR    R5,R0                                                            
         BNH   HDT110              NO                                           
         ZIC   R1,DMCB+7           GET LAST LENGTH USED BY SQUASHER             
         SH    R1,=H'2'            EXECUTED RIPPLE MVC                          
         MVI   0(R5),X'BF'                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R5),0(R5)                                                    
*                                                                               
*        BUILD BOXES                                                            
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
HDT110   L     R4,ADBOX                                                         
         MVC   BOXCOLS,XSPACES     FOR COLS 1-132                               
         MVC   BOXCOLSR,XSPACES    FOR COLS 133+                                
         MVC   BOXROWS,XSPACES                                                  
         SPACE 1                                                                
*                                                                               
         MVI   BOXROWS+5,C'T'      SET ROWS                                     
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+59,C'B'                                                  
         SPACE 1                                                                
         LA    R2,BOXCOLS                                                       
         L     R3,=A(BOXTAB)       TABLE DEFINES WHERE TO PUT THE COLS          
*                                                                               
HD50     CLI   0(R3),X'FF'        EOT                                           
         BE    HD60                                                             
         ZIC   R1,0(R3)                                                         
         LA    R1,BOXCOLS(R1)                                                   
         MVC   0(1,R1),1(R3)       MOVE IN BOX CHARACTER FROM TABLE             
         LA    R3,2(R3)                                                         
         B     HD50                                                             
*                                                                               
HD60     L     R3,=A(OPTTAB)       PRINT OPTION1/2 DATA                         
*                                                                               
HD61     CLI   0(R3),X'FF'                                                      
         BE    HD64                                                             
         CLC   0(2,R3),QOPT1                                                    
         BE    HD63                                                             
         LA    R3,LOPTTAB(R3)                                                   
         B     HD61                                                             
*                                                                               
HD63     MVC   XHEAD7+1(LOPTTAB-2),2(R3)                                        
*                                                                               
         USING COLTABD,R3                                                       
*                                                                               
HD64     L     R3,=A(COLTAB)                                                    
*                                                                               
HD65     CLI   0(R3),X'FF'                                                      
         BE    HDX                                                              
         XR    R1,R1                                                            
         ICM   R1,3,CTTO                                                        
         LA    R1,XHEAD7(R1)                                                    
         XR    R2,R2                                                            
         ICM   R2,3,CTHEAD                                                      
         LA    R2,0(R2,RC)                                                      
         MVC   0(10,R1),0(R2)                                                   
         LA    R1,L'XP(R1)                                                      
         MVC   0(10,R1),10(R2)                                                  
         LA    R3,COLTABLN(R3)                                                  
         B     HD65                                                             
*                                                                               
HDX      MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
         GOTO1 =A(BLDFOOT),DMCB,(RC)                                            
         MVC   XFOOT1,MYFOOT                                                    
         MVI   CLEARFUT,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R4               KEEP IT CLEAN                                
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT A NON HEAD LEVEL CONTINUED, IF NEEDED                            
*        R5 IS PRINT ADDRESS, BUMP IF USED                                      
*----------------------------------------------------------------------         
*                                                                               
HDCONT   NTR1                                                                   
*                                                                               
         CLC   LEVEL,DETLEVEL      IS THIS THE DETAIL LEVEL                     
         BE    HDCX                YES, DON'T CONTINUE                          
*                                                                               
         LA    RF,L'REPLEVS        IS THIS LEVEL IN REPORT                      
         LA    R1,REPLEVS                                                       
*                                                                               
HDC01    CLC   LEVEL,0(R1)                                                      
         BE    HDC05                                                            
         LA    R1,1(R1)                                                         
         BCT   RF,HDC01                                                         
         B     HDCX                THIS LEVEL NOT ON REPORT                     
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
HDC05    L     R3,=A(TOTTAB)       GET TO END OF TOTTAB                         
         LA    R0,TOTTABNM                                                      
*                                                                               
HDC10    CLC   TOTLEVEL,LEVEL                                                   
         BE    HDC20                                                            
         LA    R3,TOTTABLN(R3)                                                  
         BCT   R0,HDC10                                                         
         DC    H'0'                LEVEL NOT FOUND IN TOTTAB                    
*                                                                               
HDC20    CLI   TOTHEAD,C'Y'        IS THIS A HEADER LEVEL                       
         BE    HDCX                YES                                          
         CLI   TOTNEED,C'Y'        WILL TOTALS BE NEEDED                        
         BNE   HDCX                NO, NO CONTINUE                              
*                                                                               
         GOTO1 =A(GETPIECE),DMCB,TOTACC,TOTLEVEL                                
         MVC   0(L'TOTTITLE,R5),TOTTITLE                                        
         MVC   L'TOTTITLE(L'PIECE,R5),PIECE                                     
         MVC   L'TOTTITLE+L'PIECE(9,R5),=C'CONTINUED'                           
*                                                                               
         GOTO1 ADSQUASH,DMCB,0(R5),L'PIECE+L'TOTTITLE+9                         
         LA    R5,L'XP(R5)                                                      
HDCX     XIT1  REGS=(R5)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
*                                                                               
BOXRC    DS    A                                                                
*                                                                               
OPTTAB   DS    0C                                                               
         DC    CL2'C ',CL25'CLIENT'                                             
LOPTTAB  EQU   *-OPTTAB                                                         
         DC    CL2'P ',CL25'PRODUCT'                                            
         DC    CL2'1 ',CL25'CLIENT/MEDIA'                                       
         DC    CL2'2 ',CL25'PRODUCT/MEDIA'                                      
         DC    CL2'  ',CL25'PRODUCT/MEDIA/JOB'                                  
         DC    CL2' Y',CL25'MEDIA/JOB'                                          
         DC    X'FF'                                                            
         EJECT ,                                                                
*                                                                               
         USING NAMELD,R4                                                        
*                                                                               
VNAMOUT  ZIC   RF,NAMLN                                                         
         SH    RF,=H'3'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+6                                                           
         BR    RE                                                               
*                                                                               
         MVC   0(0,R3),NAMEREC                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*                                                                               
*---------------------------------------------------------------------          
*                                                                               
SETDATES NMOD1 0,SDATES                                                         
         L     RC,0(R1)                                                         
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY)                                      
         MVC   CURMON,TODAY                                                     
         XC    START3,START3                                                    
         XC    START2,START2                                                    
         MVC   END3,=X'FFFFFF'                                                  
         MVC   END2,=X'FFFF'                                                    
         CLC   QSTART,SPACES                                                    
         BE    SETD30                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START3)                                
         GOTO1 DATCON,DMCB,(0,QSTART),(2,START2)                                
*                                                                               
SETD30   CLC   QEND,SPACES                                                      
         BE    SETD40                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,END3)                                    
         GOTO1 DATCON,DMCB,(0,QEND),(2,END2)                                    
         MVC   CURMON,END3                                                      
*                                                                               
SETD40   CLI   QOPT7,C'I'          INVOICE DETAIL REPORT                        
         BE    SETDX               YES, NO DATE RANGE LIMIT                     
*                                                                               
         GOTO1 =V(PERVERT),DMCB,QSTART,QEND                                     
         MVC   NMTHS,DMCB+15      N'MONTHS IS IN P4, BYTES 3-4                  
         CLI   NMTHS,MAXMTHS                                                    
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
SETDX    XIT1                                                                   
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET COLS ON THE REPORT BASED ON USER REQUEST                           
*        FIND REPTYPE IN REPTAB, GET LIST OF COLS AND BUILD COLTAB              
*                                                                               
*        REGISTER USAGE:   R6 - A(COL TYPES FOT THIS REPORT)                    
*                          R5 - A(COL TAB ENTRY YOU ARE BUILDING)               
*                          R3 - OFFSET INTO BUCKETS FOR THIS COL                
*                          R2 - OFFSET INTO XP OF THIS COL                      
*                          R1 - NUMBER OF COLS DEFINED FOR REPTYPE              
*        NOTE  THE SUBROUTINES WHICH BUILD THE COLS ARE RESPONSIBLE             
*              FOR BUMPING R5 AND R2                                            
*---------------------------------------------------------------------          
*                                                                               
SETCOLS  NMOD1 0,SCOLS                                                          
         L     RC,0(R1)                                                         
         L     R2,=A(REPTAB)                                                    
         SR    RF,RF                                                            
*                                                                               
SC05     CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   REPTYPE,0(R2)                                                    
         BE    SC10                                                             
*                                                                               
         IC    RF,1(R2)                                                         
         LA    R2,0(RF,R2)                                                      
         B     SC05                                                             
*                                                                               
SC10     LR    R6,R2                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'            CALC NUMBER OF COLS FOR REPORT               
         LA    R6,2(R6)            ADDRESS DATA                                 
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
         L     R5,=A(COLTAB)                                                    
         ZIC   R2,FRSTCOL          OFFSET OF FIRST PRINTED COL                  
*                                                                               
SC20     XR    RF,RF                                                            
         XC    0(COLTABLN,R5),0(R5)  CLEAR TABLE AREA                           
         IC    RF,0(R6)                                                         
         BCTR  RF,0                RF MINUS 1                                   
         SLL   RF,2                TIMES 4                                      
         B     SC30(RF)                                                         
*                                                                               
SC30     B     ASESTV              NOTE, THESE ROUTINES MUST BUMP               
         B     ASCBLMO             NEXTCOL AND NEXTPRT                          
         B     ASCBILL                                                          
         B     ASCBAL                                                           
         B     ASCCBIL                                                          
         B     ASCBTD                                                           
         B     ASCINO                                                           
         DC    H'0'                                                             
*                                                                               
SC40     L     R5,NEXTCOL          A(NEXT COL TAB ENTRY)                        
         L     R2,NEXTPRT          OFFSET INTO XP OF NEXT COL                   
         LA    R6,1(R6)                                                         
         BCT   R1,SC20                                                          
         MVI   0(R5),X'FF'                                                      
*                                                                               
SCX      XIT1                                                                   
*                                                                               
ASESTV   DS    0H                                                               
         BAS   RE,SETESTV                                                       
         B     SC40                                                             
*                                                                               
ASCBLMO  DS    0H                                                               
         BAS   RE,SETMON                                                        
         B     SC40                                                             
*                                                                               
ASCBILL  DS    0H                                                               
         BAS   RE,SETBILL                                                       
         B     SC40                                                             
*                                                                               
ASCBAL   DS    0H                                                               
         BAS   RE,SETBAL                                                        
         B     SC40                                                             
*                                                                               
ASCCBIL  DS    0H                                                               
         BAS   RE,SETCBIL                                                       
         B     SC40                                                             
*                                                                               
ASCBTD   DS    0H                                                               
         BAS   RE,SETBTD                                                        
         B     SC40                                                             
*                                                                               
ASCINO   DS    0H                                                               
         BAS   RE,SETINO                                                        
         B     SC40                                                             
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        CURRENT ESTIMATE                                                       
*---------------------------------------------------------------------          
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
SETESTV  NTR1                                                                   
         LA    R3,HDEST-AC66D                                                   
*                                                                               
         XR    R1,R1                                                            
         IC    R1,ESTVBUCK                                                      
         MVI   CTID,ESTV                                                        
         BAS   RE,SETCOL                                                        
         B     SCX                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        BILLING                                                                
*---------------------------------------------------------------------          
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
SETBILL  NTR1                                                                   
         LA    R3,HDBIL-AC66D                                                   
*                                                                               
         XR    R1,R1                                                            
         IC    R1,BILLBUCK                                                      
         MVI   CTID,BIL                                                         
         BAS   RE,SETCOL                                                        
         B     SCX                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        BALANCE                                                                
*---------------------------------------------------------------------          
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
SETBAL   NTR1                                                                   
         LA    R3,HDBAL-AC66D                                                   
*                                                                               
         XR    R1,R1                                                            
         IC    R1,BALBUCK                                                       
         MVI   CTID,BAL                                                         
         BAS   RE,SETCOL                                                        
         B     SCX                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        CURRENT BILLING                                                        
*---------------------------------------------------------------------          
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
SETCBIL  NTR1                                                                   
         LA    R3,HDCBIL-AC66D                                                  
*                                                                               
         XR    R1,R1                                                            
         IC    R1,CBILBUCK                                                      
         MVI   CTID,CBIL                                                        
         BAS   RE,SETCOL                                                        
         B     SCX                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        BILLING TO DATE COL                                                    
*---------------------------------------------------------------------          
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
SETBTD   NTR1                                                                   
         LA    R3,HDBTD-AC66D                                                   
*                                                                               
         XR    R1,R1                                                            
         IC    R1,BTDBUCK                                                       
         MVI   CTID,BTD                                                         
         BAS   RE,SETCOL                                                        
         B     SCX                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        INVOICE NUMBER COL                                                     
*---------------------------------------------------------------------          
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
SETINO   NTR1                                                                   
         LA    R3,HDINO-AC66D                                                   
*                                                                               
         XR    R1,R1                                                            
         MVI   CTID,INO                                                         
         BAS   RE,SETCOL                                                        
         MVI   CTEDIT,CHARDATA                                                  
         B     SCX                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        SET ONE COL                                                            
*        REGISTER USAGE: R1 IS BUCKET NUMBER                                    
*                        R2 IS PRINT OFFSET                                     
*                        R3 IS A HEADER DATA                                    
*                        R5 IS A(COL TABLE ENTRY TO SET)                        
*---------------------------------------------------------------------          
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
SETCOL   NTR1                                                                   
*                                                                               
         STCM  R3,3,CTHEAD                                                      
         STCM  R2,3,CTTO                                                        
         MH    R1,=Y(BUCKLN)                                                    
         STH   R1,CTFROM                                                        
*                                                                               
         MVI   CTFLN,BUCKLN                                                     
         MVI   CTTLN,STDCOLLN                                                   
         MVI   CTEDIT,EDAMNT                                                    
         MVI   CTTYPE,CTPRIM                                                    
*                                                                               
         LA    R5,COLTABLN(R5)                                                  
         ST    R5,NEXTCOL                                                       
         LA    R2,STDCOLLN+1(R2)                                                
         ST    R2,NEXTPRT                                                       
         B     SCX                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        THE USER HAS ASKED FOR A SERIES OF MONTHLY BUCKETS                     
*        COLTAB     - ON ENTRY R5 IS A( THE COLTAB ENTRY TO START W/)           
*        THE BUCKETS ARE THE FIRST IN SRTBUCKS/ACCUMS FOR A MAX OF 12           
*        THE O/P OFFSET IS IN R2                                                
*---------------------------------------------------------------------          
*                                                                               
SETMON   NTR1                                                                   
         MVI   STAKBUCK,C'N'                                                    
         MVC   PCOLWDTH,=Y(STDCOLLN)                                            
         CLI   PRTOPT,C'D'         DOWNLOADING                                  
         BE    SM20                CAN'T STACK                                  
*                                                                               
         CLI   NMTHS,8             MONTHS GREATER THAT 8                        
         BNH   SM20                                                             
*                                                                               
         MVI   STAKBUCK,C'Y'       HAVE TO STACK THE BUCKETS                    
         MVC   PCOLWDTH,=Y(BIGCOLLN)                                            
*                                                                               
SM20     ZIC   R1,BLMOBUCK         BUCKET NUMBER                                
         MH    R1,=Y(BUCKLN)                                                    
         ZIC   R0,NMTHS            NUMBER OF COLS NEEDED                        
         LA    R3,CMONTAB-AC66D    TABLE OF BILLING MONTHS                      
*                                                                               
         USING COLTABD,R5                                                       
*                                                                               
SM30     STCM  R2,3,CTTO                                                        
         STH   R1,CTFROM                                                        
         STH   R3,CTHEAD                                                        
         MVI   CTFLN,BUCKLN                                                     
         MVI   CTTLN,STDCOLLN                                                   
         MVI   CTEDIT,EDAMNT                                                    
         MVI   CTTYPE,CTPRIM                                                    
*                                                                               
         LA    R1,BUCKLN(R1)       BUMP BUCKET                                  
         LA    R3,CMONLN(R3)       BUMP DATE                                    
         LA    R5,COLTABLN(R5)     BUMP TABLE POINTER                           
         CLI   STAKBUCK,C'Y'       STACKING DATES                               
         BNE   SM60                                                             
*                                                                               
         BCT   R0,SM40             STACK, IF THERE IS A MONTH LEFT              
*                                                                               
         AH    R2,PCOLWDTH         NO MORE SO BUMP R2 1 MORE TIME               
         B     SMX                 AND EXIT                                     
*                                                                               
SM40     LA    R4,L'XP+5(R2)                                                    
         STCM  R4,3,CTTO                                                        
         STH   R1,CTFROM                                                        
         STH   R3,CTHEAD                                                        
         MVI   CTFLN,BUCKLN                                                     
         MVI   CTTLN,STDCOLLN                                                   
         MVI   CTEDIT,EDAMNT                                                    
         MVI   CTTYPE,0                                                         
         LA    R1,BUCKLN(R1)       BUMP BUCKET                                  
         LA    R3,CMONLN(R3)       BUMP DATE                                    
         LA    R5,COLTABLN(R5)     BUMP TABLE POINTER                           
*                                                                               
SM60     AH    R2,PCOLWDTH                                                      
*                                                                               
         CLI   STAKBUCK,C'Y'       STACKING DATES                               
         BE    *+8                 YES                                          
         LA    R2,1(R2)            + 1 FOR THE BOX                              
*                                                                               
         BCT   R0,SM30                                                          
*                                                                               
SMX      ST    R5,NEXTCOL          A(NEXT COL TAB ENTRY)                        
         ST    R2,NEXTPRT          OFFSET INTO XP OF NEXT COL                   
         B     SCX                                                              
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        BUILD MYFOOT BASED ON REQUEST OPTIONS                                  
*----------------------------------------------------------------------         
*                                                                               
BLDFOOT  NMOD1 0,*FOOT*                                                         
         L     RC,0(R1)                                                         
         LA    R2,MYFOOT                                                        
         MVC   MYFOOT,XSPACES                                                   
         MVC   0(16,R2),=C'REQUEST DETAILS:'                                    
         LA    R2,16(R2)                                                        
         LA    R1,16(R2)                                                        
         CLC   QACCOUNT,SPACES                                                  
         BE    BF05                                                             
*                                                                               
         MVC   0(4,R2),=C'ACC='                                                 
         LA    R2,4(R2)                                                         
         MVC   0(L'QACCOUNT,R2),QACCOUNT                                        
         LA    R1,L'QACCOUNT(R2)                                                
         BAS   RE,COMPUT                                                        
*                                                                               
BF05     CLI   QXJOB,C' '          X JOB FILTER                                 
         BE    BF10                NO                                           
*                                                                               
         MVC   0(13,R2),=C'EXPENSE JOBS='                                       
         LA    R2,13(R2)                                                        
         MVC   0(L'QXJOB,R2),QXJOB                                              
         LA    R1,L'QXJOB(R2)                                                   
         BAS   RE,COMPUT                                                        
*                                                                               
BF10     CLC   QBILGRUP,SPACES                                                  
         BE    BF20                                                             
         MVC   0(9,R2),=C'BILL GRP='                                            
         LA    R2,9(R2)                                                         
         MVC   0(L'QBILGRUP,R2),QBILGRUP                                        
         BAS   RE,BFCHKNEG         CHECK FOR NEGATIVE FILTER                    
         LA    R1,L'QBILGRUP(R2)                                                
         BAS   RE,COMPUT                                                        
*                                                                               
BF20     CLC   QMGROUP,SPACES                                                   
         BE    BF30                                                             
         MVC   0(8,R2),=C'MED GRP='                                             
         LA    R2,8(R2)                                                         
         MVC   0(L'QMGROUP,R2),QMGROUP                                          
         BAS   RE,BFCHKNEG         CHECK FOR NEGATIVE FILTER                    
         LA    R1,L'QMGROUP(R2)                                                 
         BAS   RE,COMPUT                                                        
*                                                                               
BF30     CLC   QSTART,SPACES                                                    
         BE    BF40                                                             
         MVC   0(11,R2),=C'START DATE='                                         
         LA    R2,11(R2)                                                        
         LA    R3,QSTART                                                        
         BAS   RE,BFPDATE                                                       
         LA    R1,8(R2)                                                         
         BAS   RE,COMPUT                                                        
*                                                                               
BF40     CLC   QEND,SPACES                                                      
         BE    BF45                                                             
         MVC   0(9,R2),=C'END DATE='                                            
         LA    R2,9(R2)                                                         
         LA    R3,QEND                                                          
         BAS   RE,BFPDATE                                                       
         LA    R1,8(R2)                                                         
         BAS   RE,COMPUT                                                        
*                                                                               
BF45     CLC   QOPT1(7),SPACES                                                  
         BNE   BF47                                                             
         CLI   QOPT8,C' '                                                       
         BNE   BF47                                                             
         CLC   QOPT9(2),SPACES                                                  
         BE    BF50                                                             
*                                                                               
BF47     MVC   0(8,R2),=C'OPTIONS='                                             
         LA    R2,8(R2)                                                         
         MVC   0(7,R2),QOPT1                                                    
         MVC   7(1,R2),QOPT8                                                    
         MVC   8(2,R2),QOPT9                                                    
         LA    R1,12(R2)                                                        
         BAS   RE,COMPUT                                                        
*                                                                               
BF50     CLI   QFILTER1,C' '                                                    
         BE    BF60                                                             
         MVC   0(5,R2),=C'FLT1='                                                
         LA    R2,5(R2)                                                         
         MVC   0(1,R2),QFILTER1                                                 
         BAS   RE,BFCHKNEG         CHECK FOR NEGATIVE FILTER                    
         LA    R1,1(R2)                                                         
         BAS   RE,COMPUT                                                        
*                                                                               
BF60     CLI   QFILTER2,C' '                                                    
         BE    BF70                                                             
         MVC   0(5,R2),=C'FLT2='                                                
         LA    R2,5(R2)                                                         
         MVC   0(1,R2),QFILTER2                                                 
         BAS   RE,BFCHKNEG         CHECK FOR NEGATIVE FILTER                    
         LA    R1,1(R2)                                                         
         BAS   RE,COMPUT                                                        
*                                                                               
BF70     CLI   QSUBCOMP,C' '                                                    
         BE    BF75                                                             
         MVC   0(5,R2),=C'FLT4='                                                
         LA    R2,5(R2)                                                         
         MVC   0(1,R2),QFILTER4                                                 
         BAS   RE,BFCHKNEG         CHECK FOR NEGATIVE FILTER                    
         LA    R1,1(R2)                                                         
         BAS   RE,COMPUT                                                        
*                                                                               
BF75     CLI   QOFFICE,C' '                                                     
         BE    BF80                                                             
         MVC   0(7,R2),=C'OFFICE='                                              
         LA    R2,7(R2)                                                         
         MVC   0(L'QOFFICE,R2),QOFFICE                                          
         BAS   RE,BFCHKNEG         CHECK FOR NEGATIVE FILTER                    
         LA    R1,L'QOFFICE(R2)                                                 
         BAS   RE,COMPUT                                                        
*                                                                               
BF80     CLI   QMEDIA,C' '                                                      
         BE    BF90                                                             
         MVC   0(9,R2),=C'MED FILT='                                            
         LA    R2,9(R2)                                                         
         MVC   0(1,R2),QMEDIA                                                   
         BAS   RE,BFCHKNEG         CHECK FOR NEGATIVE FILTER                    
         LA    R1,1(R2)                                                         
         BAS   RE,COMPUT                                                        
*                                                                               
BF90     CLI   QBILTYPE,C' '                                                    
         BE    BF100                                                            
         MVC   0(10,R2),=C'BILL TYPE='                                          
         LA    R2,10(R2)                                                        
         MVC   0(1,R2),QBILTYPE                                                 
         BAS   RE,BFCHKNEG         CHECK FOR NEGATIVE FILTER                    
         LA    R1,1(R2)                                                         
         BAS   RE,COMPUT                                                        
*                                                                               
BF100    CLI   QREVERSE,C' '                                                    
         BE    BF110                                                            
         MVC   0(10,R2),=C'REVERSALS='                                          
         LA    R2,10(R2)                                                        
         MVC   0(1,R2),QREVERSE                                                 
         LA    R1,1(R2)                                                         
         BAS   RE,COMPUT                                                        
*                                                                               
BF110    CLC   QSELECT,SPACES                                                   
         BE    BFX                                                              
         MVC   0(13,R2),=C'EXCEPTN LIST='                                       
         LA    R2,13(R2)                                                        
         MVC   0(6,R2),QSELECT                                                  
         LA    R1,6(R2)                                                         
         BAS   RE,COMPUT                                                        
*                                                                               
BFX      DS    0H                  FINISH UP MYFOOT                             
         SH    R1,=H'2'            REMOVE TRAILING COMMA                        
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
BFXX     XIT1                                                                   
*                                                                               
*        PUT A COMMA AFTER THE FIRST NON SPACE BEFORE 0(R1)                     
*                                                                               
COMPUT   DS    0H                                                               
         LA    R4,MYFOOT                                                        
         LA    R4,130(R4)                                                       
         CR    R1,R4                                                            
         BNH   COMP20                                                           
         MVC   0(9,R1),=C'AND MORE.'                                            
         B     BFX                                                              
*                                                                               
COMP20   CLI   0(R1),C' '                                                       
         BNE   COMP30                                                           
         BCTR  R1,0                                                             
         B     COMP20                                                           
*                                                                               
COMP30   MVI   1(R1),C','                                                       
         LA    R1,3(R1)                                                         
         LR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
BFPDATE  MVC   0(2,R2),2(R3)  MM   CONVERT THE YYMMDD DATE AT 0(R3)             
         MVI   2(R2),C'/'          TO A MM/DD/YY DATE AT 0(R2)                  
         MVC   3(2,R2),4(R3)  DD                                                
         MVI   5(R2),C'/'                                                       
         MVC   6(2,R2),0(R3)  YY                                                
         CLI   6(R2),C'9'          IS   THE  YEAR GREATER THAN 1999 ?           
         BNH   BFPDATEX            NO,  SKIP                                    
         ZIC   R4,6(,R2)           CONVERT   X'FA' -> X'F0'                     
         SH    R4,=H'10'                     X'FB' -> X'F1'                     
         STC   R4,6(,R2)                     ...                                
*                                                                               
BFPDATEX BR    RE                  RETURN                                       
*                                                                               
BFCHKNEG TM    0(R2),X'40'         IS THIS A NEGATIVE FILTER                    
         BOR   RE                  NO                                           
         MVC   1(1,R2),0(R2)       BUMP FILTER UP                               
         MVI   0(R2),C'*'                                                       
         OI    1(R2),X'40'         CONVERT TO UPPER                             
         LA    R2,1(R2)            BUMP R2                                      
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*              HEADHOOK - CALLED FROM DDPRINT                                   
*--------------------------------------------------------------------*          
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
BXHOOK   NMOD1 0,**BXHK**                                                       
         L     R2,=A(BOXRC)                                                     
         ICM   RC,15,0(R2)                                                      
         L     R4,ADBOX                                                         
         CLI   3(R1),1             FIRST LINE                                   
         BNE   BX10                                                             
         MVI   BOXSHADE,0          STRIPES OFF                                  
         MVI   BOXSHCH1,X'40'                                                   
         MVI   BOXINIT,0                                                        
         B     BXX                                                              
*                                                                               
BX10     CLI   3(R1),10            AFTER HEADERS                                
         BL    BXX                 NOT YES                                      
         BH    BX20                ALREADY PROCESSED                            
         MVI   BOXSHADE,4          STRIPES ON                                   
         MVI   BOXSHCH1,X'42'                                                   
         MVI   BOXINIT,0                                                        
BX20     L     R3,=A(BOXTAB)                                                    
*                                                                               
BX30     CLI   0(R3),X'FF'         EOT                                          
         BE    BXX                                                              
         CLI   1(R3),C'R'          RIGHT COL                                    
         BE    BX50                                                             
         LA    R3,2(R3)                                                         
         B     BX30                                                             
*                                                                               
BX50     ZIC   R1,0(R3)                                                         
         LA    R1,1(R1)            STARTING OFFSET TO XC                        
         LA    R2,L'XP                                                          
         SR    R2,R1               LENGTH TO XC                                 
         LA    R1,XP(R1)                                                        
         BCTR  R2,0                                                             
         LA    R4,4                CLEAR 4 PRINT LINES                          
*                                                                               
BX60     EX    R2,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
         LA    R1,L'XP(R1)                                                      
         BCT   R4,BX60                                                          
*                                                                               
BXX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
         EJECT ,                                                                
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        0(R3) IS A TOTTAB ENTRY, SET PIECE TO THE ACCOUNT DATA                 
*---------------------------------------------------------------------          
*                                                                               
         USING TOTTABD,R3                                                       
*                                                                               
GETPIECE NMOD1 0,**GETP**                                                       
         L     R2,=A(BOXRC)                                                     
         ICM   RC,15,0(R2)                                                      
         L     R2,0(R1)            A(ACCOUNT YOU WANT A PIECE OF                
         L     R3,4(R1)            A(LEVEL BYTE                                 
         L     R4,=A(KEYTAB)       TABLE DESCRIBING KEY STRUCTURE               
         MVC   PIECE,SPACES                                                     
*                                                                               
GP10     CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),0(R4)                                                    
         BE    GP20                                                             
         LA    R4,KEYTABLN(R4)                                                  
         B     GP10                                                             
*                                                                               
GP20     XR    R1,R1                                                            
         IC    R1,1(R4)            GET OFFSET                                   
         LA    R2,0(R1,R2)         ADDRESS START                                
         IC    R1,2(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PIECE(0),0(R2)      EXTRACT PIECE                                
         LA    R1,1(R1)            RESTORE LEN                                  
         STC   R1,PIECELEN                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                  KEEP IT CLEAN                                
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*-       BUFFALO CSECT NOOPED UNTIL NEEDED                          -*          
*--------------------------------------------------------------------*          
**       BUFF  LINES=300,ROWS=1,COLUMNS=2,COMMENT=15,        X                  
**             FLAVOR=PACKED,KEYLIST=(3,A)                                      
*--------------------------------------------------------------------*          
*                                                                               
         EJECT ,                                                                
*                                                                               
ACCUMS   DC    AL1(LEVTRN)                                                      
         DS    (NACCUMS)PL(BUCKLN)                                              
         DC    AL1(LEVINO)                                                      
         DS    (NACCUMS)PL(BUCKLN)                                              
         DC    AL1(LEVJOB)                                                      
         DS    (NACCUMS)PL(BUCKLN)                                              
         DC    AL1(LEVJOB12)                                                    
         DS    (NACCUMS)PL(BUCKLN)                                              
         DC    AL1(LEVMED)                                                      
         DS    (NACCUMS)PL(BUCKLN)                                              
         DC    AL1(LEVPRO)                                                      
         DS    (NACCUMS)PL(BUCKLN)                                              
         DC    AL1(LEVCLI)                                                      
         DS    (NACCUMS)PL(BUCKLN)                                              
         DC    AL1(LEVOFF)                                                      
         DS    (NACCUMS)PL(BUCKLN)                                              
         DC    AL1(LEVREQ)                                                      
         DS    (NACCUMS)PL(BUCKLN)                                              
         DC    XL1'FF'                                                          
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        THIS TABLE DESCRIBES HOW TO PRINT ACCUMS/SRTBUCKS           *          
*              THE DATA IS: AL2(OFFSET  OF I/P DATA FROM BUCKET START*          
*                           AL1(LENGTH OF I/P DATA                   *          
*                           AL2(OFFSET INTO XP FOR DATA AND HEADER)  *          
*                           AL1(LEN OF O/P FIELD)                               
*                           AL2( OFFSET INTO AC6602 OF HEADER INFO)  *          
*                           AL1(EDIT ROUTINE NUMBER)                 *          
*                                                                    *          
*--------------------------------------------------------------------*          
*                                                                               
COLTAB   DS    0C                                                               
         DS    (MAXCOLS*COLTABLN)C                                              
STDCOLLN EQU   10                                                               
BIGCOLLN EQU   16                                                               
MAXCOLS  EQU   16                                                               
*                                                                               
BOXTAB   DS    0C                                                               
         DS    CL40                WHATEVER                                     
LBOXTAB  EQU   *-BOXTAB                                                         
*                                                                               
         EJECT ,                                                                
TOTTAB   DS    0C                                                               
         DS    (TOTTABNM*TOTTABLN)C                                             
TOTTABNM EQU   10                                                               
         DC    X'FF'                                                            
*                                                                               
DLBUFF   DS    (DLCBXL)C           DLFLD INTERFACE BUFFER                       
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              DSECT FOR PROGRAM                                                
*----------------------------------------------------------------------         
*                                                                               
AC66D    DSECT                                                                  
PRELOC   DS    F                                                                
DOWNLOAD DS    A                                                                
ADLBUFF  DS    A                                                                
ADBOX    DS    A                                                                
SAVERE   DS    A                                                                
SAVEAP   DS    A                                                                
NEXTCOL  DS    A                   A(NEXT COL TAB ENTRY)                        
NEXTPRT  DS    A                   OFFSET INTO XP OF NEXT COL                   
BILLCNT  DS    H                   COUNT OF BILLS ON JOB                        
PTTROFF  DS    H                   OFFSET INTO TRAN REC TO GET DATA             
*                                  (USE PL6 IF NOT DEFINED)                     
*                                                                               
PCOFF    DS    CL1                 OFFSET INTO SRTCHAR OF FIELD                 
PCTROFF  DS    CL1                 OFFSET INTO TRNREC OF CHARACTER DATA         
PCOLWDTH DS    H                   WIDTH OF CURRENT PRINT COL                   
REPTYPE  DS    CL1                                                              
PRTOPT   DS    CL1                 P=PRINT, D=DOWNLOAD                          
STAKBUCK DS    CL1                 Y, DATA IS STACKED IN COLS                   
FIRSTTOT DS    CL1                 Y, SUPRESS MIDLINE PRINTING                  
MIDPEND  DS    CL1                 Y, PRINT MID BEFORE FIRSTS                   
FRSTCOL  DS    CL1                 35, UNLESS INO THEN 45                       
*                                                                               
PTBUCK   DS    CL1                 BUCKET NUMBER TO USE                         
BILLBUCK DS    CL1                 BUCKET FOR BILLING TOTAL                     
BLMOBUCK DS    CL1                 BUCKET FOR BILLING X MONTHS                  
ESTVBUCK DS    CL1                 BUCKET FOR ESTIMATE VALUE                    
BALBUCK  DS    CL1                 BUCKET FOR BALANCE                           
CBILBUCK DS    CL1                 BUCKET FOR CURRENT BILLING                   
BTDBUCK  DS    CL1                 BUCKET FOR BILLING TO DATE                   
*                                                                               
INODATA  DS    CL1                 BUCKET FOR INVOICE NUMBER                    
*                                                                               
PL6      DS    PL6                                                              
NLEVELS  DS    AL1                 NUMBER OF LEVELS DEFINED                     
REPLEVS  DS    XL16                LIST OF LEVELS FOR THIS REPORT               
ACTIVITY DS    XL1                 LEVELS WHICH ARE ACTIVE                      
ACTJOB   EQU   1                                                                
ACTMED   EQU   2                                                                
ACTPRO   EQU   4                                                                
ACTCLI   EQU   8                                                                
ACTREQ   EQU   16                                                               
*                                                                               
COLTYPE  DS    CL1                 CURRENT COLUMN (SEE REPTAB)                  
INVSEQ   DS    CL1                 BILLING ORDER KEEPER                         
INVFILL  DS    CL1                 SET FOR INV DETAIL RECORDS                   
*                                                                               
LEVEL    DS    XL1                                                              
DETLEVEL DS    XL1                 DETAIL LEVEL                                 
LEVREQ   EQU   1                                                                
LEVOFF   EQU   2                                                                
LEVCLI   EQU   3                                                                
LEVPRO   EQU   4                                                                
LEVJOB   EQU   5                                                                
LEVMED   EQU   6                                                                
LEVTRN   EQU   7                                                                
LEVINO   EQU   8                                                                
LEVJOB12 EQU   9                                                                
*                                                                               
DETINDNT DS    AL1                 AMOUNT TO INDENT DETAIL                      
INDNTAMT EQU   2                   AMOUNT OF EACH INDENT                        
*                                                                               
MAXMTHS  EQU   12                                                               
BUCKMTHS DS    CL(DATELN*MAXMTHS)                                               
DATELN   EQU   3                   P'YYMMDD'                                    
*                                                                               
YMD      DS    0PL(DATELN)                                                      
Y        DS    PL1                                                              
M        DS    PL1                                                              
D        DS    PL1                                                              
         ORG   YMD+DATELN                                                       
*                                                                               
CMONTAB  DS    (MAXMTHS)CL(CMONLN)                                              
CMONLN   EQU   20                  C'MMM/YY    '                                
*                                                                               
HDDATA   DS    0CL20                                                            
HDEST    DS    CL20                                                             
HDBAL    DS    CL20                                                             
HDBIL    DS    CL20                                                             
HDINO    DS    CL20                                                             
HDCBIL   DS    CL20                                                             
HDBTD    DS    CL20                                                             
NHEADS   EQU   (*-HDDATA)/L'HDDATA                                              
*                                                                               
NMTHS    DS    XL1                 MONTH SPAN OF THIS REQUEST                   
*                                                                               
LEVSTAT  DS    X                   STATUS BYTE TO CONTROL WRITEING              
*                                  HIGHER LEVEL SORT RECORDS                    
GOTCLI   EQU   X'01'                                                            
GOTPRO   EQU   X'02'                                                            
GOTJOB   EQU   X'04'                                                            
GOTMED   EQU   X'08'                                                            
*                                                                               
CHKNEED  EQU   X'01'                                                            
NEEDED   EQU   X'02'                                                            
*                                                                               
SRTREC   DS    0C                                                               
SRTKEY   DS    CL20                                                             
SRTKEYLN EQU   *-SRTREC                                                         
SRTLEVEL DS    XL1                                                              
SRTACC   DS    CL12                                                             
SRTSEQ   DS    CL1                 INVOICE SEQ, IF NEEDED BY BLDKEY             
SRTFILL  DS    CL1                 TO KEEP RECORDS IN ORDER                     
SRTNAME  DS    CL36                                                             
SRTCHAR  DS    CL20                CHARACTER DATA                               
SRTASTAT DS    CL1                 STATUS BYTE FROM ACCOUNT                     
SRTDATLN EQU   *-SRTREC                                                         
SRTBUCKS EQU   *                                                                
         DS    (NACCUMS)PL(BUCKLN)                                              
SRTRECLN EQU   *-SRTREC                                                         
*                                                                               
HOLDSRT  DS    0C                                                               
         DS    CL(SRTDATLN)                                                     
HOLDBUCK EQU   *                                                                
         DS    (NACCUMS)PL(BUCKLN)                                              
*                                                                               
PREVSRT  DS    CL(SRTRECLN)                                                     
         ORG   PREVSRT                                                          
PREVKEY  DS    CL(SRTKEYLN)                                                     
PREVLEV  DS    XL1                                                              
         ORG   PREVSRT                                                          
         DS    CL(SRTDATLN)                                                     
PREVBUCK EQU   *                                                                
         ORG   PREVSRT+SRTRECLN                                                 
*                                                                               
PREVDKEY DS    CL(L'SRTKEY)        PREVIOUS DETAIL SORT KEY                     
*                                                                               
SORTFLAG DS    CL1                                                              
DONE     EQU   X'FF'                                                            
CLOSED   DS    CL1                                                              
THISACC  DS    CL12                                                             
LASTACC  DS    CL12                                                             
PIECE    DS    CL12                INTERFACE FOR GETPIECE                       
PIECELEN DS    CL1                 LENGTH OF RETURNED PIECE                     
ELCODE   DS    CL1                                                              
ESTVALUE DS    PL6                                                              
CURMNTH  DS    PL6                                                              
JOBBILLS DS    PL6                                                              
JOBCBIL  DS    PL6                                                              
JOBBTD   DS    PL6                                                              
*                                                                               
BILLAMNT DS    PL6                 NET OR GROSS AMOUNT OF BILL                  
MEDPOS   DS    CL1                                                              
TODAY    DS    PL3                                                              
CURMON   DS    PL2                 TODAY OR CURRENT MONTH                       
START3   DS    PL3                                                              
END3     DS    PL3                                                              
START2   DS    XL2                                                              
END2     DS    XL2                                                              
MYFOOT   DS    CL(L'XP)                                                         
DOWNDATA DS    2CL(L'XP)                                                        
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        GOBLOCK BUILT/MAINTAINED OFF OF RC                                     
*----------------------------------------------------------------------         
*                                                                               
GOBLK    DS    0C                                                               
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
GOBLKLN  EQU   *-GOBLK                                                          
*                                                                               
         EJECT ,                                                                
*                                                                               
COLTABD  DSECT                                                                  
CTFROM   DS    AL2                 OFFSET IN SRTREC (BUCK OR CHAR)              
CTFLN    DS    AL1                                                              
CTTO     DS    AL2                                                              
CTTLN    DS    AL1                                                              
CTHEAD   DS    AL2                                                              
CTEDIT   DS    AL1                                                              
EDAMNT   EQU   1                                                                
CHARDATA EQU   2                                                                
CTTYPE   DS    AL1                                                              
CTPRIM   EQU   1                                                                
CTID     DS    CL1                 FIELD ID                                     
COLTABLN EQU   *-COLTABD                                                        
         EJECT ,                                                                
*                                                                               
TOTTABD  DSECT                                                                  
TOTLEN   DS    AL1                                                              
TOTNEED  DS    AL1                 DO I NEED TOTALS AT THIS LEVEL               
TOTTITLE DS    CL11                TITLE OF THIS LEVE ACCOUNT                   
TOTACC   DS    CL12                ACCOUNT THESE TOTALS ARE FOR                 
TOTNAME  DS    CL36                NAME OF  ACCOUNT                             
TOTLEVEL DS    AL1                 LEVEL EQUATE FOR ACCUMS                      
TOTHEAD  DS    CL1                 Y, THIS DATA IN HEADER                       
TOTTYPE  DS    CL1                 TYPE OF TOTAL                                
TOTPN    EQU   1                   PRINT "TOTAL FOR "NAME""                     
TOTPC    EQU   2                   PRINT "TOTAL FOR "CODE""                     
TOTINDNT DS    AL1                 NUM CHARS TO INDENT THIS LEVEL               
TOTTABLN EQU   *-TOTTABD                                                        
         EJECT ,                                                                
*                                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
* ACJOBBERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT ,                                                                
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
         PRINT ON                                                               
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114ACREP6602 07/23/13'                                      
         END                                                                    
