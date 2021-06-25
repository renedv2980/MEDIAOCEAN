*          DATA SET ACREPRM02  AT LEVEL 041 AS OF 03/12/21                      
         TITLE 'REMINDER NOTICES BY EMAIL'                                      
*PHASE ACRM02B                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETCAP                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE XSORT                                                                  
*INCLUDE EMLCHK                                                                 
                                                                                
* NSHE 002 29JAN08 LO01-7243 STOP EMAILS GOING OUT ON NON WORK DAYS             
*              AND MAKE LANGUAGE SOFT                                           
* NSHE 003 06MAR08 LO01-7364 MAKE WIEDEN AND KENNEDY START 2008                 
*                  OT18258L FIX BUG WHEN RUNNING DURING DAY                     
* MPEN 004 03FEB09 <BR22955L> IF LOCLOCK=CALSTRT THEN DON'T SEND EMAIL          
* YNGX 005 18JUN09 <BR25527L> CALL GETCAL IF CHANGING COMPANY                   
* MPEN 007 27JUL09 <LO01-8967> RELINK DUE TO BIGGER COBLOCK                     
* NRAK 008 04FEB10 <BR30624L> DUPLICATE ENTRIES IN EMAILS                       
* MPEN 009 26APR10 <PR000221> REMOVE PID AND AGENCY INFO                        
* JFOS 010 24OCT11 <OT68965l> SKIP COMPANY IF NO 1R LEDGER                      
* MPEN 011 26MAR12 <PR002113> RELINK FOR LARGER COBLOCK                         
* NRAK 012 25OCT11 <PR000606> NOTIFY APPROVERS OF OVERDUES BY PERSON            
* MPEN     27JAN10 <LO01-9742> MERGE US CHANGES                                 
* JFOS 013 31JAN13 <BR20875D> UK:ALLOW CONDO=0, READTIM USES TSW'S ONLY         
*                             REINSTATE UK 'OVERDUE' TESTS                      
*NRAK 014 04MAR13 <OT75296L> BETTER DEBUG, AND VALIDATE EMAIL ADDR.             
* MPEN 022 11FEB13 <PR002557> DEAL WITH MISSING CALENDARS                       
*                             COPY US CHANGES FOR GETURL                        
*                             ENABLE US READTIM CODE FOR UK AS WELL             
* MPEN     24MAY14 <DSRD-1794> PRODUCE HTML EMAILS                              
* MPEN             <DSRD-5373> FIX GERMAN TRANSLATIONS                          
* MPEN             <DSRD-5346> GERMAN TRANSLATION FOR FIND US AT                
*                  <DSRD-5464> ADD GERMAN ADDRESS                               
* MPEN             <DSRD-5644> FIX ISSUE WITH NDO PROFILE                       
* MPEN     27APR15 <PCA001725> FULLY CLEAR LOCDATE                              
* NRAK     28APR15 <DSPCA-1731> bugfix overdue/not started reminders            
* MPEN     02JUN15 <DSRD-7402> MAKE WEB LINK COUNTRY SPECIFIC                   
* MPEN     01JUN16 <RD011140> UPDATE MF EMAILS TO USE FED AUTH                  
* MPEN     14OCT16 <RD013630>  UPDATE IMAGE SOURCE URLS                         
* MPEN 023 03JAN17 <ITIDS-12183> FIX FOR FEDERATED EMAILS                       
* MPEN 024 10JAN17 <DSRD-14632> FIX FOR CANADIAN ADDRESS                        
* MPEN     16FEB17 <DSRD-14946> FIX FOR INCORRECT URL                           
* MPEN 025 16FEB17 <DSRD-14946> FURTHER FIX FOR INCORRECT URLS                  
* MPEN 026 16FEB17 <DSRD-14946> FURTHER FIX FOR INCORRECT URLS                  
* MPEN 027 26JUL17 <DSRD-16469> FIX FOR ZERO HOUR T/S                           
* MPEN 028 28JUL17 <DSRD-16460> FIX FOR ZERO HOURS EDIT HOURS                   
* MPEN 029 08AUG17 <DSRD-15131> IMPROVEMENTS TO EMAIL FORMATTING                
* MPEN     01AUG17 <DSRD-16460> COPY US CHANGES                                 
* MPEN 030 19OCT17 <DSRD-17260> FIX FOR EMAIL FORMATTING                        
* MPEN 031 25OCT17 <DSRD-17260> ADDITIONAL FIX FOR THE ABOVE                    
* MPEN 032 31OCT17 <DSRD-17323> SKIP TERMINATED PIDS                            
* MPEN     06NOV17 <DSPCA-2796> DISABLE BO EMAILS                               
* MPEN 033 10NOV17 <DSRD-17469> Fix for ARM report                              
*NRAK 034 09May18 <ITMF-26033> SPACEND abuse - reorg memory use                 
* MPEN 035 02Jul18 <DSRD-19564> Remove bcc from emails                          
* ABID 036 18DEC18 <DSPCA-2864> MAKE ARM REPORT DOWNLOADABLE                    
* MPEN 037 21JAN19 <DSRD-21358> Amend German address                            
* SGAV 038 16MAR20 <DSPCA-3080> RSTLN3Q -> RSTLN4Q                              
* YNGX 039 30Apr20 <DSRD-24737> Update new US email address                     
* NSHE     05Aug20 <DSRD-27173> Clean up URL logic                              
* JSHA 040 19Oct20 <ITMF-50487> Fix Financial Start date for months > 9         
* JSHA 041 08Mar21 <MOSUP-1360> Fix for test files in URL code                  
**********************************************************************          
* the following features can be controlled using a PARM=? control card:         
*                                                                               
* PARM=Y??????? Y will override the email address to send to (all               
*               person codes). Override read from  QCACCT (2nd req              
*               card) with '@mediaocean.com:' attached automatically.           
*               if no email found, testmail is NOT used either.                 
*               A as above, but use testmail even if no email found.            
* PARM=?n?????? n=1-6 overrides time-of-day/nth reminder run                    
* PARM=??Y????? Y adds the person's 1R account to the email                     
***********************************************************************         
*                                                                               
ACRM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACRM02*                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          GLOBAL STORAGE                               
         LA    RC,SPACEND                                                       
         USING WORKD,RC            LOCAL STORAGE                                
         LARL  R9,GLOBALS                                                       
         USING GLOBALS,R9          RA=A(GLOBAL LITERALS)                        
         LH    RF,=Y(IOAREA-WORKD)                                              
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AIOAREA                                                       
         LH    RF,=Y(IOAREA2-WORKD)                                             
         LA    R2,WORKD                                                         
         AR    R2,RF                                                            
         ST    R2,AIOAREA2                                                      
         LAY   RF,IOAREA3                                                       
         ST    RF,AIOAREA3                                                      
         LAY   RF,TESTMAIL                                                      
         ST    RF,ATMAIL                                                        
         LAY   RF,TESTSTDT                                                      
         ST    RF,ATSTDATE                                                      
*                                                                               
         USING POPTD,RCFFPARM      PARM                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXITR2   XIT1  REGS=(R2)           PRESERVE REGISTER 2 IN THE CALL              
*                                                                               
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
*                                                                               
EXITL    XR    RE,RE               SET CC LOW                                   
         J     EXITCC                                                           
*                                                                               
EXITE    LHI   RE,1                SET CC EQUAL                                 
*                                                                               
EXITCC   CHI   RE,1                                                             
XIT      XIT1  ,                                                                
*                                                                               
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         L     RF,ADCOMFAC                                                      
         MVC   VGETRET,CGETRET-COMFACSD(RF)                                     
*                                                                               
         BRAS  RE,LOAD             LOAD Additional phases                       
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALISING                                                        *         
***********************************************************************         
RUNL     GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(20,TODAYL)                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(13,TDAYRPTD)                             
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAYC)                                
         GOTO1 DATCON,DMCB,(1,TODAYP),(0,TODAYF)                                
         GOTO1 ADDAY,DMCB,(C'Y',TODAYF),WORK,F'-1'                              
         GOTO1 DATCON,DMCB,(0,WORK),(1,MINUS1YR)                                
         GOTO1 ADDAY,DMCB,(C'M',TODAYF),WORK,F'-6'                              
         GOTO1 ADDAY,DMCB,(C'D',TODAYF),TDAYADD1,F'1'                           
         GOTO1 DATCON,DMCB,(0,TDAYADD1),(1,TODAYRPT)                            
*&&US*&& GOTO1 DATCON,DMCB,(1,TODAYRPT),(0,TODAYF)   Reset TODAYF               
         GOTO1 DATCON,DMCB,(1,TODAYRPT),(3,TODAYB)                              
         XC    RUNIND1,RUNIND1                                                  
         XC    RUNIND2,RUNIND2                                                  
*&&UK*&& OI    RCFLAG1,RCFBCURT                                                 
         MVI   FLAG,0                                                           
         MVI   LLANG,X'FF'                                                      
         L     R8,ADMASTC                                                       
         USING MASTD,R8                                                         
         MVC   CTRY,MCCTRY                                                      
         MVC   RCRUN,MCTSTRUN      Save test run indicators                     
         L     RF,MCSSB                                                         
         USING SSOOFFD,RF                                                       
         MVC   RCDSPAC,SSODSPAC                                                 
         DROP  R8,RF                                                            
*                                  ESTABLISH TSAREA                             
         L     RF,=A(30000*AT_RECL)                                             
         ST    RF,ATSABUFS                                                      
         LAY   R2,TSARREC                                                       
         ST    R2,ATSAREA                                                       
*                                                                               
         LA    R5,LOCKLNQ          GETMAIN AREA FOR SORTING ELEMENT             
         L     RE,=A(LOCKMAX)      GROUPS WITHIN A PERSON CODE                  
         MR    R4,RE                                                            
         ST    R5,LOCKTBLN         save of LOCKTAB length                       
         LA    R3,CALLNQ                                                        
         L     RE,=A(CALDMAX)                                                   
         MR    R2,RE                                                            
         STCM  R3,3,CALDAYLN       Save length of CALDAYS                       
         AR    R5,R3                                                            
         AHI   R5,L'IOAREA+L'IOAREA2                                            
         L     RF,ATSABUFS         length of TSAR storage                       
         AR    R5,RF                                                            
         ST    R5,GETMAINL         total GETMAIN length                         
*                                                                               
         L     R0,GETMAINL                                                      
         GETMAIN  R,LV=(0)         ACQUIRE TSAR BUFFER (ABOVE THE LINE)         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QCNTINUE,QCNTQ                                                   
         JNE   RUNL03                                                           
         OI    RUNIND2,RUNEMAIL    SET OVER-RIDE EMAIL-ID DETAILS               
         L     R5,ATMAIL                                                        
         MVC   0(L'TESTMAIL,R5),QCACCT                                          
         OC    0(L'TESTMAIL,R5),SPACES                                          
         LA    RF,L'TESTMAIL-17      L'SUFFIX+1 FOR FIRST INCREMENT             
         LR    R0,R5               SAVE START ADDRESS OF EMAIL ID               
*                                                                               
RUNL02   DS    0H                                                               
         LA    R5,1(R5)                                                         
         CLI   0(R5),C' '                                                       
         JNH   *+8                                                              
         JCT   RF,RUNL02                                                        
         MVC   0(16,R5),=C'@MEDIAOCEAN.COM:'                                    
         SR    R5,R0               LENGTH OF THE INPUT EMAIL                    
         AHI   R5,17               ADD SUFFIX LENGTH                            
         STC   R5,ADDRLEN          TOTAL LENTH OF THE EMAIL ADDRESS             
         LAY   R5,TARPID                                                        
         MVC   0(L'TARPID,R5),QDRAFT                                            
         OC    0(L'TARPID,R5),SPACES                                            
*        LAY   R5,TESTSTDT            not compatible with monacc                
*        CLC   QMOSSTRT,SPACES                  -rework or IDF                  
*        JNH   *+10                                                             
*        MVC   0(L'TESTSTDT,R5),QMOSSTRT                                        
*        LAY   R5,TESTENDT                                                      
*        CLC   QMOSEND,SPACES                                                   
*        JNH   *+10                                                             
*        MVC   0(L'TESTENDT,R5),QMOSEND                                         
*                                                                               
RUNL03   DS    0H                                                               
         L     R3,ADMASTC                                                       
         USING MASTD,R3                                                         
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         MVC   ATSAR,ACMVTSAR        A(TSAROFF)                                 
         ST    R1,AIOAREA3         START OF AREA                                
         LA    RF,L'IOAREA                                                      
         AR    R1,RF                                                            
         ST    R1,AIOAREA4                                                      
         LA    RF,L'IOAREA                                                      
         AR    R1,RF                                                            
         ST    R1,ACALDAYS         caldays area                                 
         ICM   RF,3,CALDAYLN                                                    
         AR    R1,RF                                                            
         ST    R1,ALOCKTAB         LOCKTAB area                                 
         L     RF,LOCKTBLN                                                      
         AR    R1,RF                                                            
         ST    R1,ATSABUF          TSAR buffer area                             
         ST    R1,MCUSRDMP         PRINT THE BUFFER IN A DUMP                   
         A     R1,ATSABUFS                                                      
         ST    R1,MCUSRDMP+4                                                    
         MVC   CTRY,MCCTRY                                                      
         GOTOR INITSAR                                                          
         DROP  R2,R3                                                            
*                                                                               
         USING BIND,RE                                                          
         L     RE,ALOCKTAB                                                      
         XC    BININ,BININ         CLEAR BIN TABLE                              
         LA    R1,LOCKLNQ                                                       
         STCM  R1,15,BINLEN        ENTRY LENGTH                                 
         XC    BINDISP,BINDISP     DISPLACEMENT TO KEY                          
         LA    R1,TIMKLNQ                                                       
         STC   R1,BINKEY+2         LENGTH OF KEY                                
         L     R1,=A(LOCKMAX)                                                   
         STCM  R1,15,BINMAX        MAXIMUM NUMBER OF ENTRIES                    
         XC    BINNUM,BINNUM       NUMBER OF BUCKETS                            
         XC    BINFST,BINFST       DISPLACEMENT TO FIRST BUCKET                 
         DROP  RE                                                               
*                                                                               
         XC    ORIGINUM,ORIGINUM                                                
         CLI   RCFFPARM+1,C'1'     FIRST TIME FOR OVERDUE TIMESHEETS            
         BNE   RUNL90                                                           
         L     R2,AEMLFILE                                                      
         OPEN  ((2),(OUTPUT))                                                   
         ST    R2,ADEMLFIL                                                      
*                                  TSAR buffer setup                            
         XC    IOKEY1,IOKEY1                                                    
         LA    R2,IOKEY1                                                        
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES                                                    
         MVI   CPYKCPY,X'41'  COMPANY CODES RANGE FROM X'41' TO X'FD'           
         B     RUNL06                                                           
* end for 1R for a company - look for next                                      
RUNL04   DS    0H                                                               
         CLI   RMAPPN,C'D'         WANT APPROVER NOTIFICATIONS TODAY?           
         JNE   RUNL05                   NOTE, INCLUDES WEEKLY TOO!              
*                                                                               
         GOTOR PROCAPP                                                          
         GOTOR INITSAR            re-initialise to drop last cpy's              
*                                                     data                      
RUNL05   NI    RUNIND1,X'FF'-(RUNIINI)                                          
         MVI   RUNREPTY,RUNOVDRT  SET REPORT TYPE TO OVERDUE REPORT             
         MVC   CUROFFC,SPACES                                                   
         MVC   IOKEY1,CSVKEY1                                                   
         LA    R2,IOKEY1                                                        
         IC    RF,CPYKCPY                                                       
         LA    RF,1(,RF)                                                        
         STC   RF,CPYKCPY                                                       
*                                                                               
RUNL06   MVC   CSVKEY1,IOKEY1                                                   
         LA    R2,IOKEY1                                                        
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,CPYKEY,CPYKEY,0                       
         BE    RUNL08                                                           
         DC    H'0'                                                             
*                                                                               
RUNL08   CLI   CPYKCPY,X'FE'       REACHED END OF COMPANY RECORDS?              
         BE    RUNL150                                                          
         CLC   CPYKEY+CPYKEND(L'CPYKEY-1),SPACES                                
         BNE   RUNL04              NOT COMPANY RECORD                           
*                                                                               
RUNL10   MVC   CSVKEY1,CPYKEY                                                   
         MVC   COCODE,CPYKCPY                                                   
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,CPYKDA,AIOAREA,DMWORK                  
         BE    RUNL12                                                           
         DC    H'0'                                                             
*                                                                               
RUNL12   GOTOR GETCPY              READ AND EXTRACT COMPANY VALUES              
         BNE   RUNL04                                                           
*                                                                               
*&&UK*&& NI    AC@MUKHT,X'FF'-X'40'                                             
         NI    AC@EMNL2,X'FF'-X'40'  Undo capitilisation                        
         NI    AC@EMNL5,X'FF'-X'40'                                             
         NI    AC@EMNL7,X'FF'-X'40'                                             
         CLI   COMPCTRY,CTRYGER      EMNL8/EMNL9 not start of line              
         BE    RUNL18                                                           
         NI    AC@EMNL8,X'FF'-X'40'                                             
         NI    AC@EMNL9,X'FF'-X'40'                                             
*                                                                               
RUNL18   GOTOR SETURL                                                           
*&&UK*&& NI    AC@PERN2,X'FF'-X'40'                                             
*&&US*&& NI    AC@PERNG,X'FF'-X'40'                                             
         NI    AC@STFPE,X'FF'-X'40'                                             
*                                                                               
         L     RF,AELE280E           Fill out text lines                        
         MVC   0(L'AC@PCHOA,RF),TEXTSPCS                                        
         MVC   0(L'AC@PCHOA,RF),AC@PCHOA                                        
*                                                                               
         L     RF,AELE805E           Find us at...                              
         MVC   0(L'AC@FUSAT,RF),TEXTSPCS                                        
         MVC   0(L'AC@FUSAT,RF),AC@FUSAT                                        
*                                                                               
         L     RF,AEMAIL1                                                       
         MVC   0(L'AC@EMNL1,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL1,RF),AC@EMNL1                                        
*                                                                               
         L     RF,AEMAIL2                                                       
         MVC   0(L'AC@EMNL2,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL2,RF),AC@EMNL2                                        
*                                                                               
         L     RF,AEMAIL3                                                       
         MVC   0(L'AC@EMNL3,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL3,RF),AC@EMNL3                                        
*                                                                               
         L     RF,AEMAIL4                                                       
         MVC   0(L'AC@EMNL4,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL4,RF),AC@EMNL4                                        
*                                                                               
         L     RF,AEMAIL5                                                       
         MVC   0(L'AC@EMNL5,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL5,RF),AC@EMNL5                                        
*                                                                               
         L     RF,AEMAIL6                                                       
         MVC   0(L'AC@EMNL6,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL6,RF),AC@EMNL6                                        
*                                                                               
         L     RF,AEMAIL7                                                       
         MVC   0(L'AC@EMNL7,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL7,RF),AC@EMNL7                                        
*                                                                               
         L     RF,AEMAIL8                                                       
         MVC   0(L'AC@EMNL8,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL8,RF),AC@EMNL8                                        
*                                                                               
         L     RF,AEMAIL9                                                       
         MVC   0(L'AC@EMNL9,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL9,RF),AC@EMNL9                                        
*                                                                               
         L     RF,AEMAILA                                                       
         MVC   0(L'AC@EMNLA,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNLA,RF),AC@EMNLA                                        
*                                                                               
         USING ACTRECD,R2                                                       
RUNL20   LA    R2,IOKEY2                                                        
         MVC   CSVKEY2,IOKEY2                                                   
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,ACTKEY,ACTKEY,0                       
         BE    RUNL24                                                           
         DC    H'0'                                                             
*                                                                               
RUNL22   LA    R2,IOKEY2                                                        
         MVI   ACTKACT+L'ACTKACT,X'FF'   READ NEXT LOW LEVEL                    
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,ACTKEY,ACTKEY,0                       
         BE    RUNL24                                                           
         DC    H'0'                                                             
*                                                                               
RUNL24   CLC   ACTKEY(ACTKACT-ACTKEY),CSVKEY2  IS KEY 1R LEDGER/ACC             
         BNE   RUNL04                                                           
         NI    RUNIND1,X'FF'-(RUNINOCP+RUNINOEM+RUNITERM)                       
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,ACTKDA,AIOAREA,DMWORK                  
         BE    RUNL26                                                           
         DC    H'0'                                                             
*                                                                               
RUNL26   L     R2,AIOAREA                                                       
         LA    R3,ACTKACT                                                       
         LLC   RF,ONERL1L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RE,RF                                                            
         AR    RF,R3                                                            
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         CLC   0(0,RF),SPACES                                                   
         EX    RE,0(R1)                                                         
         BNE   RUNL30                                                           
*                                                                               
RUNL28   GOTOR BRACLR,DMCB,AOFFBRA                                              
         XC    ASDPBRA,ASDPBRA                                                  
         XC    APERBRA,APERBRA                                                  
         GOTOR BRASWO,DMCB,AOFFBRA                                              
         MVC   ADPTBRA,ANXTBRA                                                  
         B     RUNL20                                                           
*                                                                               
RUNL30   LLC   RF,ONERL2L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RE,RF                                                            
         AR    RF,R3                                                            
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         CLC   0(0,RF),SPACES                                                   
         EX    RE,0(R1)                                                         
         BNE   RUNL32                                                           
*                                                                               
         GOTOR BRACLR,DMCB,ADPTBRA                                              
         XC    APERBRA,APERBRA                                                  
         GOTOR BRASWO,DMCB,ADPTBRA                                              
         MVC   ASDPBRA,ANXTBRA                                                  
         B     RUNL20                                                           
*                                                                               
RUNL32   LLC   RF,ONERL3L                                                       
         LLC   RE,ONERL4L                                                       
         SR    RE,RF                                                            
         AR    RF,R3                                                            
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         CLC   0(0,RF),SPACES                                                   
         EX    RE,0(R1)                                                         
         BNE   RUNL34                                                           
*                                                                               
         GOTOR BRACLR,DMCB,ASDPBRA                                              
         GOTOR BRASWO,DMCB,ASDPBRA                                              
         MVC   APERBRA,ANXTBRA                                                  
         B     RUNL20                                                           
*                                                                               
RUNL34   MVC   PERCODE,SPACES                                                   
         MVC   ONERCODE,ACTKACT                                                 
         BASR  R1,0                                                             
         MVC   PERCODE(0),0(RF)                                                 
         EX    RE,0(R1)                                                         
*                                                                               
         LAY   RF,TARPID                                                        
         CLC   0(L'TARPID,RF),SPACES       DEBUG CODE - FILTER BY PID           
         BNH   RUNL35              NO PID GIVEN                                 
         CLC   0(L'TARPID,RF),PERCODE                                           
         BNE   RUNL22                                                           
*                                                                               
RUNL35   GOTOR CSTPRF,ACTKACT                                                   
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         CLI   COEML,NOQ           DO WE WANT REMINDERS                         
         BE    RUNL22              NO                                           
         CLI   COEML,YESQ          DO WE WANT REMINDERS                         
         BE    RUNL22              NO                                           
         CP    COEML,PZERO         DO WE WANT REMINDERS                         
         BE    RUNL22              NO                                           
         ZAP   DUB1,COEML                                                       
         CVB   R4,DUB1                                                          
         STC   R4,NUMREM                                                        
         OI    NUMREM,X'F0'                                                     
         GOTO1 GETDAY,DMCB,TDAYADD1,DUB1                                        
         CLC   DUB1(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,DAYTAB                                                        
RUNL36   CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RF),0(R1)                                                    
         BE    RUNL38                                                           
         LA    RF,DAYTABLN(RF)                                                  
         B     RUNL36                                                           
*                                                                               
RUNL38   LH    R4,1(RF)                                                         
         AR    R4,R3                                                            
         CLI   0(R4),YESQ          IS IT A WORKING DAY                          
         BNE   RUNL22                                                           
*                                                                               
         GOTOR SETODUE                                                          
*&&US*&& BNE   RUNL22                                                           
*                                                                               
         XC    NTFY#(NTFYLNQ),NTFY#                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R1,24               Hours in a day                               
         CLC   CONCM,SPACES                                                     
         BE    RUNL40                                                           
*&&US*&& OC    CONCM,CONCM         ANY NOTIFICATION NEEDED?                     
*&&US*&& BZ    RUNL40                                                           
         ZAP   DUB1,CONCM                                                       
         BZ    RUNL40                                                           
         CVB   RF,DUB1                                                          
         STCM  RF,3,NTFY#                                                       
         MR    RE,R1                                                            
         STCM  RF,3,NTFYHRS                                                     
*                                                                               
RUNL40   CLC   CONCL,SPACES                                                     
         BE    RUNL42                                                           
*&&US*&& OC    CONCL,CONCL         ANY NOTIFICATION NEEDED?                     
*&&US*&& BE    RUNL42                                                           
         ZAP   DUB1,CONCL                                                       
         BZ    RUNL42                                                           
         CVB   RF,DUB1                                                          
         STCM  RF,3,LOCK#                                                       
         MR    RE,R1                                                            
         STCM  RF,3,LOCKHRS                                                     
*                                                                               
RUNL42   GOTOR BRACLR,DMCB,APERBRA                                              
         GOTOR BRASWO,DMCB,APERBRA                                              
*                                                                               
         CLC   ACTKACT,=CL12'FN162MD96907'                                      
         BNE   *+6                                                              
         LR    R1,R1                                                            
         GOTOR EXTOFC,DMCB,ACTKACT EXTRACT OFFICE/DEPT/SUB-DEPT                 
         BNE   RUNL46              NO CHANGE IN OFFICE                          
         GOTOR GETCAL              GET CALENDAR RECORDS                         
         JNE   RUNL04              NO CALENDARS FOUND BUT CONTINUE              
*                                                                               
RUNL46   GOTOR PROCPER,DMCB,PERCODE  PROCESS PERSON                             
         BNE   RUNL22              NOT VALID PERSON                             
         GOTOR PROCDTE             PROCESS DATES INTO TABLE                     
         BNE   RUNL22              NOT A BRANDOCEAN USER                        
         GOTOR READTIM             READ TIME ACCORDING TO DATES                 
         BNE   RUNL22              NO OVERDUE TIME                              
         GOTOR GETEML              GET EMAIL ADDRESS FOR USER                   
         MVC   REPORTNM,SPACES     CLEAR WORK AREA                              
         MVC   REPORTNM(18),AC@TSOVD SAVE REPORT DETAILS                        
*                                                                               
         GOTOR INIREPT             INITIALISE REPORT                            
         TM    RUNIND1,RUNINOEM+RUNINOCP+RUNITERM                               
         BNZ   RUNL50                                                           
         GOTOR INIEML              INITIALISE EMAIL                             
*                                                                               
RUNL50   GOTOR PROCOTM             PROCESS OVERDUE TIME                         
         B     RUNL22                                                           
*                                                                               
RUNL90   L     R2,AEMLFILE                                                      
         OPEN  ((2),(INPUT))                                                    
         ST    R2,ADEMLFIL                                                      
*                                                                               
RUNL92   GET   (R2),EMLFIL                                                      
RUNL94   OC    SVEMLFIL,SVEMLFIL                                                
         BNZ   RUNL98                                                           
         USING CALTABD,R4                                                       
RUNL96   L     R4,ACALDATE                                                      
         L     R0,ACALDATE         CLEAR CALENDAER                              
         LHI   R1,L'CALDATE                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     RUNL100                                                          
RUNL98   CLC   SVEMLFIL(EMLTKY1Q),EMLFIL                                        
         BNE   RUNL104                                                          
RUNL100  MVC   SVEMLFIL,EMLFIL                                                  
         LA    R3,EMLFIL                                                        
         USING EMLTABD,R3                                                       
         MVC   CALENDT,EMLTPEDT                                                 
         MVI   CALSTAT,CALSBRAU+CALSREAD                                        
         LA    R4,CALTABL(R4)                                                   
         B     RUNL92                                                           
*                                                                               
RUNL102  OI    RUNIND2,RUNIEOF                                                  
RUNL104  OC    SVEMLFIL,SVEMLFIL                                                
         BZ    RUNL150                                                          
         LA    R3,SVEMLFIL                                                      
         MVC   USRPID,EMLTPIN                                                   
         CLC   ALPCODE,EMLTALP                                                  
         BE    RUNL106                                                          
         MVC   ALPCODE,EMLTALP                                                  
         GOTOR GSECALP             GET SECURITY ALPHA ID AND COUNTRY            
         JL    RUNL92              NOT FOUND, SKIP RECORD                       
         GOTOR GLDG1R              GET 1R LEDGER INFO                           
RUNL106  MVC   SECCODE,EMLTSEC                                                  
         MVC   PERCODE,EMLTPER                                                  
         MVC   CURODS,EMLTODS                                                   
         MVC   ORIGINUM,EMLTUID                                                 
         CLC   COCODE,EMLTCPY                                                   
         BE    RUNL108             SAME COMPANY?                                
*                                                                               
         USING CPYRECD,R5                                                       
         LA    R5,IOKEY1                                                        
         MVC   CPYKEY,SPACES                                                    
         MVC   COCODE,EMLTCPY                                                   
         MVC   CPYKCPY,EMLTCPY                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,CPYKEY,CPYKEY,0                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,CPYKDA,AIOAREA,DMWORK                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GETCPY              READ COMPANY VALUES                          
         BNE   RUNL92                                                           
         DROP  R5                                                               
*                                                                               
RUNL108  MVC   NUMREM,EMLTNUM                                                   
         CLI   RCFFPARM+1,C'2'     ARE WE RUNNING MID MORNING                   
         BNE   RUNL112                                                          
         CLI   NUMREM,C'3'                                                      
         BNL   RUNL118                                                          
         B     RUNL140                                                          
RUNL112  CLI   RCFFPARM+1,C'3'     ARE WE RUNNING LUNCH TIME                    
         BNE   RUNL114                                                          
         CLI   NUMREM,C'2'                                                      
         BNL   RUNL118                                                          
         B     RUNL140                                                          
RUNL114  CLI   RCFFPARM+1,C'4'     ARE WE RUNNING MID AFTERNOON                 
         BNE   RUNL140                                                          
         CLI   NUMREM,C'4'                                                      
         BNE   RUNL140                                                          
*                                                                               
RUNL118  DS    0H                                                               
         MVC   WORK(L'CURODS),CURODS                                            
         LLC   RF,ONERL3L                                                       
         LA    RE,WORK                                                          
         AR    RF,RE                                                            
         MVC   0(L'PERCODE,RF),PERCODE                                          
         GOTOR CSTPRF,WORK                                                      
         GOTOR SETODUE                                                          
         GOTOR READTIM             READ TIME ACCORDING TO DATES                 
         BNE   RUNL140             NO OVERDUE TIME                              
         GOTOR GETEML              GET EMAIL ADDRESS FOR USER                   
         MVC   REPORTNM,SPACES     CLEAR WORK AREA                              
         MVC   REPORTNM(18),AC@TSOVD SAVE REPORT DETAILS                        
*                                                                               
         GOTOR INIREPT             INITIALISE REPORT                            
         GOTOR SETURL                                                           
         TM    RUNIND1,RUNINOEM+RUNINOCP+RUNITERM                               
         BNZ   RUNL130                                                          
         GOTOR INIEML              INITIALISE EMAIL                             
RUNL130  GOTOR PROCOTM             PROCESS OVERDUE TIME                         
RUNL140  CLC   SVEMLFIL(L'EMLTCPY),EMLFIL  IS IT NEW AGENCY                     
         BE    RUNL142                     NO                                   
         NI    RUNIND1,X'FF'-RUNIINI       YES - RESET REPORT INITIAL           
RUNL142  NI    RUNIND1,X'FF'-(RUNINOCP+RUNINOEM+RUNITERM)                       
         TM    RUNIND2,RUNIEOF                                                  
         BZ    RUNL96                                                           
*                                                                               
RUNL150  L     R2,ADEMLFIL            CLOSE FILE LIST DATASET                   
         CLOSE ((2))                                                            
         CLI   RCWRITE,NOQ                                                      
         BE    RUNL160                                                          
         TM    RUNIND1,RUNIIEML                                                 
         BZ    RUNL160                                                          
         GOTO1 VSMTP,DMCB,('SMTPAEND',0) DETACH FROM JESMAIL                    
*                                                                               
         USING BIND,R1                                                          
RUNL160  L     R1,ALOCKTAB         R2=A(CLIENT TABLE)                           
         ICM   R0,15,BININ                                                      
         BZ    RUNLX                                                            
         USING LOCKD,R2                                                         
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         OI    FLAG,FLGLCKRP       Show that we are running LOCKED rpt          
         MVI   RUNREPTY,RUNLCKRT   PROCESSING LOCKED REPORT                     
         MVI   COCODE,0            Init Saved Area for Company Code             
*                                                                               
RUNL170  CLC   LOCKCPY,COCODE      Same Company?                                
         BE    RUNL180                                                          
         NI    RUNIND1,X'FF'-(RUNIINI)                                          
         MVC   COCODE,LOCKCPY                                                   
         MVC   ORIGINUM,LOCKUID    COMPANY PRINCIPAL ID NUMBER                  
         MVC   REPORTNM,SPACES     CLEAR WORK AREA                              
         MVC   REPORTNM(L'AC@LAPRT),AC@LAPRT SAVE REPORT DETAILS                
*                                                                               
         GOTOR INIREPT             INITIALISE REPORT                            
         MVI   RCSUBPRG,2                                                       
*                                                                               
RUNL180  MVC   ONERCODE,SPACES                                                  
         MVC   ONERCODE(L'LOCKODS),LOCKODS                                      
         SR    R1,R1                                                            
         LA    RE,ONERCODE+L'ONERCODE-1                                         
RUNL190  CLI   0(RE),X'40'                                                      
         BH    RUNL200                                                          
         AHI   R1,1                                                             
         AHI   RE,-1                                                            
         B     RUNL190                                                          
*                                                                               
RUNL200  AHI   R1,-1                                                            
         MVC   1(0,RE),LOCKPER     Move in person code                          
         EX    R1,*-6                                                           
*                                                                               
         CLI   RMDOWN,YESQ         DOWNLOAD REQUEST ?                           
         JNE   RUNL202             NO, MUST BE PRINTABLE FORMAT                 
*                                  YES                                          
         MVI   RCSUBPRG,4          SET SUB-PROGRAM TYPE TO 4                    
         GOTOR PRTDOWN,DMCB,LOCKD  ADD DATA TO DOWNLOADABLE REPORT              
         J     RUNL204             CONTINUE                                     
*                                                                               
RUNL202  DS    0H                                                               
         MVC   P+1(L'LOCKPID),LOCKPID                                           
         MVC   P+10(L'LOCKFNM),LOCKFNM                                          
         MVC   P+26(L'LOCKLNM),LOCKLNM                                          
         MVC   P+84(6),=CL6'LOCKED' User Has been Locked                        
         CLI   RCFFPARM+2,YESQ     OVERRIDE TO PUT OUT 1R ACCOUNT               
         BNE   *+10                                                             
         MVC   PSECOND+1(L'ONERCODE),ONERCODE                                   
         GOTO1 DATCON,DMCB,(1,LOCKPEDT),(21,P+68)                               
         GOTO1 ACREPORT                                                         
         MVC   PSECOND,SPACES                                                   
*                                                                               
RUNL204  DS    0H                                                               
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY3                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COCODE                                                   
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,ONERCODE                                                 
         MVC   CSVKEY3,ACTKEY                                                   
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,ACTKEY,ACTKEY,0               
         MVC   SVDA,ACTKDA         Save off Disk Address for PUTREC             
*                                                                               
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,ACTKDA,AIOAREA3,DMWORK                 
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         L     R3,AIOAREA3                                                      
         LA    R4,ACTRFST                                                       
         USING RSTELD,R4                                                        
         XR    R1,R1                                                            
RUNL210  CLI   RSTEL,0                                                          
         BE    RUNL225                                                          
         CLI   RSTEL,RSTELQ                                                     
         BE    RUNL230                                                          
         IC    R1,RSTLN                                                         
         AR    R4,R1                                                            
         B     RUNL210                                                          
*                                                                               
RUNL225  LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   RSTEL,RSTELQ                                                     
*&&UK*&& MVI   RSTLN,RSTLN4Q                                                    
*&&US*&& MVI   RSTLN,RSTLN3Q                                                    
         OI    RSTSTAT7,RSTLCKTS   Turn on LOCKED bit                           
         GOTO1 =V(HELLO),DMCB,(C'P',ACCMST),ACTRECD,ELEMENT                     
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         BE    RUNL240             NO CPYEL FOUND                               
         DC    H'0'                                                             
*                                                                               
*&&UK                                                                           
RUNL230  CLI   RSTLN,RSTLN4Q       Make sure that element is big enough         
*&&                                                                             
*&&US                                                                           
RUNL230  CLI   RSTLN,RSTLN3Q       Make sure that element is big enough         
*&&                                                                             
         BL    RUNL235                                                          
         OI    RSTSTAT7,RSTLCKTS   Turn on LOCKED bit                           
         B     RUNL240                                                          
*                                                                               
RUNL235  LLC   RF,RSTLN            Make element longer if not long              
         BCTR  RF,0                enough                                       
         MVC   ELEMENT(0),RSTELD                                                
         EX    RF,*-6                                                           
         MVI   RSTEL,X'FF'                                                      
*                                                                               
         LA    R4,ELEMENT                                                       
*&&UK*&& MVI   RSTLN,RSTLN4Q                                                    
*&&US*&& MVI   RSTLN,RSTLN3Q                                                    
         OI    RSTSTAT7,RSTLCKTS   Turn on LOCKED bit                           
         GOTO1 =V(HELLO),DMCB,(C'P',ACCMST),ACTRECD,ELEMENT                     
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         BE    *+6                 NO CPYEL FOUND                               
         DC    H'0'                                                             
         GOTO1 =V(HELLO),DMCB,(C'D',ACCMST),(X'FF',ACTRECD),0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RUNL240  CLI   RCWRITE,NOQ                                                      
         BE    RUNL250                                                          
         CLI   RCRUN,RUNTST        IS this a TEST RUN?                          
         BE    RUNL250                                                          
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',ACCMST,SVDA,AIOAREA3,DMWORK            
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
RUNL250  LA    R2,LOCKLNQ(R2)                                                   
         BCT   R0,RUNL170                                                       
         DROP  R3,R4                                                            
         EJECT                                                                  
                                                                                
*                                                                               
RUNLX    DS    0H                                                               
         CLI   RMDOWN,YESQ         DOWNLOAD REPORT?                             
         JNE   XIT                                                              
         GOTOR DOWN,DMCB,DOWNCLOS  CLOSE REPORT                                 
*                                                                               
*        L     R0,GETMAINL            Release storage                           
*        FREEMAIN R,A=(1),LV=(0)                                                
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* LOAD PHASES                                                        *          
**********************************************************************          
         SPACE 1                                                                
LOAD     NTR1  BASE=*,LABEL=*                                                   
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         MVC   DUB,=CL8'GETURL'    ACGETURL                                     
         MVC   DUB+6(1),MCTEST3    LOAD EITHER LIVE OR TEST                     
**TEST CODE                                                                     
         MVI   DUB+6,C'A'                                                       
**TEST CODE                                                                     
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   LOAD10                                                           
         MVC   DUB,=CL8'GETURL'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOAD10   MVC   VGETAUL,4(R1)       A(ACGETURL)                                  
         MVC   MCAPHAS3,4(R1)      SET UP PATCHABILITY                          
*                                                                               
         ICM   R0,15,0(R1)         LENGTH OF PHASE                              
         L     R1,FULL                                                          
         AR    R0,R1               ADD LENGTH OF PREVIOUS PHASE                 
         ST    R0,FULL                                                          
         ICM   RF,15,WORD          A(GETFORM)                                   
         STCM  RF,15,MCUSRDMP                                                   
         AR    RF,R0                                                            
         LA    RF,4095(RF)                                                      
         STCM  RF,15,MCUSRDMP+4                                                 
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE REPORT AND EMAIL                                         *         
***********************************************************************         
         SPACE 1                                                                
INIREPT  NTR1  BASE=*,LABEL=*                                                   
         TM    RUNIND1,RUNIINI     HAVE WE INITIALISED FOR THIS AGENCY          
         JNZ   EXITE                                                            
*                                                                               
         CLI   POPTPQ,C'Y'         SUPPRESS FROM GOING TO PRINT QUEUE?          
         JE    INIREPT1            . YES                                        
         MVI   POPTPRNT,C'N'       NEVER PRINT EMAILS IF SENDING TO PQ          
*                                                                               
         CLI   RCWRITE,C'N'        DON'T WRITE TO COMPANY'S PRINT QUEUE         
         JE    INIREPT1            IF WRITE=NO                                  
*                                                                               
         L     RF,REMOTEC          REPORTING ON CLIENT PQ                       
         USING REMOTED,RF                                                       
         XC    REMOTKEY,REMOTKEY   START A NEW REPORT                           
         MVI   REMOTSYS,C'A'       A=ACCOUNTING                                 
         MVC   REMOTPRG,=C'RM'     THE 'RM' FROM PHASE NAME                     
         MVC   REMOTJID,REMOTSYS                                                
         MVC   REMOTDST,ORIGINUM   USE PRINCIPAL ID NUMBER FROM CO RECS         
         MVC   REMOTFRM,=C'DEF '                                                
         MVC   REMOTSUB+1(3),=C'DEF'                                            
*                                                                               
         TM    FLAG,FLGLCKRP       Are we running LOCKED rpt?                   
         JNO   *+16                                                             
         MVC   REMOTFRM,=C'LCK '                                                
         MVC   REMOTSUB+1(3),=C'LCK'                                            
         MVI   RCSUBPRG,0                                                       
*                                                                               
         CLI   RMDOWN,YESQ         DOWNLOAD REPORT REQUEST ?                    
         JNE   INIREPT1            NO , CONTINUE                                
*                                                                               
         OI    REMOTTYP,REMOTDLQ   SET REPORT TYPE TO DOWNLOAD                  
         CLI   RUNREPTY,RUNAPRPT   ARE WE RUNNING APPROVER REPORT ?             
         JNE   INIREPT1                                                         
*                                                                               
         MVC   REMOTFRM,=C'APR '                                                
         MVC   REMOTSUB+1(3),=C'APR'                                            
*                                                                               
INIREPT1 DS    0H                                                               
         OI    RUNIND1,RUNIINI                                                  
         CLI   RMDOWN,YESQ         DOWNLOAD REPORT REQUEST ?                    
         JNE   INIREPT2            NO , CONTINUE                                
*                                                                               
         MVC   P(L'RPORTLBL),RPORTLBL  PRINT REPORT LABEL                       
         MVC   P+L'RPORTLBL(L'REPORTNM),REPORTNM  SAVE REPORT NAME              
         MVC   P+70(L'DATELEBL),DATELEBL PRINT DATE LABEL                       
         MVC   P+70+L'DATELEBL(L'TDAYRPTD),TDAYRPTD   DATE DETAILS              
         MVC   PSECOND,SPACES      INITALIZE TO SPACES                          
         MVC   PTHIRD(L'AGNCYLBL),AGNCYLBL PRINT AGENCY LABEL                   
         MVC   PTHIRD+L'AGNCYLBL(L'CONAME),CONAME PRINT COMPANY NAME            
*                                                                               
         GOTOR ACREPORT                                                         
         GOTOR SETHDRS             SET DOWNLOAD REPORT HEADINGS                 
         J     EXITE               EXIT                                         
*                                                                               
INIREPT2 DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         J     EXITE                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE EMAIL                                                    *         
***********************************************************************         
         SPACE 1                                                                
INIEML   NTR1  BASE=*,LABEL=*                                                   
         TM    RUNIND1,RUNIIEML                                                 
         JNZ   EXITE                                                            
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         JE    INIEML1                                                          
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAINI',0) INITIALISE JESMAIL                     
         GOTO1 VSMTP,DMCB,('SMTPASLL',0)        SET LONG LINES                  
         GOTO1 VSMTP,DMCB,('SMTPHTMH',0) SET HTML HEADER                        
         GOTO1 VSMTP,DMCB,('SMTPAFR2',0),(L'AUFROM,AUFROM) Set from add         
*                                                                               
INIEML1  OI    RUNIND1,RUNIIEML                                                 
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE THE HEADINGS                                             *         
***********************************************************************         
         SPACE 1                                                                
SETURL   NTR1  BASE=*,LABEL=*                                                   
         XC    AENDURL,AENDURL                                                  
*                                                                               
         L     RF,AEMAIL1          SQUASH UP THE DISCLAIMER LINES TO            
         GOTO1 SQUASHER,DMCB,0(RF),160 MAKE NEAT AND TIDY                       
         L     RF,AEMAIL3                                                       
         GOTO1 SQUASHER,DMCB,0(RF),160                                          
         L     RF,AEMAIL6                                                       
         GOTO1 SQUASHER,DMCB,0(RF),160                                          
         L     RF,AEMAIL8                                                       
         GOTO1 SQUASHER,DMCB,0(RF),160                                          
*                                                                               
         LA    RF,AAURURL                                                       
         GOTO1 VGETAUL,DMCB,(RF)                                                
         L     RE,ADMASTC          GET THE UTL ADDRESS                          
         L     RE,MCUTL-MASTD(RE)                                               
*                                                                               
         L     R3,AAURURL          PULL URL FROM TABLE                          
         USING AGYURLD,R3                                                       
SETURL04 CLI   AGUSE,0             TEST OR LIVE                                 
         JE    SETURL06            LIVE                                         
         CLC   AGUSE,4(RE)         FIND MATCH ON SE NUMBER FOR TEST             
         JNE   SETURL08                                                         
         CLI   RCDSPAC,C'A'        ARE WE RUNNING FOR PRODUCTION?               
         JNE   SETURL10            IF NOT - GO AHEAD                            
         J     SETURL08                                                         
*                                                                               
SETURL06 CLI   AGUAA,0             END OF TABLE, USE DEFAULT                    
         JE    SETURL10                                                         
         CLC   AGUAA,ALPCODE       MATCH ON AGENCY ALPHA                        
         JE    SETURL10                                                         
SETURL08 AHI   R3,AGYURLQ          NO MATCH, NEXT ENTRY                         
         J     SETURL04                                                         
*                                                                               
SETURL10 LA    R0,BLDFURL                                                       
         LHI   R1,L'BLDFURL                                                     
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         XC    BLDLEN,BLDLEN                                                    
*                                                                               
         LA    R2,BLDFURL                                                       
         LA    RF,AGUHTT                                                        
         LHI   RE,L'AGUHTT-1                                                    
         CLI   AGUFED,YESQ         Is this a system with a url prefix?          
         JNE   SETURL12                                                         
         TM    CPXSTA,CPXFEDAT     Is federated auth in use?                    
         JZ    SETURL12                                                         
         LA    RF,HTTP             Always http for federated urls               
         LHI   RE,L'HTTP-1                                                      
*                                                                               
SETURL12 BASR  R1,0                                                             
         MVC   0(0,R2),0(RF)                                                    
         EX    RE,0(R1)                                                         
         AHI   R2,L'AGUHTT                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         TM    CPXSTA,CPXFEDAT     Is federated auth in use?                    
         JNZ   SETURL14                                                         
         CLC   AGUENV,SPACES       Any environment?                             
         JNH   SETURL14                                                         
         MVC   0(L'AGUENV,R2),AGUENV                                            
         AHI   R2,L'AGUENV                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
*                                                                               
SETURL14 CLI   AGUFED,YESQ         Is this a system with a url prefix?          
         JNE   SETURL16                                                         
         TM    CPXSTA,CPXFEDAT     Is federated auth in use?                    
         JZ    SETURL16                                                         
         MVC   0(L'AGUFEDP,R2),AGUFEDP                                          
         AHI   R2,L'AGUFEDP                                                     
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         MVC   0(L'FEDURL,R2),FEDURL                                            
         AHI   R2,L'FEDURL                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         MVC   0(L'AGUFEDS,R2),AGUFEDS                                          
         AHI   R2,L'AGUFEDS                                                     
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
*                                  Squash and save new url length               
SETURL16 MVC   0(L'AGUURL2,R2),AGUURL2                                          
         AHI   R2,L'AGUURL2                                                     
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         LA    RE,BLDFURL                                                       
         SR    R2,RE                                                            
         STC   R2,BLDLEN                                                        
*                                                                               
         L     R5,AELE276E         MOVE IN BASE URL                             
         MVC   0(L'ELEM276E,R5),TEXTSPCS                                        
         CLC   AGUURL2,SPACES                                                   
         JNH   SETURL18                                                         
         LA    R2,BLDFURL                                                       
         LLC   RF,BLDLEN                                                        
*                                                                               
SETURL18 CHI   RF,L'ELEM276E       Move url to html                             
         JNH   SETURL20                                                         
         MVC   0(L'ELEM276E,R5),0(R2)                                           
         SHI   RF,L'ELEM276E                                                    
         LA    R2,L'ELEM276E(R2)                                                
         LA    R5,L'ELEM276E(R5)                                                
         J     SETURL18                                                         
*                                                                               
SETURL20 BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R5),0(R2)                                                    
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R5,RF                                                            
         ST    R5,AENDURL                                                       
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* READ FOR COST PROFILE                                               *         
***********************************************************************         
         SPACE 1                                                                
CSTPRF   NTR1  BASE=*,LABEL=*                                                   
         L     R0,ACOBLOCK                                                      
         LR    R2,R1                A(1R ACCOUNT)                               
*        USING ACTRECD,R2                                                       
         LA    R1,COBLOCKX-COBLOCK                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,COCODE                                                    
         MVC   COKMTHD,SPACES                                                   
         LR    RF,R2                                                            
         LLC   R1,ONERL1L                                                       
         AHI   R1,-1                                                            
         MVC   COKOFC(0),0(RF)                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in 1R                      
         LLC   RE,ONERL2L                                                       
         LLC   R1,ONERL1L                                                       
         SR    RE,R1                                                            
         LR    R1,RE                                                            
         AHI   R1,-1                                                            
         MVC   COKDPT(0),0(RF)                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in 1R                      
         LLC   RE,ONERL3L                                                       
         LLC   R1,ONERL2L                                                       
         SR    RE,R1                                                            
         LR    R1,RE                                                            
         AHI   R1,-1                                                            
         MVC   COKSDT(0),0(RF)                                                  
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in 1R                      
         LLC   RE,ONERL4L                                                       
         LLC   R1,ONERL3L                                                       
         SR    RE,R1                                                            
         LR    R1,RE                                                            
         AHI   R1,-1                                                            
         MVC   COKPER(0),0(RF)                                                  
         EX    R1,*-6                                                           
         GOTO1 VGETCAP,DMCB,ACOBLOCK                                            
         CLI   COSTATUS,0                                                       
         JE    EXITE                                                            
         DC    H'0'                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* EXTRACT OFFICE AND OFFICE DEPT SUB-DEPT                             *         
***********************************************************************         
         SPACE 1                                                                
EXTOFC   NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)                                                         
         MVC   CURDPT,SPACES                                                    
         MVC   CURODS,SPACES                                                    
         MVC   CURSDP,SPACES                                                    
         LLC   RF,ONERL3L                                                       
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   CURODS(0),0(R3)     EXTRACT CURRENT OFF/DEPT/SUB-DEPT            
         EX    RF,0(R1)                                                         
         LLC   RF,ONERL1L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RE,RF                                                            
         AR    RF,R3                                                            
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         MVC   CURDPT(0),0(RF)     EXTRACT CURRENT DEPT                         
         EX    RE,0(R1)                                                         
         LLC   RF,ONERL2L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RE,RF                                                            
         AR    RF,R3                                                            
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         MVC   CURSDP(0),0(RF)     EXTRACT CURRENT SUB DEPT                     
         EX    RE,0(R1)                                                         
         LLC   RF,ONERL1L                                                       
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         CLC   CUROFFC(0),0(R3)    COMPARE TO PREVIOUS OFFICE                   
         EX    RF,0(R1)                                                         
         JE    EXITH               SAME AS BEFORE                               
         BASR  R1,0                                                             
         MVC   CUROFFC(0),0(R3)    DIFFERENT - EXTRACT IT                       
         EX    RF,0(R1)                                                         
         OC    CUROFFC,SPACES                                                   
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* GET CALENDAR FOR MINUS1YR AND TODAYP                                *         
***********************************************************************         
         SPACE 1                                                                
GETCAL   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ACALDATE                                                      
         L     R0,ACALDATE         CLEAR CALENDAER                              
         LHI   R1,L'CALDATE                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING CASRECD,R2                                                       
         LA    R2,IOKEY3                                                        
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKCPY,COCODE                                                   
         MVC   CASKEMOA,EM1DATE                                                 
         MVC   CASKSMOA,STM1DAT                                                 
         MVC   CASKOFC,CUROFFC                                                  
         MVC   CSVKEY3,CASKEY                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,CASKEY,CASKEY,0                       
         JE    GETCAL10                                                         
                                                                                
         MVC   CASKEY,CSVKEY3                                                   
         MVC   CASKOFC,SPACES                                                   
                                                                                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,CASKEY,CASKEY,0                       
         JNE   GETCAL30                                                         
                                                                                
GETCAL10 GOTO1 DATAMGR,DMCB,DMGET,ACCMST,CASKDA,AIOAREA3,DMWORK                 
         JE    GETCAL20                                                         
         DC    H'0'                                                             
*                                  12 MONTHS AGO                                
GETCAL20 GOTOR EXTCAL                                                           
*                                                                               
GETCAL30 LA    R2,IOKEY3                                                        
         XC    CASKEY,CASKEY       SEARCH FOR CALENDAR FOR TODAY'S DATE         
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKCPY,COCODE                                                   
         MVC   CASKEMOA,ENDDATE                                                 
         MVC   CASKSMOA,STRTDAT                                                 
         MVC   CASKOFC,CUROFFC                                                  
         MVC   CSVKEY3,CASKEY                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,CASKEY,CASKEY,0                       
         JE    GETCAL40                                                         
                                                                                
         MVC   CASKEY,CSVKEY3                                                   
         MVC   CASKOFC,SPACES                                                   
                                                                                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,CASKEY,CASKEY,0                       
         JE    GETCAL40            ANYWAY CAN SEARCH FOR LAST YEAR              
*&&UK*&& MVI   RTERRCC,5           SET ERROR CODE SO PRODUCT CAN                
         MVC   P+1(L'CALERR),CALERR FOLLOW UP                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(2),STRTDAT                                                  
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(20,WORK+10)                                
         MVC   CALYEAR,WORK+10                                                  
         GOTO1 HEXOUT,DMCB,COCODE,P+2+L'CALERR,1                                
         LA    RF,P+5+L'CALERR                                                  
         MVC   0(L'CALERR2,RF),CALERR2                                          
         GOTOR ACREPORT                                                         
         J     EXITL                                                            
*                                                                               
GETCAL40 GOTO1 DATAMGR,DMCB,DMGET,ACCMST,CASKDA,AIOAREA3,DMWORK                 
         JE    GETCAL50                                                         
         DC    H'0'                FATAL ERROR                                  
                                                                                
GETCAL50 GOTOR EXTCAL                                                           
         XC    ANXTCAL,ANXTCAL                                                  
         J     EXITE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT PERIOD INFO FROM CALENDAR RECORDS                           *         
***********************************************************************         
         SPACE 1                                                                
EXTCAL   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOAREA3                                                      
         USING CASRECD,R3                                                       
         USING CALTABD,R4                                                       
         L     R4,ACALDATE                                                      
         OC    ANXTCAL,ANXTCAL                                                  
         JZ    EXCAL10                                                          
         L     R4,ANXTCAL                                                       
EXCAL10  LA    R3,CASRFST                                                       
         USING TMPELD,R3                                                        
         XR    R0,R0                                                            
EXCAL20  CLI   TMPEL,0                                                          
         JE    EXCAL50                                                          
         CLI   TMPEL,TMPELQ                                                     
         JE    EXCAL40                                                          
EXCAL30  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         J     EXCAL20                                                          
*                                                                               
EXCAL40  DS    0H                                                               
         CLC   TMPSTART,TODAYRPT   Always include current period                
         JH    EXCAL30                                                          
         CLC   TMPEND,MINUS1YR                                                  
         JL    EXCAL30                                                          
         MVC   CALENDT,TMPEND                                                   
         MVC   CALSTRT,TMPSTART                                                 
         CLC   STC1DAT,CALSTRT     Set to lowest Start Date                     
         JL    *+10                                                             
         MVC   STC1DAT,CALSTRT                                                  
         LA    R4,CALTABL(R4)                                                   
         J     EXCAL30                                                          
*                                                                               
EXCAL50  ST    R4,ANXTCAL                                                       
         J     EXITE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*  GET SECURITY ALPHA CODE                                                      
***********************************************************************         
         SPACE 1                                                                
GSECALP  NTR1  BASE=*,LABEL=*                                                   
         MVC   SECCODE,ALPCODE                                                  
         XC    IOKEY3,IOKEY3                                                    
         LA    R6,IOKEY3                                                        
         USING CT5REC,R6           SYSTEM ACCESS RECORD                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
         MVC   CT5KALPH,SECCODE                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY3,AIOAREA3                       
         L     R6,AIOAREA3                                                      
         CLC   IOKEY3(L'CT5KEY),0(R6)                                           
         JNE   GSECALPN                                                         
*                                                                               
         LA    R6,CT5DATA                                                       
         USING CTSEAD,R6                                                        
         XR    R0,R0                                                            
GSALP02  CLI   CTSEAEL,0           TEST EOR                                     
         JE    GSECALPX                                                         
         CLI   CTSEAEL,CTSEAELQ    X'B8'                                        
         JE    GSALP08                                                          
         CLI   CTSEAEL,CTAGDELQ                                                 
         JE    GSALP10                                                          
GSALP06  IC    R0,CTSEALEN                                                      
         AR    R6,R0                                                            
         J     GSALP02                                                          
                                                                                
GSALP08  MVC   SECCODE,CTSEAAID    AGENCY ALPHA ID                              
         J     GSALP06                                                          
*                                                                               
         USING CTAGDD,R6                                                        
GSALP10  MVC   COMPCTRY,CTRY       DEFAULT IS DDS COUNTRY CODE                  
         CLI   CTAGDLEN,CTAGDCTY-CTAGDD                                         
         JL    GSALP06                                                          
         MVC   COMPCTRY,CTAGDCTY                                                
         L     R1,ADMASTC                                                       
         USING MASTD,R1                                                         
         MVC   MCCTRY,COMPCTRY                                                  
         MVC   RCCTRY,COMPCTRY                                                  
         MVC   CTRY,COMPCTRY                                                    
         J     GSALP06                                                          
         DROP  R1                                                               
                                                                                
GSECALPX J     EXITE                                                            
GSECALPN J     EXITL                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*  GET LANGUAGE CODE                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETLANG  NTR1  BASE=*,LABEL=*                                                   
         LA    R6,IOKEY3                                                        
         USING CTIREC,R6           SYSTEM ACCESS RECORD                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,ORIGINUM                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY3,AIOAREA3                       
         L     R6,AIOAREA3                                                      
         CLC   IOKEY3(L'CTIKEY),0(R6)                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,CTIDATA                                                       
         USING CTAGYD,R6                                                        
         XR    R0,R0                                                            
GLANG02  CLI   CTAGYEL,0           TEST EOR                                     
         JE    GETLANGX                                                         
GLANG04  CLI   CTAGYEL,CTAGYELQ                                                 
         JNE   *+10                                                             
         MVC   COMPLANG,CTAGYLNG                                                
*                                                                               
         IC    R0,CTAGYLEN                                                      
         AR    R6,R0                                                            
         J     GLANG02                                                          
*                                                                               
GETLANGX J     EXITE                                                            
         DROP  R6                                                               
***********************************************************************         
*  GET LANGUAGE/SECURITY TO RESOLVE DICTIONARY                        *         
***********************************************************************         
         SPACE 1                                                                
GTLGSEC  NTR1  BASE=*,LABEL=*                                                   
         GOTOR GETLANG                                                          
         CLC   COMPLANG,LLANG      TEST LANGUAGE CHANGE                         
         JE    GTLGSECX            NO                                           
         GOTO1 ADDICTAT,DMCB,C'LL  ',DATI,DATO RESOLVE DATA DICT ITEMS          
         ORG   *-2                                                              
         MVC   3(L'COMPLANG,R1),COMPLANG SET LANGUAGE FOR DICTATE               
         BASR  RE,RF                                                            
         MVC   LLANG,COMPLANG      SAVE LANGUAGE CODE                           
         MVC   RCLANG,COMPLANG                                                  
         L     R8,ADMASTC                                                       
         USING MASTD,R8                                                         
         MVC   MCLANG,COMPLANG                                                  
*                                                                               
         L     RF,AELE800E         Put year in copyright                        
         MVC   0(L'ELEM800E,RF),TODAYL                                          
*                                                                               
GTLGSECX J     EXITE                                                            
         DROP  R8                                                               
***********************************************************************         
* GET 1R LEDGER LENGTHS                                               *         
***********************************************************************         
         SPACE 1                                                                
GLDG1R   NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY2,IOKEY2                                                    
         LA    R6,IOKEY2                                                        
         USING LDGRECD,R6          READ 1R LEDGER TO GET LENGTHS                
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,COCODE                                                   
         MVC   LDGKUNT(L'ACTKLDG+L'ACTKUNT),=C'1R'                              
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,LDGKEY,LDGKEY,0                       
         JNE   EXITH               CANNOT FIND 1R LEDGER                        
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,LDGKDA,AIOAREA,DMWORK                  
         JNE   EXITH               CANNOT FIND 1R LEDGER                        
         GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('ACLELQ',AIOAREA),0,0              
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R7,12(R1)                                                        
         USING ACLELD,R7                                                        
         MVC   ONERL1L(L'ONERL1L),ACLELLVA                                      
         MVC   ONERL2L(L'ONERL2L),ACLELLVB                                      
         MVC   ONERL3L(L'ONERL3L),ACLELLVC                                      
         MVC   ONERL4L(L'ONERL4L),ACLELLVD                                      
         GOTOR BRASWO,DMCB,ABRADATE                                             
         MVC   AOFFBRA,ANXTBRA                                                  
         J     EXITE                                                            
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS BRANDOCEAN SWITCH ON/OFF DATE                               *         
***********************************************************************         
         SPACE 1                                                                
BRASWO   NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)                                                         
         ST    R4,ANXTBRA                                                       
         L     R2,AIOAREA                                                       
         USING ACTRECD,R2                                                       
         LA    R2,ACTRFST                                                       
         USING GDAELD,R2                                                        
         XR    R0,R0                                                            
BSWO002  CLI   GDAEL,0                                                          
         JE    EXITE                                                            
         CLI   GDAEL,GDAELQ                                                     
         JE    BSWO006                                                          
BSWO004  IC    R0,GDALN                                                         
         AR    R2,R0                                                            
         J     BSWO002                                                          
*                                                                               
BSWO006  CLI   GDATYPE,GDATMCST                                                 
         JNE   BSWO004                                                          
         CLC   ALPCODE,=C'WN'      WIEDEN AND KENNEDY                           
         JNE   BSWO010             ONLY WANT REMINDERS FROM JAN2008             
         CLC   GDADATE,=X'A80101'  IS THE START DATE LOWER THAN JAN 08          
         JNL   BSWO010             NO                                           
         OC    GDADATE2,GDADATE2   YES - DO WE HAVE AN END DATE                 
         JZ    BSWO008             NO                                           
         CLC   GDADATE2,=X'A80101' YES - IS END BEFORE NEW START DATE           
         JNH   BSWO004             YES IGNORE THIS ENTRY                        
                                                                                
BSWO008  MVC   0(L'GDADATE,R4),=X'A80101'  SET START DATE AS JAN 2008           
         J     BSWO012                                                          
BSWO010  MVC   0(L'GDADATE,R4),GDADATE                                          
BSWO012  MVC   L'GDADATE(L'GDADATE,R4),GDADATE2                                 
         LA    R4,2*L'GDADATE(R4)                                               
         ST    R4,ANXTBRA                                                       
         J     BSWO004                                                          
*                                                                               
BRASWOX  J     EXITE                                                            
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CLEAR BRANDOCEAN SWITCH ON/OFF DATE                                 *         
***********************************************************************         
         SPACE 1                                                                
BRACLR   NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)                                                         
BCLR002  OC    0(2*L'GDADATE,R4),0(R4)                                          
         JZ    BCLR004                                                          
         XC    0(2*L'GDADATE,R4),0(R4)                                          
         LA    R4,2*L'GDADATE(R4)                                               
         J     BCLR002                                                          
*                                                                               
BCLR004  J     EXITE                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET EMAIL ADDRESS OF APPROVER                                       *         
* EXIT CC EQUAL IF EMAIL ADDRESS FOUND                                *         
* EXIT CC LOW IF EMAIL ADDRESS NOT FOUND                              *         
* EXIT CC HIGH IF PID TERMINATED                                      *         
***********************************************************************         
         SPACE 1                                                                
GETEML   NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY3,IOKEY3                                                    
         LA    R6,IOKEY3                                                        
         USING SA0REC,R6                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECCODE     AGENCY ALPHA CODE                            
         MVC   SA0KNUM,USRPID      USER BINARY PID                              
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY3,AIOAREA3                       
         L     R6,AIOAREA3                                                      
         CLC   IOKEY3(L'SA0KEY),0(R6)                                           
         JNE   GETEMN                                                           
GETEM11  TM    SA0STAT,X'20'       LOCKED                                       
         JO    GETEMN                                                           
*                                                                               
GETEM12  LA    R6,SA0DATA                                                       
         USING SAPALD,R6                                                        
         XR    R0,R0                                                            
GETEM13  CLI   SAPALEL,0           TEST EOR                                     
         JNE   GETEM14                                                          
         DC    H'0'                                                             
GETEM14  CLI   SAPALEL,SAPALELQ                                                 
         JE    *+14                                                             
         IC    R0,SAPALLN                                                       
         AR    R6,R0                                                            
         J     GETEM13                                                          
         MVC   PIDCHAR,SAPALPID                                                 
*                                                                               
GETEM16  XC    IOKEY3,IOKEY3                                                    
         USING SAPEREC,R6                                                       
         LA    R6,IOKEY3                                                        
         MVI   SAPETYP,SAPETYPQ    C'F' - SECURITY PERSON REC                   
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECCODE     SECURITY ALPHA ID                            
         MVC   SAPEPID,PIDCHAR                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY3,AIOAREA3                       
         L     R6,AIOAREA3                                                      
         CLC   IOKEY3(SAPEDEF-SAPEKEY),0(R6)                                    
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R7,SAPEDATA                                                      
         USING SAPEED,R7                                                        
         XC    APPEMAIL,APPEMAIL                                                
         MVC   APPFSTNM,SPACES                                                  
         MVC   APPMIDNM,SPACES                                                  
         MVC   APPLSTNM,SPACES                                                  
         XR    R0,R0                                                            
GETEM18  CLI   SAPEEEL,0           TEST EOR                                     
         JE    GETEM40                                                          
         CLI   SAPEEEL,SAPERELQ    X'C6' - PERSONNEL DETAILS                    
         JE    GETEM38                                                          
         CLI   SAPEEEL,SAPEEELQ    X'E5' - PERSON EMAIL ELEM                    
         JE    GETEM22                                                          
         CLI   SAPEEEL,SANAMELQ    X'C5'                                        
         JE    GETEM32                                                          
GETEM20  IC    R0,SAPEELN                                                       
         AR    R7,R0                                                            
         J     GETEM18                                                          
*                                                                               
GETEM22  IC    RF,SAPEELN                                                       
         SHI   RF,SAPEELNQ+1                                                    
         MVC   APPEMAIL(0),SAPEEID  SAVE EMAIL ADDRESS                          
         EX    RF,*-6                                                           
         J     GETEM20                                                          
         DROP  R7                                                               
*                                                                               
         USING SANAMD,R7                                                        
GETEM32  LA    R1,SANAMELN         LENGTH OF NAME                               
         USING SANAMELN,R1                                                      
         TM    SANAMIND,SANAMIFN   TEST FIRST NAME PRESENT                      
         JZ    GETEM34                                                          
         XR    RF,RF                                                            
         IC    RF,SANAMELN                                                      
         CHI   RF,15               TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LA    RF,15               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         EXMVC RF,APPFSTNM,SANAME                                               
         LA    R1,2(RF,R1)         MOVE ONTO MIDDLE NAME                        
                                                                                
GETEM34  TM    SANAMIND,SANAMIMN   TEST MIDDLE NAME PRESENT                     
         JZ    GETEM36                                                          
         IC    RF,SANAMELN                                                      
         CHI   RF,15               TEST > MAX LENGTH                            
         JNH   *+8                                                              
         LA    RF,15               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         EXMVC RF,APPMIDNM,SANAME                                               
         LA    R1,2(RF,R1)         MOVE ONTO MIDDLE NAME                        
                                                                                
GETEM36  TM    SANAMIND,SANAMILN   TEST LAST NAME PRESENT                       
         JZ    GETEM20                                                          
         IC    RF,SANAMELN                                                      
         CHI   RF,58               TEST > MAX LENGTH                            
         BNH   *+8                                                              
         LA    RF,58               SET IT IF GREATER                            
         AHI   RF,-1                                                            
         EXMVC RF,APPLSTNM,SANAME                                               
         J     GETEM20                                                          
*                                                                               
         USING SAPERD,R7                                                        
GETEM38  OC    SAPERDTE,SAPERDTE   NO TERMINATION DATE                          
         JZ    GETEM20                                                          
         CLC   TODAYC,SAPERDTE     CHECK WHETHER PID IS TERMINATED              
         JNH   GETEM20                                                          
         OI    RUNIND1,RUNITERM    SET NO APPROVER (TERM)                       
         J     GETEM20                                                          
*                                                                               
GETEM40  TM    RUNIND1,RUNITERM    TERMINATED USER?                             
         JZ    GETEM42                                                          
         XC    APPEMAIL,APPEMAIL   CLEAR OUT EMAIL ADDRESS                      
         J     EXITH                                                            
*                                                                               
GETEM42  OC    APPEMAIL,APPEMAIL   CHECK WHETHER EMAIL IS WELL FORMED           
         JZ    GETEM44                                                          
         GOTOR =V(EMLCHK),DMCB,(L'APPEMAIL,APPEMAIL)                            
         JNE   GETEM44             NOTHING LEFT                                 
         XC    APPEMAIL,APPEMAIL   USE PARSED VERSION                           
         LLC   RF,DMCB                                                          
         SHI   RF,1                                                             
         L     R1,DMCB                                                          
         MVC   APPEMAIL(0),0(R1)                                                
         EX    RF,*-6                                                           
         J     GETEM46                                                          
*                                                                               
GETEM44  OI    RUNIND1,RUNINOEM    SET NO EMAIL ADDRESS PRESENT                 
*                                                                               
GETEM46  TM    RUNIND1,RUNILOCK    Was this person locked?                      
         JNO   GETEMY                 No - then skip                            
         USING LOCKD,RE                                                         
         LA    RE,LOCKWRK                                                       
         MVC   LOCKWRK,SPACES                                                   
         MVC   LOCKCPY,COCODE                                                   
         MVC   LOCKPID#,USRPID                                                  
         MVC   LOCKPID,PIDCHAR                                                  
         MVC   LOCKFNM,APPFSTNM                                                 
         MVC   LOCKLNM,APPLSTNM                                                 
         GOTO1 ABINADD,DMCB,(RC),LOCKWRK,ALOCKTAB   ADD TABLE ENTRY             
         DROP  RE                                                               
*                                                                               
GETEMY   J     EXITE                                                            
*                                                                               
GETEMN   OI    RUNIND1,RUNINOCP    SET NO APPROVER                              
         J     EXITL                                                            
         DROP  R1,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS OVERDUE TIME AND WRITE EMAIL                                          
***********************************************************************         
         SPACE 1                                                                
PROCOTM  NTR1  BASE=*,LABEL=*                                                   
         MVI   RCSUBPRG,1                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         MVC   LOWDTE,=X'FFFFFF'                                                
         XC    NUMRECS,NUMRECS                                                  
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PROTM32                                                          
*                                                                               
         TM    RUNIND2,RUNEMAIL    OVERRIDE EMAIL ID DETAILS ?                  
         JO    PROTM05                                                          
*                                                                               
         LA    RE,L'APPEMAIL                                                    
         LA    RF,APPEMAIL+L'APPEMAIL-1                                         
PROTM02  CLI   0(RF),C' '                                                       
         JH    PROTM04                                                          
         AHI   RF,-1                                                            
         JCT   RE,PROTM02                                                       
         DC    H'0'                                                             
PROTM04  STC   RE,ADDRLEN SAVE ACTUAL LENGTH OF ADDRESS                         
         MVI   1(RF),C':' EMAIL ADDRESS HAS TO BE TERMINATED WITH ':'           
*                                                                               
PROTM05  DS    0H                                                               
         CLI   RCFFPARM+0,C'Y'     OVERRIDE TO TEST                             
         JNE   PROTM06                                                          
         XC    APPEMAIL,APPEMAIL                                                
*&&US*&& MVC   APPEMAIL(9),=C'jim.shea:'                                        
*&&UK                                                                           
*        MVC   APPEMAIL(17),=C'YAT.NG@DDS.CO.UK:'                               
*        MVC   APPEMAIL(20),=C'TRACY.FRY@DDS.CO.UK:'                            
*        MVC   APPEMAIL(27),=C'MICHAEL.PENEYCAD@DDS.CO.UK:'                     
         L     RF,ATMAIL                                                        
         MVC   APPEMAIL,0(RF)                                                   
*&&                                                                             
PROTM06  MVC   TEXT1(L'AC@TSOVD),AC@TSOVD                                       
         CLI   RMPIDE,NOQ          SHOW PID INFORMATION?                        
         JE    PROTM16                                                          
         LA    RE,L'TEXT1                                                       
         LA    RF,TEXT1+L'TEXT1-1                                               
PROTM08  CLI   0(RF),C' '                                                       
         JH    PROTM10                                                          
         AHI   RF,-1                                                            
         JCT   RE,PROTM08                                                       
         DC    H'0'                                                             
PROTM10  LA    RF,2(RF)                                                         
         MVC   0(L'APPFSTNM,RF),APPFSTNM                                        
         LA    RE,L'TEXT1                                                       
         LA    RF,TEXT1+L'TEXT1-1                                               
PROTM12  CLI   0(RF),C' '                                                       
         JH    PROTM14                                                          
         AHI   RF,-1                                                            
         JCT   RE,PROTM12                                                       
         DC    H'0'                                                             
PROTM14  LA    RF,2(RF)                                                         
         MVC   0(L'APPLSTNM,RF),APPLSTNM                                        
*                                                                               
PROTM16  CLI   POPTPRNT,C'Y'                                                    
         JNE   PROTM18                                                          
         MVI   P,C'-'                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 ACREPORT                                                         
         MVC   P(L'APPEMAIL),APPEMAIL                                           
         MVC   PSECOND(L'TEXT1),TEXT1                                           
         GOTO1 ACREPORT                                                         
*                                                                               
PROTM18  CLI   RCWRITE,C'N'                                                     
         JE    PROTM32                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPRS',APPEMAIL),(L'TEXT1,TEXT1)                 
*                                                                               
         L     RF,AELE265E           Fill out text decription                   
*&&UK*&& MVC   0(L'AC@DTOT2,RF),AC@DTOT2                                        
*&&US*&& MVC   0(L'AC@DTOTM,RF),AC@DTOTM                                        
*                                                                               
         L     RF,AELE370E           Fill out for the period ending...          
*&&UK*&& MVC   0(L'AC@PERN2,RF),AC@PERN2                                        
*&&US*&& MVC   0(L'AC@PERNG,RF),AC@PERNG                                        
*                                                                               
         L     RF,AELE805E           Find us at...                              
         MVC   0(L'AC@FUSAT,RF),AC@FUSAT                                        
*                                                                               
         L     RF,AELE280E           Fill out please click here                 
         MVC   0(L'AC@PCHOA,RF),AC@PCHOA                                        
*&&UK                                UK/German web address                      
         L     RF,AELE840E                                                      
         MVC   0(L'ELEM840E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MUKHT,RF),AC@MUKHT                                        
         L     RF,AELE140E                                                      
         MVC   0(L'ELEM140E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MUKHT,RF),AC@MUKHT                                        
*&&                                                                             
         L     RF,AELE790E           Put region specific address                
*&&UK                                                                           
         MVC   0(L'ELEM790E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOUK,RF),AC@MOUK                                          
         CLI   COMPCTRY,CTRYGBR      Uk address                                 
         JE    PROTM20                                                          
         MVC   0(L'ELEM790E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MODE,RF),AC@MODE                                          
         CLI   COMPCTRY,CTRYGER      German address                             
         JE    PROTM20                                                          
*&&                                                                             
*&&US                                                                           
         MVC   0(L'ELEM790E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOCA,RF),AC@MOCA                                          
         CLI   COMPCTRY,CTRYCAN    Canada address                               
         JE    *+16                                                             
*&&                                                                             
         MVC   0(L'ELEM790E,RF),TEXTSPCS                                        
         MVC   0(L'AUADDR,RF),AUADDR Default is US address                      
*                                                                               
         GOTO1 SQUASHER,DMCB,0(RF),L'ELEM790E                                   
         L     RF,AELE840E         Default is US web address                    
         MVC   0(L'ELEM840E,RF),TEXTSPCS                                        
         LA    RE,AUHTML           Assume its US                                
*&&US                                                                           
         CLI   COMPCTRY,CTRYCAN    Canada address                               
         JNE   *+8                                                              
         LA    RE,AC@MCAHT                                                      
*&&                                                                             
         MVC   0(L'AUHTML,RF),0(RE)                                             
         L     RF,AELE140E                                                      
         MVC   0(L'ELEM140E,RF),TEXTSPCS                                        
         MVC   0(L'AUHTML,RF),0(RE)                                             
*                                                                               
PROTM20  L     RF,AENDURL          MOVE IN BASE URL                             
         L     R2,AEMLHTML                                                      
*                                                                               
         OC    AENDURL,AENDURL     AGENCY URL                                   
         JNZ   PROTM21A                                                         
         L     RF,AELE276E                                                      
         J     PROTM21B                                                         
*                                                                               
PROTM21A MVC   0(OVERURLL,RF),OVERURL                                           
         AHI   RF,OVERURLL         MOVE IN SPECIFIC URL                         
*                                                                               
PROTM21B MVC   0(L'LINKCB,RF),LINKCB                                            
*                                                                               
PROTM22  CLI   0(R2),X'FF'                                                      
         JE    PROTM50                                                          
         CLM   R2,15,AELEMTML      HAVE WE REACHED TIMESHEET LIST               
         JE    PROTM32             SECTION OF HTML                              
PROTM24  LLC   RF,0(R2)                                                         
         SHI   RF,2                                                             
         CHI   RF,160                                                           
         JNH   *+6                                                              
         DC    H'0'                                                             
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PROTM30                                                          
*                                                                               
PROTM26  MVI   TEXT1,C' '                                                       
         MVC   TEXT1+1(L'TEXT1-1),TEXT1                                         
         BASR  R7,0                                                             
         MVC   TEXT1(0),1(R2)                                                   
         EX    RF,0(R7)                                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   POPTPRNT,C'Y'                                                    
         JNE   PROTM28                                                          
         MVC   P(L'TEXT1),TEXT1                                                 
         GOTO1 ACREPORT                                                         
*                                                                               
PROTM28  CLI   RCWRITE,C'N'                                                     
         JE    PROTM30                                                          
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PROTM30                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
*                                                                               
PROTM30  LLC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     PROTM22                                                          
*                                                                               
         USING CALTABD,R4                                                       
         USING TABROD,R3                                                        
PROTM32  L     R4,ACALDATE                                                      
         LA    R3,TEXT1                                                         
*                                                                               
PROTM34  TM    CALSTAT,CALSMISS                                                 
         JZ    PROTM48                                                          
         CLC   LOWDTE,CALENDT                                                   
         JNH   *+10                                                             
         MVC   LOWDTE,CALENDT      SAVE EARLIEST DATE                           
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PROTM46                                                          
         GOTO1 DATCON,DMCB,(1,CALENDT),(23,WORK)                                
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1+1(L'TEXT1-1),TEXT1                                         
         MVC   TABEN1,TBROPE                                                    
         MVC   TABEN2,TBDOPE                                                    
*&&UK                                                                           
         CLI   COMPLANG,CTRYGER   For Germany format date differently           
         JNE   PROTM36                                                          
         GOTO1 DATCON,DMCB,(1,CALENDT),(21,TABGDAT)                             
         MVC   TABEN9,TBDCLO                                                    
         MVC   TABENA,TBRCLO                                                    
         J     PROTM42                                                          
*&&                                                                             
PROTM36  MVC   TABDAY,WORK+8                                                    
         MVC   TABEN3,TBDCLO                                                    
         MVC   TABEN4,TBDOPE                                                    
*                                                                               
         USING MONTABD,RF                                                       
         L     RF,AMONTAB          Lookup calendar dictionary equate            
*                                  from table                                   
PROTM38  CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MONNUM,WORK+5       match on month number                        
         JE    PROTM40                                                          
         LLC   R0,MONLEN                                                        
         AR    RF,R0                                                            
         J     PROTM38                                                          
*                                                                               
PROTM40  GOTO1 ADDICTAT,DMCB,C'LL  ',MONDD,TABMTH                               
*                                                                               
         MVC   TABEN5,TBDCLO                                                    
         MVC   TABEN6,TBDOPE                                                    
         MVC   TABYR,WORK                                                       
         MVC   TABEN7,TBDCLO                                                    
         MVC   TABEN8,TBRCLO                                                    
*                                                                               
PROTM42  GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
         CLI   RCWRITE,C'N'                                                     
         JE    PROTM44                                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PROTM44  CLI   POPTPRNT,C'Y'                                                    
         JNE   PROTM46                                                          
         MVC   P(L'TEXT1),TEXT1                                                 
         GOTO1 ACREPORT                                                         
*                                                                               
PROTM46  XR    R1,R1                                                            
         L     R1,NUMRECS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,NUMRECS                                                       
         CLI   RCFFPARM+1,C'1'                                                  
         JNE   PROTM48                                                          
         LA    R1,EMLFIL                                                        
         USING EMLTABD,R1                                                       
         MVC   EMLTCPY,COCODE                                                   
         MVC   EMLTPER,PERCODE                                                  
         MVC   EMLTODS,CURODS                                                   
         MVC   EMLTPIN,USRPID                                                   
         MVC   EMLTPEDT,CALENDT                                                 
         MVC   EMLTSEC,SECCODE                                                  
         MVC   EMLTALP,ALPCODE                                                  
         MVC   EMLTUID,ORIGINUM                                                 
         MVC   EMLTNUM,NUMREM                                                   
         L     R1,ADEMLFIL                                                      
         PUT   (1),EMLFIL                                                       
*                                                                               
         CLI   RMAPPN,C'D'         WANT APPROVER NOTIFICATIONS TODAY?           
         JNE   PROTM48                  NOTE, INCLUDES WEEKLY TOO!              
         GOTOR EMLTSAR             COPY EMLFILE TO TSAR FOR SORTING             
*                                                                               
PROTM48  LA    R4,CALTABL(R4)                                                   
         OC    CALENDT,CALENDT     HAVE WE REACHED THE END                      
         JNZ   PROTM34             NO - REPORT OTHER OVERDUE TIME               
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PROTM52                                                          
         CLI   RCWRITE,C'N'                                                     
         JE    PROTM52                                                          
         J     PROTM24                                                          
*                                                                               
PROTM50  TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PROTM52                                                          
         GOTO1 VSMTP,DMCB,('SMTPASND',0)         SEND EMAIL                     
         L     R1,NUMEMLS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,NUMEMLS                                                       
*                                                                               
PROTM52  DS    0H                                                               
         CLI   RMDOWN,YESQ         DOWNLOAD REPORT REQUEST ?                    
         JNE   PROTM54             NO , CONTINUE                                
*                                                                               
         GOTOR PRTDOWN             PRINT REPORT IN DOWNLOADABLE FORMAT          
         J     EXITE               EXIT                                         
*                                                                               
PROTM54  GOTOR PRTDET                                                           
         J     EXITE                                                            
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT DETAILS OF OUTSTANDING APPROVALS                                        
***********************************************************************         
         SPACE 1                                                                
PRTDET   NTR1  BASE=*,LABEL=*                                                   
         MVC   P+1(L'PIDCHAR),PIDCHAR                                           
         MVC   P+10(L'APPFSTNM),APPFSTNM                                        
         MVC   P+26(L'APPLSTNM),APPLSTNM                                        
         MVC   P+84(L'AC@EHBST),AC@EHBST  EMAIL HAS BEEN SENT TO                
         CLI   RCFFPARM+2,C'Y'     OVERRIDE TO PUT OUT 1R ACCOUNT               
         JNE   PDET02                                                           
         MVC   PSECOND+1(L'ONERCODE),ONERCODE                                   
PDET02   TM    RUNIND1,RUNINOEM           EMAIL ADDRESS NOT FOUND               
         JZ    PDET04                                                           
         MVC   P+84(L'AC@EANFF),AC@EANFF                                        
         J     PDET10                                                           
PDET04   TM    RUNIND1,RUNINOCP                                                 
         JZ    PDET06                                                           
         MVC   P+84(L'AC@IVPID),AC@IVPID  INVALID PID                           
         MVI   P+84+L'AC@IVPID,C'('                                             
         XOUT  USRPID,P+85+L'AC@IVPID,2                                         
         MVI   P+89+L'AC@IVPID,C')'                                             
         J     PDET10                                                           
PDET06   TM    RUNIND1,RUNITERM           TERMINATED USER                       
         JZ    PDET08                                                           
         MVC   P+84(L'AC@PIDTE),AC@PIDTE  TERMINATED PID                        
         MVI   P+L'AC@PIDTE+84,C'('                                             
         XOUT  USRPID,P+L'AC@PIDTE+85,2                                         
         MVI   P+L'AC@PIDTE+89,C')'                                             
         J     PDET10                                                           
PDET08   XR    RF,RF                                                            
         IC    RF,ADDRLEN                                                       
         SHI   RF,1                                                             
         CHI   RF,47                                                            
         JNH   *+8                                                              
         LA    RF,47                                                            
         MVC   PSECOND+84(0),APPEMAIL                                           
         EX    RF,*-6                                                           
PDET10   GOTO1 DATCON,DMCB,(1,LOWDTE),(21,P+68)                                 
         EDIT  (B4,NUMRECS),(10,P+55)                                           
         GOTO1 ACREPORT                                                         
         MVC   PSECOND,SPACES                                                   
         J     EXITE                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET DOWNLOAD REPORT HEADINGS                                        *         
***********************************************************************         
         SPACE 1                                                                
SETHDRS  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETHDR*'                                                      
*                                                                               
         GOTOR DOWN,DMCB,DOWNINI     INIT DOWNLOAD                              
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@RSPID,AC@RSPID),(R2) PID                 
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@CFNAM,AC@CFNAM),(R2) FIRST NAME          
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@CLNAM,AC@CLNAM),(R2) LAST NAME           
*                                                                               
         CLI   RUNREPTY,RUNLCKRT     REPORT TYPE - LOCKED ?                     
         JNE   SETHED01              NO, CONTINUE                               
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'TXTLSTTS,TXTLSTTS),(R2) LATEST TS           
         J     SETHED02                                                         
*                                                                               
SETHED01 DS    0H                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@NMTSA,AC@NMTSA),(R2) EARLIEST TS         
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@ETSDT,AC@ETSDT),(R2)  NUM OF TS          
*                                                                               
SETHED02 DS    0H                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'AC@CMTS,AC@CMTS),(R2)  COMMENTS             
*                                                                               
*                                                                               
SETHEDX  GOTOR DOWN,DMCB,DOWNEOL,,0(R2)                                         
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT THE DOWNLOAD REPORT                                           *         
*                                                                     *         
***********************************************************************         
         USING LOCKD,R3                                                         
PRTDOWN  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PRTDWN*'                                                      
*                                                                               
         L     R3,0(R1)                                                         
         GOTOR DOWN,DMCB,DOWNEWL                                                
*                                                                               
         CLI   RUNREPTY,RUNLCKRT     REPORT TYPE - LOCKED ?                     
         JNE   PRTDWN00              NO, PROCESS AS APPROVER/OVER DUE           
*                                                                               
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'LOCKPID,LOCKPID),0(R2) PID                  
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'LOCKFNM,LOCKFNM),0(R2) 1ST NAME             
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'LOCKLNM,LOCKLNM),0(R2) LAST NAME            
*                                                                               
         GOTO1 DATCON,DMCB,(1,LOCKPEDT),(21,TSDATE)                             
         GOTOR DOWN,DMCB,DOWNADD,(L'TSDATE,TSDATE),0(R2) DATE                   
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'TXTLCKED,TXTLCKED),0(R2) COMMENT            
         J     PRTDWNX                     END OF LINE                          
*                                                                               
PRTDWN00 DS    0H                                                               
*                                     ##  PID                                   
         GOTOR DOWN,DMCB,DOWNADD,(L'PIDCHAR,PIDCHAR),0(R2)                      
*                                     ##  APP 1ST NAME                          
         GOTOR DOWN,DMCB,DOWNADD,(L'APPFSTNM,APPFSTNM),0(R2)                    
*                                     ##  APP LAST NAME                         
         GOTOR DOWN,DMCB,DOWNADD,(L'APPLSTNM,APPLSTNM),0(R2)                    
*                                     ##  NUM OF TS                             
         XC    TEXT1,TEXT1                                                      
         EDIT  (B4,NUMRECS),(10,TEXT1),ZERO=NOBLANK                             
         GOTOR DOWN,DMCB,DOWNNUM,(10,TEXT1),0(R2)                               
*                                     ##  EARLIEST TIME SHEET                   
         GOTO1 DATCON,DMCB,(1,LOWDTE),(21,TSDATE)                               
         GOTOR DOWN,DMCB,DOWNADD,(L'TSDATE,TSDATE),0(R2)                        
*                                     ##  COMMENT                               
         XC    TEXT1,TEXT1                                                      
         MVC   TEXT1(L'AC@EHBST),AC@EHBST EMAIL HAS BEEN SENT TO                
*                                                                               
PRTDWN02 TM    RUNIND1,RUNINOEM           EMAIL ADDRESS PRESENT ?               
         JZ    PRTDWN04                   YES, CONTINUE                         
*                                         NO,                                   
         MVC   TEXT1(L'AC@EANFF),AC@EANFF SET EMAIL ADDRESS NOT FOUND           
         J     PRTDWN10                   PRINT DETAILS                         
*                                                                               
PRTDWN04 TM    RUNIND1,RUNINOCP           CHARACTER PID EXISTS ?                
         JZ    PRTDWN06                   YES, CONTINUE                         
*                                         NO,                                   
         MVC   TEXT1(L'AC@IVPID),AC@IVPID SET INVD PID PLEASE CONCT DDS         
         MVI   TEXT1+L'AC@IVPID+1,C'('    ADD '('                               
         GOTO1 HEXOUT,DMCB,USRPID,TEXT1+L'AC@IVPID+2,2  PID IN HEX              
         MVI   TEXT1+L'AC@IVPID+6,C')'    ADD ')'                               
         J     PRTDWN10                   PRINT DETAILS                         
*                                                                               
PRTDWN06 TM    RUNIND1,RUNITERM           IS PID TERMINATED ?                   
         JZ    PRTDWN08                   NO, CONTINUE                          
*                                                                               
         MVC   TEXT1(L'AC@PIDTE),AC@PIDTE YES, INPUT TERMINATED PID MSG         
         MVI   TEXT1+L'AC@PIDTE+1,C'('    ADD '('                               
         GOTO1 HEXOUT,DMCB,USRPID,TEXT1+L'AC@PIDTE+2,2                          
         MVI   TEXT1+L'AC@PIDTE+6,C')'    ADD ')'                               
         J     PRTDWN10                   CONTINUE                              
*                                                                               
PRTDWN08 XR    RF,RF                      CLEAR FOR LENGTH                      
         IC    RF,ADDRLEN                 ACTUAL EMAIL ADDRESS LENGTH           
         SHI   RF,1                       -1 FOR EX                             
         CHI   RF,47                      IS GREATER THAN 47                    
         JNH   *+8                        NO, CONTINUE                          
         LA    RF,47                      YES, SAVE MAX AS 47                   
         BASR  R1,R0                                                            
         MVC   TEXT1+L'AC@EHBST(0),APPEMAIL  SAVE EMAIL ID DETAILS              
         EX    RF,0(R1)                   FOR PRINT                             
*                                                                               
PRTDWN10 DS    0H                                                               
*                                                                               
         GOTOR DOWN,DMCB,DOWNADD,(L'TEXT1,TEXT1),0(R2) EMAIL DETAILS            
*                                                                               
PRTDWNX  GOTOR DOWN,DMCB,DOWNEOL,,0(R2)                                         
         J     EXITE                                                            
         EJECT                                                                  
         DROP  R3                                                               
***********************************************************************         
* ROUTINE TO ENCLOSE A PIECE OF TEXT IN QUOTES AND SEE IF IT WILL FIT *         
* ON THE PRINT LINE FOR A DOWNLOAD REPORT                             *         
* NTR1   P1=REQUEST TYPE                                              *         
*        P2=LENGTH OF FIELD TO BE ADDED, A(FIELD TO BE ADDED)         *         
*        P3=ADDRESS OF NEXT FREE SPACE ON PRINT LINE                  *         
*        R2=ADDRESS OF NEXT FREE SPACE ON PRINT LINE                  *         
***********************************************************************         
         SPACE 1                                                                
DOWN     NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'**DOWN**'                                                      
         L     RF,0(R1)            TYPE OF ACTION - INPUT                       
         STCM  RF,1,BYTE           SAVE FOR PROCESSING                          
         XR    R4,R4                                                            
         IC    R4,4(R1)            R4=L'FIELD                                   
         L     R5,4(R1)            R5=A(FIELD DATA)                             
         L     R2,8(R1)            R2=ADDR OF NEXT FREE SPACE ON LINE           
*                                                                               
         MVC   PSECOND,SPACES      INIT TO SPACES                               
         MVC   PTHIRD,SPACES       INIT TO SPACES                               
         MVC   PFOURTH,SPACES      INIT TO SPACES                               
*                                                                               
         CLI   BYTE,DOWNEWL        ADD NEW LINE                                 
         JE    DOWN01              YES, CONTINUE                                
*                                  NO,                                          
         CLI   BYTE,DOWNEOL        PROCESS END OF LINE                          
         JE    DOWN00              YES, CONTINUE                                
*                                  NO,                                          
         CLI   BYTE,DOWNINI        INITIALISE DOWNLOAD ?                        
         JNE   DOWN02              NO, OTHER ACTION CONTINUE                    
*                                  YES,                                         
         MVI   CLEARHED,NOQ        SET NO HEADER                                
         MVI   RCSUBPRG,4          SET FOR DOWNLOAD                             
         MVI   SKIPSPEC,NOQ        DON'T PROCESS SPECS                          
         MVI   NEWPAGE,YESQ        SET NEW PAGE TO YES                          
         MVC   P,SPACES            INIT PRINT LINE                              
         GOTO1 ACREPORT            PRINT DETAILS                                
*                                                                               
         OI    RUNIND2,RUN1DHED    SET HEADER SETUP COMPLATED                   
         GOTO1 ACREPORT            PRINT DETAILS                                
         MVI   FORCEHED,YESQ       SET HEAER TO YES                             
         J     DOWN01                                                           
*                                                                               
DOWN00   TM    RUNIND2,RUN1DHED    ARE WE DONE WITH THE HEADER ?                
         JNZ   DOWN0A              YES, CONTINUE                                
*                                                                               
         MVI   CLEARHED,NOQ        NO, CLEAR HEADER                             
         MVI   RCSUBPRG,4          SET PROG TO FF                               
         MVI   SKIPSPEC,NOQ        DON'T PROCESS SPECS                          
         MVI   NEWPAGE,NOQ         SET NEW PAGE TO NO                           
*                                                                               
DOWN0A   SHI   R2,1                                                             
         MVI   0(R2),C';'          END OLD LINE                                 
         NI    RUNIND2,X'FF'-RUN1DHED TURN OFF HEADER                           
         GOTO1 ACREPORT                                                         
         J     DOWNX               EXIT                                         
*                                                                               
DOWN01   LA    R2,P                AND START A NEW ONE                          
         MVC   P,SPACES            INIT PRINT LINE                              
         MVI   0(R2),C'"'          ADD '"'                                      
         AHI   R2,1                POINT TO THE SECOND BYTE                     
         J     DOWNX               EXIT                                         
*                                                                               
DOWN02   CLI   BYTE,DOWNADD        ADDING A FIELD, CHECK IT FITS FIRST          
         JNE   DOWN08              NO, CONTINUE                                 
         LA    RE,P+L'P            END OF THE PRINT LINE                        
         LR    RF,R2               ADDRESS OF THE PRINT LINE                    
         AR    RF,R4               ADD LENGTH OF THE CURRENT FILED              
         CR    RF,RE               MORE THAN MAX LENGTH ?                       
         JL    DOWN04              NO, CONTINUE                                 
*                                  YES,                                         
         SHI   R2,1                -1 TO MARK THE END                           
         MVI   0(R2),C' '          INIT TO SPACES                               
         MVI   CLEARHED,YESQ       SET CLEAR HEADER TO YES                      
         MVI   RCSUBPRG,4          SET TO 4                                     
         MVI   SKIPSPEC,YESQ                                                    
         MVI   NEWPAGE,NOQ                                                      
*                                  YES,                                         
         GOTO1 ACREPORT                                                         
*                                  YES,                                         
         LA    R2,P                ON HEADER WE USE PSECOND                     
         MVC   P,SPACES            INIT TO SPACES                               
         MVI   0(R2),C'"'          SET 1ST BYTE TO '"'                          
         AHI   R2,1                ADDRESS TO NEXT AVAILABLE BYTE               
*                                                                               
DOWN04   SHI   R4,1                -1 TO INSERT FIELD                           
         BASR  RE,0                ADDRESS NEXT INSTRUCTION                     
         MVC   0(0,R2),0(R5)       PUT FIELD IN                                 
         EX    R4,0(RE)            - USING EX                                   
         LR    RF,R2               PRINT LINE ADDRESS                           
         AR    RF,R4               ADD LENGTH OF THE CURRENT FIELD              
         AHI   R4,1                ADD 1 TO LENGTH OF THE FIELD                 
         LR    RE,R4               SAVE THE DETAILS                             
DOWN04A  CLI   0(RF),C'"'          GET RID OF " CHARS THEY ARE BAD              
         JNE   DOWN04B             BY SHUNTING DATA ALONG                       
         LR    R3,RF                                                            
         AHI   R3,1                                                             
         SHI   RE,2                                                             
         BASR  R1,0                                                             
         MVC   0(0,RF),0(R3)                                                    
         EX    RE,0(R1)                                                         
         AHI   RE,2                                                             
*                                                                               
DOWN04B  SHI   RF,1                                                             
         JCT   RE,DOWN04A                                                       
         AR    R2,R4                                                            
DOWN05   CLI   0(R2),C' '                                                       
         JH    DOWN06                                                           
         SHI   R2,1                                                             
         JCT   R5,DOWN05                                                        
*                                                                               
DOWN06   AHI   R2,1                                                             
         MVC   0(L'SEPQ,R2),SEPQ                                                
         AHI   R2,L'SEPQ                                                        
         J     DOWNX                                                            
*                                                                               
DOWN08   CLI   BYTE,DOWNBLK        PUT BLANK FIELD                              
         JNE   DOWN12                                                           
         LHI   R4,L'SEPQ+1                                                      
         LA    RE,P+L'P-1                                                       
         LR    RF,R2                                                            
         AR    RF,R4                                                            
         CR    RF,RE                                                            
         JL    DOWN10                                                           
         SHI   R2,1                                                             
         MVI   0(R2),C' '                                                       
         MVI   CLEARHED,YESQ                                                    
         MVI   RCSUBPRG,X'FF'                                                   
         MVI   SKIPSPEC,YESQ                                                    
         MVI   NEWPAGE,NOQ                                                      
         GOTO1 ACREPORT                                                         
         LA    R2,P                AND START A NEW ONE                          
         MVC   P,SPACES                                                         
         MVI   0(R2),C'"'                                                       
         AHI   R2,2                                                             
         MVC   0(L'SEPQ,R2),SEPQ                                                
         AHI   R2,L'SEPQ                                                        
         J     DOWNX                                                            
*                                                                               
DOWN10   MVI   0(R2),C' '                                                       
         AHI   R2,1                                                             
         MVC   0(L'SEPQ,R2),SEPQ                                                
         AHI   R2,L'SEPQ                                                        
         J     DOWNX                                                            
*                                                                               
DOWN12   CLI   BYTE,DOWNNUM        PUT IN NUMBER                                
         JNE   DOWN14                                                           
         SHI   R2,1                                                             
DOWN13   CLI   0(R5),C' '          FIND START OF TEXT                           
         JH    DOWN13A                                                          
         AHI   R5,1                                                             
         JCT   R4,DOWN13                                                        
*                                                                               
DOWN13A  LA    RE,P+L'P                                                         
         LR    RF,R2                                                            
         AR    RF,R4                                                            
         CR    RF,RE               CHECK IT WILL FIT                            
         JL    DOWN13B                                                          
         MVI   0(R2),C' '          IF NOT START A NEW LINE                      
         MVI   CLEARHED,YESQ                                                    
         MVI   RCSUBPRG,X'FF'                                                   
         MVI   SKIPSPEC,YESQ                                                    
         MVI   NEWPAGE,NOQ                                                      
         GOTO1 ACREPORT                                                         
         LA    R2,P                                                             
         MVC   P,SPACES                                                         
*                                                                               
DOWN13B  SHI   R4,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R2),0(R5)                                                    
         EX    R4,0(R1)                                                         
         AHI   R4,2                                                             
         AR    R2,R4                                                            
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         J     DOWNX                                                            
*                                                                               
DOWN14   CLI   BYTE,DOWNCLOS       END REPORT                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVI   P,C':'              ADD END OF REPORT CHAR                       
         MVI   CLEARHED,YESQ       DO NOT FORCE HEADINGS                        
         MVI   RCSUBPRG,X'EE'                                                   
         MVI   SKIPSPEC,YESQ       DOWNLOAD MEANS NO HEADINGS'RE WANTED         
         MVI   NEWPAGE,NOQ         AND NO PAGE THROWS                           
         GOTO1 ACREPORT            LAST TIME ONLY                               
*                                                                               
DOWNX    J     EXITR2                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS PERSON AS ACCORDING TO 1R ACCOUNT                           *         
***********************************************************************         
         SPACE 1                                                                
PROCPER  NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY1,IOKEY1                                                    
         XC    USRPID,USRPID                                                    
         L     R2,0(R1)                                                         
         LA    R3,IOKEY1                                                        
         USING PERRECD,R3          TIME PASSIVE DSECT                           
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,COCODE      COMPANY CODE                                 
         LLC   RF,ONERL4L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   PERKCODE(0),0(R2)                                                
         EX    RF,0(R1)                                                         
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,PERKEY,PERKEY,0                       
         JNE   EXITL                                                            
*                                                                               
PPERS02  GOTO1 DATAMGR,DMCB,DMGET,ACCMST,PERKDA,AIOAREA2,DMWORK                 
         JE    PPERS08                                                          
         DC    H'0'                                                             
*                                                                               
PPERS08  L     R3,AIOAREA2                                                      
         LA    R2,PERRFST                                                       
         USING PIDELD,R2                                                        
         USING LOCTABD,R4                                                       
         L     R4,ALOCDATE                                                      
*                                                                               
         LR    R0,R4               CLEAR OUT LOCTAB                             
         LHI   R1,LOCDATL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XR    R0,R0                                                            
PPERS10  CLI   PIDEL,0                                                          
         JE    PPERS24                                                          
         CLI   PIDEL,PIDELQ                                                     
         JE    PPERS14                                                          
         CLI   PIDEL,LOCELQ                                                     
         JE    PPERS22                                                          
         CLI   PIDEL,EMPELQ                                                     
         JE    PPERS20                                                          
PPERS12  IC    R0,PIDLN                                                         
         AR    R2,R0                                                            
         J     PPERS10                                                          
*                                                                               
PPERS14  MVC   USRPID,PIDNO                                                     
         J     PPERS12                                                          
*                                                                               
         USING EMPELD,R2                                                        
PPERS20  CLI   EMPCSTAT,EMPCTRM    IS PERSON NO LONGER WORKING                  
         JE    PPERSNO             YES - NOT INTERESTED                         
         MVC   USRHIRE,EMPHIR      EXTRACT HIRE DATE                            
         J     PPERS12                                                          
*                                                                               
         USING LOCELD,R2                                                        
PPERS22  CLC   CUROFFC,LOCOFF                                                   
         JNE   PPERS12                                                          
         CLC   CURDPT,LOCDEPT                                                   
         JNE   PPERS12                                                          
         CLC   CURSDP,LOCSUB                                                    
         JNE   PPERS12                                                          
         MVC   LOCTSTRT,LOCSTART                                                
         MVC   LOCTEND,LOCEND                                                   
         MVC   LOCTLCK,LOCLOCK                                                  
         LA    R4,LOCTABL(R4)                                                   
         J     PPERS12                                                          
*                                                                               
PPERS24  OC    USRPID,USRPID       DID WE GET A PID                             
         JZ    PPERSNO             NO - CAN'T EMAIL THEM                        
         L     R4,ALOCDATE                                                      
         OC    0(L'LOCDATE,R4),0(R4)                                            
         JZ    PPERSNO                                                          
*                                                                               
PPERSYES CLI   RMAPPN,C'D'                                                      
         JNE   EXITE                                                            
         GOTOR GETAPP              GET APPROVER FOR THIS 1R                     
         J     EXITE                                                            
*                                                                               
PPERSNO  J     EXITL                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS PERSON AS ACCORDING TO 1R ACCOUNT                           *         
***********************************************************************         
         SPACE 1                                                                
PROCDTE  NTR1  BASE=*,LABEL=*                                                   
         L     R4,ACALDATE                                                      
         USING CALTABD,R4                                                       
PDTE002  XC    CALSTAT,CALSTAT     CLEAR STATUS AREA FOR EACH PERSON            
         LA    R4,CALTABL(R4)                                                   
         OC    CALSTRT,CALSTRT                                                  
         JNZ   PDTE002                                                          
         L     R4,ACALDATE                                                      
PDTE010  L     R3,ABRADATE                                                      
         OC    0(L'GDADATE,R3),0(R3)                                            
         JZ    PDTENO                                                           
                                                                                
PDTE012  OC    0(L'GDADATE,R3),0(R3)                                            
         JZ    PDTE020                                                          
         CLC   CALENDT,0(R3)                                                    
         JL    PDTE018                                                          
         OC    L'GDADATE(L'GDADATE,R3),L'GDADATE(R3)                            
         JZ    PDTE014                                                          
         CLC   CALENDT,L'GDADATE(R3)                                            
         JNL   PDTE016                                                          
PDTE014  OI    CALSTAT,CALSBRAU                                                 
         J     PDTE018                                                          
*                                                                               
PDTE016  NI    CALSTAT,X'FF'-CALSBRAU                                           
PDTE018  LA    R3,L'GDADATE+L'GDADATE2(R3)                                      
         J     PDTE012                                                          
*                                                                               
PDTE020  DS    0H                                                               
*&&UK                                                                           
         OC    OVERDUE,OVERDUE                                                  
         JZ    PDTE020A                                                         
         CLC   CALENDT,OVERDUE     Period end date can't be later               
         JH    PDTE040             than latest overdue date                     
         J     PDTE020B                                                         
*&&                                                                             
PDTE020A CLC   CALENDT,TODAYRPT    Period cant be greater than today            
         JNH   *+14                                                             
         CLC   CALSTRT,TODAYRPT                                                 
         JH    PDTE040                                                          
*                                                                               
         USING LOCTABD,R2                                                       
PDTE020B L     R2,ALOCDATE                                                      
*                                                                               
PDTE022  OC    LOCTLCK,LOCTLCK     HAVE WE GOT A TIMESHEET LOCK DATE            
         JZ    PDTE026                                                          
         OC    LOCTEND,LOCTEND                                                  
         JZ    PDTE024                                                          
         CLC   LOCTLCK,LOCTEND                                                  
         JNL   PDTE026                                                          
PDTE024  CLC   CALSTRT,LOCTLCK     PERIOD AFTER LOCK DATE NO GOOD               
         JNL   PDTE030             GET NEXT ELEMENT                             
PDTE026  OC    LOCTEND,LOCTEND     HAVE WE GOT A LOCATION END DATE              
         JZ    *+14                NO                                           
         CLC   CALSTRT,LOCTEND     PERIOD START DATE AFTER END DATE             
         JH    PDTE030             YES - NO GOOD GET NEXT ELEMENT               
         CLC   CALSTRT,LOCTSTRT    PERIOD START DATE BEFORE LOC START           
         JNL   PDTE028             NO                                           
         CLC   LOCTSTRT,CALENDT    YES CHECK NOT LOA                            
         JH    PDTE030                                                          
PDTE028  OI    CALSTAT,CALSREAD    SET TO READ THIS PERIOD                      
*                                                                               
PDTE030  LA    R2,LOCTABL(R2)                                                   
         OC    LOCTSTRT,LOCTSTRT                                                
         JNZ   PDTE022                                                          
PDTE040  LA    R4,CALTABL(R4)                                                   
         OC    CALSTRT,CALSTRT                                                  
         JNZ   PDTE010                                                          
         J     EXITE                                                            
*                                                                               
PDTENO   J     EXITH                                                            
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* READ TIME ACCORDING TO THE DATES IN THE CALENDAR                    *         
***********************************************************************         
         SPACE 1                                                                
READTIM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING EDTRECD,R5                                                       
         LA    R5,IOKEY4                                                        
         MVC   EDTKEY,SPACES       YES READ FOR DAY HOURS                       
         MVC   CSVKEY4,SPACES                                                   
         MVI   EDTKTYP,EDTKTYPQ                                                 
         MVI   EDTKSUB,EDTKSUBQ                                                 
         MVC   EDTKCPY,COCODE                                                   
         LA    RF,CURODS                                                        
         LLC   R1,ONERL1L                                                       
         AHI   R1,-1                                                            
         MVC   EDTKOFC(0),0(RF)                                                 
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in CURODS                  
         LLC   R2,ONERL2L                                                       
         LLC   R1,ONERL1L                                                       
         SR    R2,R1                                                            
         LR    R1,R2                                                            
         AHI   R1,-1                                                            
         MVC   EDTKDPT(0),0(RF)                                                 
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in CURODS                  
         LLC   R2,ONERL3L                                                       
         LLC   R1,ONERL2L                                                       
         SR    R2,R1                                                            
         LR    R1,R2                                                            
         AHI   R1,-1                                                            
         MVC   EDTKSBD(0),0(RF)                                                 
         EX    R1,*-6                                                           
         MVC   EDTKPER,PERCODE                                                  
         MVC   EDTKYR,TODAYP                                                    
         MVI   EDTKSEQ,0                                                        
         MVI   EDTKKSTA,EDTKSDAY                                                
RDTIM010 MVC   CSVKEY4(L'EDTKEY),EDTKEY                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,EDTKEY,EDTKEY,0                       
         CLC   EDTKEY,CSVKEY4                                                   
         JE    RDTIM020                                                         
         MVC   IOKEY4,CSVKEY4                                                   
         LA    R2,EDTTAB                                                        
RDTIM015 CLI   0(R2),X'FF'         No Day edit hours                            
         JE    RDTIM025                                                         
         LA    RE,EDTKEY                                                        
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)                                                       
         AR    RE,RF                                                            
         SR    R1,R1                                                            
         IC    R1,2(R2)                                                         
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   0(0,RE),SPACES                                                   
         JH    *+12                                                             
         LA    R2,3(R2)                                                         
         J     RDTIM015                                                         
*                                                                               
         MVC   0(0,RE),SPACES                                                   
         EX    R1,*-6                                                           
         J     RDTIM010                                                         
*                                                                               
RDTIM020 GOTO1 DATAMGR,DMCB,DMGET,ACCMST,EDTKDA,AIOAREA4,DMWORK                 
*                                                                               
         XC    SVEDTKEY,SVEDTKEY   Saved Area for the Edict Key                 
*                                                                               
         CLC   PERCODE,=CL8'SUB2'                                               
         JNE   *+8                                                              
         J     *+4                                                              
RDTIM025 L     R4,ACALDATE                                                      
         USING CALTABD,R4                                                       
         NI    RUNIND1,X'FF'-(RUNIMISS+RUNILOCK)                                
         LA    R3,IOKEY3                                                        
         USING TSWRECD,R3                                                       
RDTIM030 TM    CALSTAT,CALSBRAU+CALSREAD   IS THIS A VALID PERIOD               
         JNO   RDTIM150                    NO                                   
*                                                                               
         NI    FLAG,X'FF'-(FLGREC+FLGMLCK+FLGEDIT)  Re-init Flag                
         GOTOR GETEDT,(R4)         Set up CALDAYS with DAY EDIT HOURS           
*                                                                               
         USING CALDAYD,R5                                                       
         L     R5,ACALDAYS                                                      
RDTIM035 CLI   CALDAY#,0           Find if period has valid days in it.         
         JE    RDTIM040                                                         
         CLC   CALDAY,CALSTRT                                                   
         JL    RDTIM037                                                         
         CLC   CALDAY,CALENDT                                                   
         JH    RDTIM037                                                         
         CP    CALHRS,=P'0'                                                     
         JE    RDTIM037                                                         
         OI    FLAG,FLGEDIT        Set edit hours exist                         
         J     RDTIM040                                                         
*                                                                               
RDTIM037 LA    R5,CALLNQ(R5)                                                    
         J     RDTIM035                                                         
         DROP  R5                                                               
*                                                                               
RDTIM040 XC    TSWKEY,TSWKEY       YES READ FOR TIME                            
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,COCODE                                                   
         MVC   TSWKPER,PERCODE                                                  
         XR    RE,RE                                                            
         ICM   RE,7,CALENDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,TSWKEND                                                     
         MVC   TSWKODS,CURODS                                                   
         MVC   CSVKEY3,TSWKEY                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,TSWKEY,TSWKEY,0                       
         J     RDTIM055                                                         
*                                                                               
RDTIM045 LA    R3,IOKEY3                                                        
         MVC   TSWKEY,CSVKEY4      restore key                                  
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,TSWKEY,TSWKEY,0                       
RDTIM050 GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,TSWKEY,TSWKEY,0                       
RDTIM055 CLC   TSWKEY(TSWKULC-TSWKEY),CSVKEY3                                   
         JNE   RDTIM100                                                         
         MVC   CSVKEY4,TSWKEY      SAVE OFF KEY                                 
         CLC   TSWKULC,SPACES      Any contra                                   
         JNH   RDTIM150            TIME MUST BE OK                              
         TM    TSWKSTAT,TIMSREJE+TIMSDELT IS TIME REJECTED OR DELETED           
         JNZ   RDTIM050            TIME MUST BE OK                              
         CLI   TSWKSTAT,0          IS TIME SAVED                                
         JNE   RDTIM150            YES-Check if needs to be submitted           
*                                                                               
         TM    FLAG,FLGEDIT        Any edit hours?                              
         JZ    RDTIM058                                                         
         OC    NTFY#,NTFY#         Any days to check to notify?                 
         JNZ   RDTIM060                                                         
         OC    LOCK#,LOCK#         Any days to check to lock?                   
         JNZ   RDTIM060                                                         
*                                                                               
RDTIM058 CLC   CALENDT,OVERDUE     Just check NDO profile                       
         JNL   RDTIM150                                                         
         OI    CALSTAT,CALSMISS    Treat as overdue                             
         OI    RUNIND1,RUNIMISS    Set Global Indicator                         
         J     RDTIM150                                                         
*                                                                               
RDTIM060 GOTO1 DATAMGR,DMCB,DMGET,ACCMST,TSWKDA,AIOAREA3,DMWORK                 
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TIMRECD,R3                                                       
         L     R3,AIOAREA3                                                      
         USING TIMELD,R2                                                        
         LA    R2,TIMRFST                                                       
RDTIM065 CLI   TIMEL,0                                                          
         JE    RDTIM045            Else - check next line                       
         CLI   TIMEL,TIMELQ        X'8B' - Time Element                         
         JNE   *+12                                                             
         CLI   TIMETYP,TIMETIME    TIMETIME type                                
         JE    RDTIM075                                                         
RDTIM070 SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R2,R1                                                            
         J     RDTIM065                                                         
*                                                                               
RDTIM075 LR    R5,R2                                                            
         SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R5,R1               Get end of ELM                               
         LA    R1,TIMEDAY                                                       
         USING TIMEDAY,R1                                                       
RDTIM080 CR    R5,R1               At the end of the ELM?                       
         JNH   RDTIM070                                                         
*                                                                               
         USING CALDAYD,RE                                                       
         L     RE,ACALDAYS                                                      
RDTIM085 CLI   CALDAY#,0           End of table?                                
         JE    RDTIM095                                                         
         CLC   CALDAY,TIMETDTE     Find right dates                             
         JNE   RDTIM090                                                         
         SP    CALHRS,TIMEHRS      Subtract current hours worked                
         J     RDTIM095                                                         
RDTIM090 LA    RE,CALLNQ(RE)                                                    
         J     RDTIM085                                                         
*                                                                               
RDTIM095 LA    R1,L'TIMEDAY(R1)                                                 
         J     RDTIM080                                                         
         DROP  R1,RE                                                            
*                                                                               
RDTIM100 TM    FLAG,FLGEDIT        Any edit hours?                              
         JZ    RDTIM102                                                         
         OC    NTFY#,NTFY#         Any days to check to notify?                 
         JNZ   RDTIM105                                                         
         OC    LOCK#,LOCK#         Any days to check to lock?                   
         JNZ   RDTIM105                                                         
RDTIM102 CLC   CALENDT,OVERDUE     Just check NDO profile                       
         JNL   RDTIM150                                                         
         OI    CALSTAT,CALSMISS    Treat as overdue                             
         OI    RUNIND1,RUNIMISS    Set Global Indicator                         
         J     RDTIM150                                                         
*                                                                               
         USING CALDAYD,R5                                                       
RDTIM105 L     R5,ACALDAYS                                                      
RDTIM107 CLI   CALDAY#,0           Anything in table?                           
         JE    RDTIM150             missing period                              
         CLC   CALDAY,CALSTRT                                                   
         JL    RDTIM120                                                         
         CLC   CALDAY,CALENDT                                                   
         JH    RDTIM150                                                         
         CP    CALHRS,=P'0'        Do we have hours left?                       
         JNH   RDTIM120            No - Get next day                            
*                                                                               
         GOTO1 DATCON,DMCB,(1,CALDAY),(3,NTFYDTE)                               
*                                                                               
         LA    R5,24                                                            
         SR    R0,R0                                                            
         ICM   R0,3,NTFY#                                                       
         JZ    RDTIM150                                                         
RDTIM110 LA    R1,DMCB             Normal call for PQ retain                    
         USING GETRETD,R1                                                       
         XC    GRDCB,GRDCB         Clear control block                          
         STCM  R5,3,GRDHRS         Set # of hours before notifying              
         MVC   GRDIDYMD,NTFYDTE    Set YMD binary input date                    
         GOTO1 VGETRET,(R1)                                                     
         JE    *+6                 Return code GRDRETC non-zero                 
         DC    H'0'                                                             
         MVC   NTFYDTE,GRDODYMD    Update binary input date                     
         JCT   R0,RDTIM110                                                      
         CLC   GRDODYMD,TODAYB     Should we Notify them?                       
         JH    RDTIM120            No - Get Next Day                            
         MVC   LOCKDTE,GRDODYMD                                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,LOCK#                                                       
         JZ    RDTIM140                                                         
RDTIM115 LA    R1,DMCB             Normal call for PQ retain                    
         USING GETRETD,R1                                                       
         XC    GRDCB,GRDCB         Clear control block                          
         STCM  R5,3,GRDHRS         Set # of hours before notifying              
         MVC   GRDIDYMD,LOCKDTE    Set YMD binary input date                    
         GOTO1 VGETRET,(R1)                                                     
         JE    *+6                 Return code GRDRETC non-zero                 
         DC    H'0'                                                             
         MVC   LOCKDTE,GRDODYMD                                                 
         JCT   R0,RDTIM115                                                      
         CLC   GRDODYMD,TODAYB     Should we LOCK them?                         
         JH    RDTIM125                                                         
         OI    FLAG,FLGMLCK        SET GLOBAL INDICATOR                         
         J     RDTIM125                                                         
*                                                                               
RDTIM120 LA    R5,CALLNQ(R5)                                                    
         J     RDTIM107                                                         
         DROP  R5                                                               
*                                                                               
RDTIM125 GOTO1 DATCON,DMCB,(1,CALSTRT),(0,WORK)                                 
RDTIM127 GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         CLC   CALENDT,WORK+6      Don't exceed Period Enddate                  
         JL    RDTIM150                                                         
         GOTO1 GETDAY,DMCB,WORK,DUB1                                            
         CLC   DUB1(3),SPACES                                                   
         JNE   *+6                                                              
         DC    H'0'                                                             
         USING DAYTABD,RF                                                       
         LA    RF,DAYTAB                                                        
RDTIM130 CLI   0(RF),X'FF'         CHECK THIS PERIOD HAS DAYS IN IT!            
         JE    RDTIM150                                                         
         CLC   0(1,RF),0(R1)                                                    
         JE    RDTIM135                                                         
         LA    RF,DAYTABLN(RF)                                                  
         J     RDTIM130                                                         
*                                                                               
RDTIM135 L     RE,ACOBLOCK                                                      
         USING COBLOCKD,RE                                                      
         LH    R1,1(RF)                                                         
         AR    R1,RE                                                            
         CLI   0(R1),YESQ          IS IT A WORKING DAY                          
         JE    RDTIM140                                                         
         GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+6,1                                  
         MVC   WORK(6),WORK+6                                                   
         J     RDTIM127                                                         
         DROP  RE                                                               
*                                                                               
RDTIM140 OI    CALSTAT,CALSMISS    YES - TREAT AS OVERDUE                       
         OI    RUNIND1,RUNIMISS    SET GLOBAL INDICATOR                         
         TM    FLAG,FLGMLCK        Do we need to lock them?                     
         JNO   RDTIM150            No - just leave as missing                   
*                                                                               
         USING TSWRECD,R3                                                       
RDTIM145 LA    R3,CSVKEY3          Use Save Key for Lock Table                  
         USING LOCKD,RE                                                         
*                                                                               
         LA    RE,LOCKWRK                                                       
         MVC   LOCKWRK,SPACES                                                   
         MVC   LOCKCPY,COCODE                                                   
         MVC   LOCKPER,PERCODE                                                  
         MVC   LOCKPEDT,CALENDT                                                 
         MVC   LOCKODS,CURODS                                                   
         MVC   LOCKPID#,USRPID                                                  
         MVC   LOCKUID,ORIGINUM                                                 
         GOTO1 ABINADD,DMCB,(RC),LOCKWRK,ALOCKTAB   ADD TABLE ENTRY             
         OI    RUNIND1,RUNILOCK    SET GLOBAL INDICATOR                         
         LA    R3,IOKEY3           Reset R3                                     
         DROP  RE                                                               
*                                                                               
RDTIM150 LA    R4,CALTABL(R4)      BUMP TO NEXT PERIOD                          
         OC    CALENDT,CALENDT     HAVE WE REACHED THE END                      
         JNZ   RDTIM030            NO                                           
         TM    RUNIND1,RUNIMISS    DID WE FIND ANY MISSING TIME                 
         JNZ   EXITE               YES                                          
         J     EXITH               NO                                           
         DROP  R3,R4                                                            
*                                                                               
*&&DO                                                                           
         L     R4,ACALDATE                                                      
         USING CALTABD,R4                                                       
         NI    RUNIND1,X'FF'-RUNIMISS                                           
         LA    R3,IOKEY3                                                        
         USING TSWRECD,R3                                                       
RDTIM002 TM    CALSTAT,CALSBRAU+CALSREAD   IS THIS A VALID PERIOD               
         BNO   RDTIM040                    NO                                   
         XC    TSWKEY,TSWKEY       YES READ FOR TIME                            
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,COCODE                                                   
         MVC   TSWKPER,PERCODE                                                  
         XR    RE,RE                                                            
         ICM   RE,7,CALENDT                                                     
         LNR   RE,RE                                                            
         STCM  RE,7,TSWKEND                                                     
         MVC   TSWKODS,CURODS                                                   
         MVC   CSVKEY3,TSWKEY                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,TSWKEY,TSWKEY,0                       
         CLC   TSWKEY(TSWKULC-TSWKEY),CSVKEY3                                   
         BNE   RDTIM004                                                         
         CLI   TSWKSTAT,0          IS TIME SAVED                                
         BE    RDTIM004            YES - NEEDS TO BE SUBMITTED                  
         TM    TSWKSTAT,TIMSREJE+TIMSDELT IS TIME REJECTED OR DELETED           
         BZ    RDTIM040            NO - TIME MUST BE OK                         
RDTIM004 OI    CALSTAT,CALSMISS    YES - TREAT AS OVERDUE                       
         OI    RUNIND1,RUNIMISS    SET GLOBAL INDICATOR                         
                                                                                
RDTIM040 LA    R4,CALTABL(R4)      BUMP TO NEXT PERIOD                          
         OC    CALENDT,CALENDT     HAVE WE REACHED THE END                      
         BNZ   RDTIM002            NO                                           
         TM    RUNIND1,RUNIMISS    DID WE FIND ANY MISSING TIME                 
         JNZ   EXITE               YES                                          
         J     EXITH               NO                                           
         DROP  R3,R4                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* READ TIME ACCORDING TO THE DATES IN THE CALENDAR                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CALTABD,R4                                                       
GETEDT   NTR1  BASE=*,LABEL=*                                                   
         LR    R4,R1                                                            
*                                                                               
         USING EDTRECD,R5                                                       
         L     R5,AIOAREA4                                                      
         CLI   EDTKTYP,EDTKTYPQ    Make sure we have a EDT Hour rec             
         JNE   EXITH                                                            
         CLI   EDTKSUB,EDTKSUBQ                                                 
         JNE   EXITH                                                            
         TM    EDTKKSTA,EDTKSDAY   By Day                                       
         JNO   EXITH                                                            
*                                                                               
         CLC   SVEDTKEY,0(R5)      Same key as previous?                        
         JE    GTEDTX                                                           
         MVC   SVEDTKEY,0(R5)      Save key for next compare.                   
*                                                                               
         L     R0,ACALDAYS         CLEAR CALDAYS                                
         SR    R1,R1                                                            
         ICM   R1,3,CALDAYLN                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(1,STC1DAT),(0,WORK)                                 
         GOTO1 GETDAY,DMCB,WORK,DUB1                                            
         CLC   DUB1(3),SPACES                                                   
         JNE   *+6                                                              
         DC    H'0'                                                             
         USING DAYTABD,RF                                                       
         LA    RF,DAYTAB                                                        
GTEDT002 CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RF),0(R1)                                                    
         JE    GTEDT004                                                         
         LA    RF,DAYTABLN(RF)                                                  
         J     GTEDT002                                                         
*                                                                               
GTEDT004 SR    R3,R3                                                            
         IC    R3,DAYEDT#          Get Matching Edit hour Day #                 
         STC   R3,BYTE1                                                         
         SR    R2,R2                                                            
         DROP  RF                                                               
*                                                                               
         USING CALDAYD,R3                                                       
         L     R3,ACALDAYS                                                      
GTEDT006 GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+6,(R2)                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,DUB)                                   
         CLC   DUB,TODAYRPT                                                     
         JH    GTEDT008                                                         
         SR    R1,R1                                                            
         IC    R1,BYTE1                                                         
         STC   R1,CALDAY#                                                       
         OI    CALDAY#,X'F0'                                                    
         MVC   CALDAY,DUB                                                       
         ZAP   CALHRS,PZERO                                                     
         AHI   R1,1                                                             
         CHI   R1,7                7 is the highest day                         
         JNH   *+8                                                              
         LA    R1,1                reset back to 1                              
         STC   R1,BYTE1                                                         
         LA    R3,CALLNQ(R3)                                                    
         AHI   R2,1                                                             
         J     GTEDT006                                                         
         DROP  R3                                                               
*                                                                               
GTEDT008 LA    R3,EDTRFST                                                       
GTEDT010 CLI   0(R3),0                                                          
         JE    GTEDTX                                                           
         CLI   0(R3),DEDELQ        X'54' - DAILY BUCKET ELEMENT                 
         JE    GTEDT014                                                         
GTEDT012 SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         J     GTEDT010                                                         
*                                                                               
         USING DEDELD,R3                                                        
         USING CALDAYD,R1                                                       
GTEDT014 L     R1,ACALDAYS                                                      
GTEDT016 CLI   CALDAY#,0           End of table?                                
         JE    GTEDT012                                                         
         CLC   DEDIND,CALDAY#      Match on #                                   
         JNE   GTEDT018                                                         
         CLI   DEDLN,DEDLN1Q       Is this an exception?                        
         JNH   *+14                                                             
         CLC   DEDDATE,CALDAY      Match on Date.                               
         JNE   GTEDT018                                                         
         ZAP   CALHRS,DEDHRS                                                    
GTEDT018 LA    R1,CALLNQ(R1)                                                    
         J     GTEDT016                                                         
*                                                                               
GTEDTX   J     EXITE                                                            
         DROP  R1,R4                                                            
*                                                                               
***********************************************************************         
* GET APPROVER CODE INTO TSAR REC                                               
***********************************************************************         
         SPACE 1                                                                
GETAPP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    PERAPPR,PERAPPR     APPROVER FOR THIS 1R                         
         L     R3,ATSAREA                                                       
         USING AT_D,R3                                                          
         XC    IOKEY3,IOKEY3                                                    
         LA    R2,IOKEY3                                                        
         USING DPAPASD,R2                                                       
         XC    DPAPAS,DPAPAS       BUILD DPAPASD KEY                            
         MVI   DPAPTYP,DPAPTYPQ         FOR PERSON LEVEL APPROVER               
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVI   DPAPAPPL,DPAPATIM                                                
         MVC   DPAP1RAC(L'CURODS),CURODS                                        
         LLC   RF,ONERL3L                                                       
         LA    RE,DPAP1RAC(RF)                                                  
         MVC   0(L'PERCODE,RE),PERCODE                                          
         ZAP   DPAPXVAL,=P'0'                                                   
         MVC   CSVKEY3,DPAPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETAPP15                                                         
         DC    H'0'                                                             
*                                                                               
GETAPP15 DS    0H                                                               
         CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY3                                 
         JNE   GETAPP20                                                         
         MVC   PERAPPR,DPAPPIDB                                                 
         J     GETAPPY                                                          
*                                                                               
GETAPP20 DS    0H              NOT FOUND, SUBDEPT LEVEL?                        
         XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY3+DPAPAPPL-DPAPAS                                 
         MVC   DPAP1RAC,CURODS                                                  
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,=P'0'                                                   
         MVC   CSVKEY3,DPAPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETAPP50                                                         
         DC    H'0'                                                             
*                                                                               
GETAPP50 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY3                                 
         JNE   GETAPP52                                                         
         MVC   PERAPPR,DPAPPIDB                                                 
         J     GETAPPY                                                          
*                                                                               
GETAPP52 XC    DPAPAS,DPAPAS   DEPT LEVEL                                       
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY3+DPAPAPPL-DPAPAS                                 
         LLC   RE,ONERL2L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   DPAP1RAC(0),CURODS                                               
         EX    RE,0(R1)                                                         
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,=P'0'                                                   
         MVC   CSVKEY3,DPAPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETAPP54                                                         
         DC    H'0'                                                             
*                                                                               
GETAPP54 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY3                                 
         JNE   GETAPP56                                                         
         MVC   PERAPPR,DPAPPIDB                                                 
         J     GETAPPY                                                          
*                                                                               
GETAPP56 XC    DPAPAS,DPAPAS   OFFICE LEVEL                                     
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,COCODE                                                   
         MVC   DPAPAPPL,CSVKEY3+DPAPAPPL-DPAPAS                                 
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   DPAP1RAC(0),CURODS                                               
         EX    RE,0(R1)                                                         
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,=P'0'                                                   
         MVC   CSVKEY3,DPAPAS                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DPAPAS,DPAPAS,0                       
         JE    GETAPP58                                                         
         DC    H'0'                                                             
*                                                                               
GETAPP58 CLC   DPAPAS(DPAPPIDB-DPAPASD),CSVKEY3                                 
         JNE   GETAPPN                                                          
         MVC   PERAPPR,DPAPPIDB                                                 
*                                                                               
GETAPPY  J     EXITE                                                            
*                                                                               
GETAPPN  J     EXITL                 APPROVER PID NOT FOUND                     
*                                                                               
GETAPOK  J     EXITH                 EXIT HIGH FOR APPROVER NOT NEEDED          
*                                                                               
         J     EXITE                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET PROFILES FOR COMPANY                                                      
***********************************************************************         
         SPACE 1                                                                
RDPROF   NTR1  BASE=*,LABEL=*                                                   
         XC    RMPROF,RMPROF                                                    
*READ HIGHEST LEVEL PROFILE FIRST IN CASE NO AGENCY PROFILE SET                 
         XC    WORK,WORK           GET DDS? PROFILES                            
         MVI   WORK,C'A'           C'A0RM' (0 is null byte)                     
         MVC   WORK+2(2),=C'RM'                                                 
         GOTO1 GETPROF,DMCB,(X'C0',WORK),RMPROF,DATAMGR                         
*                                                                               
         XC    WORK,WORK           GET PROGRAM PROFILES (IF ANY)                
         MVI   WORK,C'A'                                                        
         MVC   WORK+2(2),=C'RM'    NOTE OFFSET!                                 
         MVC   WORK+12(2),ALPCODE  AGENCY ALPHA                                 
         GOTO1 GETPROF,DMCB,(X'C0',WORK),RMPROF,DATAMGR                         
*                                                                               
         CLI   RMAPPN,C'N'                                                      
         JE    RDPROF10                                                         
         CLI   RMAPPN,C'D'                                                      
         JE    RDPROF10                                                         
         GOTOR GETDAY,DMCB,TDAYADD1,FULL                                        
         CLI   0(R1),0                                                          
         JNE   *+6                                                              
         DC    H'0'                BAD DATE!                                    
         OI    0(R1),X'F0'         MAKE NUMERIC                                 
         CLC   RMAPPN,0(R1)        TODAY MATCHES DAY NUMBER IN PROFI            
         JNE   RDPROF10                                                         
         MVI   RMAPPN,C'D'        YES, PRETEND DAILY TO SIMPLIFY CHE            
*                                                                               
RDPROF10 DS    0H                                                               
*                                                                               
         J     EXITE                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GENERATE EMAILS TO APPROVERS OF APPROVEE O/DUE TIME                           
***********************************************************************         
         SPACE 1                                                                
PROCAPP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ATSAREA                                                       
         USING AT_D,R3                                                          
         XC    AT_KEY(AT_KEYL),AT_KEY                                           
         XC    USRPID,USRPID                                                    
         MVC   ONERCODE,SPACES     NOT SET FOR APPROVER...                      
         XC    NUMRECS,NUMRECS                                                  
         MVC   LOWDTE,=X'FFFFFF'                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSOFFACT,TSARDH                                                
         ST    R3,T.TSAREC                                                      
         GOTOR ATSAR,T.TSARD                                                    
         JE    PAPP010             NO ERROR FROM TSAR                           
         NI    T.TSERRS,X'FF'-TSERNF  TSAR ALWAYS RETURNS THIS ON RDH           
         CLI   T.TSERRS,0                                                       
         JE    PAPP010                                                          
         TM    T.TSERRS,TSEEOF                                                  
         JNZ   EXITE               NOTHING TO EMAIL                             
         DC    H'0'                                                             
*                                                                               
PAPP010  DS    0H                  START A NEW APPROVER REPORT                  
         CLI   RMDOWN,YESQ         DOWNLOAD REPORT REQUEST ?                    
         JNE   PAPP012             NO , CONTINUE                                
         MVI   RCSUBPRG,4          PRINT HEADER DETAILS                         
         MVI   RUNREPTY,RUNAPRPT   SET REPORT TYPE IS APPROVER                  
         NI    RUNIND1,X'FF'-RUNIINI  RESET INITALIZE INDICATOR                 
         MVC   REPORTNM,SPACES     CLEAR WORK AREA                              
         MVC   REPORTNM(L'AC@APLST),AC@APLST SAVE REPORT NAME DETAILS           
*                                                                               
         GOTOR INIREPT             SET DOWNLOAD REPORT HEADINGS                 
         J     PAPP020             CONTINUE                                     
*                                                                               
PAPP012  DS    0H                  START A NEW APPROVER REPORT                  
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         J     PAPP020                                                          
*                                                                               
*  NEXT APPROVER - CLOSE LAST EMAIL (IF ANY), START NEW ONE.                    
PAPP020  OC    USRPID,USRPID       FIRST TIME THROUGH, NOTHING TO CLOSE         
         JZ    PAPP022                                                          
         CLI   RCWRITE,C'N'        NOT SENDING OUT EMAILS?                      
         JE    PAPP022                                                          
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PAPP022                                                          
         GOTO1 VSMTP,DMCB,('SMTPASND',0)        SEND EMAIL                      
*                                                                               
PAPP022  TM    T.TSERRS,TSEEOF                                                  
         JNZ   PAPP095             DONE, GO TIDY UP AND LEAVE                   
         NI    RUNIND1,X'FF'-(RUNINOCP+RUNINOEM+RUNITERM)                       
         MVC   USRPID,AT_APPR                                                   
         GOTOR GETEML                                                           
*                                                                               
         MVC   TEXT1,TEXTSPCS                                                   
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PAPP027                                                          
         GOTOR INIEML              (RE)INITIALISE EMAIL IN CASE NOT             
         TM    RUNIND2,RUNEMAIL    OVERRIDE  EMAIL ID ?                         
         JO    PAPP026A            YES, CONTINUE                                
*                                  NO                                           
         LA    RE,L'APPEMAIL                  YET DONE                          
         LA    RF,APPEMAIL+L'APPEMAIL-1                                         
PAPP025  CLI   0(RF),C' '                                                       
         JH    PAPP026                                                          
         AHI   RF,-1                                                            
         JCT   RE,PAPP025                                                       
         DC    H'0'                                                             
PAPP026  STC   RE,ADDRLEN SAVE ACTUAL LENGTH OF ADDRESS                         
         MVI   1(RF),C':' EMAIL ADDRESS HAS TO BE TERMINATED WITH ':'           
*                                                                               
PAPP026A DS    0H                                                               
         CLI   RCFFPARM+0,C'Y'     Override to test                             
         JNE   PAPP027                                                          
         XC    APPEMAIL,APPEMAIL                                                
*&&US*&& MVC   APPEMAIL(9),=C'jim.shea:'                                        
*&&UK                                                                           
*        MVC   APPEMAIL(17),=C'YAT.NG@DDS.CO.UK:'                               
*        MVC   APPEMAIL(20),=C'TRACY.FRY@DDS.CO.UK:'                            
*        MVC   APPEMAIL(27),=C'MICHAEL.PENEYCAD@DDS.CO.UK:'                     
         L     RF,ATMAIL                                                        
         MVC   APPEMAIL,0(RF)                                                   
*&&                                                                             
*                                                                               
PAPP027  CLI   RCWRITE,C'N'                                                     
         JE    PAPP040             don't want emails sent                       
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PAPP040                                                          
*                                                                               
         MVC   TEXT1(L'AC@STSOV),AC@STSOV      build subject line               
*                                                                               
         L     RF,AELE370E         Fill out for the period ending...            
         MVC   0(L'AC@STFPE,RF),AC@STFPE                                        
*                                                                               
         L     RF,AENDURL          Move in base URL                             
         L     R2,AEMLHTML                                                      
         OC    AENDURL,AENDURL                                                  
         JNZ   PAPP029                                                          
         L     RF,AELE276E                                                      
         J     PAPP030                                                          
*                                                                               
PAPP029  MVC   0(STAFURLL,RF),STAFFURL                                          
         AHI   RF,STAFURLL         Move in specific URL                         
*                                                                               
PAPP030  MVC   0(L'LINKCB,RF),LINKCB                                            
         CLI   RMPIDE,C'N'         Show PID information?                        
         JE    PAPP040                                                          
*                                                                               
         LA    RE,L'TEXT1                                                       
         LA    RF,TEXT1+L'TEXT1-1                                               
PAPP032  CLI   0(RF),C' '                                                       
         JH    PAPP034                                                          
         AHI   RF,-1                                                            
         JCT   RE,PAPP032                                                       
         DC    H'0'                                                             
PAPP034  LA    RF,2(RF)                                                         
         MVC   0(L'APPFSTNM,RF),APPFSTNM                                        
         LA    RE,L'TEXT1                                                       
         LA    RF,TEXT1+L'TEXT1-1                                               
PAPP036  CLI   0(RF),C' '                                                       
         JH    PAPP038                                                          
         AHI   RF,-1                                                            
         JCT   RE,PAPP036                                                       
         DC    H'0'                                                             
PAPP038  LA    RF,2(RF)                                                         
         MVC   0(L'APPLSTNM,RF),APPLSTNM                                        
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPRS',APPEMAIL),(L'TEXT1,TEXT1)                 
*                                                                               
         L     RF,AELE265E         Fill out text description                    
         MVC   0(L'AC@STFOT,RF),AC@STFOT                                        
PAPP040  L     R2,AEMLHTML                                                      
*                                                                               
PAPP042  CLI   0(R2),X'FF'                                                      
         JE    PAPP020                                                          
         CLM   R2,15,AELEMTML      Have we reached timesheet list               
         JE    PAPP052             section of HTML                              
*                                                                               
PAPP044  LLC   RF,0(R2)                                                         
         SHI   RF,2                                                             
         CHI   RF,160                                                           
         JNH   *+6                                                              
         DC    H'0'                                                             
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PAPP050                                                          
*                                                                               
PAPP046  MVI   TEXT1,C' '                                                       
         MVC   TEXT1+1(L'TEXT1-1),TEXT1                                         
         BASR  R7,0                                                             
         MVC   TEXT1(0),1(R2)                                                   
         EX    RF,0(R7)                                                         
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
*                                                                               
         CLI   POPTPRNT,C'Y'                                                    
         JNE   PAPP048                                                          
         MVC   P(L'TEXT1),TEXT1                                                 
         GOTO1 ACREPORT                                                         
*                                                                               
PAPP048  CLI   RCWRITE,C'N'                                                     
         JE    PAPP050                                                          
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PAPP050             Print email line                             
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PAPP050  LLC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     PAPP042                                                          
*                                                                               
         USING APPTABD,R4                                                       
PAPP052  LA    R4,TEXT1                                                         
*                                                                               
PAPP054  CLC   LOWDTE,AT_PEDT                                                   
         JNH   *+10                                                             
         MVC   LOWDTE,AT_PEDT      SAVE EARLIEST DATE                           
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PAPP090             Sending any emails?                          
         GOTO1 DATCON,DMCB,(1,AT_PEDT),(23,WORK)                                
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1+1(L'TEXT1-1),TEXT1                                         
         MVC   APPEN1,TBROPE                                                    
         MVC   APPEN2,TBDOPE                                                    
         MVC   APPFST,AT_APFNM                                                  
         MVC   APPEN3,TBDCLO                                                    
         MVC   APPEN4,TBDOPE                                                    
         MVC   APPLST,AT_APLNM                                                  
         MVC   APPEN5,TBDCLO                                                    
         MVC   APPEN6,TBDOPE                                                    
*&&UK                                                                           
         CLI   COMPLANG,CTRYGER    For Germany format date differently          
         JNE   PAPP056                                                          
         GOTO1 DATCON,DMCB,(1,AT_PEDT),(21,APPGDAY)                             
         MVC   APPEND,TBDCLO                                                    
         MVC   APPENE,TBRCLO                                                    
         J     PAPP062                                                          
*&&                                                                             
PAPP056  MVC   APPDAY,WORK+8                                                    
         MVC   APPEN7,TBDCLO                                                    
         MVC   APPEN8,TBDOPE                                                    
*                                                                               
         USING MONTABD,RF                                                       
         L     RF,AMONTAB          Lookup calendar dictionary equate            
*                                  from table                                   
PAPP058  CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MONNUM,WORK+5       match on month number                        
         BE    PAPP060                                                          
         LLC   R0,MONLEN                                                        
         AR    RF,R0                                                            
         B     PAPP058                                                          
*                                                                               
PAPP060  GOTO1 ADDICTAT,DMCB,C'LL  ',MONDD,APPMTH                               
*                                                                               
         MVC   APPEN9,TBDCLO                                                    
         MVC   APPENA,TBDOPE                                                    
         MVC   APPYER,WORK                                                      
         MVC   APPENB,TBDCLO                                                    
         MVC   APPENC,TBRCLO                                                    
*                                                                               
PAPP062  CLI   POPTPRNT,C'Y'                                                    
         JNE   PAPP064                                                          
         MVC   P(L'TEXT1),TEXT1                                                 
         GOTO1 ACREPORT                                                         
*                                                                               
PAPP064  CLI   RCWRITE,C'N'                                                     
         JE    PAPP090                                                          
         TM    RUNIND1,RUNINOCP+RUNINOEM+RUNITERM                               
         JNZ   PAPP090                                                          
*                                                                               
         GOTO1 SQUASHER,DMCB,TEXT1,L'TEXT1                                      
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
*                                                                               
PAPP090  DS    0H                                                               
* LOOP HERE FOR NEXT TSAR REC                                                   
         MVC   HALF,AT_APPR        SAVE LAST APPROVER                           
         MVI   T.TSOFFACT,TSANXT                                                
         GOTOR ATSAR,T.TSARD                                                    
         JE    PAPP092             NO ERROR FROM TSAR                           
         NI    T.TSERRS,X'FF'-TSERNF                                            
         CLI   T.TSERRS,0                                                       
         JE    PAPP092                                                          
         TM    T.TSERRS,TSEEOF                                                  
         JNZ   PAPP044             DONE, GO TIDY UP AND LEAVE                   
         DC    H'0'                                                             
*                                                                               
PAPP092  DS    0H                                                               
         CLC   AT_APPR,HALF        APPROVER CHANGE?                             
         JE    PAPP094                                                          
*                                                                               
         CLI   RMDOWN,YESQ         DOWNLOAD REPORT REQUEST ?                    
         JNE   PAPP093A            NO , CONTINUE                                
*                                                                               
         GOTOR PRTDOWN             PRINT REPORT IN DOWNLOADABLE FORMAT          
         J     PAPP093B                                                         
*                                                                               
PAPP093A GOTOR PRTDET              DO REPORT FOR APPROVER                       
*                                                                               
PAPP093B XC    NUMRECS,NUMRECS     AND RESTART STATS                            
         MVC   LOWDTE,=X'FFFFFF'                                                
*                                                                               
PAPP094  CLC   USRPID,AT_APPR      SAME APPROVER                                
         JE    PAPP054                                                          
         J     PAPP044             NEXT TSAR REC                                
*                                                                               
PAPP095  DS    0H                                                               
         MVC   ONERCODE,SPACES     NOT SET FOR APPROVER...                      
         CLI   RMDOWN,YESQ         DOWNLOAD REPORT REQUEST ?                    
         JNE   PAPP096             NO , CONTINUE                                
*                                                                               
         GOTOR PRTDOWN             PRINT REPORT IN DOWNLOADABLE FORMAT          
         GOTOR DOWN,DMCB,DOWNCLOS  CLOSE REPORT                                 
         J     EXITE               EXIT                                         
*                                                                               
PAPP096  GOTOR PRTDET              DO REPORT FOR APPROVER                       
         J     EXITE                                                            
*                                                                               
         DROP  T,R3,R4                                                          
         EJECT                                                                  
***********************************************************************         
* COPY EMLFILE ENTRY TO TSAR FOR PROCAPP                                        
***********************************************************************         
         SPACE 1                                                                
EMLTSAR  NTR1  LABEL=NO,BASE=*                                                  
         J     *+12                                                             
         DC    C'*EMLTSAR'                                                      
*                                                                               
         OC    PERAPPR,PERAPPR                                                  
         JZ    EXITE               NO APPROVER                                  
*                                                                               
         LA    R2,EMLFIL                                                        
         USING EMLTABD,R2                                                       
         L     R3,ATSAREA                                                       
         USING AT_D,R3                                                          
         MVC   AT_CPY,EMLTCPY                                                   
         MVC   AT_PERS,EMLTPER                                                  
         MVC   AT_PEDT,EMLTPEDT                                                 
         MVC   AT_ODS,EMLTODS                                                   
         MVC   AT_APFNM,APPFSTNM                                                
         MVC   AT_APLNM,APPLSTNM                                                
         MVC   AT_APPR,PERAPPR                                                  
*                                                                               
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSOFFACT,TSAADD                                                
         ST    R3,T.TSAREC                                                      
         GOTOR ATSAR,T.TSARD                                                    
         JE    EXITE                                                            
         LLC   R1,T.TSERRS                                                      
         DC    H'0'                                                             
         DROP  R2,R3,T                                                          
         EJECT                                                                  
*                                                                               
***********************************************************************         
* TSAR INITIALISATION  - note       called for each company!!!                  
***********************************************************************         
         SPACE 1                                                                
INITSAR  NTR1  LABEL=NO,BASE=*                                                  
         J     *+12                                                             
         DC    C'*INITSA*'                                                      
*                                                                               
T        USING TSARD,TSAROBUF                                                   
         XC    TSAROBUF(TSPNEWL),TSAROBUF   RETAIN A(BUFFER IF POSS             
         MVI   T.TSOFFACT,TSAINI     SET ACTION TO 'INITIALISE'                 
         MVI   T.TSRECI,TSRXTN+TSRWSSVR      USE TSRWSSVR                       
         MVI   T.TSKEYL,AT_KEYL                                                 
         MVC   T.TSABUF,ATSABUF                                                 
         MVC   T.TSAREC,ATSABUFS                                                
         LHI   R0,AT_RECL                                                       
         STCM  R0,3,T.TSRECL                                                    
         MVC   T.TSACOM,ADCOMFAC                                                
         GOTOR ATSAR,T.TSARD                                                    
         JE    EXITE                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SET OVERDUE CUTOFF DATE                                                       
***********************************************************************         
         SPACE 1                                                                
SETODUE  NTR1  LABEL=*,BASE=*                                                   
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         XC    OVERDUE,OVERDUE                                                  
         ZAP   DUB1,CONDO                                                       
*&&US*&& JZ    EXITL               If set to 0 - skip                           
         CVB   R4,DUB1                                                          
         LNR   R4,R4                                                            
*                                                                               
         MVC   WORK(L'TODAYF),TODAYF                                            
         GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+6,(R4)                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,OVERDUE)                               
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* EXTRACT COMPANY VALUES AND PROFILES                                           
***********************************************************************         
         SPACE 1                                                                
GETCPY   NTR1  LABEL=*,BASE=*                                                   
         GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('CPYELQ',AIOAREA),0,0              
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         JNE   EXITL               NO CPYEL FOUND                               
         L     R3,12(,R1)          R3 CONTAINS ADDRESS OF CPYELD                
         USING CPYELD,R3                                                        
         CLI   CPYLN,CPYLN3Q       IS ELEMENT TOO SHORT?                        
         JNH   EXITL               YES: GO TO NEXT RECORD                       
         TM    CPYSTATC,CPYSRMEM   DO WE WANT EMAIL REMINDERS                   
         JZ    EXITL               NO (DEFAULT): GO TO NEXT RECORD              
         MVC   ALPCODE,CPYALPHA    EXTRACT AGENCY ALPHA CODE                    
         MVC   ORIGINUM,CPYUID     COMPANY PRINCIPAL ID NUMBER                  
         GOTOR GTLGSEC             GET LANGUAGE AND RESOLVE LANGUAGE            
         GOTOR GSECALP             GET SECURITY ALPHA ID                        
         JL    EXITL                                                            
*                                                                               
         OC    COMPCTRY,COMPCTRY   DO WE HAVE A COUNTRY CODE                    
         JNZ   GETCPY04                                                         
         OC    COMPLANG,COMPLANG   DO WE HAVE A LANGUAGE CODE?                  
         JZ    GETCPY02                                                         
         MVC   COMPCTRY,COMPLANG   REPLACE COUNTRY CODE WITH LANG CODE          
         J     GETCPY04                                                         
*                                                                               
GETCPY02 DS    0H                                                               
*&&UK*&& MVI   COMPCTRY,CTRYGBR    HARD CODE TO UK                              
*&&US*&& MVI   COMPCTRY,CTRYUSA    HARD CODE TO USA                             
*                                                                               
GETCPY04 GOTOR RDPROF              GET PROFILES FOR COMPANY                     
                                                                                
GETCPY06 XR    RE,RE                                                            
         ICM   RE,1,CPYEL+CPYSFST-CPYELD                                        
         JNZ   *+8                                                              
         LHI   RE,X'F1'                                                         
         CHI   RE,X'F0'                                                         
         JH    GETCPY6A                                                         
         MVC   WORK(1),CPYSFST     YES - ISOLATE IT                             
         NI    WORK,X'0F'                                                       
         SR    RE,RE                                                            
         IC    RE,WORK                                                          
         LA    RE,15(0,RE)         NO, ADD CONVERSION FACTOR                    
         J     GETCPY6B                                                         
*        AHI   RE,X'F0'-X'C0'+X'09'                                             
GETCPY6A SHI   RE,X'F0'                                                         
GETCPY6B STC   RE,BYTE1                                                         
*                                                                               
         MVC   CONAME,SPACES       INITALIZE TO SPACES                          
         USING NAMELD,R3           MAP WITH NAME ELEMENT                        
         GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('NAMELQ',AIOAREA),0,0              
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         JNE   GETCPY07            NO, CONTINUE                                 
*                                                                               
         L     R3,12(,R1)          LOAD ELEMENT ADDRESS                         
         LLC   R1,NAMLN            LENGTH OF THE ELEMENT                        
         SHI   R1,NAMLN1Q+1        LENGTH OF THE NAME FIELD                     
         MVC   CONAME(0),NAMEREC   SAVE DETAILS                                 
         EX    R1,*-6                                                           
*                                                                               
GETCPY07 DS    0H                                                               
         USING CPXELD,R3                                                        
         GOTO1 =V(HELLO),DMCB,(C'G',ACCMST),('CPXELQ',AIOAREA),0,0              
         CLI   12(R1),0            WAS 'GET' SUCCESSFUL?                        
         JNE   EXITL               YES                                          
*                                                                               
         L     R3,12(,R1)                                                       
         MVC   CPXSTA,CPXSTATA                                                  
         L     R2,AIOAREA                                                       
         LA    R3,CPYRFST-CPYRECD(R2)                                           
         MVC   FEDURL,SPACES                                                    
*                                                                               
         LA    R0,FEDURL                                                        
         LHI   R1,L'FEDURL                                                      
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING FFTELD,R3                                                        
GETCPY08 CLI   FFTEL,0                                                          
         BE    GETCPY14                                                         
         CLI   FFTEL,FFTELQ                                                     
         BE    GETCPY12                                                         
GETCPY10 LLC   R0,FFTLN                                                         
         AR    R3,R0                                                            
         B     GETCPY08                                                         
*                                                                               
GETCPY12 CLI   FFTTYPE,FFTTFURL    CHECK FEDERATED URL ELEMENT                  
         BNE   GETCPY10                                                         
         LLC   RF,FFTLN                                                         
         SHI   RF,1+FFTDATA-FFTELD                                              
         LA    RE,FEDURL                                                        
         MVC   0(0,RE),FFTDATA     EXTRACT FEDERATED URL                        
         EX    RF,*-6                                                           
         B     GETCPY10                                                         
*                                                                               
GETCPY14 MVC   STRTDAT,TODAYP                                                   
         MVC   STRTDAT+1(1),BYTE1                                               
         CLC   TODAYP+1(1),BYTE1                                                
         JNL   GETCPY16                                                         
         XR    R1,R1                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(1,STRTDAT),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                              
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,STRTDAT)                               
*                                                                               
GETCPY16 GOTO1 DATCON,DMCB,(1,STRTDAT),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                              
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,ENDDATE)                               
*                                                                               
         MVC   STC1DAT,MINUS1YR    Initially set calendar Start Date            
         MVC   STM1DAT,MINUS1YR                                                 
         MVC   STM1DAT+1(1),BYTE1                                               
         CLC   MINUS1YR+1(1),BYTE1                                              
         JNL   GETCPY18                                                         
         XR    R1,R1                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(1,STM1DAT),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                              
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,STM1DAT)                               
*                                                                               
GETCPY18 GOTO1 DATCON,DMCB,(1,STM1DAT),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                              
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,EM1DATE)                               
*                                                                               
         L     R2,ABRADATE                                                      
         XC    0(L'BRADATE,R2),0(R2)                                            
         GOTOR GLDG1R                                                           
         JNE   EXITL               NO 1R LEDGER = NO TIMESHEETS                 
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
*                                                                               
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         LR    RE,R4               Perserve R4 as A(record found)               
         LR    RF,R3               Perserve R3 as A(record added)               
         LA    R6,LOCKPEDT-LOCKD                                                
         AR    RE,R6                                                            
         AR    RF,R6                                                            
         CLC   0(L'LOCKPEDT,RE),0(RF) Always Update the latest T/S              
         BNL   *+10                                                             
         MVC   0(L'LOCKPEDT,RE),0(RF)                                           
         LR    RE,R4               Reset R4 as A(record found)                  
         LR    RF,R3               Reset R3 as A(record added)                  
         LA    R6,LOCKPID-LOCKD                                                 
         AR    RE,R6                                                            
         AR    RF,R6                                                            
         CLC   0(L'LOCKPID,RF),SPACES       Any Pid?                            
         BNH   *+10                                                             
         MVC   0(L'LOCKPID,RE),0(RF)                                            
         LR    RE,R4               Reset R4 as A(record found)                  
         LR    RF,R3               Reset R3 as A(record added)                  
         LA    R6,LOCKFNM-LOCKD                                                 
         AR    RE,R6                                                            
         AR    RF,R6                                                            
         CLC   0(L'LOCKFNM,RF),SPACES       Any Pid?                            
         BNH   *+10                                                             
         MVC   0(L'LOCKFNM,RE),0(RF)                                            
         LR    RE,R4               Reset R4 as A(record found)                  
         LR    RF,R3               Reset R3 as A(record added)                  
         LA    R6,LOCKLNM-LOCKD                                                 
         AR    RE,R6                                                            
         AR    RF,R6                                                            
         CLC   0(L'LOCKLNM,RF),SPACES       Any Pid?                            
         BNH   *+10                                                             
         MVC   0(L'LOCKLNM,RE),0(RF)                                            
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LTORG EQUATES AND CONSTANTS FOR WHOLE PROGRAM                       *         
***********************************************************************         
GLOBALS  DS    0D                                                               
         LTORG                                                                  
MAXLOC   EQU   200                                                              
LOCDATL  EQU   MAXLOC*3                                                         
         SPACE 1                                                                
ACCFIL   DC    C'ACCFIL '                                                       
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
CTFILE   DC    C'CTFILE '                                                       
JESMAIL  DC    C'JESMAIL '                                                      
AUFROM   DC    C'AuraNotifications<AuraNotifications@mediaocean.com>'           
AUADDR   DC    C'MEDIAOCEAN | 45 W. 18TH ST. | NEW YORK, NY 10011'              
AUHTML   DC    C'www.mediaocean.com'                                            
LINKCB   DC    C'">'                                                            
HTTP     DC    C'HTTP://'                                                       
*                                                                               
FEDURL   DS    CL250                                                            
BLDFURL  DS    CL(L'FEDURL+L'AGUURL2+L'AGUHTT) Full url                         
BLDLEN   DS    XL1                 length of url                                
*                                                                               
OVERURL  DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-time'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-time'                                              
         DC    X'50'                                                            
         DC    C'route=time/display/myTimesheets/Overdue'                       
OVERURLL EQU   *-OVERURL                                                        
*                                                                               
STAFFURL DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-time'                              
         DC    X'50'                                                            
         DC    C'osPspId=rod-time'                                              
         DC    X'50'                                                            
         DC    C'route=time/display/myTimesheetApprovals/Overdue'               
STAFURLL EQU   *-STAFFURL                                                       
*                                                                               
VGETCAP  DC    V(GETCAP)                                                        
VGETRET  DC    A(0)                Now getting from comfacs                     
AMONTAB  DC    A(MONTAB)           MONTH TABLE LOOKUP                           
AEMLFILE DC    A(EMLFILE)                                                       
ACOBLOCK DC    A(CCOBLOCK)                                                      
ABRADATE DC    A(BRADATE)                                                       
ALOCDATE DC    A(LOCDATE)                                                       
ACALDATE DC    A(CALDATE)                                                       
SQUASHER DC    V(SQUASHER)                                                      
AEMLHTML DC    A(EMLHTML)                                                       
AELEMTML DC    A(ELEMTML)                                                       
AAGYTAB  DC    A(AGYTAB)                                                        
AELE140E DC    A(ELEM140E)                                                      
AELE265E DC    A(ELEM265E)                                                      
AELE276E DC    A(ELEM276E)                                                      
AENDURL  DS    A                                                                
AELE280E DC    A(ELEM280E)                                                      
AELE370E DC    A(ELEM370E)                                                      
AELE790E DC    A(ELEM790E)                                                      
AELE805E DC    A(ELEM805E)                                                      
AELE800E DC    A(ELEM800E)                                                      
AELE840E DC    A(ELEM840E)                                                      
AEMAIL1  DC    A(EMAILL1)                                                       
AEMAIL2  DC    A(EMAILL2)                                                       
AEMAIL3  DC    A(EMAILL3)                                                       
AEMAIL4  DC    A(EMAILL4)                                                       
AEMAIL5  DC    A(EMAILL5)                                                       
AEMAIL6  DC    A(EMAILL6)                                                       
AEMAIL7  DC    A(EMAILL7)                                                       
AEMAIL8  DC    A(EMAILL8)                                                       
AEMAIL9  DC    A(EMAILL9)                                                       
AEMAILA  DC    A(EMAILLA)                                                       
                                                                                
ABINADD  DC    A(BINADD)                                                        
*                                                                               
TXTLSTTS DC    C'LATEST TS'                                                     
TXTLCKED DC    CL6'LOCKED'         USER HAS BEEN LOCKED                         
SEPQ     DC    CL3'" "'            DOWNLOAD FILE SEPARATOR                      
AGNCYLBL DC    C'AGENCY :- '       AGENCY LABEL                                 
DATELEBL DC    C'DATE   :- '       DATE LABEL                                   
RPORTLBL DC    C'REPORT :- '       REPORT LABEL                                 
*                                                                               
PZERO    DC    P'0'                                                             
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
*                                                                               
TEXTSPCS DC    CL160' '                                                         
*                                                                               
TBROPE   DC    C'<tr>'                                                          
TBRCLO   DC    C'</tr>'                                                         
TBDOPE   DC    C'<td>'                                                          
TBDCLO   DC    C'</td>'                                                         
*                                                                               
EDTTAB   DC    AL2(EDTKPER-EDTKEY),AL1(L'EDTKPER)                               
         DC    AL2(EDTKSBD-EDTKEY),AL1(L'EDTKSBD)                               
         DC    AL2(EDTKDPT-EDTKEY),AL1(L'EDTKDPT)                               
         DC    AL2(EDTKOFC-EDTKEY),AL1(L'EDTKOFC)                               
         DC    X'FF'                                                            
*                                                                               
CALERR   DC    C'**ERROR** No calendar record found for agency'                 
CALERR2  DC    C'for year XXXX **ERROR**'                                       
         ORG   *-14                                                             
CALYEAR  DS    CL4                 YEAR                                         
         ORG                                                                    
*                                                                               
DATI     DS    0X                                                               
         DCDDL AC#EANFF,30         EMAIL ADDRESS NOT FOUND FOR...               
         DCDDL AC#IVPID,30         INVALID PID PLEASE CONTACT DDS               
         DCDDL AC#TSOVD,80         TIMESHEETS OVERDUE                           
*&&UK*&& DCDDL AC#DTOT2,80         HERE ARE THE DETAILS OF...                   
*&&US*&& DCDDL AC#DTOTM,80         HERE ARE THE DETAILS OF...                   
         DCDDL AC#STFPE,80         FOR STAFF NAME AND PERIOD ENDING...          
         DCDDL AC#STFOT,80         OVERDUE TIME FOR STAFF                       
         DCDDL AC#PCHOA,80         PLEASE CLICK HERE TO ACCESS...               
*&&UK*&& DCDDL AC#PERN2,80         FOR THE PERIOD ENDING...                     
*&&US*&& DCDDL AC#PERNG,80         FOR THE PERIOD ENDING...                     
         DCDDL AC#RSPID,3          PID                                          
         DCDDL AC#CFNAM,10         FIRST NAME                                   
         DCDDL AC#CLNAM,9          LAST NAME                                    
         DCDDL AC#LAPRT,27         LOCKED APPROVERS PID REPORT                  
         DCDDL AC#CMTS,8           COMMENTS                                     
         DCDDL AC#AGY,6            AGENCY                                       
         DCDDL AC#DATE,4           DATE                                         
         DCDDL AC#ETSDT,11         EARLIEST TS                                  
         DCDDL AC#NMTSA,9          NUM OF TS                                    
         DCDDL AC#APLST,13         APPROVER LIST                                
         DCDDL AC#EHBST,22         EMAIL HAS BEEN SENT TO                       
         DCDDL AC#EMNL1,80         STANDARD EMAIL LINE 1                        
         DCDDL AC#EMNL2,80         STANDARD EMAIL LINE 2                        
         DCDDL AC#EMNL3,80         STANDARD EMAIL LINE 3                        
         DCDDL AC#EMNL4,80         STANDARD EMAIL LINE 4                        
         DCDDL AC#EMNL5,80         STANDARD EMAIL LINE 5                        
         DCDDL AC#EMNL6,80         STANDARD EMAIL LINE 6                        
         DCDDL AC#EMNL7,80         STANDARD EMAIL LINE 7                        
         DCDDL AC#EMNL8,80         STANDARD EMAIL LINE 8                        
         DCDDL AC#EMNL9,80         STANDARD EMAIL LINE 9                        
         DCDDL AC#EMNLA,80         STANDARD EMAIL LINE A                        
         DCDDL AC#PRSN,10          PERSON                                       
         DCDDL AC#STSOV,40         STAFF                                        
         DCDDL AC#FUSAT,18         FIND US AT                                   
*&&UK*&& DCDDL AC#MODE,65          MEDIAOCEAN DE                                
*&&UK*&& DCDDL AC#MOUK,72          MEDIAOCEAN UK                                
*&&US*&& DCDDL AC#MOCA,80          MEDIAOCEAN CA                                
*&&UK*&& DCDDL AC#MUKHT,20         MEDIAOCEAN UK/GERMAN WEBSITE ADDRESS         
*&&US*&& DCDDL AC#MCAHT,20         MEDIAOCEAN CANADA    WEBSITE ADDRESS         
         DCDDL AC#PIDTE,22         PID IS TERMINATED                            
DATIX    DC    X'00'                                                            
*                                                                               
DAYTAB   DS    0X                                                               
         DC    AL1(1),AL2(CODMO-COBLOCKD),CL9'MONDAY',AL1(2)                    
         DC    AL1(2),AL2(CODTU-COBLOCKD),CL9'TUESDAY',AL1(3)                   
         DC    AL1(3),AL2(CODWE-COBLOCKD),CL9'WEDNESDAY',AL1(4)                 
         DC    AL1(4),AL2(CODTH-COBLOCKD),CL9'THURSDAY',AL1(5)                  
         DC    AL1(5),AL2(CODFR-COBLOCKD),CL9'FRIDAY',AL1(6)                    
         DC    AL1(6),AL2(CODSA-COBLOCKD),CL9'SATURDAY',AL1(7)                  
         DC    AL1(7),AL2(CODSU-COBLOCKD),CL9'SUNDAY',AL1(1)                    
         DC    X'FF'                                                            
*                                                                               
*                                                                               
* storage below is for debug/test runs. leave here for easy patching            
TARPID   DC    C'        '     person filter                                    
TESTMAIL DC    CL50' '  override email address to use (parm=y)                  
TESTSTDT DC    XL3'00'             PACKED START DATE FILTER                     
TESTENDT DS    XL3'00'             PACKED END DATE FILTER                       
*                                                                               
* large areas that don't fix in workd/spacend                                   
IOAREA3  DS    XL2000              I/O AREA 3                                   
TSARREC  DS    XL(AT_RECL)         TSAR IO AREA                                 
         DS    0H                                                               
**********************************************************************          
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB1     DS    D                                                                
AIOAREA  DS    A                   ADDRESS OF IO AREA 1                         
AIOAREA2 DS    A                   ADDRESS OF IO AREA 2                         
AIOAREA3 DS    A                   ADDRESS OF IO AREA 2                         
AIOAREA4 DS    A                   ADDRESS OF IO AREA 2                         
ANXTBRA  DS    A                   ADDRESS OF NXT ENTRY IN BRADATE              
AOFFBRA  DS    A                   ADDRESS OF NXT ENTRY AFTR AGENCY LVL         
ADPTBRA  DS    A                   ADDRESS OF NXT ENTRY AFTR AGENCY LVL         
ASDPBRA  DS    A                   ADDRESS OF NXT ENTRY AFTR AGENCY LVL         
APERBRA  DS    A                   ADDRESS OF NXT ENTRY AFTR AGENCY LVL         
ANXTCAL  DS    A                   ADDRESS OF NEXT CALENDAR                     
ADEMLFIL DS    A                   ADDRESS OF EMAIL DATASET                     
ATMAIL   DS    A                   ADDRESS OF OVERRIDE EMAIL ADDRESS            
*                                                      (TESTMAIL)               
ATSTDATE DS    A                   ADDRESS OF TEST DATE FILTERS                 
*                                                                               
ATSAREA  DS    A                   A(TSAR IO AREA)                              
ATSABUF  DS    A                   A(TSAR BUFFER) (GETMAIN)                     
ATSABUFS DS    A                   SIZE OF BUFFER REQUIRED                      
ATSAR    DS    A                   A(TSAROFF)                                   
*                                                                               
AAURURL  DS    A                   Address of AURTAB                            
VGETAUL  DS    A                   Address of GETURL module                     
*                                                                               
ALOCKTAB DS    A                   A(START) OF GETMAIN                          
ACALDAYS DS    A                   A(Calendar Days)                             
LOCKTBLN DS    F                   Length of LOCKTAB area                       
CALDAYLN DS    H                   Length of CALDAYS                            
GETMAINL DS    F                   LENGTH OF AREA                               
*                                                                               
COCODE   DS    XL1                 COMPANY CODE                                 
COMPLANG DS    XL1                 COMPANY LANGUAGE                             
LLANG    DS    XL1                 LAST LANGUAGE                                
COMPCTRY DS    XL1                 COMPANY COUNTRY                              
CTRY     DS    XL1                 COUNTRY                                      
NUMREM   DS    CL1                 NUMBER OF REMINDERS                          
PERFLG   DS    XL1                                                              
ALPCODE  DS    CL2                 AGENCY ALPHA CODE FROM COMPANY ELEM          
SECCODE  DS    CL2                 AGENCY ALPHA CODE FROM CTFILE                
PERCODE  DS    CL8                 SAVED PERSON CODE                            
PERAPPR  DS    XL(L'DPAPPIDB)      APPROVER FOR THIS 1R                         
LOWDTE   DS    PL3                 EARLIEST DATED TIMESHEET                     
TODAYP   DS    PL3                 TODAYS DATE                                  
TODAYB   DS    XL3                 TODAYS DATE (Binary)                         
TODAYC   DS    XL2                 TODAYS DATE (Compressed)                     
TODAYF   DS    CL6                 TODAYS DATE (Character)                      
TODAYL   DS    CL8                 TODAYS DATE LONG                             
TODAYRPT DS    PL3                 TODAYS DATE for report run                   
MINUS1YR DS    PL3                 TODAYS DATE MINUS 1 YEAR                     
TDAYADD1 DS    CL6                 TODAYS DATE PLUS ONE DAY                     
USRHIRE  DS    PL3                 USERS HIRE DATE                              
STRTDAT  DS    PL3                 START CALENDAR DATE                          
ENDDATE  DS    PL3                 END CALENDAR DATE                            
STM1DAT  DS    PL3                 START MINUS 1 YEAR DATE                      
EM1DATE  DS    PL3                 END MINUS 1 YEAR DATE                        
STC1DAT  DS    PL3                 START Day of 1st Calenday Period             
OVERDUE  DS    PL3                 OVERDUE DATE                                 
NTFY#    DS    XL2                 NUMBER OF DAYS  BEFORE NOTIFYING             
NTFYHRS  DS    XL2                 NUMBER OF HOURS BEFORE NOTIFYING             
NTFYDTE  DS    PL3                 DATE NOTIFICATIONS ARE SENT                  
LOCK#    DS    XL2                 NUMBER OF DAYS  BEFORE LOCKING               
LOCKHRS  DS    XL2                 NUMBER OF HOURS BEFORE LOCKING               
LOCKDTE  DS    PL3                 DATE LOCKS ARE SET                           
NTFYLNQ  EQU   *-NTFY#                                                          
BYTE1    DS    XL1                 BYTE1                                        
CPXSTA   DS    XL1                 COMPANY EXTRA STATUS BYTE A                  
*                                                                               
CONAME   DS    CL(L'NAMEREC)       COMPANY NAME                                 
REPORTNM DS    CL30                REPORT NAME                                  
TDAYRPTD DS    CL8                 TODAYS DATE - DD.MM.YY                       
*                                                                               
FLAG     DS    XL1                                                              
FLGLCKRP EQU   X'80'               Running Locked Report                        
FLGREC   EQU   X'40'               In the middle of reading records             
FLGMLCK  EQU   X'20'               Month is locked                              
FLGEDIT  EQU   X'10'               Edit hours exist                             
*                                                                               
RCRUN    DS    XL1                 SYSTEM RUN INDICATOR (MCTSTRUN)              
RUNTST   EQU   X'FF'                                                            
RCDSPAC  DS    CL1                 DSPACE A=ADV,C=CSC,Q=FQA,R=REP,T=TST         
*                                                                               
RUNIND1  DS    XL1                 RUN INDICATOR #1                             
RUNINOEM EQU   X'80'               NO EMAIL ADDRESS                             
RUNILOCK EQU   X'20'               GOT PID FROM READTIM RTN                     
RUNINOCP EQU   X'10'               NO CHARACTER PID EXISTS                      
RUNIMISS EQU   X'08'               MISSING TIMESHEETS                           
RUNIINI  EQU   X'04'               REPORTING INITIALISED                        
RUNIIEML EQU   X'02'               EMAILING INITIALISED                         
RUNITERM EQU   X'01'               PID IS TERMINATED                            
*                                                                               
RUNIND2  DS    XL1                 RUN INDICATOR #2                             
RUNIEOF  EQU   X'80'               END OF DATASET                               
RUN1DHED EQU   X'40'               DOWNLOADING HEADER                           
RUNEMAIL EQU   X'20'               OVERRIDE EMAIL ADDRESS PRESENT               
*                                                                               
RUNREPTY DS    XL1                 REPORT TYPE                                  
RUNAPRPT EQU   C'A'                APPROVER REPORT                              
RUNOVDRT EQU   C'O'                TIMESHEET OVERDUE REPORT                     
RUNLCKRT EQU   C'L'                LOCKED APPROVER PID REPORT                   
*                                                                               
TSDATE   DS    CL9                 RUN INDICATOR #2                             
*                                                                               
LOCKWRK  DS    CL(LOCKLNQ)         Work are for LOCK table entry                
*                                                                               
ONERCODE DS    CL12                1R ACCOUNT CODE                              
ONERL1L  DS    XL1                 OFFICE LENGTH                                
ONERL2L  DS    XL1                 DEPARTMENT LENGTH                            
ONERL3L  DS    XL1                 SUB-DEPT LENGTH                              
ONERL4L  DS    XL1                 PERSON LENGTH                                
*                                                                               
CUROFFC  DS    CL2                                                              
CURDPT   DS    CL6                                                              
CURSDP   DS    CL6                                                              
CURODS   DS    CL8                                                              
*                                                                               
CURDAY   DS    XL1                 Current Day                                  
*                                                                               
         DS    0H                                                               
NUMRECS  DS    F                   NUMBER OF RECORDS IN EMAILBUF                
NUMEMLS  DS    F                   NUMBER OF EMAILS                             
*                                                                               
TEXT1    DS    CL160                                                            
TEXT2    DS    CL160                                                            
*                                                                               
EMLFIL   DS    CL50                                                             
SVEMLFIL DS    CL50                                                             
SVEDTKEY DS    CL42                Saved AREA for the EDIT HOURS key            
SVDA     DS    XL4                                                              
*                                                                               
APPFSTNM DS    CL15                APPROVER FIRST NAME                          
APPMIDNM DS    CL15                APPROVER MIDDLE NAME                         
APPLSTNM DS    CL58                APPROVER LAST NAME                           
*                                                                               
USRPID   DS    XL2                 APPROVER PID                                 
PIDCHAR  DS    CL8                 PERSONAL-ID CHAR                             
APPEMAIL DS    CL50                APPROVER EMAIL ADDRESS                       
ADDRLEN  DS    XL1                 ACTUAL EMAIL ADDRESS LENGTH                  
*                                                                               
DATO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
DATOX    DS    0C                                                               
*                                                                               
*                              ##  DOWNLOAD REPORT INDICATORS  ##               
DOWNINI  EQU   0                   INITIALISE                                   
DOWNADD  EQU   1                   ADD TEXT TO LINE                             
DOWNEWL  EQU   2                   NEW LINE                                     
DOWNEOL  EQU   3                   END LINE                                     
DOWNBLK  EQU   4                   BLANK FIELD                                  
DOWNNUM  EQU   5                   NUMBER                                       
DOWNCLOS EQU   6                   CLOSE REPORT                                 
*                                  ** RM PROFILES **                            
RMPROF   DS    0X                                                               
RMPIDE   DS    CL1                 SHOW PID ON EMAILS                           
RMAPPN   DS    CL1                 NOTIFY APPROVERS DAILY/WEEKLY/NO             
RMDOWN   DS    CL1                 CREATE REPORT DOWNLOADABLE                   
         DS    CL14                N/D                                          
*                                                                               
IOKEY1   DS    CL64                I/O KEY                                      
IOKEY2   DS    CL64                I/O KEY                                      
IOKEY3   DS    CL64                I/O KEY                                      
IOKEY4   DS    CL64                I/O KEY                                      
CSVKEY1  DS    CL64                SAVED I/O KEY                                
CSVKEY2  DS    CL64                SAVED I/O KEY                                
CSVKEY3  DS    CL64                SAVED I/O KEY                                
TSAROBUF DS    XL(TSPXTNL)         TSAR BLOCK                                   
CSVKEY4  DS    CL64                SAVED I/O KEY                                
ELEMENT  DS    XL256                                                            
IOAREA   DS    XL2000              I/O AREA 1                                   
IOAREA2  DS    XL2000              I/O AREA 2                                   
LOCKMAX  EQU    40000                                                           
CALDMAX  EQU    400                                                             
*                                                                               
WORKX    EQU   *-WORKD                                                          
                                                                                
***********************************************************************         
* CALENDAR TABLE DSECT                                                *         
***********************************************************************         
         SPACE 1                                                                
CALTABD  DSECT                                                                  
CALENDT  DS    PL3                 Period end date 2's complement               
CALSTRT  DS    PL3                 Period start date 2's complement             
CALSTAT  DS    XL1                 Period status                                
CALSMISS EQU   X'80'               Not found and missing                        
CALSFOND EQU   X'40'               Found timesheet                              
CALSREAD EQU   X'10'               Read for this period                         
CALSBRAU EQU   X'08'               BrandOcean user                              
CALTABL  EQU   *-CALTABD                                                        
                                                                                
***********************************************************************         
* DAY TABLE DSECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
DAYTABD  DSECT                                                                  
DAY#     DS    XL1                 GETDAY Day #                                 
DAYDISP  DS    XL2                 Displacement to day in COBLOCKD              
DAYNME   DS    CL9                 Day Name                                     
DAYEDT#  DS    XL1                 EDIT Hrs Day #                               
DAYTABLN EQU   *-DAYTABD                                                        
                                                                                
***********************************************************************         
* CALDAY TABLE DSECT                                                  *         
***********************************************************************         
         SPACE 1                                                                
CALDAYD  DSECT                                                                  
CALDAY#  DS    XL1                                                              
CALDAY   DS    PL3                                                              
CALHRS   DS    PL4                                                              
CALLNQ   EQU   *-CALDAYD                                                        
                                                                                
***********************************************************************         
* DSECT FOR EMAIL DATA SENT TO DATASET                                *         
***********************************************************************         
         SPACE 1                                                                
EMLTABD  DSECT                                                                  
EMLTCPY  DS    XL1                 COMPANY CODE                                 
EMLTPER  DS    CL8                 PERSON CODE                                  
EMLTODS  DS    CL8                 OFFICE/DEPT/SUB-DEPT                         
EMLTKY1Q EQU   *-EMLTABD                                                        
EMLTPIN  DS    XL2                 PID BINARY                                   
EMLTPEDT DS    PL3                 PERIOD END DATE                              
EMLTSEC  DS    CL2                 SECURITY ALPHA                               
EMLTUID  DS    XL2                 AGENCY USERID                                
EMLTNUM  DS    CL1                 NUMBER OF REMINDERS A DAY                    
EMLTALP  DS    CL2                 ALPHA ID                                     
EMLAPPR  DS    XL2                 ALPHA ID                                     
EMLTABL  EQU   *-EMLTABD                                                        
         SPACE 1                                                                
***********************************************************************         
* DSECT FOR LOCATION DATE TABLE                                       *         
***********************************************************************         
         SPACE 1                                                                
LOCTABD  DSECT                                                                  
LOCTSTRT DS    PL3                 LOCATION START DATE                          
LOCTEND  DS    PL3                 LOCATION END DATE                            
LOCTLCK  DS    PL3                 LOCATION LOCK DATE                           
LOCTABL  EQU   *-LOCTABD                                                        
         SPACE 1                                                                
***********************************************************************         
* DSECT FOR APPROVER EMAIL TSAR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
AT_D     DSECT                                                                  
AT_KEY   DS    0X                                                               
AT_CPY   DS    XL1                 COMPANY CODE                                 
AT_APPR  DS    XL2                 APPROVER PID                                 
AT_PERS  DS    CL8                 APPROVEE                                     
AT_PEDT  DS    XL3                 O/S TIME END DATE                            
AT_ODS   DS    CL8                 LOCATION FOR UNIQUENESS                      
AT_KEYL  EQU   *-AT_D              LENGTH OF SORT KEY                           
AT_APFNM DS    CL(L'APPFSTNM)      APPROVEE FIRST NAME                          
AT_APLNM DS    CL(L'APPLSTNM)      APPROVEE LAST NAME                           
AT_RECL  EQU   *-AT_D                                                           
*                                                                               
***********************************************************************         
* PARM OPTION DSECT                                                             
***********************************************************************         
POPTD    DSECT                                                                  
POPTEML  DS    C     RCFFPARM+0    OVERRIDE WITH TEST EMAIL (PARM+0)            
POPTRUN  DS    C     RCFFPARM+1    RUN NUMBER               (PARM+1)            
POPT1R   DS    C     RCFFPARM+2    OUTPUT 1R ACCOUNT        (PARM+2)            
POPTPQ   DS    C     RCFFPARM+3    SUPPRESS REPORT FROM PQ  (PARM+3)            
POPTPRNT DS    C     RCFFPARM+4    PRINT EMAILS (IF PQ=N)   (PARM+4)            
                                                                                
***********************************************************************         
* DSECT TIME TABLE WORK AREA                                          *         
***********************************************************************         
         SPACE 1                                                                
LOCKD    DSECT                                                                  
LOCKKEY  DS    0C                  START OF BIN KEY                             
LOCKCPY  DS    XL1                 COMPANY CODE                                 
LOCKPID# DS    XL2                 PERSONAL ID NUMBER                           
TIMKLNQ  EQU   *-LOCKD                                                          
LOCKPER  DS    CL8                 PERSON CODE                                  
LOCKPEDT DS    PL3                 PERIOD END DATE                              
LOCKODS  DS    CL8                 OFF/DPT/SUBD                                 
LOCKPID  DS    CL8                 PERSONAL ID                                  
LOCKUID  DS    XL2                 Origin ID                                    
LOCKFNM  DS    CL15                APPROVER FIRST NAME                          
LOCKLNM  DS    CL36                APPROVER LAST NAME                           
LOCKLNQ  EQU   *-LOCKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* LIST OF TIMESHEETS TABLE ROW DATA (OVERDUE EMAIL)                   *         
***********************************************************************         
         SPACE 1                                                                
TABROD   DSECT                                                                  
TABEN1   DS    CL4                 <tr>                                         
TABEN2   DS    CL4                 <td>                                         
*&&US                                                                           
TABMTH   DS    CL9                 MONTH                                        
*&&                                                                             
*&&UK                                                                           
TABDAY   DS    CL2                 DAY                                          
*&&                                                                             
TABEN3   DS    CL5                 </td>                                        
TABEN4   DS    CL4                 <td>                                         
*&&UK                                                                           
TABMTH   DS    CL9                 MONTH                                        
*&&                                                                             
*&&US                                                                           
TABDAY   DS    CL2                 DAY                                          
*&&                                                                             
TABEN5   DS    CL5                 </td>                                        
TABEN6   DS    CL4                 <td>                                         
TABYR    DS    CL4                 YEAR                                         
TABEN7   DS    CL5                 </td>                                        
TABEN8   DS    CL5                 </tr>                                        
TABRODX  EQU   *-TABROD                                                         
*&&UK                                                                           
         ORG   TABDAY                                                           
TABGDAT  DS    CL10                Date (dd.mm.yyyy)                            
TABEN9   DS    CL5                 </td>                                        
TABENA   DS    CL5                 </tr>                                        
TABROGX  EQU   *-TABROD                                                         
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* LIST OF TIMESHEETS TABLE ROW DATA (APPROVER EMAIL)                  *         
***********************************************************************         
         SPACE 1                                                                
APPTABD  DSECT                                                                  
APPEN1   DS    CL4                 <tr>                                         
APPEN2   DS    CL4                 <td>                                         
APPFST   DS    CL15                FIRST NAME                                   
APPEN3   DS    CL5                 </td>                                        
APPEN4   DS    CL4                 <td>                                         
APPLST   DS    CL15                LAST NAME                                    
APPEN5   DS    CL5                 </td>                                        
APPEN6   DS    CL4                 <td>                                         
*&&US                                                                           
APPMTH   DS    CL9                 MONTH                                        
*&&                                                                             
*&&UK                                                                           
APPDAY   DS    CL2                 DAY                                          
*&&                                                                             
APPEN7   DS    CL5                 </td>                                        
APPEN8   DS    CL4                 <td>                                         
*&&UK                                                                           
APPMTH   DS    CL9                 MONTH                                        
*&&                                                                             
*&&US                                                                           
APPDAY   DS    CL2                 DAY                                          
*&&                                                                             
APPEN9   DS    CL5                 </td>                                        
APPENA   DS    CL4                 <td>                                         
APPYER   DS    CL4                 YEAR                                         
APPENB   DS    CL5                 </td>                                        
APPENC   DS    CL5                 </tr>                                        
APPENDX  EQU   *-APPTABD                                                        
*&&UK                                                                           
         ORG   APPDAY              For Germany display dd.mm.yyyy               
APPGDAY  DS    CL10                date                                         
APPEND   DS    CL5                 </td>                                        
APPENE   DS    CL4                 </tr>                                        
APPENGX  EQU   *-APPTABD                                                        
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* Calendar table dsect                                                *         
***********************************************************************         
         SPACE 1                                                                
MONTABD  DSECT                                                                  
MONNUM   DS    CL2                                                              
MONLEN   DS    XL1                                                              
MONDD    DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
* ACGETAULD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGETURLD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
SSOOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
COBLOCKD DSECT                                                                  
       ++INCLUDE ACCAPBLOCK                                                     
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* COMFACSD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACLDGTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLDGTABD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDICTATED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGETRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* LITERALS AND TABLES                                                 *         
***********************************************************************         
ACRM02   CSECT                                                                  
         SPACE 1                                                                
         DS    0D                  ALIGNMENT                                    
CCOBLOCK DS    CL(COBLOCKX-COBLOCK) COBLOCK FOR GETCAP                          
         DS    0D                  ALIGNMENT                                    
BRADATE  DC    20XL3'00'           TABLE OF DATES FOR BRANDOCEAN                
         DS    0D                  ALIGNMENT                                    
LOCDATE  DC    (LOCDATL)X'00'      TABLE OF DATES FOR LOCATION                  
         DS    0D                  ALIGNMENT                                    
CALDATE  DS    XL10000             TABLE OF PERIOD END DATES                    
                                                                                
       ++INCLUDE ACMONTAB          TABLE OF MONTHS                              
                                                                                
                                                                                
EMLFILE  DCB   DDNAME=EMLFILE,DSORG=PS,RECFM=FB,LRECL=50,BLKSIZE=500,  +        
               MACRF=(PM,GM),EODAD=RUNL102                                      
         SPACE 1                                                                
*                                                                               
AGYTAB   DS    0H                                                               
         DC    C'AE'                                                            
         DC    C'9J'                                                            
AGYTABL  EQU   *-AGYTAB                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* HTML TEMPLATE FOR AURA                                              *         
***********************************************************************         
         SPACE 1                                                                
EMLHTML  DS    0H                                                               
*                                                                               
ELEM5    DC    AL1(ELEM5L)                                                      
         DC    C'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 '                 
         DC    C'Transitional//EN" '                                            
         DC    C'"http://www.w3.org/TR/xhtml11/DTD/xhtml1-transitional'         
         DC    C'.dtd">'                                                        
ELEM5L   EQU   *-ELEM5                                                          
*                                                                               
ELEM10   DC    AL1(ELEM10L)                                                     
         DC    C'<html xmlns="http://www.w3.org/1999/xhtml">'                   
ELEM10L  EQU   *-ELEM10                                                         
*                                                                               
ELEM15   DC    AL1(ELEM15L)                                                     
         DC    C'<head>'                                                        
ELEM15L  EQU   *-ELEM15                                                         
*                                                                               
ELEM20   DC    AL1(ELEM20L)                                                     
         DC    C'<title>'                                                       
ELEM20L  EQU   *-ELEM20                                                         
*                                                                               
ELEM25   DC    AL1(ELEM25L)                                                     
         DC    C'</title>'                                                      
ELEM25L  EQU   *-ELEM25                                                         
*                                                                               
ELEM30   DC    AL1(ELEM30L)                                                     
         DC    C'<meta http-equiv="Content-Type" content="text/html;'           
         DC    C' charset=UTF-8" />'                                            
ELEM30L  EQU   *-ELEM30                                                         
*                                                                               
ELEM35   DC    AL1(ELEM35L)                                                     
         DC    C'<meta name="viewport" content="width=device-width; '           
         DC    C'initial-scale=1; maximum-scale=1.0"/>'                         
ELEM35L  EQU   *-ELEM35                                                         
*                                                                               
ELEM40   DC    AL1(ELEM40L)                                                     
         DC    C'</head>'                                                       
ELEM40L  EQU   *-ELEM40                                                         
*                                                                               
ELEM45   DC    AL1(ELEM45L)                                                     
         DC    C'<body bgcolor="#ffffff">'                                      
ELEM45L  EQU   *-ELEM45                                                         
*                                                                               
ELEM50   DC    AL1(ELEM50L)                                                     
         DC    C'<!-- Outer Wrap -->'                                           
ELEM50L  EQU   *-ELEM50                                                         
*                                                                               
ELEM55   DC    AL1(ELEM55L)                                                     
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0" bgcolor="#ffffff" style="table-layout'         
         DC    C':fixed;">'                                                     
ELEM55L  EQU   *-ELEM55                                                         
*                                                                               
ELEM60   DC    AL1(ELEM60L)                                                     
         DC    C'<tr>'                                                          
ELEM60L  EQU   *-ELEM60                                                         
*                                                                               
ELEM65   DC    AL1(ELEM65L)                                                     
         DC    C'<td>'                                                          
ELEM65L  EQU   *-ELEM65                                                         
*                                                                               
ELEM70   DC    AL1(ELEM70L)                                                     
         DC    C'<!-- Inner Wrap -->'                                           
ELEM70L  EQU   *-ELEM70                                                         
*                                                                               
ELEM75   DC    AL1(ELEM75L)                                                     
         DC    C'<table style="width:800px;" border="0" '                       
         DC    C'cellspacing="0" cellpadding="0" align="center">'               
ELEM75L  EQU   *-ELEM75                                                         
*                                                                               
ELEM80   DC    AL1(ELEM80L)                                                     
         DC    C'<tr>'                                                          
ELEM80L  EQU   *-ELEM80                                                         
*                                                                               
ELEM85   DC    AL1(ELEM85L)                                                     
         DC    C'<td>'                                                          
ELEM85L  EQU   *-ELEM85                                                         
*                                                                               
ELEM90   DC    AL1(ELEM90L)                                                     
         DC    C'<!-- top shadow -->'                                           
ELEM90L  EQU   *-ELEM90                                                         
*                                                                               
ELEM95   DC    AL1(ELEM95L)                                                     
         DC    C'<table width="100%" border="0" cellspacing="0"'                
         DC    C'cellpadding="0">'                                              
ELEM95L  EQU   *-ELEM95                                                         
*                                                                               
ELEM100  DC    AL1(ELEM100L)                                                    
         DC    C'<tr>'                                                          
ELEM100L EQU   *-ELEM100                                                        
*                                                                               
ELEM105  DC    AL1(ELEM105L)                                                    
         DC    C'<td height="10"></td>'                                         
ELEM105L EQU   *-ELEM105                                                        
*                                                                               
ELEM110  DC    AL1(ELEM110L)                                                    
         DC    C'</tr>'                                                         
ELEM110L EQU   *-ELEM110                                                        
*                                                                               
ELEM115  DC    AL1(ELEM115L)                                                    
         DC    C'</table>'                                                      
ELEM115L EQU   *-ELEM115                                                        
*                                                                               
ELEM120  DC    AL1(ELEM120L)                                                    
         DC    C'<!-- header-->'                                                
ELEM120L EQU   *-ELEM120                                                        
*                                                                               
ELEM125  DC    AL1(ELEM125L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellpadding="0">'                              
ELEM125L EQU   *-ELEM125                                                        
*                                                                               
ELEM130  DC    AL1(ELEM130L)                                                    
         DC    C'<tr>'                                                          
ELEM130L EQU   *-ELEM130                                                        
*                                                                               
ELEM135  DC    AL1(ELEM135L)                                                    
         DC    C'<td>'                                                          
ELEM135L EQU   *-ELEM135                                                        
*                                                                               
ELEM140  DC    AL1(ELEM140L)                                                    
         DC    C'<a href="'                                                     
ELEM140E DS    CL20                                                             
         DC    C'">'                                                            
         DC    C'<img alt="Aura" src="http://info.mediaocean.com/rs/'           
         DC    C'331-XPM-231/images/Aura-header.png"/>'                         
ELEM140L EQU   *-ELEM140                                                        
*                                                                               
ELEM145  DC    AL1(ELEM145L)                                                    
         DC    C'</a>'                                                          
ELEM145L EQU   *-ELEM145                                                        
*                                                                               
ELEM150  DC    AL1(ELEM150L)                                                    
         DC    C'</td>'                                                         
ELEM150L EQU   *-ELEM150                                                        
*                                                                               
ELEM155  DC    AL1(ELEM155L)                                                    
         DC    C'</tr>'                                                         
ELEM155L EQU   *-ELEM155                                                        
*                                                                               
ELEM160  DC    AL1(ELEM160L)                                                    
         DC    C'</table>'                                                      
ELEM160L EQU   *-ELEM160                                                        
*                                                                               
ELEM165  DC    AL1(ELEM165L)                                                    
         DC    C'<!-- end: header -->'                                          
ELEM165L EQU   *-ELEM165                                                        
*                                                                               
ELEM170  DC    AL1(ELEM170L)                                                    
         DC    C'<!-- body -->'                                                 
ELEM170L EQU   *-ELEM170                                                        
*                                                                               
ELEM175  DC    AL1(ELEM175L)                                                    
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0">'                                              
ELEM175L EQU   *-ELEM175                                                        
*                                                                               
ELEM180  DC    AL1(ELEM180L)                                                    
         DC    C'<tr>'                                                          
ELEM180L EQU   *-ELEM180                                                        
*                                                                               
ELEM185  DC    AL1(ELEM185L)                                                    
         DC    C'<td height="10"></td>'                                         
ELEM185L EQU   *-ELEM185                                                        
*                                                                               
ELEM190  DC    AL1(ELEM190L)                                                    
         DC    C'</tr>'                                                         
ELEM190L EQU   *-ELEM190                                                        
*                                                                               
ELEM195  DC    AL1(ELEM195L)                                                    
         DC    C'</table>'                                                      
ELEM195L EQU   *-ELEM195                                                        
*                                                                               
ELEM200  DC    AL1(ELEM200L)                                                    
         DC    C'<!-- tape check module -->'                                    
ELEM200L EQU   *-ELEM200                                                        
*                                                                               
ELEM205  DC    AL1(ELEM205L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellpadding="0" style="font-family:'           
ELEM205L EQU   *-ELEM205                                                        
*                                                                               
ELEM210  DC    AL1(ELEM210L)                                                    
         DC    C'Calibri, sans-serif; font-size:13px; color:#4E4D4C; '          
         DC    C'background-color:#D9D9CD; -webkit-border-radius: 3px;'         
         DC    C' -moz-border-radius: 3px; border-radius: 3px;">'               
ELEM210L EQU   *-ELEM210                                                        
*                                                                               
ELEM215  DC    AL1(ELEM215L)                                                    
         DC    C'<tr>'                                                          
ELEM215L EQU   *-ELEM215                                                        
*                                                                               
ELEM220  DC    AL1(ELEM220L)                                                    
         DC    C'<td height="20" colspan="5" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM220L EQU   *-ELEM220                                                        
*                                                                               
ELEM225  DC    AL1(ELEM225L)                                                    
         DC    C'<img height="20" alt="spacer" src="http://info.'               
         DC    C'mediaocean.com/rs/331-XPM-231/images/spacer_icon.png"'         
         DC    C'/>'                                                            
ELEM225L EQU   *-ELEM225                                                        
*                                                                               
ELEM230  DC    AL1(ELEM230L)                                                    
         DC    C'</td>'                                                         
ELEM230L EQU   *-ELEM230                                                        
*                                                                               
ELEM235  DC    AL1(ELEM235L)                                                    
         DC    C'</tr>'                                                         
ELEM235L EQU   *-ELEM235                                                        
*                                                                               
ELEM240  DC    AL1(ELEM240L)                                                    
         DC    C'<tr>'                                                          
ELEM240L EQU   *-ELEM240                                                        
*                                                                               
ELEM245  DC    AL1(ELEM245L)                                                    
         DC    C'<td width="20">'                                               
ELEM245L EQU   *-ELEM245                                                        
*                                                                               
ELEM250  DC    AL1(ELEM250L)                                                    
         DC    C'<img height="1" width="20" alt="spacer" '                      
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
ELEM250L EQU   *-ELEM250                                                        
*                                                                               
ELEM255  DC    AL1(ELEM255L)                                                    
         DC    C'</td>'                                                         
ELEM255L EQU   *-ELEM255                                                        
*                                                                               
ELEM260  DC    AL1(ELEM260L)                                                    
         DC    C'<td valign="top" width="360" '                                 
         DC    C'style="font-family: Calibri, sans-serif; '                     
         DC    C'font-size:30px; color:#4E4D4C; line-height:36px;">'            
ELEM260L EQU   *-ELEM260                                                        
*                                                                               
ELEM265  DC    AL1(ELEM265L)                                                    
         DC    C'<div style="line-height:36px;">'                               
ELEM265E DS    CL80                Here are the details.....                    
         DC    C'</div>'                                                        
ELEM265L EQU   *-ELEM265                                                        
*                                                                               
ELEM270  DC    AL1(ELEM270L)                                                    
         DC    C'<p style="line-height:11pt;">'                                 
ELEM270L EQU   *-ELEM270                                                        
*                                                                               
ELEM275  DC    AL1(ELEM275L)                                                    
         DC    C'<a style="color:#4E4D4C; font-size:13px; '                     
         DC    C'text-decoration:none; font-weight:bold;" href="'               
ELEM275L EQU   *-ELEM275                                                        
*                                                                               
ELEM276  DC    AL1(ELEM276L)                                                    
ELEM276E DS    CL160               Url e.g. http://aura.mediaocean.com          
ELEM276L EQU   *-ELEM276                                                        
*                                                                               
ELEM278  DC    AL1(ELEM278L)                                                    
ELEM278E DS    CL160               Url e.g. http://aura.mediaocean.com          
ELEM278L EQU   *-ELEM278                                                        
*                                                                               
ELEM280  DC    AL1(ELEM280L)                                                    
ELEM280E DS    CL80               Please click here to access....               
ELEM280L EQU   *-ELEM280                                                        
*                                                                               
ELEM285  DC    AL1(ELEM285L)                                                    
         DC    C'</a>'                                                          
ELEM285L EQU   *-ELEM285                                                        
*                                                                               
ELEM290  DC    AL1(ELEM290L)                                                    
         DC    C'</p>'                                                          
ELEM290L EQU   *-ELEM290                                                        
*                                                                               
ELEM295  DC    AL1(ELEM295L)                                                    
         DC    C'</td>'                                                         
ELEM295L EQU   *-ELEM295                                                        
*                                                                               
ELEM300  DC    AL1(ELEM300L)                                                    
         DC    C'<td width="20">'                                               
ELEM300L EQU   *-ELEM300                                                        
*                                                                               
ELEM305  DC    AL1(ELEM305L)                                                    
         DC    C'<img height="1" width="20" alt="spacer" src="'                 
         DC    C'http://info.mediaocean.com/rs/331-XPM-231/'                    
         DC    C'images/spacer_icon.png"/>'                                     
ELEM305L EQU   *-ELEM305                                                        
*                                                                               
ELEM310  DC    AL1(ELEM310L)                                                    
         DC    C'</td>'                                                         
ELEM310L EQU   *-ELEM310                                                        
*                                                                               
ELEM315  DC    AL1(ELEM315L)                                                    
         DC    C'<td style="vertical-align:top; font-family: Calibri, '         
         DC    C'sans-serif; font-size:13px; color:#4E4D4C; '                   
         DC    C'line-height:14pt;">'                                           
ELEM315L EQU   *-ELEM315                                                        
*                                                                               
ELEM320  DC    AL1(ELEM320L)                                                    
         DC    C'<table border="0" style="background-color:#ffffff;">'          
ELEM320L EQU   *-ELEM320                                                        
*                                                                               
ELEM325  DC    AL1(ELEM325L)                                                    
         DC    C'<tr>'                                                          
ELEM325L EQU   *-ELEM325                                                        
*                                                                               
ELEM330  DC    AL1(ELEM330L)                                                    
         DC    C'<td width="360" colspan="2">'                                  
ELEM330L EQU   *-ELEM330                                                        
*                                                                               
ELEM335  DC    AL1(ELEM335L)                                                    
         DC    C'<img height="20" width="360" border="0" alt="spacer"'          
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM335L EQU   *-ELEM335                                                        
*                                                                               
ELEM340  DC    AL1(ELEM340L)                                                    
         DC    C'</td>'                                                         
ELEM340L EQU   *-ELEM340                                                        
*                                                                               
ELEM345  DC    AL1(ELEM345L)                                                    
         DC    C'</tr>'                                                         
ELEM345L EQU   *-ELEM345                                                        
*                                                                               
ELEM346  DC    AL1(ELEM346L)                                                    
         DC    C'<tr>'                                                          
ELEM346L EQU   *-ELEM346                                                        
*                                                                               
ELEM350  DC    AL1(ELEM350L)                                                    
         DC    C'<td width="20">'                                               
ELEM350L EQU   *-ELEM350                                                        
*                                                                               
ELEM355  DC    AL1(ELEM355L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer"'            
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM355L EQU   *-ELEM355                                                        
*                                                                               
ELEM360  DC    AL1(ELEM360L)                                                    
         DC    C'</td>'                                                         
ELEM360L EQU   *-ELEM360                                                        
*                                                                               
ELEM365  DC    AL1(ELEM365L)                                                    
         DC    C'<td width="340" style="font-family: Calibri, '                 
         DC    C'sans-serif; font-size:13px;">'                                 
ELEM365L EQU   *-ELEM365                                                        
*                                                                               
ELEM370  DC    AL1(ELEM370L)                                                    
         DC    C'<div style="width:360px; background-color:#ffffff;">'          
ELEM370E DS    CL80                 for the period ending                       
         DC    C'</div>'                                                        
         DC    C'</td>'                                                         
ELEM370L EQU   *-ELEM370                                                        
*                                                                               
ELEM375  DC    AL1(ELEM375L)                                                    
         DC    C'</tr>'                                                         
ELEM375L EQU   *-ELEM375                                                        
*                                                                               
ELEM380  DC    AL1(ELEM380L)                                                    
         DC    C'<tr>'                                                          
ELEM380L EQU   *-ELEM380                                                        
*                                                                               
ELEM385  DC    AL1(ELEM385L)                                                    
         DC    C'<td width="20">'                                               
ELEM385L EQU   *-ELEM385                                                        
*                                                                               
ELEM390  DC    AL1(ELEM390L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer" '           
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM390L EQU   *-ELEM390                                                        
*                                                                               
ELEM395  DC    AL1(ELEM395L)                                                    
         DC    C'</td>'                                                         
ELEM395L EQU   *-ELEM395                                                        
*                                                                               
ELEM400  DC    AL1(ELEM400L)                                                    
         DC    C'<td>'                                                          
ELEM400L EQU   *-ELEM400                                                        
*                                                                               
ELEM405  DC    AL1(ELEM405L)                                                    
         DC    C'<div style="width:340px; height:380px; '                       
         DC    C'overflow-y:scroll; background-color:#ffffff;">'                
ELEM405L EQU   *-ELEM405                                                        
*                                                                               
ELEM410  DC    AL1(ELEM410L)                                                    
         DC    C'<table cellpadding="5" style="font-family: Calibri, '          
         DC    C'sans-serif; font-size:13px;">'                                 
ELEM410L EQU   *-ELEM410                                                        
*                                                                               
** list of timesheets goes here see TABROD DSECT **                             
*                                                                               
ELEMTML  DS    0X                                                               
*                                                                               
ELEM415  DC    AL1(ELEM415L)                                                    
         DC    C'</table>'                                                      
ELEM415L EQU   *-ELEM415                                                        
*                                                                               
ELEM420  DC    AL1(ELEM420L)                                                    
         DC    C'</div>'                                                        
ELEM420L EQU   *-ELEM420                                                        
*                                                                               
ELEM425  DC    AL1(ELEM425L)                                                    
         DC    C'</td>'                                                         
ELEM425L EQU   *-ELEM425                                                        
*                                                                               
ELEM430  DC    AL1(ELEM430L)                                                    
         DC    C'</tr>'                                                         
ELEM430L EQU   *-ELEM430                                                        
*                                                                               
ELEM440  DC    AL1(ELEM440L)                                                    
         DC    C'<tr>'                                                          
ELEM440L EQU   *-ELEM440                                                        
*                                                                               
ELEM445  DC    AL1(ELEM445L)                                                    
         DC    C'<td width="360" colspan="2">'                                  
ELEM445L EQU   *-ELEM445                                                        
*                                                                               
ELEM450  DC    AL1(ELEM450L)                                                    
         DC    C'<img height="20" width="360" border="0" alt="spacer" '         
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM450L EQU   *-ELEM450                                                        
*                                                                               
ELEM455  DC    AL1(ELEM455L)                                                    
         DC    C'</td>'                                                         
ELEM455L EQU   *-ELEM455                                                        
*                                                                               
ELEM460  DC    AL1(ELEM460L)                                                    
         DC    C'</tr>'                                                         
ELEM460L EQU   *-ELEM460                                                        
*                                                                               
ELEM465  DC    AL1(ELEM465L)                                                    
         DC    C'</table>'                                                      
ELEM465L EQU   *-ELEM465                                                        
*                                                                               
ELEM470  DC    AL1(ELEM470L)                                                    
         DC    C'</td>'                                                         
ELEM470L EQU   *-ELEM470                                                        
*                                                                               
ELEM475  DC    AL1(ELEM475L)                                                    
         DC    C'<td width="20">'                                               
ELEM475L EQU   *-ELEM475                                                        
*                                                                               
ELEM480  DC    AL1(ELEM480L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM480L EQU   *-ELEM480                                                        
*                                                                               
ELEM485  DC    AL1(ELEM485L)                                                    
         DC    C'</td>'                                                         
ELEM485L EQU   *-ELEM485                                                        
*                                                                               
ELEM490  DC    AL1(ELEM490L)                                                    
         DC    C'</tr>'                                                         
ELEM490L EQU   *-ELEM490                                                        
*                                                                               
ELEM495  DC    AL1(ELEM495L)                                                    
         DC    C'<tr>'                                                          
ELEM495L EQU   *-ELEM495                                                        
*                                                                               
ELEM500  DC    AL1(ELEM500L)                                                    
         DC    C'<td height="30" colspan="3" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM500L EQU   *-ELEM500                                                        
*                                                                               
ELEM505  DC    AL1(ELEM505L)                                                    
         DC    C'<img height="30" width="20" border="0" alt="spacer" '          
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM505L EQU   *-ELEM505                                                        
*                                                                               
ELEM510  DC    AL1(ELEM510L)                                                    
         DC    C'</td>'                                                         
ELEM510L EQU   *-ELEM510                                                        
*                                                                               
ELEM515  DC    AL1(ELEM515L)                                                    
         DC    C'</tr>'                                                         
ELEM515L EQU   *-ELEM515                                                        
*                                                                               
ELEM520  DC    AL1(ELEM520L)                                                    
         DC    C'</table>'                                                      
ELEM520L EQU   *-ELEM520                                                        
*                                                                               
ELEM525  DC    AL1(ELEM525L)                                                    
         DC    C'<!-- no background module -->'                                 
ELEM525L EQU   *-ELEM525                                                        
*                                                                               
ELEM530  DC    AL1(ELEM530L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellspadding="0" style="font-family: '         
         DC    C'Calibri, sans-serif; font-size:10px; color:#4E4D4C; '          
ELEM530L EQU   *-ELEM530                                                        
*                                                                               
ELEM535  DC    AL1(ELEM535L)                                                    
         DC    C'background-color:#ffffff; -webkit-border-radius: 3px;'         
         DC    C' -moz-border-radius: 3px; border-radius: 3px;">'               
ELEM535L EQU   *-ELEM535                                                        
*                                                                               
ELEM540  DC    AL1(ELEM540L)                                                    
         DC    C'<tr>'                                                          
ELEM540L EQU   *-ELEM540                                                        
*                                                                               
ELEM545  DC    AL1(ELEM545L)                                                    
         DC    C'<td rowspan="3" width="30"><img height="1" width="30"'         
         DC    C' border="0" alt="spacer" src="http://info.mediaocean'          
         DC    C'.com/rs/331-XPM-231/images/spacer_icon.png"/>'                 
         DC    C'</td>'                                                         
ELEM545L EQU   *-ELEM545                                                        
*                                                                               
ELEM550  DC    AL1(ELEM550L)                                                    
         DC    C'<td>'                                                          
         DC    X'50'                                                            
         DC    C'nbsp;</td>'                                                    
ELEM550L EQU   *-ELEM550                                                        
*                                                                               
ELEM555  DC    AL1(ELEM555L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
ELEM555L EQU   *-ELEM555                                                        
*                                                                               
ELEM560  DC    AL1(ELEM560L)                                                    
         DC    C'</td>'                                                         
ELEM560L EQU   *-ELEM560                                                        
*                                                                               
ELEM565  DC    AL1(ELEM565L)                                                    
         DC    C'</tr>'                                                         
ELEM565L EQU   *-ELEM565                                                        
*                                                                               
ELEM570  DC    AL1(ELEM570L)                                                    
         DC    C'<tr>'                                                          
ELEM570L EQU   *-ELEM570                                                        
*                                                                               
ELEM575  DC    AL1(ELEM575L)                                                    
         DC    C'<td style="vertical-align:top;">'                              
ELEM575L EQU   *-ELEM575                                                        
*                                                                               
ELEM580  DC    AL1(ELEM580L)                                                    
         DC    C'<p>'                                                           
ELEM580L EQU   *-ELEM580                                                        
*                                                                               
ELEM585  DC    AL1(ELEM585L)                                                    
EMAILL1  DS    CL80                                                             
EMAILL2  DS    CL80                                                             
ELEM585L EQU   *-ELEM585                                                        
*                                                                               
ELEM600  DC    AL1(ELEM600L)                                                    
EMAILL3  DS    CL80                                                             
EMAILL4  DS    CL80                                                             
ELEM600L EQU   *-ELEM600                                                        
*                                                                               
ELEM605  DC    AL1(ELEM605L)                                                    
EMAILL5  DS    CL80                                                             
         DC    C'</p>'                                                          
ELEM605L EQU   *-ELEM605                                                        
*                                                                               
ELEM610  DC    AL1(ELEM610L)                                                    
         DC    C'<p>'                                                           
ELEM610L EQU   *-ELEM610                                                        
*                                                                               
ELEM615  DC    AL1(ELEM615L)                                                    
EMAILL6  DS    CL80                                                             
EMAILL7  DS    CL80                                                             
ELEM615L EQU   *-ELEM615                                                        
*                                                                               
ELEM630  DC    AL1(ELEM630L)                                                    
EMAILL8  DS    CL80                                                             
EMAILL9  DS    CL80                                                             
ELEM630L EQU   *-ELEM630                                                        
*                                                                               
ELEM632  DC    AL1(ELEM632L)                                                    
EMAILLA  DS    CL80                                                             
ELEM632L EQU   *-ELEM632                                                        
*                                                                               
ELEM635  DC    AL1(ELEM635L)                                                    
         DC    C'</p>'                                                          
ELEM635L EQU   *-ELEM635                                                        
*                                                                               
ELEM655  DC    AL1(ELEM655L)                                                    
         DC    C'</td>'                                                         
ELEM655L EQU   *-ELEM655                                                        
*                                                                               
ELEM660  DC    AL1(ELEM660L)                                                    
         DC    C'</tr>'                                                         
ELEM660L EQU   *-ELEM660                                                        
*                                                                               
ELEM665  DC    AL1(ELEM665L)                                                    
         DC    C'<tr>'                                                          
ELEM665L EQU   *-ELEM665                                                        
*                                                                               
ELEM670  DC    AL1(ELEM670L)                                                    
         DC    C'<td height="20" style="font-size:1px; '                        
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM670L EQU   *-ELEM670                                                        
*                                                                               
ELEM675  DC    AL1(ELEM675L)                                                    
         DC    C'<img height="20" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
ELEM675L EQU   *-ELEM675                                                        
*                                                                               
ELEM680  DC    AL1(ELEM680L)                                                    
         DC    C'</td>'                                                         
ELEM680L EQU   *-ELEM680                                                        
*                                                                               
ELEM685  DC    AL1(ELEM685L)                                                    
         DC    C'</tr>'                                                         
ELEM685L EQU   *-ELEM685                                                        
*                                                                               
ELEM690  DC    AL1(ELEM690L)                                                    
         DC    C'</table>'                                                      
ELEM690L EQU   *-ELEM690                                                        
*                                                                               
ELEM695  DC    AL1(ELEM695L)                                                    
         DC    C'<!-- end: body -->'                                            
ELEM695L EQU   *-ELEM695                                                        
*                                                                               
ELEM700  DC    AL1(ELEM700L)                                                    
         DC    C'<!-- Footer -->'                                               
ELEM700L EQU   *-ELEM700                                                        
*                                                                               
ELEM705  DC    AL1(ELEM705L)                                                    
         DC    C'<table width="100%" cellspacing="0" cellpadding="0" '          
         DC    C'style="height:78px; font-family: Calibri, sans-serif;'         
ELEM705L EQU   *-ELEM705                                                        
*                                                                               
ELEM710  DC    AL1(ELEM710L)                                                    
         DC    C' font-size:10px; color:#ffffff; '                              
         DC    C'background-color:#4E4D4C; text-align:center; '                 
         DC    C'border-bottom:1px solid #4E4D4C; '                             
ELEM710L EQU   *-ELEM710                                                        
*                                                                               
ELEM715  DC    AL1(ELEM715L)                                                    
         DC    C'-webkit-border-radius: 3px; -moz-border-radius: 3px; '         
         DC    C'border-radius: 3px;">'                                         
ELEM715L EQU   *-ELEM715                                                        
*                                                                               
ELEM720  DC    AL1(ELEM720L)                                                    
         DC    C'<tr>'                                                          
ELEM720L EQU   *-ELEM720                                                        
*                                                                               
ELEM725  DC    AL1(ELEM725L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
ELEM725L EQU   *-ELEM725                                                        
*                                                                               
ELEM730  DC    AL1(ELEM730L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer"'            
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM730L EQU   *-ELEM730                                                        
*                                                                               
ELEM735  DC    AL1(ELEM735L)                                                    
         DC    C'</td>'                                                         
ELEM735L EQU   *-ELEM735                                                        
*                                                                               
ELEM740  DC    AL1(ELEM740L)                                                    
         DC    C'<td colspan="2" height="15" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM740L EQU   *-ELEM740                                                        
*                                                                               
ELEM745  DC    AL1(ELEM745L)                                                    
         DC    C'<img height="15" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
ELEM745L EQU   *-ELEM745                                                        
*                                                                               
ELEM750  DC    AL1(ELEM750L)                                                    
         DC    C'</td>'                                                         
ELEM750L EQU   *-ELEM750                                                        
*                                                                               
ELEM755  DC    AL1(ELEM755L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
ELEM755L EQU   *-ELEM755                                                        
*                                                                               
ELEM760  DC    AL1(ELEM760L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer"'            
         DC    C' src="http://info.mediaocean.com/rs/331-XPM-231/'              
         DC    C'images/spacer_icon.png"/>'                                     
ELEM760L EQU   *-ELEM760                                                        
*                                                                               
ELEM765  DC    AL1(ELEM765L)                                                    
         DC    C'</td>'                                                         
ELEM765L EQU   *-ELEM765                                                        
*                                                                               
ELEM770  DC    AL1(ELEM770L)                                                    
         DC    C'</tr>'                                                         
ELEM770L EQU   *-ELEM770                                                        
*                                                                               
ELEM775  DC    AL1(ELEM775L)                                                    
         DC    C'<tr style="vertical-align:bottom;">'                           
ELEM775L EQU   *-ELEM775                                                        
*                                                                               
ELEM780  DC    AL1(ELEM780L)                                                    
         DC    C'<td style="text-align:left;">'                                 
ELEM780L EQU   *-ELEM780                                                        
*                                                                               
ELEM785  DC    AL1(ELEM785L)                                                    
         DC    C'<div>'                                                         
ELEM785L EQU   *-ELEM785                                                        
*                                                                               
ELEM790  DC    AL1(ELEM790L)                                                    
ELEM790E DS    CL90                                                             
ELEM790L EQU   *-ELEM790                                                        
*                                                                               
ELEM795  DC    AL1(ELEM795L)                                                    
         DC    C'<br />'                                                        
ELEM795L EQU   *-ELEM795                                                        
*                                                                               
ELEM800  DC    AL1(ELEM800L)                                                    
         DC    X'50'                                                            
         DC    C'copy '                                                         
ELEM800E DS    CL4                                                              
         DC    C' MEDIAOCEAN'                                                   
ELEM800L EQU   *-ELEM800                                                        
*                                                                               
ELEM805  DC    AL1(ELEM805L)                                                    
         DC    C'| '                                                            
ELEM805E DS    CL18                                                             
         DC    C':'                                                             
ELEM805L EQU   *-ELEM805                                                        
*                                                                               
ELEM810  DC    AL1(ELEM810L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://www.facebook.com/team.mediaocean">'              
         DC    C'FACEBOOK'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
ELEM810L EQU   *-ELEM810                                                        
*                                                                               
ELEM815  DC    AL1(ELEM815L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://twitter.com/teammediaocean">'                    
         DC    C'TWITTER'                                                       
         DC    C'</a>'                                                          
         DC    C' |'                                                            
ELEM815L EQU   *-ELEM815                                                        
*                                                                               
ELEM820  DC    AL1(ELEM820L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://www.linkedin.com/company/mediaocean">'            
         DC    C'LINKEDIN'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
ELEM820L EQU   *-ELEM820                                                        
*                                                                               
ELEM822  DC    AL1(ELEM822L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://instagram.com/teammediaocean">'                   
         DC    C'INSTAGRAM'                                                     
         DC    C'</a>'                                                          
ELEM822L EQU   *-ELEM822                                                        
*                                                                               
ELEM825  DC    AL1(ELEM825L)                                                    
         DC    C'</div>'                                                        
ELEM825L EQU   *-ELEM825                                                        
*                                                                               
ELEM830  DC    AL1(ELEM830L)                                                    
         DC    C'</td>'                                                         
ELEM830L EQU   *-ELEM830                                                        
*                                                                               
ELEM835  DC    AL1(ELEM835L)                                                    
         DC    C'<td style="text-align:right;">'                                
ELEM835L EQU   *-ELEM835                                                        
*                                                                               
ELEM840  DC    AL1(ELEM840L)                                                    
         DC    C'<a href="'                                                     
ELEM840E DS    CL20                                                             
         DC    C'">'                                                            
ELEM840L EQU   *-ELEM840                                                        
*                                                                               
ELEM845  DC    AL1(ELEM845L)                                                    
         DC    C'<img src="http://info.mediaocean.com/rs/331-XPM-231/'          
         DC    C'images/MO-Aura-footer-logo.png" alt="MediaOcean" />'           
ELEM845L EQU   *-ELEM845                                                        
*                                                                               
ELEM850  DC    AL1(ELEM850L)                                                    
         DC    C'</a>'                                                          
ELEM850L EQU   *-ELEM850                                                        
*                                                                               
ELEM855  DC    AL1(ELEM855L)                                                    
         DC    C'</td>'                                                         
ELEM855L EQU   *-ELEM855                                                        
*                                                                               
ELEM860  DC    AL1(ELEM860L)                                                    
         DC    C'</tr>'                                                         
ELEM860L EQU   *-ELEM860                                                        
*                                                                               
ELEM865  DC    AL1(ELEM865L)                                                    
         DC    C'<tr>'                                                          
ELEM865L EQU   *-ELEM865                                                        
*                                                                               
ELEM870  DC    AL1(ELEM870L)                                                    
         DC    C'<td colspan="2" height="15" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
ELEM870L EQU   *-ELEM870                                                        
*                                                                               
ELEM875  DC    AL1(ELEM875L)                                                    
         DC    C'<img height="15" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
ELEM875L EQU   *-ELEM875                                                        
*                                                                               
ELEM880  DC    AL1(ELEM880L)                                                    
         DC    C'</td>'                                                         
ELEM880L EQU   *-ELEM880                                                        
*                                                                               
ELEM885  DC    AL1(ELEM885L)                                                    
         DC    C'</tr>'                                                         
ELEM885L EQU   *-ELEM885                                                        
*                                                                               
ELEM890  DC    AL1(ELEM890L)                                                    
         DC    C'</table>'                                                      
ELEM890L EQU   *-ELEM890                                                        
*                                                                               
ELEM895  DC    AL1(ELEM895L)                                                    
         DC    C'<!-- end: Footer -->'                                          
ELEM895L EQU   *-ELEM895                                                        
*                                                                               
ELEM900  DC    AL1(ELEM900L)                                                    
         DC    C'</td>'                                                         
ELEM900L EQU   *-ELEM900                                                        
*                                                                               
ELEM905  DC    AL1(ELEM905L)                                                    
         DC    C'</tr>'                                                         
ELEM905L EQU   *-ELEM905                                                        
*                                                                               
ELEM910  DC    AL1(ELEM910L)                                                    
         DC    C'</table>'                                                      
ELEM910L EQU   *-ELEM910                                                        
*                                                                               
ELEM915  DC    AL1(ELEM915L)                                                    
         DC    C'<!-- End Inner Wrap -->'                                       
ELEM915L EQU   *-ELEM915                                                        
*                                                                               
ELEM920  DC    AL1(ELEM920L)                                                    
         DC    C'</td>'                                                         
ELEM920L EQU   *-ELEM920                                                        
*                                                                               
ELEM925  DC    AL1(ELEM925L)                                                    
         DC    C'</tr>'                                                         
ELEM925L EQU   *-ELEM925                                                        
*                                                                               
ELEM930  DC    AL1(ELEM930L)                                                    
         DC    C'</table>'                                                      
ELEM930L EQU   *-ELEM930                                                        
*                                                                               
ELEM935  DC    AL1(ELEM935L)                                                    
         DC    C'<!-- End Outer Wrap -->'                                       
ELEM935L EQU   *-ELEM935                                                        
*                                                                               
ELEM940  DC    AL1(ELEM940L)                                                    
         DC    C'</body>'                                                       
ELEM940L EQU   *-ELEM940                                                        
*                                                                               
ELEM945  DC    AL1(ELEM945L)                                                    
         DC    C'</html>'                                                       
ELEM945L EQU   *-ELEM945                                                        
*                                                                               
EMLHTMLL EQU   *-EMLHTML                                                        
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041ACREPRM02 03/12/21'                                      
         END                                                                    
