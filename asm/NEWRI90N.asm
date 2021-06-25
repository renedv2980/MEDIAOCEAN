*          DATA SET NEWRI90N   AT LEVEL 027 AS OF 05/01/02                      
*          DATA SET NEWRI90    AT LEVEL 182 AS OF 08/14/98                      
*PHASE T32090A,+0                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*                                                                               
         TITLE 'T32090 - NETWORK HISTORY AD HOC REPORT'                         
************************************************************                    
*                                                                               
* THIS REPORT READS HISTORY RECORDS AND WRITES REPORT OF CHANGES                
*                                                                               
*************************************************************                   
         SPACE 2                                                                
T32090   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T32090*,RA                                                    
         USING T32090,RB,RA                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
                                                                                
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
                                                                                
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
*                                                                               
         L     R1,ANETWS1        ANETWS1 AND 2  FOR CLIENT RECORD               
         ST    R1,NBACLI                                                        
*                                                                               
         L     R7,ANETWS3                                                       
         USING MYWORKD,R7                                                       
                                                                                
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    RE,HEDSPECS                                                      
         ST    RE,SPECS                                                         
****     LA    R1,HDRTN                                                         
****     ST    R1,HEADHOOK                                                      
         CLI   PRDBRK,1                                                         
         BNE   SKIP2                                                            
         LA    RE,HEDSPEC2        ALWAYS PAGE BREAK                             
         ST    RE,SPECS                                                         
         LA    R1,HDRTN2                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
SKIP2    L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'165'                                                 
         L     R1,BOXAWIDE                                                      
         ST    R1,ADRWIDE                                                       
         DROP  R1                                                               
                                                                                
*                                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    MAINLINE                                                         
EXIT     XIT1                                                                   
                                                                                
         DROP  R6                                                               
                                                                                
         EJECT                                                                  
*                                                                               
MAINLINE DS    0H                                                               
         SPACE                                                                  
         BAS   RE,INIT         INITIALIZE                                       
         BAS   RE,RCODES       READ AND STORE REASON CODES                      
         BAS   RE,WRTRPRT      WRITE REPORT                                     
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
INIT     NTR1                                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAYC)     COMPRESSED                      
*                                                                               
         CLI   REQACTST,0          ACTIVITY DATE INPUT?                         
         BNE   INIT03                                                           
         MVC   REQACTST,TODAYC     NO/USE TODAY'S DATE                          
         MVC   REQACTND,TODAYC     NO/USE TODAY'S DATE                          
*                                                                               
INIT03   CLI   NBSELSTR,0          DO WE HAVE REQ UNIT START DATE?              
         BE    INIT04                                                           
         GOTO1 DATCON,DMCB,(0,NBSELSTR),(2,NBCMPSTR)                            
INIT04   CLI   NBSELEND,0          DO WE HAVE REQ UNIT END DATE?                
         BE    INIT05                                                           
         GOTO1 DATCON,DMCB,(0,NBSELEND),(2,NBCMPEND)                            
*                                                                               
INIT05   GOTO1 SORTER,DMCB,SORTCARD,RECCARD      INITIALIZE SORTER              
         B     INIT10                                                           
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,21,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
*                                                                               
                                                                                
INIT10   OC    HDTITLE,HDTITLE     WAS THERE A TITLE OVERRIDE?                  
         BZ    INIT11                                                           
         B     *+10                                                             
INIT11   MVC   HDTITLE(22),=C'ANALYSIS CHANGE REPORT'                           
         OC    HDTITLE,SPACES                                                   
         GOTO1 CENTER,DMCB,HDTITLE,60                                           
*                                                                               
         CLI   REQRSN,0            FILTERING ON REASON CODE?                    
         BE    INIT20                                                           
         LA    R1,3                                                             
         LA    R2,REQRSN+2                                                      
INIT12   CLI   0(R2),0                                                          
         BNE   INIT20                                                           
         MVI   0(R2),X'40'         END FILL WITH BLANKS                         
         BCTR  R2,0                                                             
         BCT   R1,INIT12                                                        
                                                                                
INIT20   MVI   WHOTBLX,X'FF'                                                    
         MVI   TIMSUB,0                                                         
INITX    B     EXIT                                                             
*                                                                               
*                                                                               
         EJECT                                                                  
*****************************************************                           
*  WRITE REPORT                                                                 
*                                                                               
*****************************************************                           
WRTRPRT  NTR1                                                                   
*                                                                               
* READ HISTORY RECORDS                                                          
WRT00    XC    KEY,KEY                                                          
         MVI   KEY,X'40'                                                        
         MVC   KEY+1(1),AMSAVE     AGENCY/MEDIA                                 
         BAS   RE,MYHIGH                                                        
         B     WRT16                                                            
*                                                                               
WRT15    BAS   RE,MYSEQ           GET NEXT HISTORY REC                          
*                                                                               
WRT16    CLC   KEY(2),KEYSAVE                                                   
         BNE   WRT30                                                            
         BAS   RE,CHKFILTS         CHECK FILTERS AGAINST HIST KEY               
         BE    WRT17               OK READ RECORD                               
         BNE   WRT15               NOT OK READSEQ                               
                                                                                
WRT17    BAS   RE,MYGET           GET RECORD                                    
         L     R6,=A(MYIO2)       R6 -> RECORD                                  
         USING NHRECD,R6                                                        
         B     WRT20                                                            
                                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
******************************************************************              
* SET UP SORT WITH X'01' DATA                                                   
* RECS ARE SET UP IN SRTTBL                                                     
* 'COPY' GETS MOVED TO SORTC                                                    
* 'CHANGE' STAYS IN SRTTBL                                                      
*                                                                               
*                                                                               
WRT20    XC    SRTTBL(SRTBLEN),SRTTBL     CLEAR TBL                             
         XC    SORTC(SRTBLEN),SORTC       CLEAR TBL                             
                                                                                
* - SORT KEY FIELDS                                                             
         MVC   SAVAMC,NHKPAM       SAVE AGY/MEDIA/CLI (3 BYTES)                 
         MVC   SRTKCLT,NHCLIENT    CLIENT                                       
         CLI   PRDBRK,C'Y'                                                      
         BNE   NOPRDBRK                                                         
         MVC   SRPROD,NNHOPRD      SET PRODUCT FOR CALL                         
         MVC   SRTAMC,SAVAMC       SET AGY/MED/CLT                              
         BAS   RE,GETPRD                                                        
         MVC   SRTKPROD,WORK                                                    
NOPRDBRK DS    0H                                                               
*                                                                               
         MVC   SRTKGRP,NHADTGRP    AUDIT GROUP                                  
         OC    SRTKGRP,SPACES                                                   
         MVC   SRTKNET,NHKNET      NETWORK                                      
         MVC   SRTKPRG,NHKPROG     PROGRAM                                      
         MVC   SRTKDAT,NHKDATE     AIR DATE                                     
         MVC   SRTKEST,NHKEST      ESTIMATE                                     
         MVC   SRTKSUB,NHKSUB      SUBLINE                                      
         MVC   SRTKPKG,NHPKG       PACKAGE                                      
                                                                                
         GOTO1 DATCON,DMCB,(3,NHADDAT),(2,SRTKCHD) DATE HISTORY CREATED         
         MVC   SRTKTIM,NHADDTM                     TIME HIST CREATED            
                                                                                
* - SORT DATA FIELDS                                                            
         MVC   SRUNTDAT,NHODATE    DATE                                         
         MVC   SRPRGNM,NHOPRNME    PROGRAM NAME                                 
         MVC   SRROT,NHOROT        ROTATION                                     
         MVC   SRTIM,NHOTIME       TIME                                         
         MVC   SRLEN,NHOLEN        LENGTH                                       
         MVC   SRCOST,NHOACT       COST                                         
         OC    SRCOST,SRCOST       DO WE HAVE COST $ ?                          
         BNZ   HASCOST             YES                                          
         TM    NHSTAT,X'02'        NO - ACT COST OVERRIDE ?                     
         BNO   *+8                                                              
         MVI   SRCOST,C'0'         YES                                          
HASCOST  MVC   SRPROD,NNHOPRD      PRODUCT                                      
         MVC   SRREASON,NHREASN    REASON CODE                                  
         MVC   SRCOMN,NHOCMMT      COMMENT                                      
         MVC   SRBUYER,NHUSER      USER                                         
         MVC   SRPRMT,NHOPRE       PREEMPT STATUS                               
         MVC   SRTAMC,SAVAMC       SET AGY/MED/CLT                              
         MVI   SRTYPE,C'D'         ASSUME DELETE                                
         MVI   WORK,0                                                           
         TM    NHSTAT,X'01'        NEW UNIT                                     
         BO    WRT20A              YES                                          
* ORIGINAL UNITS WITH NO SUBSEQUENT CHANGES ARE SKIPPED                         
         CLI   NHCHGDAT,0         ANY CHANGES?                                  
         BE    WRT15              NO - SKIP                                     
         GOTO1 DATCON,DMCB,(2,NHCHGDAT),(3,WORK)                                
         CLC   WORK(3),NHADDAT     LAST CHANGE = ADD DATE = ?                   
         BNE   WRT20A              NO - SO SUBSEQUENT CHANGES-PROCESS           
         CLC   NHADDTM,NHCHGTIM    YES - ADD TIME = CHANGE TIME ?               
         BE    WRT15                    YES/SKIP                                
*                                                                               
WRT20A   DS    0H                  ANY CHANGE ELEMENTS ?                        
         L     R6,=A(MYIO2)       R6 -> RECORD                                  
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         BNE   WRT20AB             NO                                           
         MVC   SORTC,SRTTBL        YES - SAVE THIS AS 'COPY'                    
         B     WRT20B                                                           
                                                                                
WRT20AB  BAS   RE,CHKSPCL          CHECK AGAINST SPCL TYPES                     
         BNE   WRT15               NOT OK                                       
         BAS   RE,CHKFILT2         CHECK AGAINST RECORD FILTER                  
         BNE   WRT15               NOT OK                                       
         MVI   SRTYPE,C'A'         OK - SET IT AS 'ADD'                         
         BAS   RE,PUTSORT1         SEND TO SORTER                               
         B     WRT15               GET NEXT HISTORY REC                         
                                                                                
         EJECT                                                                  
                                                                                
* - NOW HANDLE CHANGES  (THE X'05' ELEMENTS)                                    
         USING NCHAEL,R6                                                        
WRT20B   DS    0H                                                               
*                                                                               
WRT20F   DS    0H                                                               
         MVI   SRTYPE,C'A'                ASSUME FINAL CHANGE                   
         MVC   SRTKCHD,NCHGDATE           SET DATE OF CHANGE                    
         MVC   SRTKTIM,NCHGTIME           SET TIME OF CHANGE                    
***      BAS   RE,CHKTMSUB                                                      
WRT20H   MVC   SRREASON,NCHGREAS          REASON                                
         MVC   SRSTAT,NCHGSTAT            STATUS                                
         MVC   SRBUYER,NCHGUSER           USER                                  
         MVC   SRTAMC,SAVAMC              SET AGY/MED/CLT                       
         BAS   RE,MOVEDATA                MOVE DATA                             
*                                                                               
         CLI   PRDBRK,C'Y'         >>IF BREAKING BY PRODUCT                     
         BNE   NOPBRK                                                           
         CLI   NCHGFCOD,C'B'       >>AND IF X'05' ELEM IS PROD CAHNGE           
         BNE   NOPBRK                                                           
         BAS   RE,GETPRD           >>GET PRINTABLE PROD CODE                    
         MVC   SRTKPROD,WORK                                                    
*                                                                               
NOPBRK   DS    0H                                                               
         MVC   BYTE,NCHGFCOD                                                    
         BAS   RE,NEXTEL                  ANY MORE CHANGE ELEMENTS              
         BNE   WRT25                      NO                                    
         CLC   SRTKTIM,NCHGTIME           YES/IS IT SAME TIME?                  
         BNE   WRT20I                                                           
         CLC   SRTKCHD,NCHGDATE           YES/IS IT SAME DATE?                  
         BNE   WRT20I                                                           
***      BE    WRT20H                                                           
         CLI   BYTE,C'G'           IF MKGD                                      
         BE    *+12                                                             
         CLI   BYTE,C'M'           OR MISSED                                    
         BNE   WRT20H                 NO - MOVE DATA TO SAME SORTREC            
         CLI   NCHGFCOD,C'G'       AND IF NEXT O5 ELEM                          
         BE    *+12                                                             
         CLI   NCHGFCOD,C'M'       MKGD OR MDS                                  
         BNE   WRT20H                                                           
         BAS   RE,FILTERS          CHECK IT AGAINST FILTERS                     
         BNE   WRT20K                                                           
* PUT BOTH COPY AND CHANGE TO SORT                                              
* THEN CLEAR COPY AREA SO NEXT MISSED/MKGD IS AN ADD WITH NO COPY               
* ATTEMPTING TO DO 2 AND 3 FOR ONE MKGS                                         
         MVI   SRTYPE,C'B'         SPECIAL TYPE OF ADD(NO SKIP LINE)            
         BAS   RE,PUTSORT2         PUT COPY/CHANGE TO SORT                      
         XC    SORTC,SORTC         CLEAR COPY AREA                              
         B     WRT20F                                                           
                                                                                
* - MORE 05 ELEMS                                                               
WRT20I   DS    0H                                                               
         BAS   RE,FILTERS                                                       
         BNE   WRT20K                                                           
         BAS   RE,PUTSORT2         OK SEND COPY AND CHANGE TO SORTER            
WRT20K   MVI   SRTYPE,C'D'         CHANGE BECOMES COPY FOR NXT X'05'            
         MVC   SORTC,SRTTBL        CHANGE BECOMES COPY FOR NXT CHANGE           
         B     WRT20F              GET NEXT X'05' ELEM                          
                                                                                
* - NO MORE 05 ELEMS                                                            
WRT25    DS    0H                                                               
         BAS   RE,FILTERS                                                       
         BNE   WRT15                                                            
         B     WRT27               OK                                           
                                                                                
WRT27    BAS   RE,PUTSORT2         PUTS COPY AND CHANGE TO SORTER               
         B     WRT15               GET NEXT HISTORY REC                         
*                                                                               
FILTERS  NTR1                                                                   
         BAS   RE,CHKSPCL          CHECK AGAINST SPCL TYPES                     
         BNE   *+8                 NO - GET NXT HISTORY REC                     
         BAS   RE,CHKFILT2         CHECK AGAINST REC FILTERS                    
         B     EXIT                NOT OK                                       
                                                                                
************************************************************                    
CHKTMSUB NTR1       IF TIME OF PREV REC=TIME OF THIS REC                        
*                                          BUMP SUB TIME                        
         MVI   SRTKTSUB,0                                                       
         OC    TIMESV,TIMESV       FIRST TIME?                                  
         BZ    CTM30               YES                                          
         CLC   TIMESV,SRTKTIM      NO   SAME TIME AS PREVIOUS ?                 
         BNE   CTM30                                                            
         ZIC   R1,TIMSUB           YES SO BUMP SUB TIME                         
         LA    R1,1(R1)                                                         
         STC   R1,TIMSUB                                                        
         STC   R1,SRTKTSUB                     SET SUB LINE                     
         B     CTMX                            RETURN                           
                                                                                
CTM30    MVC   TIMESV,SRTKTIM       SAVE NEW TIME                               
         MVI   TIMSUB,0                                                         
*                                                                               
CTMX     B     EXIT                                                             
                                                                                
         EJECT                                                                  
**********************************************************                      
* - NO MORE HISTORY RECS / PROCESS SORTED RECS                                  
WRT30    GOTO1 SORTER,DMCB,=C'GET'                                              
*                                                                               
         MVI   DOWNIT,1                                                         
*                                                                               
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    EXIT                                                             
WRT31    MVC   SRTTBL(200),0(R3)  MOVE SORTED REC TO SORTBL                     
                                                                                
         B     SKIPP1                                                           
**       GOTO1 =V(PRNTBL),DMCB,=C'SRT',SRTTBL,C'DUMP',100,=C'1D'                
**       GOTO1 =V(PRNTBL),DMCB,=C'TYP',SRTYPE,C'DUMP',2,=C'1D'                  
SKIPP1   B     SKIPPRT                                                          
**       GOTO1 =V(PRNTBL),DMCB,=C'SRT',SRTTBL,C'DUMP',400,=C'1D'                
                                                                                
SKIPPRT  CLC   SVSRTCLT(14),SRTKCLT    NEW CLIENT/AUDGRP/NET/PROD               
         BE    WRT35                                                            
         CLI   DOWNIT,0            SKIP DISTRIBUTION IF DOWNLOAD                
         BNE   WRT35                                                            
         BAS   RE,NEWCLT                                                        
                                                                                
WRT35    DS    0H                                                               
                                                                                
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         LA    R2,XP                  R2 -> PRINT LINE                          
         USING PLINED,R2                                                        
                                                                                
* - ACTIVITY DATE                                                               
         GOTO1 DATCON,DMCB,(2,SRTKCHD),(X'20',WORK)                             
         MVC   PLACTDAT(2),WORK+2                     MM/DD/YY                  
         MVI   PLACTDAT+2,C'/'                                                  
         MVC   PLACTDAT+3(2),WORK+4                                             
         MVI   PLACTDAT+5,C'/'                                                  
         MVC   PLACTDAT+6(2),WORK                                               
                                                                                
* - STATUS                                                                      
         MVC   PLSTATUS(3),=C'ADD'                                              
         CLI   SRTYPE,C'A'                                                      
         BE    PLSTATX                                                          
         CLI   SRTYPE,C'B'         SPECIAL ADD?                                 
         BE    PLSTATX                                                          
         MVC   PLSTATUS,=C'DELETE'                                              
PLSTATX  DS    0H                                                               
                                                                                
* - UNIT DATE                                                                   
         CLI   SRUNTDAT,0                                                       
         BE    WRTPRG                                                           
         GOTO1 DATCON,DMCB,(2,SRUNTDAT),(4,PLUNTDAT)                            
         MVI   PLUNTDAT+5,C'-'                                                  
         EDIT  (B1,SRTKSUB),(3,PLUNTDAT+6),ALIGN=LEFT                           
         EDIT  (B1,SRTKPKG),(3,PLUNTDAT+10),ALIGN=LEFT                          
                                                                                
* - PROGRAM NAME                                                                
WRTPRG   MVC   PLPROG,SRPRGNM      SET PROGRAM NAME                             
         CLI   REQCODE,C'Y'        REQUEST TO SHOW PROG CODE?                   
         BNE   WRTDAY              NO                                           
         XC    PLPROG,PLPROG       YES                                          
         MVC   PLPROG(6),SRTKPRG                                                
         EDIT  (B1,SRTKEST),(3,PLPROG+7)                                        
         EDIT  (B1,SRTKPKG),(3,PLPROG+12)                                       
                                                                                
* - DAY                                                                         
WRTDAY   CLI   SRROT,0                                                          
         BE    WRTTIME                                                          
         GOTO1 UNDAY,DMCB,SRROT,PLDAY                                           
                                                                                
* - TIME                                                                        
WRTTIME  OC    SRTIM,SRTIM                                                      
         BZ    WRTLEN                                                           
         GOTO1 UNTIME,DMCB,SRTIM,PLTIME                                         
                                                                                
* - LENGTH                                                                      
WRTLEN   EDIT  (B1,SRLEN),(3,PLLEN)                                             
                                                                                
* - ACTUAL COST                                                                 
         OC    SRCOST,SRCOST                                                    
         BZ    WRTPROD                                                          
         CLI   SRCOST,C'0'                                                      
         BNE   *+14                                                             
         MVC   PLCOST(2),=C'$0'                                                 
         B     ENDACT                                                           
         ICM   R1,15,SRCOST        ACTUAL COST / DROP PENNIES                   
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(7,PLCOST)                                                  
ENDACT   DS    0H                                                               
                                                                                
* - BRAND                                                                       
WRTPROD  CLI   SRPROD,0                                                         
         BE    WRTRSN                                                           
         BAS   RE,GETPRD                                                        
         MVC   PLBRAND,WORK                                                     
                                                                                
                                                                                
* - REASON CODE                                                                 
WRTRSN   DS    0H                  JUNK IN REASON FIELD?                        
         MVC   PLREASON,SRREASON                                                
                                                                                
* - TRAIL (MAKE GOOD/MISSED)                                                    
TRAIL01  CLI   SRPRMT,C'Y'             IF PREEMPT                               
         BNE   TRAIL08                                                          
         MVC   PLTRAIL(8),=C'PRE-EMPT'    SHOW AS PREEMPT                       
         MVC   PLSTATUS,=C'DELETE'        AND MAKE IT A DELETE                  
         B     WRTCOM                                                           
TRAIL08  LA    R3,SRMKGMSD                                                      
         USING NUMGEL,R3                                                        
         CLI   0(R3),0            IF MKGD/MISSED                                
         BE    WRTCOM             PRINT THEM                                    
TRAIL10  MVC   PLTRAIL(7),=C'MKGD BY'                                           
         CLI   0(R3),X'07'                                                      
         BE    *+10                                                             
         MVC   PLTRAIL+5(3),=C'FOR'                                             
         MVC   PLTRAIL+9(16),NUMGPNM                                            
         GOTO1 DATCON,DMCB,(2,NUMGDATE),(7,PLTRAIL+25)       MMMDD              
         MVI   PLTRAIL+31,C'-'                                                  
         EDIT  (B1,NUMGSUB),(2,PLTRAIL+32),ALIGN=LEFT                           
         TM    SRSTAT,X'01'    WAS MKGD REMOVED?                                
         BNO   *+8                                                              
         MVI   PLTRAIL-1,C'*'                                                   
                                                                                
* - COMMENTS                                                                    
WRTCOM   DS    0H                                                               
         MVC   PLCOMM,SRCOMN                                                    
                                                                                
* - BUYER                                                                       
         MVC   PLBUYER,SRBUYER                                                  
         CLI   PLBUYER,C'?'         IF UNKNOWN                                  
         BNE   *+10                                                             
         XC    PLREASON,PLREASON    CLEAR REASON CODE FOR NOW                   
                                                                                
* - PRINT                                                                       
         CLI   DOWNIT,0            ARE WE DOWNLOADING?                          
         BE    NODOWNLD            NO                                           
*                                                                               
         BAS   RE,FORMATLN         YES/FORMAT AND DOWNLOAD                      
         BE    WRTPX               THAT'S ALL                                   
         B     WRT31               HANDLE NEXT RECORD                           
                                                                                
NODOWNLD GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   SRTYPE,C'D'         IS IT A DELETE?                              
         BE    WRTNXT              DON'T SKIP LINE                              
         CLI   SRTYPE,C'B'         IS IT A SPECIAL ADD?                         
         BE    WRTNXT              DON'T SKIP LINE                              
         GOTO1 SPOOL,DMCB,(R8)     GIVE A BLANK LINE                            
                                                                                
* - GET NEXT SORT REC                                                           
WRTNXT   B     WRT30                                                            
                                                                                
                                                                                
                                                                                
                                                                                
WRTPX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
********************************************                                    
* PRINT DISTRIBUTION/REASON CODE  PAGE AT NEWCLIENT                             
NEWCLT   NTR1                                                                   
* -      GET DISTRIBUTION INFO                                                  
                                                                                
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
                                                                                
         CLC   SAVECLT,SRTKCLT     SAME CLIENT?                                 
         BE    *+8                                                              
         BAS   RE,GETCLT                                                        
         CLC   SVSRTGRP,SRTKGRP    NEW AUDIT GROUP CODE?                        
         BE    *+8                                                              
         BAS   RE,GETGRP                                                        
                                                                                
         CLI   PRDBRK,C'Y'         PRODUCT IN HEADLINES?                        
         BE    DST07               YES                                          
         CLI   PRDBRK,1            ALWAYS PGBRK                                 
         BE    DST07               YES                                          
*                                  NO                                           
         CLI   SVSRTCLT,0          FIRST TIME ?                                 
         BNE   DST04                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         B     DST33               YES/PRINT REASON CODES                       
                                                                                
DST04    BAS   RE,PRNTIT           NO - SKIP LINES                              
         BAS   RE,PRNTIT                                                        
         BAS   RE,PRNTIT                                                        
DST05    MVC   XP+1(6),=C'CLIENT'       PRINT MID-LINE INFO                     
         MVC   XP+13(3),SRTKCLT          PRINT MID-LINE INFO                    
         MVC   XP+18(20),SAVCLTNM                                               
         MVC   XP2+1(11),=C'AUDIT GROUP'                                        
         MVC   XP2+13(4),SRTKGRP                                                
         MVC   XP2+18(20),SAVGRPNM                                              
         MVC   XP3+1(7),=C'NETWORK'                                             
         MVC   XP3+13(4),SRTKNET                                                
*                                                                               
         BAS   RE,PRNTIT                                                        
         BAS   RE,PRNTIT                SKIP A LINE                             
         MVC   SVSRTCLT,SRTKCLT         SAVE NEW CLIENT                         
         MVC   SVSRTGRP,SRTKGRP                                                 
         MVC   SVSRTNET,SRTKNET                                                 
         MVC   SVSRTPRD,SRTKPROD                                                
         B     EXIT                     AND GET OUT                             
                                                                                
* DISTRIBUTION LIST ONLY IF PRDBRK=Y                                            
*********************************************************************           
DST07    CLC   SVSRTCLT(6),SRTKCLT    HAS CLIENT/PROD  CHANGED?                 
         BE    DST70               NO - JUST PAGEBREAK                          
         MVI   FORCEHED,C'Y'       YES GET DISTRIBUTION LIST                    
         MVI   RCSUBPRG,4                                                       
                                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D69'       DISTRIBUTION REC                           
         MVC   KEY+2(3),SRTAMC       AGY/MED/CLI                                
*                                                                               
         CLI   PRDBRK,C'Y'         IF PRDBRK                                    
         BNE   DST09                                                            
         MVC   KEY+5(3),SRTKPROD    USE PRODCODE                                
         OC    KEY+5(4),SPACES                                                  
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    DST09C              OK                                           
         MVC   KEY(13),KEYSAVE     NOT OK                                       
         B     DST09                                                            
*                                                                               
DST09    DS    0H                                                               
         MVC   KEY+5(4),DISTCOD    DISTRIBUTION CODE                            
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(5),KEYSAVE                                                   
         BNE   DST30                                                            
         CLI   DISTCOD,0                                                        
         BE    *+14                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DST30                                                            
*                                                                               
DST09C   L     R3,=A(MYIO3)       READ RECS INTO MYIO3                          
         USING DSTRECD,R3                                                       
         LA    R4,MYDMWORK                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R3),(R4),0           
         LA    R3,24(R3)                                                        
                                                                                
* - LOAD DISTRIBUTION LIST TO MYDIST                                            
         L     RE,=A(MYDIST)                                                    
         LA    RF,L'MYDIST                                                      
         XCEF                                                                   
         LA    R2,3                ONLY 3 LEVELS OF DISTRIBUTION                
DDT05    CLI   0(R3),5                                                          
         BE    DDT07                                                            
DDT06    ZIC   R1,1(R3)                                                         
         LTR   R1,R1                                                            
         BZ    DDT08                                                            
         AR    R3,R1                                                            
         BCT   R2,DDT05                                                         
         B     DDT08                                                            
DDT07    DS    0H                                                               
         L     R1,=A(MYDIST)                                                    
DDT07B   CLI   0(R1),0                                                          
         BE    DDT07C                                                           
         LA    R1,200(R1)                                                       
         B     DDT07B                                                           
DDT07C   MVC   0(200,R1),3(R3)                                                  
         B     DDT06                                                            
                                                                                
* PRINT DISTRIBUTION NAMES IN MYDIST                                            
DDT08    DS    0H                                                               
         L     R3,=A(MYDIST)                                                    
         MVC   XHEAD6+10(17),=C'DISTRIBUTION LIST'                              
         MVC   XHEAD7+10(17),=C'-----------------'                              
         MVC   XP+10(20),0(R3)     HEADLINES                                    
         LA    R3,200(R3)                                                       
         MVC   XP+61(20),0(R3)                                                  
         LA    R3,200(R3)                                                       
         MVC   XP+112(20),0(R3)                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         LA    R2,12               MAX NAMES                                    
         L     R3,=A(MYDIST)       MYDIST                                       
         LA    R3,20(R3)           R3->DIST NAMES                               
         LR    R4,R3               SAVE START OF NAMES IN R4                    
DDT10    XC    XP,XP                                                            
         MVC   XP+10(15),0(R3)                                                  
         LA    R3,200(R3)                                                       
         MVC   XP+61(15),0(R3)                                                  
         LA    R3,200(R3)                                                       
         MVC   XP+112(15),0(R3)                                                 
         OC    XP,XP               IF P LINE EMPTY                              
         BZ    DST30               NO MORE TO PRINT                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,15(R4)          BUMP R4 TO NEXT NAME                          
         LR    R3,R4              SET R3 TO POINT TO NEW LIST OF NAMES          
         BCT   R2,DDT10                                                         
         B     DST30                                                            
                                                                                
DST30    DS    0H                                                               
         MVI   SPACING,5                                                        
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
         MVI   SPACING,1                                                        
                                                                                
                                                                                
********************************************************************            
DST33    DS    0H                                                               
         MVC   XP+10(13),=C'REASON CODES:'                                      
         LA    R3,XP+24                                                         
         L     R2,=A(MYIO)         PRINT REASON CODES                           
*                                  3 CODES/TEXT PER PRINT LINE                  
DST35    OC    0(6,R2),0(R2)                                                    
         BZ    DST50                                                            
         MVC   0(6,R3),0(R2)       CODE                                         
         MVI   6(R3),C'='                                                       
         MVC   8(40,R3),6(R2)      TEXT                                         
         LA    R3,51(R3)                                                        
         LA    R2,46(R2)                                                        
*                                                                               
         OC    0(6,R2),0(R2)                                                    
         BZ    DST50                                                            
         MVC   0(6,R3),0(R2)       CODE                                         
         MVI   6(R3),C'='                                                       
         MVC   8(40,R3),6(R2)      TEXT                                         
         LA    R3,51(R3)                                                        
         LA    R2,46(R2)                                                        
*                                                                               
         OC    0(6,R2),0(R2)                                                    
         BZ    DST50                                                            
         MVC   0(6,R3),0(R2)       CODE                                         
         MVI   6(R3),C'='                                                       
         MVC   8(40,R3),6(R2)      TEXT                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,XP+24             RESET P LINE                                
         LA    R2,46(R2)           BUMP REASON CODES                            
         B     DST35                                                            
*                                                                               
DST50    BAS   RE,PRNTIT                                                        
                                                                                
DST70    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1          SET USUAL HEAD PRINTING                      
         CLI   PRDBRK,C'Y'         IF PRODUCT IN HEADLINES                      
         BE    DST72                                                            
         CLI   PRDBRK,1            IF PG BRK ALWAYS                             
         BE    DST72                                                            
         B     DST05               NO                                           
DST72    MVC   SVSRTCLT,SRTKCLT    YES  SAVE NEW CLIENT                         
         MVC   SVSRTGRP,SRTKGRP                                                 
         MVC   SVSRTNET,SRTKNET                                                 
         CLI   PRDBRK,1                                                         
         BE    EXIT                                                             
         MVC   SVSRTPRD,SRTKPROD                                                
         MVI   RCSUBPRG,3                                                       
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
                                                                                
*                                                                               
GETCLT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SRTAMC                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MYDMWORK                                                      
         L     R4,=A(MYIO2)                                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R4),(R3),0           
         USING CLTHDR,R4                                                        
         GOTO1 CLUNPK,DMCB,(CPROF+6,SRTKCLT+1),SAVECLT                          
         MVC   SAVCLTNM,CNAME                                                   
*                                                                               
         CLC   SRTKPROD,SVSRTPRD    SAME PRODUCT?                               
         BE    EXIT                                                             
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SRTAMC                                                  
         MVC   KEY+4(3),SRTKPROD                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MYDMWORK                                                      
         L     R4,=A(MYIO2)                                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R4),(R3),0           
         USING PRDHDR,R4                                                        
         MVC   SAVPRDNM,PNAME                                                   
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
                                                                                
                                                                                
GETGRP   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING NPRECD,R1                                                        
         MVI   KEY,X'02'                                                        
         MVC   NPKAM(3),SRTAMC                                                  
         MVC   NPKNET,SRTKNET                                                   
         MVC   NPKEST,SRTKEST                                                   
         MVC   NPKPACK,SRTKPKG                                                  
                                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR',KEY,KEY,0                     
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MYDMWORK                                                      
         L     R6,=A(MYIO2)                                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFILE',KEY+21,(R6),(R3),0           
         XC    SAVGRPNM,SAVGRPNM                                                
         MVC   SAVGRPNM(4),=C'????'                                             
         MVI   ELCODE,9                                                         
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         USING NAUDD,R6                                                         
         MVC   SAVGRPNM,NAUDCOM                                                 
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
*                                                                               
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
*                                                                               
PUTSORT1 NTR1                                                                   
         BAS   RE,PUTIT                                                         
         B     EXIT                                                             
*                                                                               
PUTSORT2 NTR1                                                                   
         CLI   SORTC,0                      ARE WE PASSING 'COPY' ?             
         BE    PUTS10                           NO                              
         BAS   RE,CHKTMSUB                                                      
         DS    0H                               YES                             
* MOVE CHANGE REC'S CHANGE DATE TO COPY'S CHANGE DATE                           
* MOVE CHANGE REC'S TIME TO COPY'S TIME                                         
* SET CHANGE REC'S TIME TO COPY TIME +1 IN SUBTIME                              
* THIS FUDGE NECESSARY TO KEEP COPY AND CHANGE TOGETHER IN SORT                 
* ALSO FUDGE SRTKPROD                                                           
         LA    R3,SORTC                         YES                             
         LA    R3,SRTKCHD-SRTKCLT(R3)                                           
         MVC   0(2,R3),SRTKCHD     MOVE IN CHANGE DATE                          
*                                  SO COPY/CHANGE FOLLOW EACH OTHER             
*                                  IN SORT                                      
         LA    R3,SORTC                                                         
         LA    R3,SRTKTIM-SRTKCLT(R3)                                           
         MVC   0(5,R3),SRTKTIM     MOVE IN CHANGE TIME+SUBTIME                  
         CLI   TIMSUB,250                                                       
         BNH   *+8                                                              
         MVI   TIMSUB,0                                                         
         ZIC   R2,TIMSUB                                                        
         LA    R2,1(R2)                                                         
         STC   R2,4(R3)                                                         
*                                                                               
         LA    R3,SORTC                       FUDGE PROD                        
         LA    R3,SRTKPROD-SRTKCLT(R3)                                          
         MVC   0(3,R3),SRTKPROD    MOVE IN CHANGE PROD                          
*                                                                               
* - IF COPY IS NOT MKGD/MSD AND CHANGE IS                                       
* - DO NOT SHOW COPY                                                            
         LA    R3,SORTC                                                         
         LA    R3,SRMKGMSD-SRTKCLT(R3) IS COPY MKGD/MSD                         
         CLI   0(R3),X'40'         IS COPY MKGD/MSD ?                           
         BE    PUTS07              NO                                           
         CLI   0(R3),0             IS COPY MKGD/MSD ?                           
         BNE   PUTS08              YES                                          
PUTS07   CLI   SRMKGMSD,0          NO - IS CHANGE MKGD/MSD ?                    
         BE    PUTS08                   NO                                      
         XC    SORTC,SORTC              YES   CLEAR COPY                        
         B     PUTS10                         ONLY PASS CHANGE                  
*                                                                               
PUTS08   GOTO1 SORTER,DMCB,=C'PUT',SORTC                                        
         LA    R2,1(R2)            BUMP SUBTIME                                 
         STC   R2,SRTKTSUB         AND SET IT IT CHANGE                         
         STC   R2,TIMSUB                                                        
PUTS10   BAS   RE,PUTIT                      PASS CHANGE                        
*        GOTO1 SORTER,DMCB,=C'PUT',SRTTBL    PASS CHANGE                        
         B     EXIT                                                             
                                                                                
PUTIT    NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SRTTBL    PASS CHANGE                        
         B     PUTITX                                                           
**       GOTO1 =V(PRNTBL),DMCB,=C'PUT',SRTTBL,C'DUMP',100,=C'1D'                
PUTITX   B     EXIT                                                             
                                                                                
         EJECT                                                                  
                                                                                
***********************************                                             
*                                                                               
*   EXPECTS R6 -> X'05' ELEMENT IN NEGENHIST                                    
*                                                                               
MOVEDATA NTR1                                                                   
         USING NCHAEL,R6                                                        
* NOW HANDLE SPECIFIC TYPE OF CHANGE                                            
         LA    R2,OUTPUTBL          TABLE OF TYPE/LENGTH/OUTAREA                
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                FOR TESTING MUST BE FOUND                    
MOV21    CLC   NCHGFCOD,0(R2)      CODES MATCH?                                 
         BE    MOV22                                                            
         LA    R2,6(R2)                                                         
         B     MOV21                                                            
*MOV22    ZIC   R1,1(2)            GET LENGTH                                   
MOV22    ZIC   R1,NCHGLEN         ..GET LENGTH                                  
         S     R1,=F'21'          ..MINUS ELEM INFO                             
         BCTR  R1,0               ..TO GET LENGTH OF INPUT                      
         LA    RE,SRTTBL           GET START OF SORT REC                        
         ICM   RF,15,2(R2)         GET DISPLACEMENT                             
         AR    RE,RF               ADD TO SORTREC START                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),NCHGFLD     MOVE IN NEW DATA                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
OUTPUTBL DS    0CL6                                                             
         DC    C'B',AL1(6),AL4(SRPROD-SRTTBL)                                   
         DC    C'L',AL1(1),AL4(SRLEN-SRTTBL)                                    
         DC    C'A',AL1(4),AL4(SRCOST-SRTTBL)                                   
         DC    C'D',AL1(2),AL4(SRUNTDAT-SRTTBL)                                 
         DC    C'T',AL1(4),AL4(SRTIM-SRTTBL)                                    
         DC    C'N',AL1(16),AL4(SRPRGNM-SRTTBL)                                 
         DC    C'R',AL1(1),AL4(SRROT-SRTTBL)                                    
         DC    C'G',AL1(35),AL4(SRMKGMSD-SRTTBL)                                
         DC    C'M',AL1(35),AL4(SRMKGMSD-SRTTBL)                                
         DC    C'P',AL1(1),AL4(SRPRMT-SRTTBL)                                   
         DC    C'C',AL1(60),AL4(SRCOMN-SRTTBL)                                  
         DC    X'00'                                                            
*                                                                               
                                                                                
         EJECT                                                                  
***************************************************************                 
* EXPECTS HISTORY REC IN MYIO2                                                  
* CLIENT REC IN ANETWS1                                                         
* PRODUCT IN SRPROD                                                             
*                                                                               
*  NOTEEEEEEEEE ONLY GETS 1ST PROD IN LIST OF 6 POSSIBLE PRODS                  
******************************************************************              
GETPRD   NTR1                                                                   
         L     R1,ANETWS1        CLIENT REC SITS IN ANETWS1                     
         CLC   SRTAMC,1(R1)       SAME CLIENT ?                                 
         BE    PRD10               YES                                          
         MVC   MYKEY,KEY           NO / SAVE KEY                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SRTAMC     AGY/MED/CLT                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ANETWS1                                                       
         LA    R4,MYDMWORK                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R3),(R4),0           
                                                                                
         MVC   KEY,MYKEY       RESET HISTORY READ                               
         BAS   RE,MYHIGH                                                        
                                                                                
PRD10    L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220              MAX NUMBER OF PRODUCTS                       
PRD12    CLC   3(1,R1),SRPROD                                                   
         BE    PRD20                                                            
         LA    R1,4(R1)                                                         
         BCT   R2,PRD12                                                         
         MVC   WORK(3),=C'UNA'                                                  
         B     PRD30                                                            
*                                                                               
PRD20    MVC   WORK(3),0(R1)                                                    
*                                                                               
PRD30    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
************************************************************                    
* READ AND STORE REASON CODES                                                   
*                                                                               
RCODES   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D77'       REASON CODES                               
         MVC   KEY+2(2),NBSELAGY                                                
         MVI   KEY+4,C'N'                                                       
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(5),KEYSAVE                                                   
         BNE   RSN30                                                            
*                                                                               
         LA    R6,100             MAX NUMBER OF CODES                           
         L     R2,=A(MYIO)        SAVE REASON CODES IN MYIO                     
         L     R3,=A(MYIO3)       READ RECS INTO MYIO3                          
         USING RSNRECD,R3                                                       
         LA    R4,MYDMWORK                                                      
RSN10    GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R3),(R4),0           
         MVC   0(6,R2),RSNKCODE                                                 
         MVC   6(40,R2),RSNTEXT                                                 
         LA    R2,46(R2)                                                        
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(5),KEYSAVE                                                   
         BNE   RSN30                                                            
         BCT   R6,RSN10                                                         
         DC    H'0'              TABLE BLOWN - TOO MANY REASON CODES            
                                                                                
RSN30    DS    0H                                                               
         B     EXIT                                                             
******************************************************************              
         EJECT                                                                  
MYHIGH   NTR1                                                                   
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         B     DIRALL                                                           
                                                                                
MYSEQ    NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
                                                                                
MYDWRT   NTR1                                                                   
         MVC   COMMAND,=CL8'DMWRT'                                              
         B     DIRALL                                                           
                                                                                
DIRALL   DS    0H                                                               
         CLI   FILE,0              WAS REP DIR OVERRIDEN ?                      
         BE    *+6                 NO                                           
         DC    H'0'                SHOULD NOT BE HERE                           
         MVC   FILE,=CL8'UNTDIR'   DEFAULT                                      
         ZIC   R4,UPDTBIT          SET OPTIONAL BIT                             
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,KEY,KEY,0                       
         MVI   FILE,0              CLEAR FILE FOR REP DEFAULT                   
         B     DDRECX                                                           
*                                                                               
MYGET    NTR1                                                                   
         LA    RF,=C'GETREC'                                                    
         ZIC   R4,UPDTBIT          SET OPTIONAL READ FOR UPDATE                 
         B     DDREC5                                                           
MYADD    NTR1                                                                   
         LA    RF,=C'ADDREC'                                                    
         B     DDREC5                                                           
MYPUT    NTR1                                                                   
         LA    RF,=C'PUTREC'                                                    
         B     DDREC5                                                           
*                                                                               
DDREC5   ST    RF,DMCB                                                          
         CLI   FILE,0              OVERIDE DEFAULT UNT FILE?                    
         BNE   DDREC7                                                           
         MVC   FILE,=CL8'UNTFILE'  NO                                           
         L     R3,=A(MYIO2)        HIST RECS READ INTO MYIO2                    
         LA    R2,KEY+21                                                        
         B     DDREC10                                                          
                                                                                
DDREC7   MVC   FILE,=CL8'SPTFILE'                                               
         LA    R2,KEY+14                                                        
         L     R3,=A(MYIO2)         SPOT RECS READ INTO MYIO2                   
         B     DDREC10                                                          
                                                                                
DDREC10  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,,FILE,(R2),(R3),MYDMWORK,0                          
         MVI   UPDTBIT,0           DEFAULT NOT READ FOR UPDATE                  
         MVI   FILE,0              DEFAULT UNT FILE                             
*                                                                               
DDRECX   CLI   8(R1),0             SET CC ON EXIT                               
         B     EXIT                                                             
*                                                                               
         DS    0F                                                               
FILE     DS    CL8                                                              
MYDMWORK DS    CL96                                                             
UPDTBIT  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
HEDSPECS SPROG 3,4                                                              
         WSPEC H1,1,C'CLIENT'                                                   
         WSPEC H2,1,C'PRODUCT'                                                  
         SPROG 3                                                                
         WSPEC H3,1,C'AUDIT GROUP'                                              
         WSPEC H4,1,C'NETWORK'                                                  
         SPROG 1,2,3,4                                                          
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,150,PAGE                                                      
         DC    X'00'                                                            
                                                                                
                                                                                
HDRTN    NTR1                                                                   
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         CLI   PRDBRK,C'Y'                                                      
         BNE   HDR10                                                            
         MVC   XHEAD1+12(3),SRTKCLT                                             
         MVC   XHEAD1+17(20),SAVCLTNM                                           
         MVC   XHEAD2+12(3),SRTKPROD                                            
         MVC   XHEAD2+17(20),SAVPRDNM                                           
         CLI   RCSUBPRG,4              IF DOING DISTRIBUTION LIST               
         BE    HDRTNX                  GET OUT HERE                             
         MVC   XHEAD3+12(4),SRTKGRP                                             
         MVC   XHEAD3+17(16),SAVGRPNM                                           
         MVC   XHEAD4+12(4),SRTKNET                                             
HDR10    CLI   RCSUBPRG,2                                                       
         BE    HDRTNX                                                           
         LA    R1,XHEAD6                                                        
         USING PLINED,R1                                                        
         MVC   XHEAD1+40(60),HDTITLE                                            
         MVC   PLACTDAT,=C'ACTIVITY'                                            
         MVC   PLACTDAT+200(4),=C'DATE'                                         
         MVC   PLSTATUS,=C'STATUS'                                              
         MVC   PLUNTDAT+2(4),=C'UNIT'                                           
         MVC   PLUNTDAT+200(4),=C'DATE'                                         
         MVC   PLPROG(7),=C'PROGRAM'                                            
         MVC   PLDAY(3),=C'DAY'                                                 
         MVC   PLTIME(4),=C'TIME'                                               
         MVC   PLLEN(3),=C'LEN'                                                 
         MVC   PLCOST+1(5),=C'$COST'                                            
         MVC   PLBRAND(5),=C'BRAND'                                             
         MVC   PLREASON(3),=C'RSN'                                              
         MVC   PLTRAIL(5),=C'TRAIL'                                             
         MVC   PLCOMM+1(8),=C'COMMENTS'                                         
         MVC   PLBUYER(5),=C'BUYER'                                             
HDRTNX   B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
* FOR ALWAYS PAGE BREAKING                                                      
HEDSPEC2 SPROG 1,2,3,4                                                          
         WSPEC H1,1,C'CLIENT'                                                   
         WSPEC H2,1,C'AUDIT GROUP'                                              
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,150,PAGE                                                      
         SPROG 1,2,3                                                            
         WSPEC H3,1,C'NETWORK'                                                  
         DC    X'00'                                                            
                                                                                
HDRTN2   NTR1                                                                   
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         MVC   XHEAD1+12(3),SRTKCLT                                             
         MVC   XHEAD1+17(20),SAVCLTNM                                           
         MVC   XHEAD2+12(4),SRTKGRP                                             
         MVC   XHEAD2+17(16),SAVGRPNM                                           
         CLI   RCSUBPRG,4                                                       
         BE    HDRTNX2                                                          
         MVC   XHEAD3+12(4),SRTKNET                                             
         CLI   RCSUBPRG,2                                                       
         BE    HDRTNX2                                                          
         LA    R1,XHEAD6                                                        
         USING PLINED,R1                                                        
         MVC   XHEAD1+40(60),HDTITLE                                            
         MVC   PLACTDAT,=C'ACTIVITY'                                            
         MVC   PLACTDAT+200(4),=C'DATE'                                         
         MVC   PLSTATUS,=C'STATUS'                                              
         MVC   PLUNTDAT+2(4),=C'UNIT'                                           
         MVC   PLUNTDAT+200(4),=C'DATE'                                         
         MVC   PLPROG(7),=C'PROGRAM'                                            
         MVC   PLDAY(3),=C'DAY'                                                 
         MVC   PLTIME(4),=C'TIME'                                               
         MVC   PLLEN(3),=C'LEN'                                                 
         MVC   PLCOST+1(5),=C'$COST'                                            
         MVC   PLBRAND(5),=C'BRAND'                                             
         MVC   PLREASON(3),=C'RSN'                                              
         MVC   PLTRAIL(5),=C'TRAIL'                                             
         MVC   PLCOMM+1(8),=C'COMMENTS'                                         
         MVC   PLBUYER(5),=C'BUYER'                                             
HDRTNX2  B     EXIT                                                             
         DROP  R1                                                               
                                                                                
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
                                                                                
         EJECT                                                                  
*****************************************************                           
* FILTERING AGAINST HISTORY KEY                                                 
CHKFILTS NTR1                                                                   
         LA    R6,KEY                   R6 -> KEY                               
         USING NHRECD,R6                                                        
*                                                                               
         CLI   NBSELCGR,0         CLIENT GROUP FILTERING ?                      
         BE    FLT05                                                            
         BAS   RE,CGRPFILT                                                      
         BNE   FLTNO                                                            
*                                                                               
FLT05    CLI   ONECLT,0            CLIENT FILTERING ?                           
         BE    FLT10                                                            
         CLC   ONECLT,NHKPCLT                                                   
         BNE   FLTNO                                                            
                                                                                
FLT10    DS    0H                                                               
         CLC   =C'ALL',NBSELNET     NETWORK                                     
         BE    FLT15                                                            
         CLI   NBSELNET,C' '                                                    
         BNH   FLT15                                                            
         CLC   NBSELNET,NHKNET                                                  
         BNE   FLTNO                                                            
                                                                                
FLT15    DS    0H                                                               
         CLI   REQPROG,C' '      PROGRAM                                        
         BNH   FLT20                                                            
         CLC   REQPROG,NHKPROG                                                  
         BNE   FLTNO                                                            
                                                                                
FLT20    DS    0H                                                               
         CLI   NBCMPSTR,0          START/END UNIT DATE                          
         BE    FLT30                                                            
         CLC   NHKDATE,NBCMPSTR                                                 
         BL    FLTNO                                                            
         CLC   NHKDATE,NBCMPEND                                                 
         BH    FLTNO                                                            
***************************                                                     
         B     FLT30              TESTING                                       
         CLI   NHKSUB,2                                                         
         BNE   FLTNO                                                            
******************************                                                  
                                                                                
FLT30    DS    0H                                                               
         CLI   NBSELEST,0                                                       
         BE    FLT40                                                            
         CLC   NHKEST,NBSELEST                                                  
         BNE   FLTNO                                                            
                                                                                
FLT40    DS    0H                                                               
         CLI   NBSELDP,0                                                        
         BE    FLT50                                                            
         CLC   NBSELDP,NHKDP                                                    
         BNE   FLTNO                                                            
                                                                                
FLT50    DS    0H                                                               
         CLC   NBSELPRD,=C'POL'    IF POL , NO PROD FILTERING                   
         BE    FLT60                                                            
         CLC   NBSELPRD,=C'ALL'    IF NOT 'ALL', THEN FILTER                    
         BNE   FLT52                                                            
         CLI   NBSELPGR,0          IF 'ALL', PRD GROUP FILTER?                  
         BE    FLT60               NO                                           
* FOR PRODUCT FILTERING / MATCH AGAINST PRODUCT ON UNIT                         
FLT52    MVC   MYKEY,KEY           SAVE CURRENT HIST REC KEY                    
         MVI   KEY,X'84'           CHANGE KEY TO UNIT PASSIVE POINTER           
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         BAS   RE,MYGET            READS UNIT INTO MYIO2                        
*                                  GENERALLY RESERVED FOR HISTORY REC           
         L     R3,=A(MYIO2)                                                     
         USING NUKEY,R3                                                         
         ZIC   R1,NUPRD            CHECK 1 PRODUCT                              
         LA    R2,NBPRDMSK                                                      
         BAS   RE,TESTMASK                                                      
         BE    FLT5YX              YES                                          
         ZIC   R1,NUPRD2           NOT OK / CHECK 2 PRODUCT                     
         LA    R2,NBPRDMSK                                                      
         BAS   RE,TESTMASK                                                      
         BE    FLT5YX              YES                                          
         B     FLT5NX              NO PROD MATCH                                
         DROP  R3                                                               
FLT5NX   DS    0H                                                               
         MVC   KEY,MYKEY           RESET SAVED HISTORY KEY                      
         GOTO1 HIGH                RESET HIGH CALL                              
         B     FLTNO               AND REJECT RECORD                            
*                                                                               
FLT5YX   DS    0H                                                               
         MVC   KEY,MYKEY           RESET SAVED HISTORY KEY                      
         GOTO1 HIGH                RESET HIGH CALL                              
         B     FLT60                                                            
                                                                                
*                                                                               
FLT60    DS    0H                                                               
*                                                                               
FLTYES   SR    RE,RE                                                            
*                                                                               
FLTNO    LTR   RE,RE                                                            
         B     EXIT                                                             
         DROP  R6                                                               
                                                                                
         EJECT                                                                  
*****************************************************                           
* FILTERING AGAINST HISTORY RECORD                                              
CHKFILT2 NTR1                                                                   
*                                                                               
                                                                                
CK200    DS    0H                                                               
         CLI   REQGRP,0            AUDIT GROUP FILTERING ?                      
         BE    CK210                                                            
         LA    R1,4                                                             
         LA    RE,REQGRP                                                        
         LA    RF,SRTKGRP                                                       
CK202    CLI   0(RE),C'*'          WILD CARD?                                   
         BE    CK203                                                            
         CLC   0(1,RE),0(RF)                                                    
         BNE   CK2NO                                                            
CK203    LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,CK202                                                         
         B     CK210                                                            
                                                                                
*                                                                               
CK210    DS    0H                                                               
         CLI   REQACTST,0                                                       
         BNE   *+12                                                             
         CLI   REQACTND,0                                                       
         BE    CK220                                                            
         CLI   REQACTST,0                                                       
         BE    *+12                                                             
         CLC   SRTKCHD,REQACTST     ACTIVITY START                              
         BL    CK2NO                                                            
         CLI   REQACTND,0                                                       
         BE    CK220                                                            
         CLC   SRTKCHD,REQACTND     ACTIVITY END                                
         BH    CK2NO                                                            
         B     CK220                                                            
*                                                                               
CK220    DS    0H                                                               
                                                                                
CK230    DS    0H                                                               
         B     CK2YES                                                           
                                                                                
*                                                                               
CK2YES   SR    RE,RE                                                            
*                                                                               
CK2NO    LTR   RE,RE                                                            
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
TESTMASK NTR1                                                                   
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BITTEST,0(R2)                                                    
         NC    BITTEST,0(R1)                                                    
         CLI   BITTEST,0                                                        
         BE    NO                                                               
         B     YES                                                              
YES      SR    RE,RE                                                            
NO       LTR   RE,RE                                                            
         XIT1                                                                   
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
BITTEST  DS    CL1                                                              
         EJECT                                                                  
                                                                                
         EJECT                                                                  
***************************************                                         
* THIS ROUTINE HANDLES SITUATIONS WHERE CLIENT HAS REQUESTED THAT               
* CERTAIN KINDS OF CHANGES NOT PRINT                                            
* 1)REASON CODE X CHANGES SHOULD BE IGNORED                                     
* 2)UNIT ADDED AS MKGD SHOULD ONLY HAVE 'ADD' RECORD AND NO 'DELETE'            
CHKSPCL  NTR1                                                                   
         CLI   SRREASON,C'X'            SKIP IF REASON = X                      
         BE    SPLNO                                                            
                                                                                
                                                                                
SPL20    CLI   REQRSN,0            REASON CODE FILTER?                          
         BE    SPL30                                                            
         CLC   REQRSN,SRREASON                                                  
         BNE   SPLNO                                                            
                                                                                
SPL30    CLI   REQLEN,0            LENGTH FILTER ?                              
         BE    SPL35                                                            
         CLC   REQLEN,SRLEN                                                     
         BNE   SPLNO                                                            
                                                                                
SPL35    CLI   REQCMNT,0           COMMENTS ONLY ?                              
         BE    SPL40                                                            
         CLI   SRCOMN,X'40'                                                     
         BNH   SPLNO                                                            
*                                                                               
SPL40    DS    0H                                                               
         CLI   REQRTN,0            ROTATION?                                    
         BE    SPL42                                                            
         CLC   REQRTN,SRROT                                                     
         BNE   SPLNO                                                            
*                                                                               
SPL42    DS    0H                                                               
         OC    REQTIME,REQTIME     START-END TIME                               
         BZ    SPL44                                                            
         CLC   REQTIME,SRTIM                                                    
         BNE   SPLNO                                                            
*                                                                               
SPL44    OC    REQCOST,REQCOST                                                  
         BZ    SPL46                                                            
         CLC   REQCOST,SRCOST                                                   
         BNE   SPLNO                                                            
*                                                                               
SPL46    DS    0H                                                               
*                                                                               
SPL90    DS    0H                                                               
         CLI   SRPRMT,C'Y'         IS IT PREEMPT?                               
         BNE   SPL92               NO                                           
         XC    SORTC,SORTC         YES,CLEAR COPY                               
         B     SPLYES                                                           
*                                                                               
SPL92    DS    0H                                                               
         CLI   SRMKGMSD,7          IS IT MADE GOOD?                             
         BNE   SPL100              NO                                           
         TM    SRSTAT,X'01'        YES/IF REMOVED                               
         BO    SPLYES                  CONTINUE                                 
         XC    SORTC,SORTC             ELSE,CLEAR COPY                          
*        MVI   SRTYPE,C'S'                  SKIPS LINE                          
         MVI   SRTYPE,C'D'                  DOES NOT SKIP LINE                  
         B     SPLYES                                                           
*                                                                               
*                                                                               
                                                                                
* HANDLE PRINTING OF MKGD REQUIREMENTS                                          
SPL100   LA    R3,SORTC                IF CHNGE DATE/TIM = COPY DAT/TIM         
         LA    R3,SRTKCHD-SRTKCLT(R3)                                           
         CLC   SRTKCHD(6),0(R3)                                                 
         BNE   SPLYES                   NO                                      
         LA    R3,SORTC                 YES                                     
         LA    R3,SRMKGMSD-SRTKCLT(R3)                                          
         CLI   0(R3),0             IF COPY MKGD=BLANK                           
         BNE   SPLYES                                                           
         CLI   SRMKGMSD,0          AND CHANGE MKGD NOT = BLANK                  
         BE    SPLYES                                                           
         XC    SORTC,SORTC         ASSUME UNIT ADDED AS MKGD                    
         B     SPLYES              PRINT CHANGE AS ADD/NO COPY                  
*                                                                               
SPLYES   SR    RE,RE                                                            
SPLNO    LTR   RE,RE                                                            
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
* - CLIENT GROUP FILTERING                                                      
*   KEY HAS HISTORY RECORD                                                      
CGRPFILT NTR1                                                                   
         MVC   MYKEY,KEY           SAVE CURRENT BILL REC KEY                    
         MVI   BYTE,0              CLEAR NEW CLIENT FLAG                        
         L     R4,ANETWS1          CLIENT HEADER SITS IN ANETWS1                
         USING CLTHDR,R4                                                        
         CLC   CKEYCLT,KEY+2       ...DO WE NEED NEW CLIENT HEADER              
         BE    VCL2                                                             
         MVI   BYTE,C'Y'                                                        
         NETGO NVSETSPT,DMCB                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MYKEY+1     AGY/MED + CLIENT                            
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   FULL,AIO            SAVE CURRENT AIO                             
         MVC   AIO,ANETWS1         CLIENT REC SITS IN ANETWS1                   
         GOTO1 GETREC                                                           
         MVC   AIO,FULL            RESTORE ORIGINAL AIO                         
         XC    KEY,KEY            RESET SEQ READ FOR HISTORY RECORD             
         MVC   KEY,MYKEY                                                        
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         GOTO1 HIGH                                                             
* - CHECK CLIENT GROUP FILTERING                                                
VCL2     LA    R0,5                                                             
         LA    RF,CGRP1                                                         
         CLC   NBSELCGR(1),0(RF)       CHECK SCHEME LETTER                      
         BE    VCL5                                                             
VCL4     LA    RF,3(RF)                                                         
         BCT   R0,*-14                                                          
         B     VCLNO                                                            
VCL5     UNPK  DUB(5),1(3,RF)      UNPK PWOS                                    
         LA    R3,DUB                                                           
         LA    RE,NBSELCGR+1                                                    
         LA    R1,4                                                             
VCL6     CLI   0(RE),X'C1'         IF LETTER OR NUMBER                          
         BL    VCL7                                                             
         CLC   0(1,RE),0(R3)       MUST MATCH                                   
         BNE   VCL4                IF NO MATCH,TEST AGAINST NXT CGRP            
VCL7     LA    RE,1(RE)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,VCL6                                                          
         MVC   NBACTCGR(2),1(RF)   SET CLIENT GROUP CODE  PWOS                  
*                                                                               
         CLI   BYTE,C'Y'           IS IT NEW CLIENT                             
         BNE   VCLYES                                                           
***      BAS   RE,SETPRD           YES/SET PRODUCT                              
***      BAS   RE,SETEST           YES/SET ESTIMATES                            
***      XC    KEY,KEY            RESET SEQ READ FOR BILL HEADR RECS            
***      MVC   KEY,MYKEY                                                        
***      MVC   FILENAME,=C'SPTDIR  '                                            
***      GOTO1 HIGH                                                             
***      MVC   AIO,AMYIO           AND RESET I/O AREA FOR BILL REC              
*                                                                               
VCLYES   SR    RE,RE               CLIENT PASSED TESTS                          
VCLNO    LTR   RE,RE                                                            
VCLX     B     EXIT                                                             
*                                                                               
         EJECT                                                                  
FORMATLN NTR1                                                                   
         GOTO1 =A(FORMATIT),DMCB,(RC)                                           
         XIT1 REGS=(R3)                                                         
                                                                                
                                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*                                                                               
         DC    CL8'**MYIO**'                                                    
MYIO     DS    0D                                                               
         DS    4048C                                                            
MYIOLNE  EQU   *-MYIO                                                           
                                                                                
         DC    CL8'**MYIO2*'                                                    
MYIO2    DS    0D                                                               
         DS    4048C                                                            
MYIO2LNE EQU   *-MYIO2                                                          
                                                                                
         DC    CL8'**MYIO3*'                                                    
MYIO3    DS    0D                                                               
         DS    4048C                                                            
MYIO3LNE EQU   *-MYIO3                                                          
*                                                                               
         DC    CL8'**MYDST*'                                                    
MYDIST   DS    CL600         ROOM FOR 3 LEVELS OF DISTRIBUTION                  
         EJECT                                                                  
* WHEN DOWNLOADING THIS ROUTINE GETS THE RECS FROM SORTER                       
* FORMATS THE LINE FOR DOWLOAD AND PRINTS IT.                                   
FORMATIT NMOD1 0,**ANXX**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
                                                                                
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         LA    R2,XP                  R2 -> PRINT LINE                          
         USING PLINED,R2                                                        
                                                                                
         LA    R3,MYP                                                           
         LA    R3,1(R3)                                                         
         MVC   MYP,SPACES                                                       
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLACTDAT,R3),PLACTDAT                                        
         LA    R3,L'PLACTDAT(R3)                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLSTATUS,R3),PLSTATUS                                        
         LA    R3,L'PLSTATUS(R3)                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLUNTDAT,R3),PLUNTDAT                                        
         LA    R3,L'PLUNTDAT(R3)                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLPROG,R3),PLPROG                                            
         LA    R3,L'PLPROG(R3)                                                  
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLDAY,R3),PLDAY                                              
         LA    R3,L'PLDAY(R3)                                                   
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLTIME,R3),PLTIME                                            
         LA    R3,L'PLTIME(R3)                                                  
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLLEN,R3),PLLEN                                              
         LA    R3,L'PLLEN(R3)                                                   
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         CLC   PLCOST,=7X'40'      ANY COST?                                    
         BH    MOVECOST                                                         
         MVI   0(R3),C'0'          NEED A ZERO FOR DOWNLOAD                     
         B     *+14                                                             
MOVECOST MVC   0(L'PLCOST,R3),PLCOST       NUMERIC/NO QUOTES                    
         LA    R3,L'PLCOST(R3)                                                  
         LA    R3,2(R3)                                                         
                                                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLBRAND,R3),PLBRAND                                          
         LA    R3,L'PLBRAND(R3)                                                 
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLREASON,R3),PLREASON                                        
         LA    R3,L'PLREASON(R3)                                                
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         LA    R4,L'PLTRAIL                  LENGTH OF PLTRAIL                  
         GOTO1 SQUASHER,DMCB,PLTRAIL,(R4)    LET'S DROP EXCESS BLANKS           
         L     R1,4(R1)                    GET LENGTH OF OUTPUT                 
         LA    RE,98                    98=LEN OF OUTPUT TO NOW                 
         AR    RE,R1                                                            
         ST    RE,FULL                                                          
         C     RE,=F'130'          IS THERE ROOM FOR QUOTE AND SPACE?           
         BL    FORMAT5                      YES                                 
         GOTO1 =V(PRINT),DMCB,MYP,=C'BL01'      NO - PRINT THIS LINE            
         XC    FULL,FULL                       CLEAR LEN COUNTER                
         LA    R3,MYP                          START AGAIN                      
         LA    R3,1(R3)                                                         
         MVC   MYP,SPACES                                                       
FORMAT5  MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                R1=LENGTH OF OUTPUT (-1 FOR MVC)             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PLTRAIL                                                  
         LA    R1,1(R1)            TO RESTORE -1 FOR MVC ABOVE                  
         AR    R3,R1                                                            
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         LA    R4,L'PLCOMM                                                      
         GOTO1 SQUASHER,DMCB,PLCOMM,(R4)     LET'S DROP EXCESS BLANKS           
         L     R1,4(R1)                LENGTH OF OUTPUT                         
         L     RE,FULL             LENGTH UP TO NOW                             
         AR    RE,R1                                                            
         ST    RE,FULL                                                          
         C     RE,=F'130'          ROOM FOR BLANK AND INDICATOR?                
         BL    FORMAT7 YES                                                      
         GOTO1 =V(PRINT),DMCB,MYP,=C'BL01'      NO - WRITE LINE                 
         XC    FULL,FULL                         CLEAR LEN COUNTER              
         LA    R3,MYP                            START AGAIN                    
         LA    R3,1(R3)                                                         
         MVC   MYP,SPACES                                                       
FORMAT7  MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                FOR MVC                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PLCOMM                                                   
         LA    R1,1(R1)            RESOTRE -1 FOR MVC PREV                      
         AR    R3,R1                                                            
         MVI   0(R3),C'"'                                                       
         LA    R3,2(R3)                                                         
                                                                                
         LA    R1,L'PLBUYER                                                     
         L     RE,FULL                                                          
         AR    RE,R1                                                            
         C     RE,=F'130'          IS THERE ENOUGH ROOM                         
         BL    FORMAT10            YES                                          
         GOTO1 =V(PRINT),DMCB,MYP,=C'BL01' NO PRINT THIS                        
         LA    R3,MYP                      AND START AGAIN                      
         LA    R3,1(R3)                                                         
         MVC   MYP,SPACES                                                       
FORMAT10 MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(L'PLBUYER,R3),PLBUYER                                          
         LA    R3,L'PLBUYER(R3)                                                 
         MVI   0(R3),C'"'                                                       
         LA    R3,1(R3)                                                         
                                                                                
         GOTO1 SORTER,DMCB,=C'GET' ANY MORE RECS                                
         L     R2,4(R1)                                                         
         LTR   R2,R2                                                            
         BZ    *+12                                                             
         MVI   0(R3),X'5E'         SEMICOLON = END OF THIS LINE                 
         B     *+8                                                              
         MVI   0(R3),C':'          END OF REPORT                                
         CLI   DOWNIT,2                                                         
         BE    FORMAT20                                                         
         MVI   DOWNIT,2                                                         
         GOTO1 =V(PRINT),DMCB,MYP,=C'BC01'   SKIP TO NEWPAGE                    
         B     FORMAT22                                                         
*                                                                               
FORMAT20 GOTO1 =V(PRINT),DMCB,MYP,=C'BL01'                                      
FORMAT22 DS    0H                                                               
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         XC    XP,XP               CLEAR PRINT LINE                             
         LR    R3,R2               PASS REC ADDR IN R3                          
         LTR   R3,R3               SET CONDITION CODE                           
         B     FORMATX                                                          
FORMATX  XIT1  REGS=(R3)                                                        
         DROP  R5                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
                                                                                
MYWORKD  DSECT                                                                  
AMSAVE   DS    CL1    *** FROM EDIT MODULE - DO NOT MOVE                        
ONECLT   DS    CL2    ***                                                       
DISTCOD  DS    CL4    ***                                                       
HDTITLE  DS    CL60   ***          TITLE OVERRIDE                               
REQPROG  DS    CL6    ***                                                       
REQRSN   DS    CL3    ***                                                       
REQCOST  DS    CL4    ***                                                       
REQTIME  DS    CL4    ***                                                       
REQLEN   DS    CL1    ***                                                       
REQCMNT  DS    CL1    ***                                                       
REQACTST DS    CL2                 ACTIVITY START TIME                          
REQACTND DS    CL2                 ACTIVITY END TIME                            
REQRTN   DS    CL1                 ROTATION                                     
PRDBRK   DS    CL1                 Y=BREAK BY PRODUCT                           
REQGRP   DS    CL4                 AUDIT GROUP                                  
REQCODE  DS    CL1                 SHOW PROG CODE/EST/PKG                       
DOWNIT   DS    CL1    ***                                                       
*                                                                               
AUNITREC DS    A                   UNIT RECORD POINTER                          
ADRWIDE  DS    A                   ADDRESS OF DDWIDE                            
*                                                                               
PREVREC  DS    CL1                                                              
SAVECLT  DS    CL3                                                              
SAVEPROD DS    CL7                                                              
SAVEEST  DS    CL1                                                              
SAVENET  DS    CL4                                                              
*                                                                               
TIMESV   DS    CL4                                                              
TIMSUB   DS    CL1                                                              
*                                                                               
*                                                                               
*                                                                               
AGYNAMSV DS    CL33                                                             
AGYADRSV DS    CL33                                                             
*                                                                               
TODAYB   DS    CL3                 BINARY                                       
TODAYC   DS    CL2                 COMPRESSED                                   
*                                                                               
SVSRTCLT DS    CL3                                                              
SVSRTPRD DS    CL3                                                              
SVSRTGRP DS    CL4                                                              
SVSRTNET DS    CL4                                                              
*                                                                               
CHANGDAT DS    CL8                                                              
SAVCLTNM DS    CL20                                                             
SAVGRPNM DS    CL16                                                             
SAVPRDNM DS    CL20                                                             
*                                                                               
SAVAMC   DS    CL3                 SAVE AGY/MED/CLT OF HIST REC                 
MYKEY    DS    CL40                                                             
                                                                                
*                                                                               
WHOTBL   DS    CL(12*50)           ROOM FOR 50 ID(2)/AGY(2)/NAME(8)             
WHOTBLX  DS    CL1                 END OF TABLE = X'FF'                         
*                                                                               
*********************************************************                       
*                                                                               
SRTTBL   DS    0CL200    *** NOTE HARD CODED                                    
SRTKCLT  DS    CL3               * CLIENT                                       
SRTKPROD DS    CL3               * PRODUCT                                      
SRTKGRP  DS    CL4               * AUDIT GROUP                                  
SRTKNET  DS    CL4               * NETWORK                                      
SRTKCHD  DS    CL2               * CHANGEDATE                                   
SRTKTIM  DS    CL4               * CHANGETIME                                   
SRTKTSUB DS    CL1                 TIME SUB-LINE                                
*                                                                               
SRTKKEY  EQU   *-SRTTBL                                                         
*                                                                               
SRTKPRG  DS    CL6               * PROGRAM                                      
SRTKDAT  DS    CL2               * AIR DATE         HIST KEY                    
SRTKEST  DS    CL1               * ESTIMATE         HIST KEY                    
SRTKSUB  DS    CL1               * SUB LINE         HIST KEY                    
SRTKPKG  DS    CL1               * PACKAGE                                      
*                                                                               
SRDATA   DS    0CL2                DATA FIELDS START HERE                       
SRUNTDAT DS    CL2                 DATE                                         
SRPRGNM  DS    CL16                PROGRAM NAME                                 
SRROT    DS    CL1                 ROTATION                                     
SRTIM    DS    CL4                 TIME                                         
SRLEN    DS    CL1                 LENGTH                                       
SRCOST   DS    CL4                 ACTUAL                                       
SRPROD   DS    CL6                 PRODUCT                                      
SRREASON DS    CL3                 REASON                                       
SRSTAT   DS    CL1                 STATUS                                       
SRCOMN   DS    CL60                COMMENT                                      
SRBUYER  DS    CL8                 USER CODE                                    
SRMKGMSD DS    CL35                MAKEGOOD/MISSED                              
SRPRMT   DS    CL1                 PREEMPT C'Y'                                 
SRTAMC   DS    CL3                 AGY/MEDIA/CLI                                
SRTPRDNM DS    CL20                PRODNAME                                     
         DS    CL2               * SPARE                                        
SRTYPE   DS    CL1                 A=ADD/D=DELETE                               
SRTBLEN  EQU   *-SRTTBL            TOTAL SORT REC LENGTH                        
*                                                                               
*                                                                               
*                                                                               
SORTC    DS    CL(L'SRTTBL)         SAVE 'COPY' SORTREC HERE                    
*                                                                               
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYWRKDLE EQU   *-MYWORKD                                                        
*                                                                               
                                                                                
*                                                                               
PLINED   DSECT                                                                  
PLACTDAT DS    CL8                                                              
         DS    CL2                                                              
PLSTATUS DS    CL6                                                              
         DS    CL2                                                              
PLUNTDAT DS    CL8                                                              
         DS    CL2                                                              
PLPROG   DS    CL16                                                             
         DS    CL2                                                              
PLDAY    DS    CL3                                                              
         DS    CL2                                                              
PLTIME   DS    CL10                                                             
         DS    CL2                                                              
PLLEN    DS    CL3                                                              
         DS    CL2                                                              
PLCOST   DS    CL7                                                              
         DS    CL2                                                              
PLBRAND  DS    CL4                                                              
         DS    CL2                                                              
PLREASON DS    CL3                                                              
         DS    CL3                                                              
PLTRAIL  DS    CL33                                                             
         DS    CL2                                                              
PLCOMM   DS    CL30                                                             
         DS    CL1                                                              
PLBUYER  DS    CL8                                                              
PLLENE   EQU   *-PLINED                                                         
*                                                                               
RECD     DSECT                                                                  
RECLN    DS    XL2                                                              
         DS    CL2                                                              
REC      DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
       ++INCLUDE NEGENHIST                                                      
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENDIST                                                      
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICFD                                                       
*                                                                               
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027NEWRI90N  05/01/02'                                      
         END                                                                    
