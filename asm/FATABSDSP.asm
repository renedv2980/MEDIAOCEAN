*          DATA SET FATABSDSP  AT LEVEL 016 AS OF 01/07/20                      
*PHASE TABSDSPA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE SCANNER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE DDWTO                                                                  
         TITLE 'FATABSDSP - HANDLE TABLES DATA SPACE '                          
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
TABSDSP  CSECT                                                                  
         NBASE WORKL,*TABSDSP,=A(WORKAREA),RA,R9                                
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         ST    RC,SAVERC                                                        
*                                                                               
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
TDSP01   BRAS  RE,INIT             READ CARDS ETC                               
         BNE   XBASE                                                            
*                                                                               
         LA    R2,UPDMAJ                                                        
         LA    RE,UPDMIN                                                        
         MVC   UPDMIN,DSPACE                                                    
         ENQ   ((R2),(RE),E,8,SYSTEM),RET=TEST                                  
         LTR   RF,RF                                                            
         BZ    TB001                                                            
         TM    3(RF),X'08'         OWNED BY ME?                                 
         BO    TB001                                                            
         TM    3(RF),X'04'         OWNED BY SOMEONE ELSE                        
         BZ    TB001                                                            
         CLI   MODE,C'R'           Report on dataspace?                         
         BE    TDSP04              Yes                                          
         CLI   TEST,YES                                                         
         BE    TB001                                                            
         WTO   'DATASPACE ALREADY STARTED '                                     
         B     XBASE                                                            
*                                                                               
TB001    CLI   MODE,C'I'           INITIALISE                                   
         BNE   TDSP04                                                           
*                                                                               
         CLI   TEST,YES                                                         
         BE    TB001A                                                           
         ENQ   ((R2),(RE),E,8,SYSTEM),RET=HAVE                                  
*                                                                               
TB001A   BRAS  RE,MAIN             MAIN LOOP FOR MODE=INIT                      
         BRAS  RE,MESSAGE          OUTPUT INITIALISED MESSAGE                   
*                                                                               
         CLI   TEST,YES            TEST RUN?                                    
         BNE   TDSP02                                                           
         BRAS  RE,RESULTS                                                       
         CLI   KILL,YES                                                         
         BNE   TDSP04                                                           
         DC    H'0',C'KILL PROCESS'                                             
*                                                                               
TDSP02   BRAS  RE,SETWAIT                                                       
         BL    XBASE                                                            
         B     TDSP01                                                           
*                                                                               
TDSP04   CLI   MODE,C'R'           REPORT ON EXISTING DSPACE                    
         BNE   TDSP06                                                           
*                                                                               
         BRAS  RE,GETSPC                                                        
         MVC   AHEADER,DMOFFS                                                   
         BRAS  RE,RESULTS                                                       
         B     XBASE                                                            
*                                                                               
TDSP06   B     XBASE                                                            
*                                                                               
ALITS    DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  ,                                                                
         MVC   TITLE(L'CTITLE),CTITLE                                           
*                                                                               
         XR    R1,R1               TELL IT YOU HAVE PRINT LINE COVERED          
         MVC   P(L'I1),I1                                                       
         BRAS  RE,DOMSG                                                         
         MVC   P(L'I2),I2                                                       
         BRAS  RE,DOMSG                                                         
         MVC   P(L'I3),I3                                                       
         BRAS  RE,DOMSG                                                         
         MVC   P(L'I4),I4                                                       
         BRAS  RE,DOMSG                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         LHI   R1,1                BEGAN READING INPUT CARDS                    
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     R3,ASVCARD                                                       
INIT02   GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
         CLC   =C'/*',0(R3)        END OF CARDS?                                
         BE    INIT04              YES                                          
         AHI   R3,80                                                            
         B     INIT02                                                           
*                                                                               
INIT04   LHI   R1,2                ENDED READING INPUT CARDS                    
         BRAS  RE,DOMSG                                                         
         LHI   R1,3                BEGAN PROCESSING INPUT CARDS                 
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     R3,ASVCARD          NOW PROCESS CARDS INDIVIDUALLY               
INIT06   CLC   =C'/*',0(R3)                                                     
         BE    INIT08                                                           
         MVC   P(80),0(R3)                                                      
         GOTO1 VPRINTER            PRINT PARAMETER CARD                         
         MVC   CARD,0(R3)                                                       
         BRAS  RE,CARDVAL          VALIDATE KEYWORD=VALUE                       
         AHI   R3,80                                                            
         B     INIT06                                                           
*                                                                               
INIT08   LHI   R1,4                ENDED PROCESSING INPUT CARDS                 
         BRAS  RE,DOMSG                                                         
*                                                                               
ORG      USING HDRTABD,R3                                                       
CPY      USING HDRTABD,RE                                                       
         LA    R3,JOBTAB           Copy Jobs entries to sv jobs                 
         LA    RE,SVJOBTAB         Set by JOBTAB= card                          
         MVC   CPY.HDRNUM,ORG.HDRNUM                                            
*                                                                               
         LA    R3,SOONCLS          Copy Class entries to sv class               
         LA    RE,SVSOONCL         Set by CLASS= card                           
         MVC   CPY.HDRNUM,ORG.HDRNUM                                            
         DROP  ORG,CPY             Original, Copy                               
*                                                                               
         USING SSB,RE                                                           
         L     RE,=V(SSB)                                                       
         MVC   SSODSPAC,DSPACE+3   Set up DSPACE                                
*&&US                                                                           
         CLI   SSODSPAC,C'T'                                                    
         BNE   INIT09                                                           
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),=CL8'DDSION'                                             
*&&                                                                             
         DROP  RE                                                               
*                                                                               
INIT09   OC    ERRCNT,ERRCNT       ERRORS?                                      
         BZ    INIT10              NO                                           
         LHI   R1,7                                                             
         BRAS  RE,DOMSG                                                         
         B     EXITL                                                            
*                                                                               
INIT10   LHI   R1,5                BEGAN SAVING INPUT CARDS TO OLDPARMS         
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     R3,ASVCARD                                                       
         OPEN  (OLDPARMS,OUTPUT)                                                
         LTR   RF,RF                                                            
         BZ    INIT12                                                           
         LHI   R1,8                OLDPARMS WON'T OPEN                          
         BRAS  RE,DOMSG                                                         
         B     EXITL                                                            
*                                                                               
INIT12   PUT   OLDPARMS,0(R3)                                                   
         CLC   =C'/*',0(R3)                                                     
         BE    INIT14                                                           
         AHI   R3,80                                                            
         B     INIT12                                                           
*                                                                               
INIT14   CLOSE OLDPARMS                                                         
         LHI   R1,6                ENDED SAVING INPUT CARDS TO OLDPARMS         
         BRAS  RE,DOMSG                                                         
*                                                                               
         CLC   =CL4'TEST',RUN      TEST RUN GOES WITHIN CORE                    
         BNE   *+8                                                              
         MVI   TEST,YES                                                         
*                                                                               
         CLI   MODE,C'I'           TEST INIT MODE                               
         BNE   EXITOK                                                           
*                                                                               
         BRAS  RE,SETOPS           SET UP OPER COMMS                            
*                                                                               
         LA    R0,4                MAKE JOB NON SWAPPABLE                       
         LNR   R0,R0                                                            
         SVC   247                                                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET SYSTEM INITIALISED MESSAGE                                      *         
***********************************************************************         
MESSAGE  NTR1  ,                                                                
         MVC   MSGFAC,=CL4'????'                                                
         CLC   TABSREP,SYSTEM      REP DATASPACE?                               
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'REP '                                                
         CLC   TABSCSC,SYSTEM      CSC DATASPACE?                               
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'CSC '                                                
         CLC   TABSADV,SYSTEM      ADV DATASPACE?                               
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'ADV '                                                
         CLC   TABSTST,SYSTEM      TEST DATASPACE?                              
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'TEST'                                                
*&&UK                                                                           
         CLC   TABSBAR,SYSTEM      BAR DATASPACE?                               
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'BAR '                                                
*&&                                                                             
*&&US                                                                           
         CLC   TABSBOTH,SYSTEM     BOTH ADV+REP SYSTEMS?                        
         BNE   *+10                                                             
         MVC   MSGFAC,=CL4'PROD'                                                
*&&                                                                             
         LA    R3,MSGL                                                          
         WTO   TEXT=(R3)                                                        
         B     EXITOK                                                           
*                                                                               
MSGL     DC    AL2(60)                                                          
MSG1     DC    CL60' '                                                          
         ORG   MSG1                                                             
         DC    C'TABS dataspace initialised for '                               
MSGFAC   DC    CL04' '                                                          
         ORG   MSG1+L'MSG1                                                      
         EJECT                                                                  
***********************************************************************         
* MAIN CODE FOR MODE=INIT                                             *         
***********************************************************************         
MAIN     NTR1                                                                   
         BAS   RE,FRKPAGES         SET COUNT OF 4K PAGES REQUIRED               
*                                                                               
         CLI   TEST,YES            TEST RUN GOES WITHIN CORE                    
         BNE   MAIN02                                                           
         BAS   RE,GETMAIN                                                       
         B     MAIN04                                                           
*                                                                               
MAIN02   BAS   RE,FREESPC          FREE ANY OLD DATASPACES                      
         BAS   RE,MAKESPC          MAKE A NEW ONE                               
         BAS   RE,GETSPC           GET ADDRESS OF IT                            
*                                                                               
         MVC   AHEADER,DMOFFS                                                   
         MVC   ADSDATA,DMOFFS      STARTING FROM OFFS                           
*                                                                               
MAIN04   BAS   RE,ADCONS           BUILD ADCONS                                 
         BAS   RE,ADCINI           INITIALISE ADCON BLOCKS                      
*                                                                               
         BAS   RE,HEADERS          BUILD HEADER ENTRIES                         
         BAS   RE,TABLES           BUILD TABLE ENTRIES                          
*                                                                               
MAINX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET PAGES REQUIRED FOR EACH TABLE. TOTAL RETURNED IN KB             *         
***********************************************************************         
* Area accounted for                                                            
*  1) Headers                                                                   
*  2) On-line dumps                                                             
*  3) Stats for Graphical api for system health                                 
*  4) LOCKTAB                                                                   
*  5) Runit (RUNNER) Server + queues                                            
*  6) Common Utility                                                            
*  7) Tempest                                                                   
*  8) Script ?                                                                  
*  9) HDRTABS                                                                   
* 10) Dummy Entry                                                               
***********************************************************************         
FRKPAGES NTR1  ,                                                                
         LHI   RF,((RMAXRES*64)+1023)                                           
         SRL   RF,10               TABLE HEADERS                                
*                                                                               
         LHI   RE,((TDLENQ*(FACIDMAX+1))+1023)                                  
         SRL   RE,10               DUMP HEADERS                                 
         AR    RF,RE                                                            
*                                                                               
         A     RF,DCSTATSK         KB FOR STATS AREA                            
*                                                                               
         L     RE,NLOCKS           LOCKTAB                                      
         MHI   RE,TLKLEN                                                        
         AHI   RE,1023                                                          
         SRL   RE,10                                                            
         AR    RF,RE                                                            
*                                                                               
         L     RE,#SERVERS                                                      
         MHI   RE,TABSSVRL         Length of server entry                       
         L     R1,#QENTRES                                                      
         MHI   R1,TABSQUEL         Length of queue entry                        
         AR    RE,R1                                                            
         AHI   RE,TABSRUNL         Length of header                             
         AHI   RE,1023             Need at least 1K                             
         SRL   RE,10               Divide by 1024                               
         ST    RE,RUNRPGES                                                      
         AR    RF,RE                                                            
*                                                                               
         LHI   RE,TCOLEN           Common Utility Area                          
         AR    RF,RE                                                            
*                                                                               
         L     RE,TPST             TEMPEST BUFFER                               
         MHI   RE,L'TLUID                                                       
         AHI   RE,TTSSHDRL         ADD HEADER                                   
         ST    RE,TPSTF            AND SAVE                                     
         MHI   RE,FACIDMAX+1                                                    
         AHI   RE,TTMSHDRL+1023    PLUS HEADER LENGTH                           
         SRL   RE,10                                                            
         AR    RF,RE                                                            
*                                                                               
         L     R0,SSCT             SCRIPT TRACE BUFFER SIZE                     
         A     R0,STRC             TRACE BUFFER SIZE                            
         LTR   R0,R0               TRACE BUFFERS?                               
         BZ    FRKP02              NO                                           
         SRDL  R0,32                                                            
*                                                                               
         ICM   RE,15,DCTST         NUMBER OF TSTTAB ENTRIES                     
         BZ    FRKP02              NO TSTTAB                                    
         BCTR  RE,0                FIRST LINE TAKEN IS SPECIAL                  
*                                                                               
         MR    R0,RE               * NUMBER OF TSTTAB ENTRIES                   
         AHI   R1,1023                                                          
         SRL   R1,10               ROUND TO NEXT HIGHEST 1K MULTIPLE            
         AR    RF,R1               ADD TO TOTAL REQUIRED                        
*                                                                               
         USING HDRTABD,R3                                                       
FRKP02   LA    R3,HDRTAB           TABLE OF DATASPACE ENTRIES                   
*                                                                               
FRKP04   CLI   HDRNAME,X'FF'       REACHED END OF HEADER TABLE?                 
         BE    FRKP08              YES                                          
*                                                                               
         ICM   R0,15,HDRNUM        NUMBER OF ENTRIES                            
         BNZ   FRKP05                                                           
         AHI   RF,1                Add one 1k page for empties                  
         B     FRKP06                                                           
*                                                                               
FRKP05   MH    R0,HDRWIDTH         * WIDTH OF 1 ENTRY                           
         AHI   R0,1023                                                          
         SRL   R0,10               ROUND TO NEXT HIGHEST 1K MULTIPLE            
         AR    RF,R0               ADD TO TOTAL REQUIRED                        
*                                                                               
FRKP06   AHI   R3,HDRTABL          NEXT TABLE ENTRY                             
         B     FRKP04                                                           
*                                                                               
FRKP08   AHI   RF,4                EXTRA FOR DUMMY WRITE AREA                   
         AHI   RF,3                ROUND TO NEXT HIGHEST 4K                     
         SRL   RF,2                                                             
         ST    RF,PAGES            SET TOTAL NUMBER OF PAGES REQUIRED           
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD ADCONS                                                        *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
ADCONS   NTR1  ,                                                                
         SAM31                                                                  
*                                                                               
         L     R1,ADSDATA                                                       
         LHI   RF,((RMAXRES*64)+1023)  TABLE HEADERS                            
         SRL   RF,10                                                            
         SLL   RF,10                                                            
         AR    R1,RF                                                            
         ST    R1,ADSDATA                                                       
*                                                                               
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         L     RF,ADSDATA                                                       
         STCM  RF,15,TABSDUMP      SET A(DUMP INFORMATION BLOCK)                
         LHI   RE,((TDLENQ*(FACIDMAX+1))+1023)                                  
         SRL   RE,10                                                            
         SLL   RE,10                                                            
         AR    RF,RE                                                            
*                                                                               
         STCM  RF,15,TABSRUN       SET A(SERVER TABLE)                          
         L     R1,RUNRPGES                                                      
         MHI   R1,K                                                             
         AR    RF,R1                                                            
*                                                                               
         STCM  RF,15,TABSCOMM      SET A(COMMON UTILITY BLOCK)                  
         LHI   RE,TCOLEN           4K                                           
         AR    RF,RE                                                            
*                                                                               
         STCM  RF,15,TABSTMS       TEMPEST BUFFERS                              
         ICM   RE,15,TPSTF                                                      
         MHI   RE,FACIDMAX+1                                                    
         AHI   RE,TTMSHDRL                                                      
         AHI   RE,1023                                                          
         SRL   RE,10                                                            
         SLL   RE,10                                                            
         AR    RF,RE                                                            
*                                                                               
         STCM  RF,15,TABSLOX       LOCK TABLE                                   
         ICM   RE,15,NLOCKS                                                     
         MHI   RE,TLKLEN                                                        
         AHI   RE,1023                                                          
         SRL   RE,10                                                            
         SLL   RE,10                                                            
         AR    RF,RE                                                            
*                                                                               
         STCM  RF,15,ADUMMY                                                     
         AHI   RF,4*K                                                           
         STCM  RF,15,ADSDATA                                                    
         SAC   0                                                                
         SAM24                                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE ADCONS                                                   *         
***********************************************************************         
ADCINI   NTR1  ,                                                                
*&&US*&& BRAS  RE,SETUSDMP     *** BUILD DUMP HEADER DETAILS                    
*&&UK*&& BRAS  RE,SETUKDMP                                                      
         SAM31                                                                  
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER      *** BUILD SERVER INDEX BLOCKS                    
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSRUN                                                    
         USING TABSRUND,R2                                                      
         MVC   TABSLRUN,=CL8'*RUN*RUN'                                          
         MVI   TABSSRWD,TABSSVRL   Server entry width                           
         MVI   TABSJBWD,TABSQUEL   Job (queued) entry width                     
         MVI   TABSIND1,TABSINEW   New vs old style                             
         LR    R0,R2                                                            
         AHI   R0,TABSRUNL         Length of header                             
         ST    R0,TABSASVR         SET A(SERVER TABLE)                          
         L     R1,RUNRPGES                                                      
         STC   R1,TABSPGES         of 1K pages                                  
*                                                                               
         L     R1,#SERVERS         Number of servers to run                     
         STH   R1,TABSNSVR         SET N'SERVER ENTRIES                         
         MHI   R1,TABSSVRL         Width of server entry                        
         AR    R0,R1                                                            
         ST    R0,TABSAQUE         SET A(QUEUE TABLE)                           
*                                                                               
         L     R0,#QENTRES                                                      
         STH   R0,TABSNQUE         SET N'QUEUE ENTRIES                          
*                                                                               
         L     R2,AHEADER      *** BUILD PQ INDEX BLOCKS                        
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSCOMM                                                   
         USING TCOMMOND,R2                                                      
         MVC   TCOID,=CL16'*COM*COM*COM*COM'                                    
         DROP  R2                                                               
*                                                                               
         L     R2,AHEADER      *** BUILD LOCK TABLE                             
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSLOX                                                    
         ICM   RF,15,NLOCKS                                                     
         STCM  RF,15,12(R2)    SET NUMBER OF ENTRIES                            
         MHI   RF,TLKLEN                                                        
         AR    RF,R2                                                            
         STCM  RF,15,4(R2)     SET A(END)                                       
         MVI   8(R2),C'Y'                                                       
         LA    RF,TLKLEN(R2)                                                    
         STCM  RF,15,0(R2)     SET A(CURRENT)                                   
*                                                                               
         L     R2,AHEADER      *** BUILD TEMPEST INFORMATION                    
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSTMS                                                    
         USING TTMSHDRD,R2                                                      
         XC    0(TTMSHDRL,R2),0(R2)                                             
         MVC   0(8,R2),=CL8'*TEMPEST'                                           
         ICM   RE,15,TPST                                                       
         STCM  RE,15,TTMSNTRY                                                   
*                                                                               
         SAM24                                                                  
ADCIX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD HEADER INFO                                                   *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
HEADERS  NTR1  ,                                                                
         SAM31                                                                  
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER          R2=start of TABS dataspace                   
         SAC   512                 ACCESS REG MODE                              
         USING DMSPACED,R2                                                      
*                                                                               
         LA    R3,HDRTAB           TABLE OF DATASPACE ENTRIES                   
         USING HDRTABD,R3                                                       
         AHI   R2,L'DSPHDR         FIRST 64 BYTES HAS ADCONS                    
*                                                                               
HDR02    CLI   HDRNAME,X'FF'       REACHED END OF HEADER TABLE?                 
         BE    HDR10               YES                                          
         STCM  R2,15,HDRADR        SAVE A(IN DATASPACE) FOR LATER               
*                                                                               
         MVC   DSPNAME,HDRNAME     EYECATCHER                                   
         MVC   DSPTYPE,HDRTYPE     TYPE                                         
         MVC   DSPTWIDE,HDRWIDTH   WIDTH                                        
*                                                                               
         OC    HDRNUM,HDRNUM       ANYTHING SET?                                
         BNZ   HDR04               YES                                          
         MVC   DSPUSER,DEFUSR      SET EMPTY ROW                                
*                                                                               
* If want to use DUMMY then may add code to check for HDRUSED field             
*                                                                               
*        CLI   HDRUSED,HDDUMMY                                                  
*        BNE   HDR03                                                            
*        ICM   RF,15,ADUMMY                                                     
*        STCM  RF,15,DSPECB                                                     
*        AH    RF,HDRWIDTH                                                      
*        BCTR  RF,0                                                             
*        STCM  RF,15,DSPTEND                                                    
*        B     HDR06                                                            
*                                                                               
HDR03    ICM   RF,15,ADSDATA       A(FIRST ROW)                                 
         STCM  RF,15,DSPECB                                                     
         AHI   RF,K                Reserve 1K no mater what                     
         B     HDR05                                                            
*                                                                               
HDR04    ICM   RF,15,ADSDATA       A(FIRST ROW)                                 
         STCM  RF,15,DSPECB        A(Start of table)                            
         ICM   R1,15,HDRNUM          NUMBER OF ENTRIES                          
         MH    R1,HDRWIDTH         * WIDTH OF 1 ENTRY                           
         AR    RF,R1               + A(START of next table)                     
*                                                                               
HDR05    BCTR  RF,0                -1 for end of current table                  
         STCM  RF,15,DSPTEND       A(END)                                       
         AHI   RF,1                Reset A(Start next table)                    
*                                                                               
         AHI   RF,1023             ROUND TO NEXT HIGHEST 1K MULTIPLE            
         SRL   RF,10                                                            
         SLL   RF,10                                                            
         STCM  RF,15,ADSDATA       SET A(NEXT TABLE)                            
*                                                                               
HDR06    AHI   R3,HDRTABL          NEXT TABLE ENTRY                             
*                                                                               
HDR08    AHI   R2,L'DSPHDR         NEXT HEADER                                  
         B     HDR02                                                            
*                                                                               
HDR10    L     R2,AHEADER          SET A(END OF DATASPACE)                      
         USING FATABSD,R2                                                       
         L     RF,ADSDATA                                                       
         STCM  RF,15,TABSSTAT      Stats area                                   
         L     RE,DCSTATSK                                                      
         SLL   RE,10                                                            
         AR    RF,RE                                                            
         ST    RF,TABSLAST                                                      
         SAC   0                                                                
         SAM24                                                                  
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE DETAILS                                                 *         
* NTRY: ADSDATA POINTS TO FIRST TABLE POSITION                        *         
***********************************************************************         
TABLES   NTR1  ,                                                                
         BAS   RE,TSTSET                                                        
         BAS   RE,DDASET                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP TSTTAB ENTRIES                                               *         
***********************************************************************         
TSTSET   NTR1  ,                                                                
         SAM31                                                                  
         STAR  CLEAR=ARZERO,ARS=ON                                              
         LAM   AR2,AR2,DMALET                                                   
         ICM   R2,15,=AL4(DTTST)   ADDRESS TSTTAB HEADER ENTRY                  
         SLL   R2,17                                                            
         SRL   R2,17-6                                                          
         A     R2,AHEADER                                                       
         USING DMSPACED,R2                                                      
*                                                                               
         CPYA  ARF,AR2                                                          
*                                                                               
         XR    R4,R4               SET UP BXLE PARAMETERS                       
         ICM   R4,3,DSPTWIDE                                                    
         ICM   R5,15,DSPTEND                                                    
         ICM   R2,7,DSPTFRST+1                                                  
         USING TSTTABD,R2          PASS TSTTAB TO CLEAR A(TRCTAB)               
*                                                                               
         XC    TSTTRC,TSTTRC       FIRST LINE HOLDS SPECIAL INFORMATION         
         XC    TSTSCT,TSTSCT                                                    
         ICM   RF,15,PTSTDA        FIRST 2 BYTES ARE # TRACKS PER CI            
         STCM  RF,3,0(R2)                                                       
         ICM   RF,15,DCTST         NEXT 2 BYTES ARE # ENTRIES IN TABLE          
         STCM  RF,3,2(R2)                                                       
         B     TSTS06              REST OF LINE IS EMPTY                        
*                                                                               
TSTS02   XC    TSTTRC,TSTTRC                                                    
         ICM   RF,15,ADSDATA       NEXT FREE ADDRESS                            
         ST    RF,TSTTRC           SET BUFFER ADDRESS                           
*                                                                               
         ICM   R0,15,STRC          GET LENGTH OF TRC BUFFER                     
         BZ    TSTS04                                                           
         AR    R0,RF                                                            
         BCTR  R0,0                                                             
         STCM  R0,15,40(RF)        SET END ADDRESS                              
*                                                                               
         LA    R0,4                SET EYECATCHER                               
         MVC   0(8,RF),=C'*TRCBUFF'                                             
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ICM   RF,15,ADSDATA                                                    
         ICM   R0,15,STRC                                                       
         AR    RF,R0                                                            
         STCM  RF,15,ADSDATA       BUMP ADSDATA TO NEXT FREE                    
*                                                                               
TSTS04   XC    TSTSCT,TSTSCT       PROCESS SCRIPT TRACE TABLE                   
         ICM   R0,15,SSCT          TEST NEED SCRIPT TRACE BUFFER                
         BZ    TSTS06                                                           
*                                                                               
         STCM  RF,15,TSTSCT         SET BUFFER ADDRESS                          
         AR    R0,RF                                                            
         BCTR  R0,0                                                             
         STCM  R0,15,SCTEND-SCTTABD(RF)  SET END ADDRESS                        
*                                                                               
         LA    R0,4                SET EYECATCHER                               
         MVC   0(8,RF),=C'*SCTBUF*'                                             
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ICM   RF,15,ADSDATA                                                    
         ICM   R0,15,SSCT                                                       
         AR    RF,R0                                                            
         STCM  RF,15,ADSDATA       BUMP ADSDATA TO NEXT FREE                    
*                                                                               
TSTS06   BXLE  R2,R4,TSTS02                                                     
         DROP  R2                                                               
*                                                                               
         REAR  ARS=OFF                                                          
         SAM24                     GET OUT OF 31-BIT MODE                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP DARE MAIL TABLE ENTRIES                                      *         
***********************************************************************         
DDASET   NTR1  ,                                                                
         SAM31                                                                  
         STAR  CLEAR=ARZERO,ARS=ON                                              
         LAM   AR2,AR2,DMALET                                                   
         ICM   R2,15,=AL4(DTDARE)  ADDRESS DARETAB HEADER ENTRY                 
         SLL   R2,17                                                            
         SRL   R2,17-6                                                          
         A     R2,AHEADER                                                       
         USING DMSPACED,R2                                                      
         ICM   R2,7,DSPTFRST+1                                                  
*                                                                               
         USING TABDARED,R2                                                      
         LHI   RF,TBDSORT-TABDARED STORAGE HEADER LENGTH                        
         XR    RE,RE                                                            
         LA    R0,TBDDARL          LENGTH OF A SLOT                             
         DR    RE,R0               NUMBER OF ENTRIES FOR STORAGE HEADER         
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RE,1(RF)            PUT SPACE FOR FRONT REQUIRED IN RE           
*                                                                               
         ICM   RF,15,DCDARE        HALVE THE NUMBER OF ENTRIES                  
         SR    RF,RE                                                            
         SRL   RF,1                                                             
         STCM  RF,15,TBDMAX        SET MAX NUMBER OF ENTRIES                    
*                                                                               
         LA    RE,TBDDARL          LENGTH OF A SINGLE ENTRY                     
         MSR   RF,RE               RF=LENGTH OF UNSORTED TABLE                  
         AR    RF,R2                                                            
         AHI   RF,TBDSORT-TABDARED PLUS HEADER                                  
         STCM  RF,15,TBDFRST       SET A(UNSORTED TABLE)                        
*                                                                               
         XC    TBDNOW,TBDNOW       RESET CURRENT COUNT                          
*                                                                               
         USING BSPARA,TBDBSP   *** SET PARAMETERS FOR BINSRCH                   
         LA    RF,TBDSORT                                                       
         STCM  RF,15,BSPSTRT       SET A(SORTED TABLE)                          
*                                                                               
         LA    RF,TBDKEYL                                                       
         STCM  RF,15,BSPLENK       KEY LENGTH                                   
         MVI   BSPKEYD,0           DISPLACEMENT TO KEY                          
         LA    RF,TBDDARL                                                       
         STCM  RF,15,BSPLENR       RECORD LENGTH                                
*                                                                               
         ICM   RE,15,DCDARE        HALVE THE NUMBER OF ENTRIES                  
         SRL   RE,1                                                             
         LA    RF,TBDDARL          LENGTH OF A SINGLE ENTRY                     
         MSR   RE,RF               RE=LENGTH OF SORTED TABLE                    
*                                                                               
         AHI   RE,-(TBDSORT-TABDARED)                                           
         SRDL  RE,32               REDUCE BY LENGTH OF HEADER INFO              
         LA    R0,TBDDARL                                                       
         DR    RE,R0               FIND HOW MANY WILL FIT INTO SPACE            
         STCM  RF,15,BSPEND        SET MAX NUMBER OF RECORDS                    
*                                                                               
         XC    BSPNOR,BSPNOR       TABLE IS EMPTY PRESENTLY                     
         STAM  AR2,AR2,BSPARS      SET ACCESS REGISTER                          
*                                                                               
         REAR  ARS=OFF                                                          
         SAM24                     GET OUT OF 31-BIT MODE                       
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
SETOPS   NTR1  ,                                                                
         LHI   R1,9                BEGAN SETTING OPERATOR COMMS                 
         BRAS  RE,DOMSG                                                         
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
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
         LHI   R1,10               ENDED SETTING OPERATOR COMMS                 
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATASPACE ROUTINES                                                  *         
***********************************************************************         
FREESPC  ST    RE,SAVERE           DELETE DATASPACE                             
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'DEL '                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                 NB - IGNORE CONDITON CODE                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MAKESPC  ST    RE,SAVERE           CREATE NEW DATASPACE                         
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'MAKE'                                                 
         CLI   FPROT,NO            Not read or write                            
         BNE   *+10                                                             
         MVC   WORK(4),=C'MAKF'    Read-only                                    
         MVC   WORK+4(12),DSPACE                                                
         ICM   RF,15,PAGES         NUMBER OF 4K PAGES                           
         STCM  RF,15,WORK+16                                                    
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GETSPC   ST    RE,SAVERE           GET ALET                                     
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DMOFFS,WORK+20      EXTRACT VALUES                               
         MVC   DMALET,WORK+24                                                   
         MVC   DMTOKN,WORK+28                                                   
         OC    DMALET,DMALET                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GETMAIN ROUTINES                                                    *         
***********************************************************************         
GETMAIN  NTR1  ,                                                                
         ICM   R3,15,PAGES         NUMBER OF 4K PAGES                           
         SLL   R3,12                                                            
*                                                                               
         SAM31                                                                  
         STORAGE OBTAIN,LENGTH=(3),LOC=ANY,BNDRY=PAGE,COND=YES                  
         SAM24                                                                  
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         STCM  R0,15,GETLEN                                                     
         STCM  R1,15,ADSDATA                                                    
         STCM  R1,15,AHEADER                                                    
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* SETWAIT - SET TIMER AND WAIT INDEFINITELY - ONLY RETURN WHEN AN     *         
* INTERRUPT IS DETECTED                                               *         
***********************************************************************         
SETWAIT  NTR1  ,                                                                
SETWAIT1 L     R1,AOPERECB                                                      
         WAIT  ECB=(1)                                                          
*                                                                               
         L     RF,ACOMM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BE    SETWAITX                                                         
*                                                                               
         CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         XR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
         MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CARD(0),CIBDATA                                                  
*                                                                               
*NOP     LA    R1,CARD             NO OPS COMMANDS REQUIRED                     
*NOP     BRAS  RE,VALCARD          VALIDATE CARD INPUT                          
*NOP     BE    CHEK020             NEQ SO TRY GROUP COMMAND                     
*                                                                               
         MVC   PLINE(32),=C'INVALID KEYWORD               //'                   
         MVC   PLINE+16(13),CARD                                                
         GOTO1 VDDWTO,DMCB,PLINE,0                                              
         B     CHEKRSET                                                         
*                                                                               
CHEK020  EQU   *                                                                
*                                                                               
CHEKRSET L     RF,ACOMM            RESET OPER COMMS                             
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    EXITOK                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     SETWAIT1                                                         
         DROP  RF                                                               
*                                                                               
SETWAITX B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* OUTPUT INFORMATION MESSAGE                                          *         
* NTRY: R1  NZ      INDEX TO MESSAGE                                  *         
*       R1  ZERO    MESSAGE IS ALREADY ON PRINT LINE                  *         
***********************************************************************         
DOMSG    NTR1  ,                                                                
         LTR   R1,R1                                                            
         BZ    DOMSG02                                                          
         BCTR  R1,0                                                             
         MHI   R1,L'MESSTAB                                                     
         A     R1,AMESSTAB                                                      
         MVC   P(L'MESSTAB),0(R1)                                               
*                                                                               
DOMSG02  GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
CARDVAL  NTR1  ,                                                                
         ST    RD,CARDRD                                                        
         LA    R2,CARD             R2=A(CARD START)                             
         LA    R1,79(R2)                                                        
         ST    R1,CARDEND          SAVE A(LAST CHAR)                            
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 =V(SCAN31),DMCB,(R2),SCNBLK,0,(1,SCICARD),20                     
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         LA    R3,CARDTAB                                                       
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,*+8                                                           
         BE    CARDV06                                                          
         CLC   SC1STFLD(0),CNAME                                                
CARDV04  LA    R3,CARDTABL(R3)                                                  
         B     CARDV02                                                          
*                                                                               
CARDV06  CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CARDV08             NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CARDV10             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         MVC   0(0,RE),SC2NDFLD                                                 
         EX    RF,*-6                                                           
         B     EXITOK                                                           
*                                                                               
CARDV10  DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,INVLIN                                                        
         B     CERR                                                             
*                                                                               
CEINVKEY LA    R1,INVKEY                                                        
         B     CERR                                                             
*                                                                               
CENOTNUM LA    R1,NOTNUM                                                        
         B     CERR                                                             
*                                                                               
CENOTCHR LA    R1,NOTCHR                                                        
         B     CERR                                                             
*                                                                               
CETOOSHT LA    R1,TOOSHT                                                        
         B     CERR                                                             
*                                                                               
CETOOLNG LA    R1,TOOLNG                                                        
         B     CERR                                                             
*                                                                               
CETOOLOW LA    R1,TOOLOW                                                        
         B     CERR                                                             
*                                                                               
CETOOBIG LA    R1,TOOBIG                                                        
         B     CERR                                                             
*                                                                               
CENOINP  LA    R1,NOINP                                                         
         B     CERR                                                             
*                                                                               
CERR     L     RD,CARDRD                                                        
         MVC   P(15),=CL15'!! ***ERROR*** '                                     
         MVC   P+15(40),0(R1)                                                   
         GOTO1 VPRINTER                                                         
         BRAS  RE,CARDHLP                                                       
         LH    R0,ERRCNT                                                        
         AHI   R0,1                                                             
         STH   R0,ERRCNT                                                        
         B     EXITL                                                            
*                                                                               
INVKEY   DC    CL40'Invalid Keyword'                                            
INVLIN   DC    CL40'Invalid line format'                                        
TOOLOW   DC    CL40'Numeric value too small'                                    
TOOBIG   DC    CL40'Numeric value too large'                                    
NOINP    DC    CL40'Invalid/Missing value'                                      
NOTNUM   DC    CL40'Value not a valid number'                                   
NOTCHR   DC    CL40'Value not a valid character string'                         
TOOSHT   DC    CL40'Length of input string too short'                           
TOOLNG   DC    CL40'Length of input string too long'                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT HELP FOR THIS CARD BASED ON LAST TIME INFORMATION *         
* NTRY:  R2    = A(SCANBLK ENTRY FOR THIS FIELD)                      *         
*        R3    = A(CARDTAB ENTRY FOR THIS FIELD)                      *         
***********************************************************************         
         USING SCANBLKD,R2                                                      
         USING CARDTABD,R3                                                      
CARDHLP  NTR1  ,                                                                
         LA    R4,P                                                             
         MVC   0(L'CHMS01,R4),CHMS01                                            
         CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BE    CHLP02              NO                                           
         MVC   0(L'CHMS02,R4),CHMS02                                            
         CLI   CTYPE,CTCHR         CHARACTER INPUT?                             
         BE    CHLP02              NO                                           
         MVC   P,SPACES                                                         
         B     EXITOK                                                           
*                                                                               
CHLP02   AHI   R4,L'CHMS01+1                                                    
         EDIT  (B4,CMIN),(9,(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                     
         AR    R4,R0                                                            
         MVC   1(L'TO,R4),TO                                                    
         AHI   R4,L'TO+2                                                        
         EDIT  (B4,CMAX),(9,(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                     
         GOTO1 VPRINTER                                                         
*                                                                               
CHLP04   OPEN  (OLDPARMS,INPUT)                                                 
         LTR   RF,RF                                                            
         BZ    CHLP06                                                           
         LHI   R1,8                OLDPARMS WON'T OPEN                          
         BRAS  RE,DOMSG                                                         
         B     EXITL                                                            
*                                                                               
CHLP06   GET   OLDPARMS,OLDCARD                                                 
         CLC   =C'/*',OLDCARD                                                   
         BE    CHLP08                                                           
         CLI   OLDCARD,C'*'                                                     
         BE    CHLP06                                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,SC1STLEN                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   CHLP06                                                           
         CLC   SC1STFLD(0),OLDCARD                                              
*                                                                               
         MVC   P(L'CHMS03),CHMS03                                               
         MVC   P+L'CHMS03(80),OLDCARD                                           
         GOTO1 VPRINTER                                                         
         B     CARDHLPX                                                         
*                                                                               
CHLP08   MVC   P(L'CHMS04),CHMS04                                               
         GOTO1 VPRINTER                                                         
         B     CARDHLPX                                                         
*                                                                               
CARDHLPX CLOSE OLDPARMS                                                         
         B     EXITOK                                                           
*                                                                               
CHMS01   DC    CL33'!! Numeric value must be in range'                          
CHMS02   DC    CL33'!! Input string must be of length'                          
CHMS03   DC    CL33'!! OLDPARMS parameter value was: '                          
CHMS04   DC    CL41'!! OLDPARMS parameter value was not found'                  
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* OUTPUT DSPACE INFORMATION FOR ANALYSIS                              *         
***********************************************************************         
RESULTS  NTR1  ,                                                                
         SAM31                                                                  
         ZAP   LINE,=P'99'                                                      
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'DTITLE),DTITLE                                           
         GOTO1 VPRINTER                                                         
*                                                                               
*        CLI   PKEY,C'9'                                                        
*        BNE   RSLT02                                                           
*        SPKA  144                                                              
                                                                                
RSLT02   LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER                                                       
         SAC   512                                                              
*                                                                               
*        SPKA  144                                                              
         MVC   WORK(64),0(R2)     NEED TO RELOCATE THESE 16 ADCONS              
*        SPKA  128                                                              
         LAM   AR2,AR2,ARZERO                                                   
         SAC   0                                                                
*                                                                               
         L     R2,AHEADER                                                       
         GOTOR GOPRINT,DMCB,=C'FATABSD',WORK,C'DUMP',64,=C'1D'                  
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R2,AHEADER                                                       
         AHI   R2,64                                                            
*                                                                               
RSLT04   LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET                                                   
         SAC   512                                                              
         USING DMSPACED,R2                                                      
         OC    DSPNAME,DSPNAME                                                  
         BZ    RSLT06                                                           
         DROP  R2                                                               
*                                                                               
         MVC   WORK(64),0(R2)                                                   
         LAM   AR2,AR2,ARZERO                                                   
         SAC   0                                                                
*                                                                               
         LA    R3,WORK                                                          
         USING DMSPACED,R3                                                      
         MVC   P(L'DSPNAME),DSPNAME             NAME                            
*                                                                               
         MVC   P+10(6),=CL6'Start='             START ADDRESS                   
         ICM   RF,15,DSPECB                                                     
         S     RF,AHEADER                                                       
         ST    RF,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL,P+16,4,0                                       
*                                                                               
         MVC   P+25(6),=CL6'Width='             WIDTH                           
         GOTO1 VHEXOUT,DMCB,DSPTWIDE,P+31,2,0                                   
*                                                                               
         MVC   P+36(6),=CL6'End-1='             END ADDRESS                     
         ICM   RF,15,DSPTEND                                                    
         S     RF,AHEADER                                                       
         ST    RF,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL,P+42,4,0                                       
*                                                                               
         MVC   P+51(12),=CL12'Num Entries='     ENTRY COUNT                     
*                                                                               
         ICM   RE,15,DSPTEND                                                    
         AHI   RE,1                                                             
         ICM   RF,15,DSPECB                                                     
         N     RF,=X'0FFFFFFF'                                                  
         SR    RE,RF                                                            
         SRDL  RE,32                                                            
         XR    R0,R0                                                            
         ICM   R0,3,DSPTWIDE                                                    
         DR    RE,R0                                                            
         EDIT  (RF),(11,P+63),0,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
         GOTO1 VPRINTER                                                         
         AHI   R2,L'DSPHDR         NEXT HEADER                                  
         B     RSLT04                                                           
         DROP  R3                                                               
*                                                                               
RSLT06   LAM   AR0,ARF,ARZERO                                                   
         SAC   0                                                                
*                                                                               
         ZAP   LINE,=P'99'         ENSURE NEW PAGE                              
         GOTO1 VPRINTER                                                         
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER                                                       
         USING FATABSD,R2                                                       
         SAC   512                                                              
*                                                                               
         L     R0,AIOA             COPY OUT TEMPEST BLOCK                       
         LHI   R1,128                                                           
         L     RE,TABSTMS                                                       
         CPYA  ARE,AR2                                                          
         LHI   RF,128                                                           
         MVCL  R0,RE                                                            
         LAM   AR0,ARF,ARZERO                                                   
         SAC   0                                                                
*                                                                               
         GOTOR GOPRINT,DMCB,=C'TEMPEST',AIOA,C'DUMP',128,=C'1D'                 
*                                                                               
         ZAP   LINE,=P'99'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER                                                       
         USING FATABSD,R2                                                       
         SAC   512                                                              
*                                                                               
         L     R0,AIOA                                                          
         LHI   R1,4096                                                          
         L     RE,TABSDUMP         DUMP BLOCK                                   
         CPYA  ARE,AR2                                                          
         LHI   RF,4096                                                          
         MVCL  R0,RE                                                            
         LAM   AR0,ARF,ARZERO                                                   
         SAC   0                                                                
*                                                                               
         GOTOR GOPRINT,DMCB,=C'TABSDUMP',AIOA,C'DUMP',4096,=C'1D'               
*                                                                               
         ZAP   LINE,=P'99'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER                                                       
         USING FATABSD,R2                                                       
         SAC   512                                                              
*                                                                               
         L     R0,AIOA                                                          
         LHI   R1,2048                                                          
         L     RE,TABSCOMM         COMMON BLOCK                                 
         CPYA  ARE,AR2                                                          
         LHI   RF,2048                                                          
         MVCL  R0,RE                                                            
         LAM   AR0,ARF,ARZERO                                                   
         SAC   0                                                                
*                                                                               
         GOTOR GOPRINT,DMCB,=C'TABSCOMM',AIOA,C'DUMP',2048,=C'1D'               
*                                                                               
         ZAP   LINE,=P'99'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER                                                       
         USING FATABSD,R2                                                       
         SAC   512                                                              
*                                                                               
         USING TABSRUND,RE                                                      
         L     RE,TABSRUN          RUNNER BLOCK                                 
         ST    RE,FULL                                                          
         CPYA  ARE,AR2                                                          
         L     R0,AIOA                                                          
         LLC   R1,TABSPGES                                                      
         DROP  RE                                                               
                                                                                
         MHI   R1,K                                                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LAM   AR0,ARF,ARZERO                                                   
         SAC   0                                                                
*                                                                               
         USING TABSRUND,R5                                                      
         L     R5,AIOA                                                          
         GOTOR GOPRINT,DMCB,=C'RUNNER',(R5),C'DUMP',TABSRUNL,=C'1D'             
*                                                                               
         L     R5,AIOA                                                          
         LLC   R3,TABSSRWD         Width of server                              
         MH    R3,TABSNSVR         Number of servers max                        
         L     R6,FULL                                                          
         L     R7,TABSASVR                                                      
         SR    R7,R6                                                            
         AR    R5,R7                                                            
         GOTOR GOPRINT,DMCB,=C'SERVERS',(R5),C'DUMP',(R3),=C'1D'                
*                                                                               
         L     R5,AIOA                                                          
         LLC   R3,TABSJBWD                                                      
         MH    R3,TABSNQUE                                                      
         L     R6,FULL                                                          
         L     R7,TABSAQUE                                                      
         SR    R7,R6               R7= displacement to queue                    
         AR    R5,R7                                                            
         GOTOR GOPRINT,DMCB,=C'QUEUES',(R5),C'DUMP',(R3),=C'1D'                 
         DROP  R5                                                               
*                                                                               
         ZAP   LINE,=P'99'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET                                                   
         L     R2,AHEADER                                                       
         USING FATABSD,R2                                                       
         SAC   512                                                              
*                                                                               
         L     R0,AIOA                                                          
         LHI   R1,4096                                                          
         L     RE,TABSLOX          LOCKSPC LOCK TABLE                           
         CPYA  ARE,AR2                                                          
         LHI   RF,4096                                                          
         MVCL  R0,RE                                                            
         LAM   AR0,ARF,ARZERO                                                   
         SAC   0                                                                
*                                                                               
         GOTOR GOPRINT,DMCB,=C'LOCKSPC TABLE',AIOA,C'DUMP',4096,=C'1D'          
         SAM24                                                                  
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CALL PRNTBL                                                                   
***********************************************************************         
GOPRINT  NTR1                                                                   
         SAM24                                                                  
         GOTOR VPRNTBL,DMCB                                                     
         B     EXIT                                                             
                                                                                
***********************************************************************         
*        SET UP DUMP TABLES FOR US SYSTEM                             *         
***********************************************************************         
*&&US                                                                           
*                  ? |T |A |A |R |A |M |A |A |R |A |C |A |A |R |F |             
*                  ? |S |D |D |E |D |E |D |D |E |D |S |D |D |E |Q |             
*                  ? |T |V |V |P |V |L |V |V |P |V |C |V |V |P |A |             
*                  ? |  |1 |5 |A |2 |  |3 |4 |C |6 |  |7 |8 |B |  |             
DUMPADV  DC    AL1(00,00,13,13,00,13,00,13,13,00,13,00,13,10,00,00)             
DUMPREP  DC    AL1(00,00,00,00,17,00,00,00,00,16,00,00,00,00,16,00)             
DUMPTSTS DC    AL1(00,30,00,00,00,00,07,00,00,00,00,19,00,00,00,19)             
***********************************************************************         
*                                                                               
* Size of DMPFILE = (# of dumps+1) x (# of cylinder/dump)                       
* 2K parts based on blocksize is 24/cylinder                                    
* Total per dump is 24 x (# of cylinders/dump) See FADMPHDR                     
*                                                                               
*14 PER EACH ADV, EXCEPT 11 FOR ADV8  (total=109)                               
*18 FOR REPA, 17 FOR REPB/C  (total=52)                                         
*31 FOR TST, 20 FOR CSC, 20 for FQA, 8 FOR MEL  (total=79)                      
*                                                                               
***********************************************************************         
SETUSDMP NTR1                                                                   
         CLC   TABSTST,SYSTEM      TST/MEL DATASPACE?                           
         BE    SETUS10                                                          
         CLC   TABSCSC,SYSTEM      CSC DATASPACE?                               
         BE    SETUS10                                                          
         CLC   TABSFQA,SYSTEM      FQA DATASPACE?                               
         BE    SETUS10                                                          
         B     SETUS20                                                          
*                        DO IT FOR TST/MEL/FQA/CSC B/C ONLY 1 DUMP FILE         
SETUS10  LA    R2,DMPFILT          TST                                          
         LA    R3,DMPBLKT                                                       
         BRAS  RE,DMPRPT           REPORT ON DUMP FILE DETAILS                  
         MVC   WORK,DUMPTSTS                                                    
         MVI   BYTE,0                                                           
         BRAS  RE,DSDPINF          FILL DATASPACE WITH DUMP INFO                
         B     SETUSX                                                           
*                         DO IT FOR ADV+REP B/C 2 DUMP FILES,                   
*                                           BUT ONLY 1 PROD TABS DS             
SETUS20  LA    R2,DMPFILA          ADV                                          
         LA    R3,DMPBLKA                                                       
         BRAS  RE,DMPRPT           REPORT ON DUMP FILE DETAILS                  
         MVC   WORK,DUMPADV                                                     
         MVI   BYTE,0                                                           
         BRAS  RE,DSDPINF          FILL DATASPACE WITH DUMP INFO                
*                                                                               
         LA    R2,DMPFILR          REP                                          
         LA    R3,DMPBLKR                                                       
         BRAS  RE,DMPRPT           REPORT ON DUMP FILE DETAILS                  
         MVC   WORK,DUMPREP                                                     
         MVI   BYTE,C'+'                                                        
         BRAS  RE,DSDPINF          FILL DATASPACE WITH DUMP INFO                
*                                                                               
SETUSX   B     EXITOK                                                           
*                                                                               
***********************************************************************         
*        SET UP DUMP TABLES FOR UK SYSTEM                             *         
***********************************************************************         
*&&UK                                                                           
DUMPTST  DC    AL1(0,15,0,6,0,6,0,0,0,0,0,0,0,0) 16 TST 7 TTS/NEW               
DUMPADV  DC    AL1(0,0,4,0,4,0,4,0,4,0,4,0,4,0,4,0) 5 EACH ON ADV               
DUMPCSC  DC    AL1(0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0) 9 FOR CSC ONLY              
DUMPFQA  DC    AL1(0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0) 9 FOR FQA ONLY              
DUMPBAR  DC    AL1(0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0) 3 FOR BAR ONLY              
*                                                                               
SETUKDMP NTR1                                                                   
         CLC   TABSTST,SYSTEM      TST DATASPACE?                               
         BNE   SETUK001                                                         
         LA    R2,DMPFILT          TST                                          
         LA    R3,DMPBLKT                                                       
         BRAS  RE,DMPRPT           REPORT ON DUMP FILE DETAILS                  
         MVC   WORK,DUMPTST                                                     
         BRAS  RE,DSDPINF          FILL DATASPACE WITH DUMP INFO                
         B     SETUKX                                                           
*                                                                               
SETUK001 CLC   TABSADV,SYSTEM      ADV DATASPACE?                               
         BNE   SETUK002                                                         
         LA    R2,DMPFILA          ADV                                          
         LA    R3,DMPBLKA                                                       
         BRAS  RE,DMPRPT           REPORT ON DUMP FILE DETAILS                  
         MVC   WORK,DUMPADV                                                     
         BRAS  RE,DSDPINF          FILL DATASPACE WITH DUMP INFO                
         B     SETUKX                                                           
*                                                                               
SETUK002 CLC   TABSCSC,SYSTEM      CSC DATASPACE?                               
         BNE   SETUK003                                                         
         LA    R2,DMPFILC          CSC                                          
         LA    R3,DMPBLKC                                                       
         BRAS  RE,DMPRPT           REPORT ON DUMP FILE DETAILS                  
         MVC   WORK,DUMPCSC                                                     
         BRAS  RE,DSDPINF          FILL DATASPACE WITH DUMP INFO                
         B     SETUKX                                                           
*                                                                               
SETUK003 CLC   TABSFQA,SYSTEM      FQA DATASPACE?                               
         BNE   SETUK004                                                         
         LA    R2,DMPFILF          FQA                                          
         LA    R3,DMPBLKC                                                       
         BRAS  RE,DMPRPT           REPORT ON DUMP FILE DETAILS                  
         MVC   WORK,DUMPFQA                                                     
         BRAS  RE,DSDPINF          FILL DATASPACE WITH DUMP INFO                
         B     SETUKX                                                           
*                                                                               
SETUK004 CLC   TABSBAR,SYSTEM      BAR DATASPACE?                               
         BNE   SETUK005                                                         
         LA    R2,DMPFILB          BAR                                          
         LA    R3,DMPBLKC                                                       
         BRAS  RE,DMPRPT           REPORT ON DUMP FILE DETAILS                  
         MVC   WORK,DUMPBAR                                                     
         BRAS  RE,DSDPINF          FILL DATASPACE WITH DUMP INFO                
         B     SETUKX                                                           
*                                                                               
SETUK005 DC    H'0'                WHAT DATASPACE IS IT THEN?                   
*                                                                               
SETUKX   B     EXITOK                                                           
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* REPORT ON DUMP FILE DETAILS                                         *         
* NTRY: R2     = A(DUMP FILE DCB)                                     *         
*       R3     = A(DUMP FILE OUTPUT BLOCK)                            *         
***********************************************************************         
         USING DTFPHD,R2                                                        
         USING DMPINFOD,R3                                                      
DMPRPT   NTR1  ,                                                                
         XC    P1(24),P1           OPEN DMPFILE AND GET DEVICE DATA             
         OI    DTFOPEN,DTF_RO                                                   
         GOTO1 ADADDS,P1,14,,,(R2)                                              
         LHI   R0,DMPBLKQ          Size of dump block                           
         GOTO1 (RF),(R1),16,0,(R0),(R2)                                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,P3+2                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    RF,DRECTRK          SAVE RECORDS PER TRACK                       
         L     RF,P2                                                            
         LH    RF,2(RF)                                                         
         ST    RF,DTRKCYL          SAVE TRKS PER CYL                            
*                                                                               
         LA    RF,DMTX                                                          
         TM    DIND,DINDXAM        High core extent matrix                      
         BZ    *+8                 No                                           
         ICM   RF,15,DMTX          A(High core extent matrix)                   
*                                                                               
         USING EXTENTD,RF                                                       
         SAM31                                                                  
         XR    RE,RE                                                            
         XR    R0,R0                                                            
DIN02    CLI   0(RF),X'FF'                                                      
         BE    DIN04                                                            
         ICM   R0,3,EXT#TRKS                                                    
         AR    RE,R0                                                            
         LA    RF,EXTLNQ(,RF)                                                   
         B     DIN02                                                            
         DROP  R2,RF                                                            
*                                                                               
DIN04    SAM24                                                                  
         ST    RE,DTOTTRKS                                                      
         SRDL  RE,32                                                            
         D     RE,DTRKCYL          RF=NUM OF CYLS IN DMPFILE                    
         ST    RF,DTOTCYLS                                                      
*                                                                               
         XR    RE,RE                                                            
         D     RE,DMPCYLS                                                       
         AHI   RF,-1               THIS IS FOR THE FIRST DUMP ON FILE           
         ST    RF,DNUMDMPS                                                      
*                                                                               
         XC    P1(24),P1                                                        
         GOTO1 ADADDS,P1,15,,,(R2)                                              
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FILL TABS DATASPACE WITH DUMP INFO                                  *         
* NTRY: WORK   = DUMP ALLOCATION TABLE                                *         
***********************************************************************         
DSDPINF  NTR1  ,                                                                
         SAM31                                                                  
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET      NOW FILL DATASPACE WITH DUMP INFO            
         L     R2,AHEADER                                                       
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSDUMP      R2=A(DATASPACE SLOT)                         
         USING TORDUMPD,R2                                                      
         L     R3,VFACITAB         R3=A(FACPAK NAME)                            
         USING FACITABD,R3                                                      
         LA    R1,WORK             R1=DUMP ALLOCATION TABLE                     
         LA    RE,2                RE=CURRENT DUMP NUMBER                       
*                                                                               
DSDPI020 CLI   FACISN4,X'FF'       END OF TABLE?                                
         BE    DSDPIX              YES                                          
         MVC   TDSYSNA,FACISN4     SET SYSTEM NAME                              
*                                                                               
         CLI   BYTE,C'+'           ADD TO DUMP INFO TABLE                       
         BE    DSDPI050                DON'T SET DEFAULT VALUE OF 1             
*                                                                               
         LHI   RF,1                DEFAULT TO FIRST SLOT                        
         STCM  RF,15,TDDUMPFS                                                   
         STCM  RF,15,TDDUMPNO                                                   
         STCM  RF,15,TDDUMPMX                                                   
*                                                                               
DSDPI050 CLI   0(R1),0             NO DUMPS TO ALLOCATE                         
         BE    DSDPI900                                                         
*                                                                               
         STCM  RE,15,TDDUMPFS      SET CURRENT AS FIRST                         
         STCM  RE,15,TDDUMPNO      SET ON FIRST DUMP TOO                        
         SR    RF,RF                                                            
         IC    RF,0(R1)            NUMBER OF DUMPS FOR THIS SYSTEM              
         AR    RE,RF                                                            
         STCM  RE,15,TDDUMPMX      SET MAX DUMP FOR THIS SYSTEM                 
         LA    RE,1(RE)                                                         
*                                                                               
DSDPI900 LA    R1,1(R1)            NEXT IN DUMP TABLE                           
         AHI   R2,TDLENQ           NEXT IN DATASPACE                            
         AHI   R3,L'FACIDTAB       NEXT IN TABLE                                
         B     DSDPI020                                                         
*                                                                               
DSDPIX   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         SAM24                                                                  
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
                                                                                
         ORG   TABSDSP+((((*-TABSDSP)/16)+1)*16)                                
CARDTAB  DC    CL8'MODE    ',F'001',F'0000010'                                  
         DC    AL1(03,CTCHR,L'MODE,0),AL4(MODE)                                 
         DC    CL8'FPROT   ',F'001',F'0000003'                                  
         DC    AL1(04,CTCHR,L'FPROT,0),AL4(FPROT)                               
         DC    CL8'PKEY    ',F'001',F'0000001'                                  
         DC    AL1(03,CTCHR,L'PKEY,0),AL4(PKEY)                                 
         DC    CL8'RUN     ',F'001',F'0000010'                                  
         DC    AL1(02,CTCHR,L'RUN,0),AL4(RUN)                                   
         DC    CL8'KILL    ',F'001',F'0000004'                                  
         DC    AL1(03,CTCHR,L'KILL,0),AL4(KILL)                                 
         DC    CL8'DSPACE  ',F'001',F'0000012'                                  
         DC    AL1(05,CTCHR,L'DSPACE,0),AL4(DSPACE)                             
         DC    CL8'LOCKTAB ',F'001',F'1000000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(NLOCKS)                                    
         DC    CL8'SYSTEM  ',F'001',F'0000004'                                  
         DC    AL1(05,CTCHR,L'SYSTEM,0),AL4(SYSTEM)                             
         DC    CL8'Y2K     ',F'001',F'0000001'                                  
         DC    AL1(02,CTCHR,L'DCY2K,0),AL4(DCY2K)                               
         DC    CL8'CTBUFFER',F'001',F'0000064'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCCTB)                                     
         DC    CL8'WRKR    ',F'001',F'0000064'                                  
         DC    AL1(03,CTNUM,0,0),AL4(DCWRKR)                                    
         DC    CL8'TRACE   ',F'000',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCTRACE)                                   
*                                                                               
*        DDLINK / RUNNER                                                        
         DC    CL8'SERVERS ',F'032',F'0000128'                                  
         DC    AL1(06,CTNUM,0,0),AL4(#SERVERS)                                  
         DC    CL8'QENTRIES',F'100',F'0001024'                                  
         DC    AL1(07,CTNUM,0,0),AL4(#QENTRES)                                  
*                                                                               
         DC    CL8'LOCKET  ',F'001',F'0050000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCLOCK)                                    
         DC    CL8'JOBTAB  ',F'001',F'0005000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCJOB)                                     
         DC    CL8'PRTQ    ',F'001',F'0000100'                                  
         DC    AL1(03,CTNUM,0,0),AL4(DCPRTQ)                                    
         DC    CL8'TSTTAB  ',F'000',F'0000020'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCTST)                                     
         DC    CL8'DARETAB ',F'000',F'0005000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDARE)                                    
         DC    CL8'ASSIST  ',F'000',F'0008000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCASS)                                     
         DC    CL8'FUSION  ',F'000',F'0050000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCFUSN)                                    
         DC    CL8'TSTTRKS ',F'000',F'0000020'                                  
         DC    AL1(06,CTNUM,0,0),AL4(PTSTDA)                                    
         DC    CL8'VRSNTAB ',F'001',F'0005000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCVRSN)                                    
         DC    CL8'BCTAB   ',F'001',F'0005000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCBRD)                                     
         DC    CL8'TRCBUFF ',F'000',F'0500000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(STRC)                                      
         DC    CL8'SCTBUFF ',F'000',F'0500000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(SSCT)                                      
         DC    CL8'TEMPEST ',F'001',F'0500000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(TPST)                                      
*                                                                               
*        These ???dumps parms are no longer needed or honored.                  
         DC    CL8'REPDUMPS',F'001',F'0000050'                                  
         DC    AL1(07,CTNUM,0,0),AL4(NMDMPR)                                    
         DC    CL8'ADVDUMPS',F'001',F'0000050'                                  
         DC    AL1(07,CTNUM,0,0),AL4(NMDMPA)                                    
         DC    CL8'TSTDUMPS',F'001',F'0000100'                                  
         DC    AL1(07,CTNUM,0,0),AL4(NMDMPT)                                    
*                                                                               
         DC    CL8'DDISP   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDDSP)                                    
         DC    CL8'DBOOK   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDBOK)                                    
         DC    CL8'DSTATION',F'001',F'0001000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCDSTN)                                    
         DC    CL8'DMASTER ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDMST)                                    
         DC    CL8'DNAME   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDNAM)                                    
         DC    CL8'DCODE   ',F'001',F'0001000'                                  
         DC    AL1(04,CTNUM,0,0),AL4(DCDCDE)                                    
         DC    CL8'DCONTROL',F'001',F'0001000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCDCTL)                                    
         DC    CL8'DADJUST ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDADJ)                                    
         DC    CL8'DFORMULA',F'001',F'0001000'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCDFRM)                                    
         DC    CL8'DFMTAB  ',F'001',F'0001000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCDFMTB)                                   
         DC    CL8'ALPHMKT ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCDALF)                                    
         DC    CL8'NADUNV  ',F'001',F'0002000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCDNUN)                                    
         DC    CL8'SOONCLS ',F'001',F'0001000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCCLASS)                                   
         DC    CL8'SOON    ',F'001',F'0001000'                                  
         DC    AL1(03,CTNUM,0,0),AL4(DCSOON)                                    
         DC    CL8'MONS    ',F'001',F'0001000'                                  
         DC    AL1(03,CTNUM,0,0),AL4(DCMONS)                                    
         DC    CL8'MKTNAME ',F'000',F'0000400'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCMKTN)                                    
         DC    CL8'NEWDFORM',F'000',F'0000100'                                  
         DC    AL1(07,CTNUM,0,0),AL4(DCNFRM)                                    
         DC    CL8'RENTRAK ',F'000',F'0040000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCRNTRK)                                   
         DC    CL8'GETRET  ',F'000',F'0010000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCGETRET)                                  
         DC    CL8'RENTNET ',F'000',F'0010000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCRNTNE)                                   
         DC    CL8'COMJOBS ',F'16387',F'0065539'                                
         DC    AL1(06,CTNUM,0,0),AL4(DCCOMJOB)                                  
         DC    CL8'COMSNAT ',F'000',F'0080000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCCSNAT)                                   
         DC    CL8'COMSLON ',F'000',F'0040000'                                  
         DC    AL1(06,CTNUM,0,0),AL4(DCCSLON)                                   
         DC    CL8'PHSCNT  ',F'000',F'0032000'                                  
         DC    AL1(05,CTNUM,0,0),AL4(DCPHSCNT)                                  
*                                                                               
CARDTABX DC    AL1(CARDEOT)                                                     
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
LITERALS DS    0D                                                               
         LTORG                                                                  
*                                                                               
UPDMAJ   DC    C'TABSDSP'                                                       
UPDMIN   DC    C'TABSDSP'                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'*TST****'                                                    
DMPFILT  DMDA                                                                   
*                                                                               
         DS    0D                                                               
         DC    CL8'*ADV****'                                                    
DMPFILA  DMDA                                                                   
*                                                                               
         DS    0D                                                               
         DC    CL8'*REP****'                                                    
DMPFILR  DMDA                                                                   
*                                                                               
*&&UK                                                                           
         DS    0D                                                               
         DC    CL8'*CSC****'                                                    
DMPFILC  DMDA                                                                   
*                                                                               
         DS    0D                                                               
         DC    CL8'*FQA****'                                                    
DMPFILF  DMDA                                                                   
*                                                                               
         DS    0D                                                               
         DC    CL8'*BAR****'                                                    
DMPFILB  DMDA                                                                   
*&&                                                                             
*                                                                               
DMPCYLS  DC    A(DMPCYLQ)          NUMBER OF CYLINDERS PER DUMP                 
AIOA     DC    A(IOA)                                                           
ASVCARD  DC    A(SVCARD)                                                        
AMESSTAB DC    A(MESSTAB)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VPRNTBL  DC    V(PRNTBL)                                                        
VCARDS   DC    V(CARDS)                                                         
VDDWTO   DC    V(DDWTO)                                                         
VPRINTER DC    V(PRINTER)                                                       
VFACITAB DC    V(FACIDTAB)                                                      
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
ADADDS   DC    V(DADDS)                                                         
*                                                                               
SAVERC   DC    A(0)                                                             
SAVERD   DC    A(0)                                                             
*                                                                               
ARZERO   DC    16F'0'                                                           
RMAXRES  EQU   256                 INCREASE IF MORE THAN 256 TABLES             
K        EQU   1024                                                             
*                                                                               
DCSTATSK DC    A(80)                                                            
RUNRPGES DC    A(0)                                                             
*                                                                               
TABSTST  DC    C'T'                TEST SYSTEM                                  
TABSREP  DC    C'R'                REP  SYSTEM                                  
TABSCSC  DC    C'C'                CSC  SYSTEM                                  
TABSADV  DC    C'A'                ADV  SYSTEM                                  
TABSFQA  DC    C'F'                FQA  SYSTEM                                  
*&&UK                                                                           
TABSBAR  DC    C'B'                                                             
*&&                                                                             
TABSBOTH DC    C'B'                BOTH ADV+REP SYSTEMS                         
*                                                                               
DEFUSR   DC    C'MPTY'             TABLE IS NOT USED                            
TO       DC    C'To'                                                            
*                                                                               
CTITLE   DC    C'Input Card Details'                                            
DTITLE   DC    C'Test results'                                                  
*                   123456789012345678901234567890123456789                     
I1       DS    0CL78' '                                                         
         DC    CL39'The parameter cards listed below come f'                    
         DC    CL39'rom DDS.PARMS(TABSXXX) where the XXX is'                    
I2       DS    0CL78' '                                                         
         DC    CL39'one of TST,REP or ADV depending on the '                    
         DC    CL39'name of the corresponding jobstep.     '                    
I3       DS    0CL78' '                                                         
         DC    CL39'The dataset OLDPARMS (see the input JCL'                    
         DC    CL39' for the DSN) holds the parameters reco'                    
I4       DS    0CL78' '                                                         
         DC    CL39'rded for the last successful initialisa'                    
         DC    CL39'tion.                                  '                    
*                                                                               
* FATABSDEQU                                                                    
*                                                                               
       ++INCLUDE FATABSDEQU                                                     
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
OLDPARMS DCB   DSORG=PS,MACRF=(GM,PM),RECFM=FB,LRECL=80,BLKSIZE=400,   +        
               DDNAME=OLDPARMS                                                  
*                                                                               
AOPERECB DC    A(0)                                                             
ACOMM    DC    A(0)                                                             
*                                                                               
MODE     DC    CL10'INIT'                                                       
RUN      DC    CL4'DSP '                                                        
FPROT    DC    AL1(YES)            FETCH PROTECTION FOR DATASPACE               
PKEY     DC    C'8'                PROTECTION KEY                               
KILL     DC    CL4'N   '                                                        
TEST     DC    AL1(NO)                                                          
DSPACE   DC    CL12' '                                                          
SYSTEM   DC    CL4' '                                                           
DCY2K    DC    CL1'N'                                                           
*                                                                               
NLOCKS   DC    F'5'                NUMBER OF LOCKS IN DSPACE LOCKTAB            
SSCT     DC    F'0'                SIZE OF SCRIPT BUFFER                        
TPST     DC    F'1000'             NUMBER OF ENTRIES IN TEMPEST PAGE            
TPSTF    DC    F'0'                SIZE OF BLOCK FOR TEMPEST PAGE               
STRC     DC    F'0'                SIZE OF TRACE BUFFER                         
PTSTDA   DC    F'0'                TSTTAB, TRACKS PER CI                        
#SERVERS DC    F'32'               Minimum number of servers                    
#QENTRES DC    F'100'              Minimum number of queue entries              
*                                                                               
*********************************************************************           
*THESE DUMPS# VARIABLES ARE NO LONGER NEEDED OR HONORED.                        
*********************************************************************           
NMDMPR   DC    F'0'                NUMBER OF DUMPS PER SYSTEM ADV               
NMDMPA   DC    F'0'                NUMBER OF DUMPS PER SYSTEM REP               
NMDMPT   DC    F'0'                NUMBER OF DUMPS PER SYSTEM TST               
CTDMPR   DC    F'0'                                                             
CTDMPA   DC    F'0'                                                             
CTDMPT   DC    F'0'                                                             
         EJECT                                                                  
***********************************************************************         
* DATASPACE HEADER TABLE ENTRY DETAILS                                *         
***********************************************************************         
         ORG   TABSDSP+((((*-TABSDSP)/16)+1)*16)                                
HDRTAB   DS    0XL16                                                            
*                                                                               
         DC    CL8'CTBUFFER'   *** CTBUFFER TABLE                               
DCCTB    DC    F'01'                                                            
         DC    Y(4096)             WIDTH                                        
         DC    AL1(HDTABLE)        TABLE                                        
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'4 RUNIT '   *** RUNNER DUMMY WAIT TABLE                      
DCRUNIT  DC    F'01'                                                            
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'WRKR FLS'   *** WRKR FILE INDEX TABLE                        
DCWRKR   DC    F'01'                                                            
         DC    Y(4096)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'Locket  '   *** LOCK TABLE                                   
DCLOCK   DC    F'0'                                                             
         DC    Y(UPDTABL)          WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
JOBTAB   DC    CL8'JobTable'   *** JOB TABLE                                    
DCJOB    DC    F'0'                                                             
         DC    Y(64)               WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'PQ BUFFR'   *** PQ BUFFER INTERFACE                          
DCPRTQ   DC    F'02'                                                            
         DC    Y(4096)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'Not Used'   *** TEST TABLE                                   
DCTST    DC    F'0'                                                             
         DC    Y(TSTLNEQ)          WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*                                                                               
         DC    CL8'VrsnTble'   *** VERSION TABLE                                
DCVRSN   DC    F'0'                                                             
         DC    Y(VRSNLEN)          WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'Not Used'   *** BROADCAST TABLE                              
DCBRD    DC    F'0'                                                             
         DC    Y(BCTABL)           WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*                                                                               
         DC    CL8'DareNtfy'   *** DARE NOTIFY                                  
DCDARE   DC    F'0'                                                             
         DC    Y(8)                WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'DareAsst'   *** DARE ASSIST                                  
DCASS    DC    F'0'                                                             
         DC    Y(ASSISTLQ)         WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'Not Used'   *** WAS PROFILES1 - now free                     
DCUSR1   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*                                                                               
*&&UK                                                                           
         DC    CL8'NOT USED'   *** UK - N/D                                     
DCFUSN   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    C'T'                DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-FUSN '   *** US - FUSION DATA                             
DCFUSN   DC    F'100'                                                           
         DC    Y(108)              WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DDISP'   *** US - DDISP                                   
DCDDSP   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDDSP   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DBOOK'   *** US - DBOOK                                   
DCDBOK   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
DADBOK   DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDBOK   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DSTN '   *** DSTATION                                     
DCDSTN   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDSTN   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DMSTR'   *** US - DMASTER                                 
DCDMST   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDMST   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DNAME'   *** US - DNAME                                   
DCDNAM   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
DADNAM   DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDNAM   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DCODE'   *** US - DCODE                                   
DCDCDE   DC    F'02'                                                            
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDCDE   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DCNTL'   *** US - DCONTROL                                
DCDCTL   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDCTL   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DADJS'   *** US - DADJUST                                 
DCDADJ   DC    F'02'                                                            
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDADJ   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DFRM '   *** US - DFORMULA                                
DCDFRM   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDFRM   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-DFMTB'   *** US - DFMTAB                                  
DCDFMTB  DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDFMTB  DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-ALPHM'   *** US - ALPHMKT                                 
DCDALF   DC    F'02'                                                            
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
DADALF   DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDALF   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-NADUN'   *** US - NAD UNIVERSE                            
DCDNUN   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCDNUN   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
         DC    CL8'4 SOONS*'   *** EXTRA AREA FOR SOONS                         
DCSOON   DC    F'1'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*                                                                               
SOONCLS  DC    CL8'SOON CLS'   *** SOON CLASSES                                 
DCCLASS  DC    F'1'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'TRACES**'   *** TRACE AREA                                   
DCTRACE  DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'Not Used'   *** CT5RECS                                      
DC5NUM   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'Not Used'   *** CT0RECS                                      
DC0NUM   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'Not Used'   *** CTFRECS                                      
DCFNUM   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'MONSOON '   *** MONSOON       N                              
DCMONS   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&US                                                                           
         DC    CL8'US-MKTNM'   *** US - MARKET NAMES                            
DCMKTN   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCMKTN   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-NFORM'   *** US - MARKET NAMES                            
DCNFRM   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
*&&UK                                                                           
         DC    CL8'Not Used'   *** UK - Free                                    
DCNFRM   DC    F'0'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    AL1(HDDUMMY)        DUMMY (NOT DEFINED)                          
         DC    A(0)                                                             
*&&                                                                             
*&&US                                                                           
         DC    CL8'US-RNTRK'   *** US - RENTRAK DEMO TABLE                      
DCRNTRK  DC    F'0'                                                             
         DC    Y(RENLNQ)           WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
SVJOBTAB DC    CL8'SV JOBS '   *** SAVE SOONS TAB FOR DEBUGGING                 
DCSVJOBS DC    F'1'                                                             
         DC    Y(64)               WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
SVSOONCL DC    CL8'SV CLASS'   *** SAVE CLASS TAB FOR DEBUGGING                 
DCSVCLAS DC    F'1'                                                             
         DC    Y(1024)             WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'*GETRET*'   *** Holiday TAB FOR GETRET                       
DCGETRET DC    F'1'                                                             
         DC    Y(16*16)            WIDTH 16 long x 16 partitions                
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&US                                                                           
         DC    CL8'US-RNTNE'   *** US - RENTRAK NETWORK TABLE                   
DCRNTNE  DC    F'0'                                                             
         DC    Y(RTKNLNQ)          WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'COMJOBS*'   *** ComScore Batch Jobs table                    
DCCOMJOB DC    F'0'                                                             
         DC    Y(CJBNLNQ)          WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'US-CSNAT'   *** US - COMSCORE NATIONAL DEMOS                 
DCCSNAT  DC    F'0'                                                             
         DC    Y(RENLNQ)           WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
         DC    CL8'US-CSLON'   *** US - COMSCORE LOCAL DEMOS BY NUMBER          
DCCSLON  DC    F'0'                                                             
         DC    Y(CSDLNQ)           WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*&&                                                                             
         DC    CL8'PHASECNT'   *** PHASE LOAD COUNT                             
DCPHSCNT DC    F'0'                                                             
         DC    Y(16)               WIDTH                                        
         DC    AL1(HDTABLE)                                                     
         DC    X'00'                                                            
         DC    A(0)                                                             
*                                                                               
DEND     DC    X'FFFFFFFF'         END OF TABLE                                 
         EJECT                                                                  
***********************************************************************         
* MESSAGES                                                            *         
***********************************************************************         
MESSTAB  DS    0CL60                                                            
  DC CL60'Began reading input parameters from cards                   '         
  DC CL60'Ended reading input parameters from cards                   '         
  DC CL60'Began processing input parameters                           '         
  DC CL60'Ended processing input parameters                           '         
  DC CL60'Began saving parameter cards to OLDPARMS dataset            '         
  DC CL60'Ended saving parameter cards to OLDPARMS dataset            '         
  DC CL60'Card validation error - Application terminating             '         
  DC CL60'Error on open of OLDPARMS dataset - Application terminating '         
  DC CL60'Began setting operator communications                       '         
  DC CL60'Ended setting operator communications                       '         
         EJECT                                                                  
***********************************************************************         
* SSB off-line                                                        *         
***********************************************************************         
         DS    0Q                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DS    0F                                                               
         ORG   SSB                                                              
       ++INCLUDE FASSBOFF                                                       
         ORG   SSOXTND                                                          
         DC    X'FF'                                                            
         ORG   SSOMASTC                                                         
         DC    V(MASTC)                                                         
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* SAVE AREAS AND BUFFERS                                              *         
***********************************************************************         
IOA      DS    (4*K)C                                                           
*                                                                               
SVCARD   DC    100CL80' '                                                       
         EJECT                                                                  
***********************************************************************         
* WORK AREA                                                           *         
***********************************************************************         
         DS    0D                                                               
WORKAREA DC    60000X'00'                                                       
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
                                                                                
WORKD    DSECT                                                                  
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
NDMPS    DS    F                   NUMBER OF DUMPS PER SYSTEM                   
RDMPS    DS    F                   REMAINDER                                    
ADUMMY   DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
*                                                                               
CARDEND  DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
GETLEN   DS    F                   LENGTH RETURNED IF GETMAIN                   
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
WAITER   DS    A                                                                
*                                                                               
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
*                                                                               
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
*                                                                               
PLINE    DS    CL166                                                            
*                                                                               
DMPBLKT  DS    XL(DMPINFOL)                                                     
DMPBLKA  DS    XL(DMPINFOL)                                                     
DMPBLKR  DS    XL(DMPINFOL)                                                     
DMPBLKC  DS    XL(DMPINFOL)                                                     
DMPBLKF  DS    XL(DMPINFOL)                                                     
*                                                                               
CARD     DS    CL80                                                             
OLDCARD  DS    CL80                                                             
ERRCNT   DS    H                                                                
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
***********************************************************************         
* DSECT TO COVER DATASPACE HEADERS                                    *         
***********************************************************************         
HDRTABD  DSECT                                                                  
HDRNAME  DS    CL8                 IDENTIFIER IN DATASPACE                      
HDRNUM   DS    F                   NUMBER OF ENTRIES (FROM INPUT PARMS)         
HDRWIDTH DS    H                   WIDTH OF A SINGLE ENTRY                      
HDRTYPE  DS    X                   TYPE (USUALLY TABLE)                         
HDTABLE  EQU   C'T'                                                             
HDRUSED  DS    X                   ENTRY IS REALLY IN USE                       
HDDUMMY  EQU   C'D'                TABLE IS NOT USED YET                        
HDRADR   DS    A                   A(ENTRY IN DATASPACE)                        
HDRTABL  EQU   *-HDRTABD                                                        
*                                                                               
***********************************************************************         
* DSECT TO COVER DUMP INFO                                            *         
***********************************************************************         
DMPINFOD DSECT                                                                  
DRECTRK  DS    F                                                                
DTRKCYL  DS    F                                                                
DTOTTRKS DS    F                                                                
DTOTCYLS DS    F                                                                
DNUMTRKS DS    F                                                                
DNUMCYLS DS    F                                                                
DNUMDMPS DS    F                                                                
DNUMSYS  DS    F                                                                
DMPINFOL EQU   *-DMPINFOD                                                       
                                                                                
***********************************************************************         
* Working storage DSECT                                                         
***********************************************************************         
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
*                                                                               
       ++INCLUDE DDLOCKTLK                                                      
SWORKX   DS    0C                                                               
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
                                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* FATABSDMP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDMP                                                      
         PRINT ON                                                               
* FATABSJOB                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSJOB                                                      
         PRINT ON                                                               
* FATABSTMS                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSTMS                                                      
         PRINT ON                                                               
* FATABSCOM                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSCOM                                                      
         PRINT ON                                                               
* FATABSRUN                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSRUN                                                      
         PRINT ON                                                               
* FADMPHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADMPHDR                                                       
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FASCTTAB                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASCTTAB                                                       
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FAPRQ                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAPRQ                                                          
         PRINT ON                                                               
* FAUPDTAB                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAUPDTAB                                                       
         PRINT ON                                                               
* DDASSISTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDASSISTD                                                      
         PRINT ON                                                               
* FADARETABD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDAR                                                      
         PRINT ON                                                               
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
* DDMASTC                          (Need for SCANNER)                           
         PRINT OFF                                                              
       ++INCLUDE DDMASTC                                                        
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* DMXTNTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMXTNTD                                                        
         PRINT ON                                                               
* FATSTTAB                                                                      
         PRINT OFF                                                              
       ++INCLUDE FATSTTAB                                                       
TSTLNEQ  EQU   *-TSTTABD                                                        
         PRINT ON                                                               
*&&US                                                                           
* DERENTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DERENTABD                                                      
         PRINT ON                                                               
* FATABCBJ                                                                      
         PRINT OFF                                                              
       ++INCLUDE FATABCBJ                                                       
         PRINT ON                                                               
*&&                                                                             
* FABCTAB                                                                       
         PRINT OFF                                                              
       ++INCLUDE FABCTAB                                                        
         PRINT ON                                                               
* FAVRSNTAB                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAVRSNTAB                                                      
         PRINT ON                                                               
* FATABOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FATABOFF                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016FATABSDSP 01/07/20'                                      
         END                                                                    
         EJECT                                                                  
