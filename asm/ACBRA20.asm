*          DATA SET ACBRA20    AT LEVEL 055 AS OF 05/17/20                      
*PHASE T62420A                                                                  
                                                                                
ACBRA20  TITLE '- BRA General download Server 2'                                
                                                                                
***********************************************************************         
* Level change comments                                                         
* ---------------------                                                         
* UK Levels                                                                     
* ---------                                                                     
* TKLU 001 05MAY06 New version as extension to ACMCS11                          
* TKLU 002 16MAY06 <DU01-5456> Report format download                           
* TKLU 003 29MAY06 Currency and Office d/load from ACMCS11                      
* TKLU 004 08SEP06 Quickreports LimList and General Initial downloads           
* NSHE 005 19SEP06 Quickreports download for scribe formats                     
* TKLU 007 27SEP06 Preparations for MCS -> BRA                                  
* TKLU 008 10OCT06 MERGE IN US CHANGES FROM JIM SHEA                            
* TKLU 009 20OCT06 Con/Sec Person Search download                               
* TKLU 010 23OCT06 FINAL RENAME FROM MCS TO BRA                                 
* NSHE 011 24OCT06 find out whether approver initial call                       
* TKLU 012 20DEC06 <DU01-5868> currency call type to incl agy curr              
* TKLU 013 08JAN07 US Merger                                                    
* TKLU 014 13FEB07 <LO01-5992> new QuickReports transmission type               
* TKLU 015 18APR07 <UKCR00012322> show office if 2D account is locked           
* TKLU 015 10MAY07 <LO01-6412> Format record office set up                      
* NSHE 016 08NOV07 <LO01-6995> Set FACPAK code for analysis                     
* TKLU     14NOV07 <DU01-5915> Suspense a/c call for internal orders            
* TKLU 019 11DEC07 US merger (JSHA, comments only)                              
* TKLU 020 28JAN08 <DU01-6624> 'LimitList/GroupList' indicator returned         
* TKLU 021 04MAR08 <LO01-7220> New 'Send mail for password' request             
* NSHE 023 30APR08 change currency download                                     
* TKLU     08MAY08 <LO01-7220> Make 'Send mail for password' available          
*                  for testing on account 0 and X only and add some             
*                  extra validation                                             
* TKLU     09JUN08 Foreign name in suspense a/c and office d/load               
* TKLU     17JUN08 Offal parameter bug fix                                      
* TKLU 024 04JUL08 Preparations to make SENDMAIL live on ADV/CSC                
* NSHE 029 03JUL08 UKCR00017884 Add Japanese Yen to currency d/load             
* NSHE 032 25AUG08 Change of routines                                           
* TKLU 033 16SEP08 <UKCR00018678> Suspense A/C look up UK fix                   
* NSHE 034 24OCT08 remove pidrec for limit list and approver                    
* JFOS 035 20MAR09 <LO01-7636> Support 13B VAT for Germany                      
* NSHE 037 03AUG09 Change to limit list structure                               
* SMAN 038 26JAN10 <BR30209L> Read next GrpList rec as necessary                
* NSHE 039 03FEB10 Show suspense account name                                   
* JFOS 040 29MAR10 Support Old Vat                                              
* JFOS 041 01SEP10 <UKCR029202> fix single 13b Vat code bug                     
* NSHE 043 03JAN11 Add validation for VAT regions                               
* NSHE 045 19OCT11 Remove obsolete code regarding person search                 
* MPEN 046 30MAY14 <DSRD-2340> Forgot password email new aura format            
* NSHE 047 28AUG14 <DSRD-4096> Check for security violation                     
* NSHE 048 22SEP14 <DSRD-4433> Update NY address and instagram link             
* MPEN 049 14JAN15 <DSRD-5722> Update emails for Germany                        
* NSHE 050 24Apr15 <DSBO-1406> Remove SETFAC                                    
* NSHE 051 11May15 US fixes passed by Jim Shea                                  
* NSHE 052 10Jul15 <DSSUP-4764> Ensure emails not sent out for CSC              
* MPEN 053 11Oct16 <DSRD-13630> Update email image source urls                  
* MPEN 054 29Nov16 <DSRD-14212> Province filter for Canadian tax list           
* VGUP 055 18May20 <DSRD-26302> Changed NY office address in email              
*                                                                               
* US Levels                                                                     
* ---------                                                                     
* JSHA 005 28Sep07 All UK Levels up to 15                                       
* JSHA 006 29Nov07 US/UK Merge for level 16                                     
*                                                                               
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           x        
               SLOWLIST=SLOWS,FACS=FACS,WORKERKEY=ACBO,ABENDLIST=FAILS,x        
               SYSPHASE=X'0624',SYSTEM=ACCSYSQ,APPEND=Y,               x        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             x        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#TWA,TWAD)                  
                                                                                
SLOWS    DC    C':'                                                             
FAILS    DC    C':'                                                             
                                                                                
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO20**,CLEAR=YES,RR=RE                                       
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R1                                                      
         XR    R4,R4                                                            
         ICM   R4,7,RUNPARUN                                                    
         USING RUNFACSD,R4         R4=A(RUNFACS)                                
         LARL  RA,GLOBALS          RA=A(ON/OFFLINE TWA)                         
         USING GLOBALS,RA                                                       
         DROP  R1                                                               
                                                                                
         LHI   R0,LVALUESL                                                      
         CHI   R0,OVALUESL                                                      
         BE    *+6                                                              
         DC    H'0'                LITERALS OUT OF STEP                         
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   INIT02                                                           
         ICM   R9,15,LP_ABLK1      ROOT SETS A(WORKD)                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         B     INIT03                                                           
*                                                                               
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         XR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
INIT03   ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         LR    RC,R8                                                            
         AHI   RC,4096                                                          
         USING SAVED+4096,RC       RC=A(2ND 4K OF W/S)                          
         MVC   ATWA,LP_ATWA        OFFLINE TWA AREA SET BY RUNNER               
*                                                                               
         USING TWAD,R7              R7=A(TWA)                                   
INIT04   L     R7,ATWA                                                          
         MVI   TWAMODE,0                                                        
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
*                                                                               
         MVC   VSMTP,RSMTP         SAVE OFF A(SMTP)                             
*                                                                               
         L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R1                                                      
         CLI   RUNPMODE,RRUNSTRQ   FIRST FOR RUN                                
         BE    RUNSTR                                                           
         CLI   RUNPMODE,RPRCWRKQ   PROCESS WORK                                 
         BE    PRCWRK                                                           
         CLI   RUNPMODE,RRUNREQQ   RUN REQUEST                                  
         BE    RUNREQ                                                           
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   ICM   RF,15,RMASTC        TEST RUNNING OFFLINE                         
         BZ    RUNSTR02                                                         
                                                                                
         L     RF,RCOMFACS         YES - LOAD FACILITIES OVERLAYS               
         ST    RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
                                                                                
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
RUNSTR02 MVC   OVALUES(OVALUESL),LVALUES                                        
                                                                                
         LA    R0,SAVED                                                         
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)              BLOCK #2         
                                                                                
         OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
                                                                                
         GOTOR VDATCON,DMCB,(5,0),(20,X#TODL)                                   
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   XC    REQVALS(REQVALLQ),REQVALS                                        
                                                                                
         LA    R0,RQ_DATA          CLEAR REQUEST VALUES                         
         LA    R1,RQMAXQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         ICM   RF,15,RMASTC        TEST RUNNING OFFLINE                         
         BZ    PRCWRK02                                                         
                                                                                
         LA    R0,WORKD            CLEAR I/O AREAS                              
         AHI   R0,IOAREA1-WORKD                                                 
         LHI   R1,IOAREASL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
PRCWRK02 DS    0H                                                               
                                                                                
         J     EXITY                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   MVI   GIND2,GI2EBUY       set to eBuyer (although for all)             
                                                                                
         GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
         GOTO1 VDICTATE,DMCB,C'LL  ',DATI,DATO                                  
                                                                                
         XR    RF,RF                                                            
         ICM   RF,3,LP_QMAPN                                                    
                                                                                
                                                                                
         CHI   RF,A#VCDL           *** VAT code list download                   
         BE    VATCDL                                                           
         CHI   RF,A#QRLL           *** QuickReports Limit List                  
         BE    QRLLDL                                                           
         CHI   RF,A#SMPW           *** Send mail for password                   
         BE    SMPWDL                                                           
         DC    H'0'                UNKNOWN REQUEST                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* VAT code list download                                              *         
***********************************************************************         
                                                                                
         USING LW_D,RF                                                          
VATCDL   DS    0H                                                               
                                                                                
         MVC   X#DATE,TODAYP                                                    
                                                                                
         CLC   RQ_VCDAT,SPACES                                                  
         BNH   VATCDL05                                                         
                                                                                
         GOTOR VDATCON,DMCB,(0,RQ_VCDAT+2),(1,X#DATE)                           
                                                                                
VATCDL05 MVC   X#OFFC,RQ_VCOFF                                                  
         OC    X#OFFC,SPACES                                                    
                                                                                
         MVI   BYTE1,TAXIELQ                                                    
         CLI   RQ_VCOUT,YESQ                                                    
*&&UK*&& BNE   VATCDL10                                                         
*&&US*&& BNE   VATCDL15                                                         
                                                                                
         MVI   BYTE1,TAXOELQ                                                    
         B     VATCDL15                                                         
                                                                                
*&&UK                                                                           
VATCDL10 DS    0H                                                               
         TM    SCPYEL+CPYSTAT5-CPYELD,CPYSNVAT                                  
         BNZ   VATCDL15                                                         
                                                                                
***      MVC   LP_ERROR,=AL2(AE$INCPY)                                          
***      J     XERROR                                                           
                                                                                
         GOTOR LSTVATO                                                          
         JE    EXITY                                                            
         J     VATCDLER                                                         
*&&                                                                             
                                                                                
VATCDL15 DS    0H                                                               
                                                                                
         GOTOR LSTVAT                                                           
         JE    EXITY                                                            
                                                                                
VATCDLER MVC   LP_ERROR,ROUERRV                                                 
         J     XERROR                                                           
         DROP  RF                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Send mail for password request download                             *         
***********************************************************************         
                                                                                
         USING LW_D,RF                                                          
SMPWDL   DS    0H                                                               
                                                                                
         CLI   RQ_SMTYP,RQ_SMT1Q                                                
         BE    SMPWDL05                                                         
                                                                                
         MVC   LP_ERROR,=AL2(AE$IVTYP)                                          
         J     XERROR                                                           
                                                                                
SMPWDL05 GOTOR SNDMFP                                                           
         JE    EXITY                                                            
                                                                                
         MVC   LP_ERROR,ROUERRV                                                 
         J     XERROR                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* QuickReports LimitList download                                     *         
***********************************************************************         
                                                                                
         USING LW_D,RF                                                          
QRLLDL   DS    0H                                                               
                                                                                
         CLI   RQ_QLTYP,RQ_QLTLQ                                                
         BE    QRLLDL05                                                         
                                                                                
         MVC   LP_ERROR,=AL2(AE$IVTYP)                                          
         J     XERROR                                                           
                                                                                
QRLLDL05 GOTOR GETQRL                                                           
         JE    EXITY                                                            
                                                                                
         MVC   LP_ERROR,ROUERRV                                                 
         J     XERROR                                                           
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* GENERAL EXIT AND DECLARATIONS                                      *          
**********************************************************************          
                                                                                
EXITN    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITNX                                                           
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITNX   LTR   RB,RB                                                            
         J     EXIT                                                             
EXITY    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITYX                                                           
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITYX   CR    RE,RE                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* General ERROR Exit                                                  *         
***********************************************************************         
                                                                                
XERROR   DS    0H                                                               
         MVI   LP_RMODE,LP_RERRR                                                
         MVI   LP_EMSYS,6                                                       
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* LOCAL ROUTINES                                                      *         
***********************************************************************         
* Get VAT code list                                                             
LSTVAT   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*LSTVAT*'                                                      
                                                                                
         XC    TEMP(6),TEMP                                                     
         XC    X#VATRG,X#VATRG                                                  
*&&UK                                                                           
         CLC   RQ_VCSUP,SPACES     TEST SUPPLIER CODE SET                       
         BNH   LSTVAT14                                                         
         OC    RQ_VCSUP,SPACES                                                  
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES       LOOK UP POSS. 13B VAT CODES ON SUPP          
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,RQ_VCSUP                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BNE   LSTVAT14                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         AHI   RF,ACTRFST-ACTRECD                                               
         USING FFTELD,RF                                                        
LSTVAT02 CLI   FFTEL,0                                                          
         BE    LSTVAT14                                                         
         CLI   FFTEL,ASTELQ                                                     
         BE    LSTVAT12                                                         
         CLI   FFTEL,FFTELQ                                                     
         BE    LSTVAT06                                                         
                                                                                
LSTVAT04 IC    R0,FFTLN                                                         
         AR    RF,R0                                                            
         B     LSTVAT02                                                         
                                                                                
LSTVAT06 CLI   FFTTYPE,FFTTG13B                                                 
         BNE   LSTVAT08                                                         
                                                                                
         LLC   RE,FFTDLEN                                                       
         AHI   RE,-1                                                            
         MVC   TEMP+2(0),FFTDATA                                                
         EX    RE,*-6                                                           
         AHI   RE,1+1                                                           
         STC   RE,TEMP             STORE TOTAL N'CODES HERE                     
         STC   RE,BYTE2            INIT N'LEFT TO MATCH                         
         B     LSTVAT04                                                         
                                                                                
LSTVAT08 CLI   FFTTYPE,FFTTAXLO                                                 
         BNE   LSTVAT04                                                         
         MVC   X#VATRG,FFTDATA                                                  
         B     LSTVAT04                                                         
                                                                                
         USING ASTELD,RF                                                        
LSTVAT12 CLI   AST13VAT,0          1ST ONE HERE                                 
         BNH   LSTVAT04            NOT SET, SO OTHERS WON'T BE                  
         MVC   TEMP+1(1),AST13VAT                                               
         MVI   TEMP,1                                                           
         MVI   BYTE2,1             INIT N'LEFT TO MATCH                         
         B     LSTVAT04                                                         
*&&                                                                             
         USING TAXRECD,R2                                                       
LSTVAT14 LA    R2,IOKEY            READ FOR CURRENT OFFICE VAT RECORD           
         XC    TAXKEY,TAXKEY                                                    
         MVI   TAXKTYP,TAXKTYPQ                                                 
         MVC   TAXKCPY,CUXCPY                                                   
         MVC   TAXKOFF,X#OFFC                                                   
*&&US                                                                           
         CLC   RQ_PROVC,SPACES     PROVINCE FILTER FOR THE US                   
         JNH   *+10                                                             
         MVC   TAXKPRV,RQ_PROVC                                                 
*&&                                                                             
         XR    RE,RE                                                            
         ICM   RE,7,X#DATE                                                      
         LNR   RE,RE                                                            
         STCM  RE,7,TAXKDATE                                                    
                                                                                
         MVC   CSVKEY1,TAXKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   CSVKEY1(TAXKDATE-TAXRECD),TAXKEY                                 
         BE    LSTVAT16                                                         
                                                                                
         MVC   TAXKEY,CSVKEY1      read for current agency VAT record           
         MVC   TAXKOFF,FFS                                                      
         MVC   CSVKEY1,TAXKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   CSVKEY1(TAXKDATE-TAXRECD),TAXKEY                                 
         BE    LSTVAT16                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$VRNDE)                                           
         B     LSTVATN                                                          
                                                                                
LSTVAT16 DS    0H                  process VAT record                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         USING TAXELD,R3                                                        
         LA    R3,TAXRFST          look for VAT elements                        
                                                                                
LSTVAT18 CLC   TAXEL,BYTE1                                                      
         BE    LSTVAT30                                                         
         CLI   TAXEL,0                                                          
         BE    LSTVATY                                                          
                                                                                
LSTVAT20 LLC   R0,TAXLN                                                         
         AR    R3,R0                                                            
         B     LSTVAT18                                                         
                                                                                
LSTVAT30 CLI   RQ_VCCOD,C' '       any code filter?                             
         BNH   LSTVAT32                                                         
         CLC   RQ_VCCOD,TAXCODE                                                 
         BNE   LSTVAT20                                                         
         B     LSTVAT38                                                         
                                                                                
LSTVAT32 DS    0H                                                               
*&&UK                                                                           
         OC    TEMP(6),TEMP        TEST 13B VATCODES FROM SUPPLIER              
         BZ    LSTVAT38            NO                                           
         CLI   BYTE2,0             N'LEFT TO MATCH                              
         BZ    LSTVATY             FINISHED                                     
         SR    RE,RE                                                            
         IC    RE,TEMP             RE=N'CODES TO MATCH                          
LSTVAT34 LA    RF,TEMP(RE)         RF=A(CODE TO MATCH)                          
         CLC   TAXCODE,0(RF)                                                    
         BE    LSTVAT36                                                         
         AHI   RE,-1                                                            
         BNZ   LSTVAT34            BUMP TO NEXT CODE                            
         B     LSTVAT20            NO MATCH - NEXT TAXEL                        
                                                                                
LSTVAT36 IC    RE,BYTE2                                                         
         AHI   RE,-1                                                            
         STC   RE,BYTE2            DECREMENT N'CODES LEFT                       
*&&                                                                             
LSTVAT38 LA    R0,VC_VALS          clear output area                            
         LHI   R1,VC_LNQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   VC_ACCO,TAXACT      pass VAT account code                        
         MVC   VC_CODE,TAXCODE     pass VAT code                                
         MVC   VC_COLA,TAXTYPE     pass VAT code label                          
         XR    R4,R4               pass VAT rate                                
         ICM   R4,3,TAXRATE                                                     
         CURED (R4),(L'VC_RATE,VC_RATE),0,ZERO=YES,ALIGN=LEFT                   
                                                                                
         MVC   VC_ACNA,SPACES      pass VAT account name                        
         XR    R4,R4                                                            
         IC    R4,TAXLN                                                         
         SHI   R4,TAXLN1Q                                                       
         LTR   R4,R4                                                            
         BNP   LSTVAT90                                                         
         SHI   R4,1                                                             
         MVC   VC_ACNA(0),TAXNAME                                               
         EX    R4,*-6                                                           
*&&UK                                                                           
         XC    X#VATEU,X#VATEU                                                  
         XC    X#VATNEU,X#VATNEU                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES       LOOK UP POSS. 13B VAT CODES ON SUPP          
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,TAXACT                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         AHI   RF,ACTRFST-ACTRECD                                               
         USING FFTELD,RF                                                        
LSTVAT40 CLI   FFTEL,0                                                          
         BE    LSTVAT50                                                         
         CLI   FFTEL,FFTELQ                                                     
         BE    LSTVAT44                                                         
         CLI   FFTEL,SPAELQ                                                     
         BE    LSTVAT46                                                         
                                                                                
LSTVAT42 LLC   R0,FFTLN                                                         
         AR    RF,R0                                                            
         B     LSTVAT40                                                         
                                                                                
LSTVAT44 CLI   FFTTYPE,FFTTAXLO                                                 
         BNE   LSTVAT42                                                         
         CLC   X#VATRG,FFTDATA                                                  
         BNE   LSTVAT20                                                         
         B     LSTVAT90                                                         
                                                                                
         USING SPAELD,RF                                                        
LSTVAT46 CLI   SPATYPE,SPATSGEU                                                 
         BNE   LSTVAT48                                                         
         MVC   X#VATEU,SPAAVAT                                                  
         B     LSTVAT42                                                         
                                                                                
LSTVAT48 CLI   SPATYPE,SPATSGNE                                                 
         BNE   LSTVAT42                                                         
         MVC   X#VATNEU,SPAAVAT                                                 
         B     LSTVAT42                                                         
                                                                                
LSTVAT50 CLC   X#VATNEU,SPACES                                                  
         BNH   LSTVAT58                                                         
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES       LOOK UP POSS. 13B VAT CODES ON SUPP          
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'SG'                              
         MVC   ACTKACT,X#VATNEU                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         AHI   RF,ACTRFST-ACTRECD                                               
         USING FFTELD,RF                                                        
LSTVAT52 CLI   FFTEL,0                                                          
         BE    LSTVAT58                                                         
         CLI   FFTEL,FFTELQ                                                     
         BE    LSTVAT56                                                         
                                                                                
LSTVAT54 LLC   R0,FFTLN                                                         
         AR    RF,R0                                                            
         B     LSTVAT52                                                         
                                                                                
LSTVAT56 CLI   FFTTYPE,FFTTAXLO                                                 
         BNE   LSTVAT54                                                         
         CLC   X#VATRG,FFTDATA                                                  
         BE    LSTVAT90                                                         
                                                                                
LSTVAT58 CLC   X#VATEU,SPACES                                                   
         BNH   LSTVAT66                                                         
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES       LOOK UP POSS. 13B VAT CODES ON SUPP          
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'SG'                              
         MVC   ACTKACT,X#VATEU                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         AHI   RF,ACTRFST-ACTRECD                                               
         USING FFTELD,RF                                                        
LSTVAT60 CLI   FFTEL,0                                                          
         BE    LSTVAT66                                                         
         CLI   FFTEL,FFTELQ                                                     
         BE    LSTVAT64                                                         
                                                                                
LSTVAT62 LLC   R0,FFTLN                                                         
         AR    RF,R0                                                            
         B     LSTVAT60                                                         
                                                                                
LSTVAT64 CLI   FFTTYPE,FFTTAXLO                                                 
         BNE   LSTVAT62                                                         
         CLC   X#VATRG,FFTDATA                                                  
         BE    LSTVAT90                                                         
                                                                                
LSTVAT66 CLC   X#VATRG,SPACES                                                   
         BNH   LSTVAT68                                                         
         CLI   X#VATRG,C'N'        National - treat as spaces                   
         BNE   LSTVAT20                                                         
LSTVAT68 CLC   X#VATEU,SPACES                                                   
         BH    LSTVAT20                                                         
         CLC   X#VATNEU,SPACES                                                  
         BH    LSTVAT20                                                         
*&&                                                                             
LSTVAT90 GOTOR LP_APUTO,LP_D       output data                                  
                                                                                
         B     LSTVAT20            next element                                 
                                                                                
LSTVATY  CR    RB,RB                                                            
         XIT1                                                                   
                                                                                
LSTVATN  LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R2,R3                                                            
                                                                                
*&&UK                                                                           
* Get old VAT code list                                                         
LSTVATO  NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*LSTVAO*'                                                      
                                                                                
         XC    TEMP(6),TEMP                                                     
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         BE    LSTVAON             NOT VALID                                    
                                                                                
         MVC   LDGAUL,=C'SG'                                                    
         GOTOR (#SETLDG,ASETLDG)                                                
         BNE   LSTVAON                                                          
         MVI   BYTE1,0             SET MINIMUM L'LOW-LVL ACCOUNT                
         CLI   LDGAL4,0                                                         
         BNH   *+14                                                             
         MVC   BYTE1,LDGAL3                                                     
         B     LSTVO02                                                          
         CLI   LDGAL3,0                                                         
         BNH   *+14                                                             
         MVC   BYTE1,LDGAL2                                                     
         B     LSTVO02                                                          
         CLI   LDGAL2,0                                                         
         BNH   *+14                                                             
         MVC   BYTE1,LDGAL1                                                     
*                                  ONE-LEVEL LEDGER, SO MIN L'ACCT = 1          
         USING ACTRECD,R2                                                       
LSTVO02  LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'LDGAUL),LDGAUL                                         
         MVI   ACTKACT,X'41'                                                    
         CLC   RQ_VCCOD,SPACES                                                  
         BNH   *+10                                                             
         MVC   ACTKACT,RQ_VCCOD    SET REQUESTED ACCOUNT                        
                                                                                
LSTVO04  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
                                                                                
         CLC   ACTKUNT(L'LDGAUL),LDGAUL   TEST SG                               
         BNE   LSTVAOY                    NO - FINISHED                         
         CLC   RQ_VCCOD,SPACES                                                  
         BNH   *+14                                                             
         CLC   ACTKACT,RQ_VCCOD    TEST REQUESTED ACCOUNT                       
         BNE   LSTVAON                                                          
         CLC   ACTKACT+L'ACTKACT(L'ACTKEY-ACTKEND),SPACES  TEST ACCOUNT         
         BH    LSTVO06                                     NO                   
                                                                                
         SR    RF,RF               TEST LOW-LEVEL ACCOUNT                       
         IC    RF,BYTE1                                                         
         LA    RF,ACTKACT(RF)                                                   
         CLI   0(RF),C' '                                                       
         BH    LSTVO08             YES - PROCESS IT                             
                                                                                
LSTVO06  SR    RF,RF               BUMP KEY TO SKIP TO NEXT ACCOUNT             
         MVC   ACTKACT+L'ACTKACT(L'ACTKEY-ACTKEND),SPACES                       
         IC    RF,ACTKACT+L'ACTKACT-1                                           
         AHI   RF,1                                                             
         STC   RF,ACTKACT+L'ACTKACT-1                                           
         B     LSTVO04                                                          
                                                                                
LSTVO08  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         AHI   R3,ACTRFST-ACTRECD                                               
         USING RSTELD,R3                                                        
LSTVO10  CLI   RSTEL,0                                                          
         BE    LSTVO20                                                          
         CLI   RSTEL,RSTELQ                                                     
         BE    LSTVO14                                                          
         CLI   RSTEL,RATEVATQ                                                   
         BE    LSTVO16                                                          
         CLI   RSTEL,NAMELQ                                                     
         BE    LSTVO18                                                          
LSTVO12  SR    RF,RF                                                            
         IC    RF,RSTLN                                                         
         AR    R3,RF                                                            
         B     LSTVO10                                                          
                                                                                
LSTVO14  LA    RF,BNZQ             FILTER INPUT/OUTPUT VAT ACCOUNT              
         CLI   RQ_VCOUT,YESQ       TEST OUTPUT VAT WANTED                       
         BE    *+8                                                              
         LA    RF,BZQ                                                           
         TM    RSTSTAT1,RSTSIVAT   TEST INPUT VAT                               
         NOP   LSTVO06                                                          
         EX    RF,*-4                                                           
         B     LSTVO12                                                          
                                                                                
         USING RATELD,R3                                                        
LSTVO16  SR    RF,RF               VAT RATE                                     
         ICM   RF,3,RATRATE                                                     
         CURED (RF),(L'VC_RATE,VC_RATE),0,ZERO=YES,ALIGN=LEFT                   
         B     LSTVO12                                                          
                                                                                
         USING NAMELD,R3                                                        
LSTVO18  MVC   VC_ACNA,SPACES      VAT ACCOUNT NAME                             
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         AHI   RF,-(NAMLN1Q+1)                                                  
         MVC   VC_ACNA(0),NAMEREC                                               
         EX    RF,*-6                                                           
         B     LSTVO12                                                          
                                                                                
LSTVO20  MVC   VC_ACCO,ACTKACT     VAT ACCOUNT CODE                             
                                                                                
         GOTOR LP_APUTO,LP_D       OUTPUT DATA                                  
         CLC   RQ_VCCOD,SPACES     TEST SPECIFIC ACCOUNT REQUESTED              
         BNH   LSTVO06             NO - RETURN ALL                              
         B     LSTVAOY             YES - FINISHED                               
                                                                                
LSTVAOY  CR    RB,RB                                                            
         XIT1                                                                   
                                                                                
LSTVAON  LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R2,R3                                                            
*&&                                                                             
                                                                                
* Get Quickreports LimList                                                      
                                                                                
GETQRL   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*GETQRL*'                                                      
                                                                                
         L     R4,AGENAREA         set and init table                           
         MVI   0(R4),FF                                                         
         XR    R3,R3                                                            
         USING LLSRECD,R2                                                       
         LA    R2,IOKEY            Read limit list record                       
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,CCTPID                                                  
         MVC   CSVKEY1,LLSKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BE    GQRL006                                                          
         DC    H'0'                                                             
                                                                                
GQRL004  LA    R2,IOKEY                                                         
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         LHI   RF,(LLSKGRP-LLSKEY)-1                                            
         CLI   LLSKSUB,LLSKSUBQ      Test Limit list record                     
         BE    *+8                                                              
         LHI   RF,(GLSKSEQ-GLSKEY)-1 No - use  Group list length                
         EXCLC RF,LLSKEY,CSVKEY1                                                
         BNE   GQRL100                                                          
                                                                                
GQRL006  CLC   LLSKEY(LLSKGRP-LLSRECD),CSVKEY1                                  
         BNE   GQRL100                                                          
GQRL008  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING LIDELD,R1                                                        
         L     R2,AIO3                                                          
         MVC   CSVKEY1,0(R2)       Save the record key                          
         LA    R1,LLSRFST                                                       
GQRL030  CLI   LIDEL,0                                                          
         BE    GQRL004                                                          
GQRL032  CLI   LIDEL,LIDELQ                                                     
         BE    GQRL036                                                          
*                                                                               
GQRL034  XR    R0,R0                                                            
         IC    R0,LIDLN                                                         
         AR    R1,R0                                                            
         B     GQRL030                                                          
*                                                                               
GQRL036  CLI   LIDTYPE,LIDTSCRB                                                 
         BNE   GQRL034                                                          
                                                                                
GQRL050  XR    RE,RE               POINT TO NEXT ELEMENT                        
         IC    RE,LIDLN                                                         
         LA    RE,LIDELD(RE)                                                    
         XR    RF,RF                                                            
         IC    RF,LIDITLN                                                       
         LA    R6,LIDDATA                                                       
RPT      USING LIDDATA,R6                                                       
                                                                                
GQRL055  CR    R6,RE               End of element?                              
         BNL   GQRL034                                                          
         TM    RPT.LIDLAPPL,LIDLREPT                                            
         BZ    GQRL060                                                          
         MVC   0(L'QR_FRMCD,R4),SPACES                                          
         MVC   0(L'LIDLREP,R4),RPT.LIDLREP                                      
         AHI   R3,1                add up number of entries                     
         LA    R4,L'QR_FRMCD(R4)   point R4 to next free entry                  
         MVI   0(R4),X'FF'                                                      
GQRL060  LA    R6,LIDLLN5Q(R6)     point R6 to next data                        
         B     GQRL055                                                          
         DROP  RPT                                                              
                                                                                
GQRL100  LTR   R3,R3                                                            
         BZ    GETQRLY                                                          
         GOTO1 VXSORT,DMCB,AGENAREA,(R3),L'QR_FRMCD,L'QR_FRMCD,0                
                                                                                
         L     R4,AGENAREA                                                      
GQRL110  CLI   0(R4),FF            IS IT THE END                                
         BE    GETQRLY             YES                                          
         XC    QR_VALS(QR_LNQ),QR_VALS                                          
         LA    R3,IOKEY                                                         
         USING RESRECD,R3                                                       
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ                                                 
         MVI   RESKSUB,RESKSUBQ                                                 
         MVC   RESKCPY,CUXCPY      CONNECTED ID                                 
         MVC   RESKFORM,0(R4)                                                   
         MVC   CSVKEY1,RESKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         BNE   GQRL160             FORMAT DOESN'T EXIST                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO3                                                          
         MVC   QR_FRMCD,RESKFORM                                                
         LA    R3,RESRFST                                                       
         USING RPFELD,R3                                                        
GQRL112  CLI   RPFEL,0                                                          
         BE    GQRL150                                                          
         CLI   RPFEL,RPFELQ                                                     
         BE    GQRL120                                                          
         CLI   RPFEL,NAMELQ                                                     
         BE    GQRL122                                                          
         CLI   RPFEL,DTSELQ                                                     
         BE    GQRL124                                                          
         CLI   RPFEL,STYELQ                                                     
         BE    GQRL126                                                          
GQRL114  XR    R0,R0                                                            
         IC    R0,RPFLN                                                         
         AR    R3,R0                                                            
         B     GQRL112                                                          
*                                                                               
GQRL120  TM    RPFXMIT,RPFXACNT+RPFXQREP                                        
         BZ    GQRL160             skip all but Accent or QuickReports          
         B     GQRL114                                                          
*                                                                               
         USING NAMELD,R3                                                        
GQRL122  XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         MVC   QR_FRMNM(0),NAMEREC                                              
         EX    RF,*-6                                                           
         B     GQRL114                                                          
*                                                                               
         USING DTSELD,R3                                                        
GQRL124  CLI   DTSTYPE,DTSTSCR                                                  
         BNE   GQRL114                                                          
         GOTOR VDATCON,DMCB,(2,DTSDATE),(1,QR_DATE)                             
         B     GQRL114                                                          
*                                                                               
         USING STYELD,R3                                                        
GQRL126  LA    R2,REPTBL                                                        
         USING REPTYPD,R2                                                       
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         LA    RE,1                                                             
*                                                                               
GQRL130  CLI   REPTYLLN,0         EOT                                           
         BNE   *+6                                                              
         DC    H'0'               NO REPORT TYPE                                
         IC    R0,REPTYLLN                                                      
         LR    RF,R2                                                            
         AR    RF,R0              R5 POINTS AT NEXT REC                         
         BCTR  RF,0               R5 -> LAST CODE                               
         LA    R6,REPCODES                                                      
GQRL135  CLC   0(1,R6),STYCODE                                                  
         BE    GQRL140                                                          
         BXLE  R6,RE,GQRL135                                                    
         LR    R2,R6              ON LOOP EXIT R6 -> NEXT REP TYPE              
         B     GQRL130                                                          
*                                                                               
GQRL140  MVC   QR_FRMTY,REPTYPE                                                 
         B     GQRL114                                                          
                                                                                
GQRL150  GOTOR LP_APUTO,LP_D       output data                                  
GQRL160  LA    R4,L'QR_FRMCD(R4)                                                
         B     GQRL110                                                          
                                                                                
GETQRLY  CR    RB,RB                                                            
         XIT1                                                                   
                                                                                
GETQRLN  LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* Send mail for password                                              *         
***********************************************************************         
         SPACE 1                                                                
SNDMFP   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SNDMFP*'                                                      
                                                                                
         OC    CCTPID,CCTPID       This only works on a specific PID            
         JZ    SNDMFPN                                                          
         MVC   TEMP2(L'CCTPID),CCTPID                                           
         GOTOR (#GETPID,AGETPID)                                                
         JNE   SNDMFPN                                                          
*        CLC   TEMP2(8),=CL8'BRANDSEC'                                          
*        BNE   SNDMFPN                                                          
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   SNDMFP00                                                         
         GOTO1 VGETFACT,DMCB,(X'80',X#SYST),F#SSYSNA                            
         J     SNDMFP01                                                         
                                                                                
SNDMFP00 L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R1                                                      
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         USING RUNFACSD,RE         R4=A(RUNFACS)                                
         ICM   RF,15,RMASTC        Set DSPACE setting                           
         ICM   RE,15,MCSSB-MASTD(RF)                                            
         MVC   X#SYST,SSODSPAC-SSOOFF(RE)                                       
         DROP  R1,RE                                                            
                                                                                
SNDMFP01 OC    RQ_SMPER,SPACES                                                  
         MVI   SM_CONFI,NOQ                                                     
         MVC   SM_EMAIL,SPACES                                                  
         MVC   XERRTXT,SPACES                                                   
                                                                                
         CLC   RQ_SMPER,SPACES                                                  
         JNH   SNDMFPX2                                                         
                                                                                
         MVC   TEMP2(8),RQ_SMPER                                                
         GOTOR (#GETPIN,AGETPIN)                                                
         JNE   SNDMFPX2                                                         
                                                                                
         CLC   APPEMAIL,SPACES                                                  
         JNH   SNDMFPX2                                                         
                                                                                
         USING SAPWDD,R2                                                        
         L     R2,AIO1                                                          
         AHI   R2,SAPEDATA-SAPEREC                                              
         XR    R0,R0                                                            
SNDMFP02 CLI   SAPWDEL,SAPWDELQ                                                 
         JE    SNDMFP06                                                         
         CLI   SAPWDEL,SALLOELQ    Log on activity                              
         JE    SNDMFP08                                                         
         CLI   SAPWDEL,0                                                        
         JE    SNDMFP10                                                         
SNDMFP04 IC    R0,SAPWDLN                                                       
         AR    R2,R0                                                            
         J     SNDMFP02                                                         
                                                                                
SNDMFP06 MVC   XERRTXT(10),SAPWDCOD      Save off password                      
         J     SNDMFP04                                                         
         DROP  R2                                                               
                                                                                
         USING SALLOD,R2                                                        
SNDMFP08 CLI   SALLOLEN,SALLOFLG-SALLOD  Is element long enough to              
         JNH   SNDMFP04                   have violation flag                   
         TM    SALLOFLG,SALLOMLC         Had user violated security             
         JNZ   SNDMFPN                   Yes - exit security lockout            
         J     SNDMFP04                                                         
         DROP  R2                                                               
                                                                                
SNDMFP10 CLC   XERRTXT,SPACES            Did we find a password                 
         JNH   SNDMFPN                                                          
         MVI   SM_CONFI,YESQ                                                    
         MVC   SM_EMAIL,APPEMAIL                                                
*                                        Send email now                         
         GOTO1 VSMTP,DMCB,('SMTPASLL',0) Set long lines                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ         app is connected                             
         JNE   SNDMFP12            Set Aura html emails + from address          
         GOTO1 VSMTP,DMCB,('SMTPHTMH',0) SET HTML HEADER                        
         GOTO1 VSMTP,DMCB,('SMTPAFR2',0),(L'AUFROM,AUFROM) Set from add         
*                                                                               
SNDMFP12 MVC   WORK,SPACES                                                      
         MVC   WORK(L'APPEMAIL),APPEMAIL                                        
         CLI   X#SYST,C'C'         *** LCSC SYSTEM ***                          
*        MVC   WORK,SPACES                                                      
*        MVC   WORK(22),=C'THOMAS.KLUTH@DDS.CO.UK'                              
***      MVC   WORK(27),=C'ANNE-MARIE.HEWITT@DDS.CO.UK'                         
***      MVC   WORK(20),=C'GUIDO.GOEWERT@DDS.DE'                                
         JE    SNDMFPN             If LCSC don't send out!!!                    
                                                                                
SNDMFP14 MVC   TEXT1,TEXTSPCS                                                   
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether Brandocean rewrite             
         CHI   RF,XPRODIKQ         app is connected                             
         JE    SNDMFP16                                                         
*                              **  Old style BrandOcean password **             
         LA    R1,TEXT1        **  reminders                     **             
         MVC   0(21,R1),=C'Password reminder for'                               
         MVC   22(8,R1),RQ_SMPER                                                
         GOTO1 VSMTP,DMCB,('SMTPAPRS',WORK),(L'TEXT1,TEXT1)                     
         MVC   TEXT1,TEXTSPCS                                                   
         LA    R1,TEXT1                                                         
         MVC   0(3,R1),=C'>>>'                                                  
         MVC   4(10,R1),XERRTXT                                                 
         MVC   15(3,R1),=C'<<<'                                                 
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
         J     SNDMFPX                                                          
*                               ** New style Aura emails **                     
SNDMFP16 MVC   TEXT1(L'AC@PASSR),AC@PASSR                                       
         GOTO1 VSMTP,DMCB,('SMTPAPRS',WORK),(L'TEXT1,TEXT1)                     
*                                                                               
         NI    AC@EMNL2,X'FF'-X'40'  Undo capitilisation                        
         NI    AC@EMNL5,X'FF'-X'40'                                             
         NI    AC@EMNL7,X'FF'-X'40'                                             
         CLI   CUCTRY,CTRYGER        EMNL8/EMNL9 not start of line              
         JE    SNDMFP1A                                                         
         NI    AC@EMNL8,X'FF'-X'40'                                             
         NI    AC@EMNL9,X'FF'-X'40'                                             
*                                                                               
SNDMFP1A L     RF,AEMALS1                                                       
         MVC   0(L'AC@EMNL1,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL1,RF),AC@EMNL1                                        
*                                                                               
         L     RF,AEMALS2                                                       
         MVC   0(L'AC@EMNL2,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL2,RF),AC@EMNL2                                        
*                                                                               
         L     RF,AEMALS3                                                       
         MVC   0(L'AC@EMNL3,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL3,RF),AC@EMNL3                                        
*                                                                               
         L     RF,AEMALS4                                                       
         MVC   0(L'AC@EMNL4,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL4,RF),AC@EMNL4                                        
*                                                                               
         L     RF,AEMALS5                                                       
         MVC   0(L'AC@EMNL5,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL5,RF),AC@EMNL5                                        
*                                                                               
         L     RF,AEMALS6                                                       
         MVC   0(L'AC@EMNL6,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL6,RF),AC@EMNL6                                        
*                                                                               
         L     RF,AEMALS7                                                       
         MVC   0(L'AC@EMNL7,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL7,RF),AC@EMNL7                                        
*                                                                               
         L     RF,AEMALS8                                                       
         MVC   0(L'AC@EMNL8,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL8,RF),AC@EMNL8                                        
*                                                                               
         L     RF,AEMALS9                                                       
         MVC   0(L'AC@EMNL9,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL9,RF),AC@EMNL9                                        
*                                                                               
         L     RF,AEMALSA                                                       
         MVC   0(L'AC@EMNLA,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNLA,RF),AC@EMNLA                                        
*                                                                               
         L     RF,AEMS610E                                                      
*&&UK                                                                           
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOUK,RF),AC@MOUK                                          
         CLI   CUCTRY,CTRYGBR            UK address                             
         JE    SNDMFP17                                                         
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MODE,RF),AC@MODE                                          
         CLI   CUCTRY,CTRYGER            German address                         
         JE    SNDMFP17                                                         
*&&                                                                             
*&&US                                                                           
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOCA,RF),AC@MOCA                                          
         CLI   CUCTRY,CTRYCAN      Canadian address                             
         JE    SNDMFP17                                                         
*&&                                                                             
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AUADDR,RF),AUADDR Default is US address                      
*                                                                               
SNDMFP17 L     R4,AEMS625E                 Find us here...                      
         MVC   0(L'EMLS625E,R4),TEXTSPCS                                        
         MVC   0(L'AC@FUSAT,R4),AC@FUSAT                                        
*                                                                               
         L     RF,AEMS620E         Put year in copyright                        
         MVC   0(L'EMLS620E,RF),X#TODL                                          
*                                                                               
         L     RF,AEMS285E               Move in message text                   
         MVC   0(80,RF),SPACES                                                  
         MVC   0(L'AC@YPAS,RF),AC@YPAS                                          
         AHI   RF,L'AC@YPAS                                                     
         MVI   0(RF),C':'                                                       
         MVC   2(10,RF),XERRTXT                                                 
*                                                                               
         LA    R2,EMLSUM                                                        
*                                                                               
SNDMFP18 CLI   0(R2),X'FF'               Loop through table outputting          
         JE    SNDMFPX                   HTML                                   
         LLC   RF,0(R2)                                                         
         SHI   RF,2                                                             
         CHI   RF,160                                                           
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R2)                                                   
         EX    RF,0(RE)                                                         
         CLC   TEXT1,TEXTSPCS            Line all spaces?                       
         JNH   SNDMFP20                                                         
*                                                                               
         GOTO1 VSQUASH,DMCB,TEXT1,L'TEXT1                                       
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)    PRINT EMAIL LINE                
*                                                                               
SNDMFP20 LLC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     SNDMFP18                                                         
                                                                                
SNDMFPX  GOTO1 VSMTP,DMCB,('SMTPASND',0) Send email                             
         GOTO1 VSMTP,DMCB,('SMTPAEND',0) DETACH FROM JESMAIL                    
         GOTO1 VSMTP,DMCB,('SMTPAINI',0) INITIALISE JESMAIL                     
SNDMFPX2 GOTOR LP_APUTO,LP_D             output data                            
         J     EXITY                                                            
                                                                                
SNDMFPN  MVC   ROUERRV,=AL2(AE$SECLK)    Security lockout                       
         J     EXITN                                                            
         DROP  R5                                                               
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
                                                                                
FACS     DS    0X                                                               
                                                                                
***********************************************************************         
* REQUEST DEFINITIONS (REQUEST and OUTPUT)                            *         
***********************************************************************         
                                                                                
*** VAT code list Download ********************************************         
                                                                                
REQVCDL  LKREQ H,A#VCDL,OUTVCDL                                                 
                                                                                
Date     LKREQ F,01,(D,B#SAVED,RQ_VCDAT),CHAR,OLEN=L'RQ_VCDAT,         X        
               MAXLEN=L'RQ_VCDAT,TEXT=AC#DATE,COL=*                             
OffC     LKREQ F,02,(D,B#SAVED,RQ_VCOFF),CHAR,OLEN=L'RQ_VCOFF,         X        
               MAXLEN=L'RQ_VCOFF,TEXT=AC#OFFC,COL=*                             
OutR     LKREQ F,03,(D,B#SAVED,RQ_VCOUT),CHAR,OLEN=L'RQ_VCOUT,         X        
               MAXLEN=L'RQ_VCOUT,TEXT=AC#VATOP,COL=*                            
Fltr     LKREQ F,04,(D,B#SAVED,RQ_VCCOD),CHAR,OLEN=L'RQ_VCCOD,         X        
               MAXLEN=L'RQ_VCCOD,TEXT=AC#FLT,COL=*                              
Splr     LKREQ F,05,(D,B#SAVED,RQ_VCSUP),CHAR,OLEN=L'RQ_VCSUP,         X        
               MAXLEN=L'RQ_VCSUP,TEXT=AC#SUP,COL=*                              
*&&US                                                                           
Prov     LKREQ F,06,(D,B#SAVED,RQ_PROVC),CHAR,OLEN=L'RQ_PROVC,         X        
               MAXLEN=L'RQ_PROVC,TEXT=AC#PRVD,COL=*                             
*&&                                                                             
         LKREQ E                                                                
                                                                                
                                                                                
*** QuickReports LimitList download ***********************************         
                                                                                
REQQRLL  LKREQ H,A#QRLL,OUTQRLL                                                 
                                                                                
Type     LKREQ F,01,(D,B#SAVED,RQ_QLTYP),CHAR,OLEN=L'RQ_QLTYP,         X        
               MAXLEN=L'RQ_QLTYP,TEXT=AC#TYPE,COL=*                             
                                                                                
         LKREQ E                                                                
                                                                                
*** Send mail for password download ***********************************         
                                                                                
REQSMPW  LKREQ H,A#SMPW,OUTSMPW                                                 
                                                                                
Type     LKREQ F,01,(D,B#SAVED,RQ_SMTYP),CHAR,OLEN=L'RQ_SMTYP,         X        
               MAXLEN=L'RQ_SMTYP,TEXT=AC#TYPE,COL=*                             
Person   LKREQ F,02,(D,B#SAVED,RQ_SMPER),CHAR,OLEN=L'RQ_SMPER,         X        
               MAXLEN=L'RQ_SMPER,TEXT=AC#PRSN,COL=*                             
                                                                                
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
                                                                                
*** VAT code list Download ********************************************         
                                                                                
OUTVCDL  LKOUT H                                                                
OUTVCDLS LKOUT R,R#VCDL                                                         
                                                                                
AccC     LKOUT C,01,(D,B#SAVED,VC_ACCO),CHAR,ND=Y                               
AccN     LKOUT C,02,(D,B#SAVED,VC_ACNA),CHAR,ND=Y                               
Code     LKOUT C,03,(D,B#SAVED,VC_CODE),CHAR,ND=Y                               
Rate     LKOUT C,04,(D,B#SAVED,VC_RATE),CHAR,ND=Y                               
Labl     LKOUT C,05,(D,B#SAVED,VC_COLA),CHAR,ND=Y                               
                                                                                
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
*** scribe format list download  **************************************         
                                                                                
OUTQRLL  LKOUT H                                                                
OUTQRLLS LKOUT R,R#QRLL                                                         
                                                                                
Code     LKOUT C,01,(D,B#SAVED,QR_FRMCD),CHAR,ND=Y                              
Name     LKOUT C,02,(D,B#SAVED,QR_FRMNM),CHAR,ND=Y                              
Type     LKOUT C,03,(D,B#SAVED,QR_FRMTY),CHAR,ND=Y                              
Date     LKOUT C,04,(D,B#SAVED,QR_DATE),PDAT,ND=Y                               
                                                                                
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
*** return data for send mail for password ****************************         
                                                                                
OUTSMPW  LKOUT H                                                                
OUTSMPWS LKOUT R,R#SMPW                                                         
                                                                                
Conf     LKOUT C,01,(D,B#SAVED,SM_CONFI),CHAR,ND=Y                              
EMail    LKOUT C,02,(D,B#SAVED,SM_EMAIL),CHAR,ND=Y                              
                                                                                
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
GLOBALS  DS    0D                                                               
         LTORG                                                                  
         SPACE 1                                                                
LVALUES  DS    0D                                                               
         DC    CL8'DMKEY'                                                       
         DC    CL8'ACCDIR'                                                      
         DC    CL8'ACCMST'                                                      
         DC    CL8'ACCARC'                                                      
         DC    PL1'0'                                                           
         DC    XL4'FFFFFFFF'                                                    
LVALUESL EQU   *-LVALUES                                                        
*                                                                               
AUADDR   DC    C'MEDIAOCEAN | 120 BROADWAY | NEW YORK, NY 10271'                
*                                                                               
WORKLEN  DC    AL2(WORKL)                                                       
*                                                                               
DATI     DS    0X                                                               
         DCDDL AC#YPAS,13          YOUR PASSWORD                                
         DCDDL AC#PASSR,18         PASSWORD REMINDER                            
         DCDDL AC#EMNL1,80         STANDARD EMAIL LINE 1                        
         DCDDL AC#EMNL2,80         STANDARD EMAIL LINE 2                        
         DCDDL AC#EMNL3,80         STANDARD EMAIL LINE 3                        
         DCDDL AC#EMNL4,80         STANDARD EMAIL LINE 4                        
         DCDDL AC#EMNL5,80         STANDARD EMAIL LINE 5                        
         DCDDL AC#EMNL6,80         STANDARD EMAIL LINE 6                        
         DCDDL AC#EMNL7,80         STANDARD EMAIL LINE 7                        
         DCDDL AC#EMNL8,80         STANDARD EMAIL LINE 8                        
         DCDDL AC#EMNL9,80         STANDARD EMAIL LINE 9                        
         DCDDL AC#EMNLA,80         STANDARD EMAIL LINE 9                        
         DCDDL AC#MODE,80          MEDIAOCEAN DE                                
*&&UK*&& DCDDL AC#MOUK,80          MEDIAOCEAN UK                                
*&&US*&& DCDDL AC#MOCA,80          MEDIAOCEAN CA                                
         DCDDL AC#FUSAT,18         FIND US AT                                   
                                                                                
DATIX    DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* LIST OF ACCOUNT FILES TO OPEN IN ALL SYSTEMS                        *         
***********************************************************************         
         SPACE 1                                                                
FILES    DS    0X                  ** FILE INFO **                              
         DC    C'ACCOUNT'          SYSTEM NAME FOR OPEN                         
         DC    C'nCTFILE '         FILE LIST                                    
         DC    C'UACCDIR '                                                      
         DC    C'UACCMST '                                                      
         DC    C'UACCARC '                                                      
         DC    C'X'                                                             
*                                                                               
AUFROM   DC    C'AuraNotifications<AuraNotifications@mediaocean.com>'           
*                                   ADDRESSES IN THE HTML EMAIL                 
         DS    0H                                                               
AEMS285E DC    A(EMLS285E)                                                      
AEMS610E DC    A(EMLS610E)                                                      
AEMS620E DC    A(EMLS620E)                                                      
AEMS625E DC    A(EMLS625E)                                                      
AEMALS1  DC    A(EMAILS1)                                                       
AEMALS2  DC    A(EMAILS2)                                                       
AEMALS3  DC    A(EMAILS3)                                                       
AEMALS4  DC    A(EMAILS4)                                                       
AEMALS5  DC    A(EMAILS5)                                                       
AEMALS6  DC    A(EMAILS6)                                                       
AEMALS7  DC    A(EMAILS7)                                                       
AEMALS8  DC    A(EMAILS8)                                                       
AEMALS9  DC    A(EMAILS9)                                                       
AEMALSA  DC    A(EMAILSA)                                                       
*                                                                               
TEXTSPCS DC    CL160' '             SPACED FILLED AREA                          
*                                                                               
REPTBL   DC    0H                                                               
REPRCV   DC    AL1(REPRCVLN)        LENGTH OF REC    - DEBOTRS                  
         DC    AL1(QR_DRQ)          DDICT ENTRY                                 
REPLN    EQU   *-REPTBL             LENGTH OF RECORD WITHOUT CODES              
         DC    AL1(REP#RCV,REP#ADV,REP#BAL)   CODE THAT GO WITH DDICT           
REPRCVLN EQU   *-REPRCV                                                         
*                                                                               
REPINC   DC    AL1(REPINCLN)                    - INCOME                        
         DC    AL1(QR_INCQ)                                                     
         DC    AL1(REP#INC,REP#SUP,REP#ICST)                                    
REPINCLN EQU   *-REPINC                                                         
*                                                                               
REPPAY   DC    AL1(REPPAYLN)                     - CREDITORS                    
         DC    AL1(QR_CRQ)                                                      
         DC    AL1(REP#PAY,REP#PAYQ,REP#PAYS,REP#PAYT,REP#PAYU)                 
         DC    AL1(REP#PAYV,REP#PAYW,REP#PAYX,REP#PAYY,REP#PAYC)                
         DC    AL1(REP#PAYF)                                                    
REPPAYLN EQU   *-REPPAY                                                         
*                                                                               
REPEXP   DC    AL1(REPEXPLN)                     - EXPENSES                     
         DC    AL1(QR_EXPQ)                                                     
         DC    AL1(REP#EXP,REP#EXPF,REP#EXPL,REP#EXPD,REP#EXPB)                 
         DC    AL1(REP#EXPP)                                                    
REPEXPLN EQU   *-REPEXP                                                         
*                                                                               
REPPRO   DC    AL1(REPPROLN)                                                    
         DC    AL1(QR_PRODQ)                                                    
         DC    AL1(REP#PROD)                                                    
REPPROLN EQU   *-REPPRO                                                         
*                                                                               
REPCST   DC    AL1(REPCSTLN)                                                    
         DC    AL1(QR_MANQ)                                                     
         DC    AL1(REP#CST)                                                     
REPCSTLN EQU   *-REPCST                                                         
*                                                                               
REPCSH   DC    AL1(REPCSHLN)                                                    
         DC    AL1(QR_CASQ)                                                     
         DC    AL1(REP#CASH)                                                    
REPCSHLN EQU   *-REPCSH                                                         
*                                                                               
REPPNL   DC    AL1(REPPNLLN)                                                    
         DC    AL1(QR_PNLQ)                                                     
         DC    AL1(REP#PNL)                                                     
REPPNLLN EQU   *-REPPNL                                                         
*                                                                               
REPGLG   DC    AL1(REPGLGLN)                                                    
         DC    AL1(QR_FINQ)                                                     
         DC    AL1(REP#GNL,REP#GNLP)                                            
REPGLGLN EQU   *-REPGLG                                                         
*                                                                               
REPMED   DC    AL1(REPMEDLN)                                                    
         DC    AL1(QR_MEDQ)                                                     
         DC    AL1(REP#MEDA)                                                    
REPMEDLN EQU   *-REPMED                                                         
*                                                                               
REPFI    DC    AL1(REPFILN)                                                     
         DC    AL1(QR_FINQ)                                                     
         DC    AL1(REP#FI)                                                      
REPFILN  EQU   *-REPFI                                                          
*                                                                               
REPMAN   DC    AL1(REPMANLN)                                                    
         DC    AL1(QR_MANQ)                                                     
         DC    AL1(REP#M2)                                                      
REPMANLN EQU   *-REPMAN                                                         
*                                                                               
         DC    X'00'                                                            
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT                                                                  
VSMTP    DS    A                   A(SMTP)                                      
*                                                                               
REQVALS  DS    0A                  REQUEST VALUES                               
REQIND   DS    XL1                                                              
REQAREQ  DS    AL3                                                              
REQVALLQ EQU   *-REQVALS                                                        
*                                                                               
OVALUES  DS    0D                                                               
DMKEY    DS    CL8                                                              
ACCDIR   DS    CL8                                                              
ACCMST   DS    CL8                                                              
ACCARC   DS    CL8                                                              
PZERO    DS    PL1                                                              
FFS      DS    XL4                                                              
OVALUESL EQU   *-OVALUES                                                        
*                                                                               
XLOCAL   DS    0X                  Local storage                                
                                                                                
X#DATE   DS    PL3                                                              
X#TODL   DS    CL8                                                              
X#OFFC   DS    CL2                                                              
X#SYST   DS    CL3                                                              
X#VATRG  DS    CL1                                                              
X#VATNEU DS    CL(L'ACTKACT)                                                    
X#VATEU  DS    CL(L'ACTKACT)                                                    
*                                                                               
DATO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
DATOX    DS    0C                                                               
*                                                                               
TEXT1    DS    CL160                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
***********************************************************************         
                                                                                
RQ_VALS  DS    0X                  ** REQUEST VALUES                            
RQ_TYPE  DS    XL1                 - Request type                               
RQ_DATA  DS    0XL1                - Request data                               
                                                                                
* Person Search                                                                 
RQ_PSSTR DS    CL16                Person Search                                
         ORG   RQ_DATA                                                          
                                                                                
* Suspense account download                                                     
RQ_SATYP DS    CL1                 Type                                         
RQ_SATMQ EQU   C'M'                - media                                      
RQ_SATWQ EQU   C'W'                - w/c                                        
RQ_SACLI DS    CL6                 Client                                       
RQ_SAPRO DS    CL6                 Product                                      
RQ_SAJOB DS    CL7                 Job                                          
RQ_SAWCD DS    CL2                 Work code                                    
RQ_SALED DS    CL1                 Ledger                                       
         ORG   RQ_DATA                                                          
                                                                                
* QuickReports LimitList                                                        
RQ_QLTYP DS    CL1                                                              
RQ_QLTLQ EQU   C'L'                                                             
         ORG   RQ_DATA                                                          
                                                                                
* Send mail for password                                                        
RQ_SMTYP DS    CL1                                                              
RQ_SMT1Q EQU   C'1'                                                             
RQ_SMPER DS    CL8                                                              
         ORG   RQ_DATA                                                          
                                                                                
* General Initial call                                                          
RQ_GITYP DS    CL1                                                              
RQ_GITGQ EQU   C'G'                                                             
         ORG   RQ_DATA                                                          
                                                                                
* VAT code list                                                                 
RQ_VCDAT DS    CL8                 Date for VAT list                            
RQ_VCOFF DS    CL2                 Office                                       
RQ_VCOUT DS    CL1                 Output requested (Y/N)                       
RQ_VCCOD DS    CL1                 Code filter                                  
RQ_VCSUP DS    CL14                SUPPLIER (GERMANY)                           
*&&US                                                                           
RQ_PROVC DS    CL2                 Province code                                
*&&                                                                             
RQMAXQ   EQU   *-RQ_DATA+64        <--- adjust for big RQ entries               
                                                                                
         ORG   RQ_DATA+RQMAXQ                                                   
                                                                                
         EJECT                                                                  
                                                                                
SAVEVALS DS    0X                                                               
                                                                                
* VAT code list                                                                 
VC_VALS  DS    0X                                                               
VC_ACCO  DS    CL14                account code                                 
VC_ACNA  DS    CL36                account name                                 
VC_CODE  DS    CL1                 VAT code                                     
*&&UK                                                                           
VC_RATE  DS    CL4                 rate                                         
*&&                                                                             
*&&US                                                                           
VC_RATE  DS    CL5                 rate                                         
*&&                                                                             
VC_COLA  DS    CL10                VAT code label                               
VC_LNQ   EQU   *-VC_VALS                                                        
         ORG   SAVEVALS                                                         
                                                                                
* Suspense account download                                                     
SA_VALS  DS    0X                                                               
SA_SACC  DS    CL14                suspense account code                        
SA_SACN  DS    CL36                and name                                     
SA_SAFN  DS    CL36                and foreign name                             
SA_LNQ   EQU   *-SA_VALS                                                        
         ORG   SAVEVALS                                                         
                                                                                
* QuickReports LimitList                                                        
QL_VALS  DS    0X                                                               
QL_CODE  DS    CL1                 code                                         
QL_LNQ   EQU   *-QL_VALS                                                        
         ORG   SAVEVALS                                                         
                                                                                
* General Initila values                                                        
GI_VALS  DS    0X                                                               
GI_CODE  DS    CL1                 code                                         
GI_LLGL  DS    CL1                 Limit/Group List                             
GI_LNQ   EQU   *-GI_VALS                                                        
         ORG   SAVEVALS                                                         
                                                                                
* Person Search                                                                 
PS_VALS  DS    0X                                                               
PS_CODE  DS    CL8                 - code                                       
PS_FNAM  DS    CL20                - first name                                 
PS_LNAM  DS    CL20                - last name                                  
PS_LNQ   EQU   *-PS_VALS                                                        
         ORG   SAVEVALS                                                         
                                                                                
* Send mail for password                                                        
SM_VALS  DS    0X                                                               
SM_CONFI DS    CL1                 - Y/N confirmation flag                      
SM_EMAIL DS    CL52                - email address                              
SM_LNQ   EQU   *-SM_VALS                                                        
         ORG   SAVEVALS                                                         
                                                                                
* quick report format (scribe) list                                             
QR_VALS  DS    0X                                                               
QR_FRMCD DS    CL8                 - scribe format code                         
QR_FRMNM DS    CL36                - scribe format name                         
QR_FRMTY DS    CL1                 - scribe type                                
QR_MANQ  EQU   C'1'                - manpower                                   
QR_PRODQ EQU   C'2'                - production                                 
QR_FINQ  EQU   C'3'                - financials                                 
QR_CRQ   EQU   C'4'                - creditors                                  
QR_DRQ   EQU   C'5'                - debtors                                    
QR_INCQ  EQU   C'6'                - income                                     
QR_CASQ  EQU   C'7'                - cash                                       
QR_MEDQ  EQU   C'8'                - media                                      
QR_EXPQ  EQU   C'9'                - expense                                    
QR_PNLQ  EQU   C'A'                - profit and loss                            
QR_DATE  DS    PL3                 - date of last modification                  
QR_LNQ   EQU   *-QR_VALS                                                        
         ORG   SAVEVALS                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* included books and DSECTS                                           *         
***********************************************************************         
                                                                                
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE ACBRAWRKD                                                      
       ++INCLUDE DDSMTPD                                                        
*&&UK                                                                           
       ++INCLUDE GEGENCUR                                                       
*&&                                                                             
       ++INCLUDE FAJESMAILD                                                     
         PRINT ON                                                               
***********************************************************************         
* HTML FOR EMAIL                                                      *         
***********************************************************************         
         SPACE 1                                                                
SVRDEF   CSECT                                                                  
                                                                                
EMLSUM   DS    0H                   SUMMARY EMAIL                               
*                                                                               
EMLS05   DC    AL1(EMLS05L)                                                     
         DC    C'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 '                 
         DC    C'Transitional//EN" '                                            
         DC    C'"http://www.w3.org/TR/xhtml11/DTD/xhtml1-transitional'         
         DC    C'.dtd">'                                                        
EMLS05L  EQU   *-EMLS05                                                         
*                                                                               
EMLS10   DC    AL1(EMLS10L)                                                     
         DC    C'<html xmlns="http://www.w3.org/1999/xhtml">'                   
EMLS10L  EQU   *-EMLS10                                                         
*                                                                               
EMLS15   DC    AL1(EMLS15L)                                                     
         DC    C'<head>'                                                        
EMLS15L  EQU   *-EMLS15                                                         
*                                                                               
EMLS20   DC    AL1(EMLS20L)                                                     
         DC    C'<title>'                                                       
EMLS20L  EQU   *-EMLS20                                                         
*                                                                               
EMLS25   DC    AL1(EMLS25L)                                                     
         DC    C'</title>'                                                      
EMLS25L  EQU   *-EMLS25                                                         
*                                                                               
EMLS30   DC    AL1(EMLS30L)                                                     
         DC    C'<meta http-equiv="Content-Type" content="text/html;'           
         DC    C' charset=UTF-8" />'                                            
EMLS30L  EQU   *-EMLS30                                                         
*                                                                               
EMLS35   DC    AL1(EMLS35L)                                                     
         DC    C'<meta name="viewport" content="width=device-width; '           
         DC    C'initial-scale=1; maximum-scale=1.0"/>'                         
EMLS35L  EQU   *-EMLS35                                                         
*                                                                               
EMLS40   DC    AL1(EMLS40L)                                                     
         DC    C'</head>'                                                       
EMLS40L  EQU   *-EMLS40                                                         
*                                                                               
EMLS45   DC    AL1(EMLS45L)                                                     
         DC    C'<body bgcolor="#ffffff">'                                      
EMLS45L  EQU   *-EMLS45                                                         
*                                                                               
EMLS50   DC    AL1(EMLS50L)                                                     
         DC    C'<!-- Outer Wrap -->'                                           
EMLS50L  EQU   *-EMLS50                                                         
*                                                                               
EMLS55   DC    AL1(EMLS55L)                                                     
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0" bgcolor="#ffffff" '                            
         DC    C'style="table-layout:fixed;">'                                  
EMLS55L  EQU   *-EMLS55                                                         
*                                                                               
EMLS60   DC    AL1(EMLS60L)                                                     
         DC    C'<tr>'                                                          
EMLS60L  EQU   *-EMLS60                                                         
*                                                                               
EMLS65   DC    AL1(EMLS65L)                                                     
         DC    C'<td>'                                                          
EMLS65L  EQU   *-EMLS65                                                         
*                                                                               
EMLS70   DC    AL1(EMLS70L)                                                     
         DC    C'<!-- top shadow -->'                                           
EMLS70L  EQU   *-EMLS70                                                         
*                                                                               
EMLS75   DC    AL1(EMLS75L)                                                     
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0">'                                              
EMLS75L  EQU   *-EMLS75                                                         
*                                                                               
EMLS80   DC    AL1(EMLS80L)                                                     
         DC    C'<tr>'                                                          
EMLS80L  EQU   *-EMLS80                                                         
*                                                                               
EMLS85   DC    AL1(EMLS85L)                                                     
         DC    C'<td height="10">'                                              
EMLS85L  EQU   *-EMLS85                                                         
*                                                                               
EMLS90   DC    AL1(EMLS90L)                                                     
         DC    C'</tr>'                                                         
EMLS90L  EQU   *-EMLS90                                                         
*                                                                               
EMLS95   DC    AL1(EMLS95L)                                                     
         DC    C'</table>'                                                      
EMLS95L  EQU   *-EMLS95                                                         
*                                                                               
EMLS100  DC    AL1(EMLS100L)                                                    
         DC    C'<!-- header -->'                                               
EMLS100L EQU   *-EMLS100                                                        
*                                                                               
EMLS105  DC    AL1(EMLS105L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellpadding="0">'                              
EMLS105L EQU   *-EMLS105                                                        
*                                                                               
EMLS110  DC    AL1(EMLS110L)                                                    
         DC    C'<tr>'                                                          
EMLS110L EQU   *-EMLS110                                                        
*                                                                               
EMLS115  DC    AL1(EMLS115L)                                                    
         DC    C'<td>'                                                          
EMLS115L EQU   *-EMLS115                                                        
*                                                                               
EMLS120  DC    AL1(EMLS120L)                                                    
         DC    C'<a href="http://mediaocean.com">'                              
EMLS120L EQU   *-EMLS120                                                        
*                                                                               
EMLS125  DC    AL1(EMLS125L)                                                    
         DC    C'<img alt="Aura" src="http://info.mediaocean.com/rs/'           
         DC    C'331-XPM-231/images/Aura-header.png"/>'                         
EMLS125L EQU   *-EMLS125                                                        
*                                                                               
EMLS130  DC    AL1(EMLS130L)                                                    
         DC    C'</a>'                                                          
EMLS130L EQU   *-EMLS130                                                        
*                                                                               
EMLS135  DC    AL1(EMLS135L)                                                    
         DC    C'</td>'                                                         
EMLS135L EQU   *-EMLS135                                                        
*                                                                               
EMLS140  DC    AL1(EMLS140L)                                                    
         DC    C'</tr>'                                                         
EMLS140L EQU   *-EMLS140                                                        
*                                                                               
EMLS145  DC    AL1(EMLS145L)                                                    
         DC    C'</table>'                                                      
EMLS145L EQU   *-EMLS145                                                        
*                                                                               
EMLS150  DC    AL1(EMLS150L)                                                    
         DC    C'<!-- end: header -->'                                          
EMLS150L EQU   *-EMLS150                                                        
*                                                                               
EMLS155  DC    AL1(EMLS155L)                                                    
         DC    C'<!-- body -->'                                                 
EMLS155L EQU   *-EMLS155                                                        
*                                                                               
EMLS160  DC    AL1(EMLS160L)                                                    
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0">'                                              
EMLS160L EQU   *-EMLS160                                                        
*                                                                               
EMLS165  DC    AL1(EMLS165L)                                                    
         DC    C'<tr>'                                                          
EMLS165L EQU   *-EMLS165                                                        
*                                                                               
EMLS170  DC    AL1(EMLS170L)                                                    
         DC    C'<td height="10">'                                              
EMLS170L EQU   *-EMLS170                                                        
*                                                                               
EMLS175  DC    AL1(EMLS175L)                                                    
         DC    C'</td>'                                                         
EMLS175L EQU   *-EMLS175                                                        
*                                                                               
EMLS180  DC    AL1(EMLS180L)                                                    
         DC    C'</tr>'                                                         
EMLS180L EQU   *-EMLS180                                                        
*                                                                               
EMLS185  DC    AL1(EMLS185L)                                                    
         DC    C'</table>'                                                      
EMLS185L EQU   *-EMLS185                                                        
*                                                                               
EMLS190  DC    AL1(EMLS190L)                                                    
         DC    C'<!-- tape check module -->'                                    
EMLS190L EQU   *-EMLS190                                                        
*                                                                               
EMLS195  DC    AL1(EMLS195L)                                                    
         DC    C'<table width="800" bgcolor="#ffffff" border="0" '              
         DC    C'cellspacing="0" cellpadding="0" '                              
         DC    C'style="font-family: Calibri, sans-serif; '                     
         DC    C'font-size:13px; color:#4E4D4C; '                               
EMLS195L EQU   *-EMLS195                                                        
*                                                                               
EMLS200  DC    AL1(EMLS200L)                                                    
         DC    C'background-color:#D9D9CD; '                                    
         DC    C'-webkit-border-radius: 3px; '                                  
         DC    C'-moz-border-radius: 3px; '                                     
         DC    C'border-radius: 3px;">'                                         
EMLS200L EQU   *-EMLS200                                                        
*                                                                               
EMLS205  DC    AL1(EMLS205L)                                                    
         DC    C'<tr>'                                                          
EMLS205L EQU   *-EMLS205                                                        
*                                                                               
EMLS210  DC    AL1(EMLS210L)                                                    
         DC    C'<td height="20" colspan="3" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS210L EQU   *-EMLS210                                                        
*                                                                               
EMLS215  DC    AL1(EMLS215L)                                                    
         DC    C'<img height="20" alt="spacer" '                                
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS215L EQU   *-EMLS215                                                        
*                                                                               
EMLS220  DC    AL1(EMLS220L)                                                    
         DC    C'</td>'                                                         
EMLS220L EQU   *-EMLS220                                                        
*                                                                               
EMLS225  DC    AL1(EMLS225L)                                                    
         DC    C'</tr>'                                                         
EMLS225L EQU   *-EMLS225                                                        
*                                                                               
EMLS230  DC    AL1(EMLS230L)                                                    
         DC    C'<tr>'                                                          
EMLS230L EQU   *-EMLS230                                                        
*                                                                               
EMLS235  DC    AL1(EMLS235L)                                                    
         DC    C'<td width="20">'                                               
EMLS235L EQU   *-EMLS235                                                        
*                                                                               
EMLS240  DC    AL1(EMLS240L)                                                    
         DC    C'<img height="1" width="20" alt="spacer" '                      
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS240L EQU   *-EMLS240                                                        
*                                                                               
EMLS245  DC    AL1(EMLS245L)                                                    
         DC    C'</td>'                                                         
EMLS245L EQU   *-EMLS245                                                        
*                                                                               
EMLS250  DC    AL1(EMLS250L)                                                    
         DC    C'<td valign="top" width="360" '                                 
         DC    C'style="font-family: Calibri, sans-serif; '                     
         DC    C'font-size:30px; color:#4E4D4C; line-height:36px;">'            
EMLS250L EQU   *-EMLS250                                                        
*                                                                               
EMLS255  DC    AL1(EMLS255L)                                                    
         DC    C'<div>'                                                         
EMLS255E DS    CL80                                                             
         DC    C'</div>'                                                        
EMLS255L EQU   *-EMLS255                                                        
*                                                                               
EMLS260  DC    AL1(EMLS260L)                                                    
EMLS260E DS    CL160                                                            
EMLS260L EQU   *-EMLS260                                                        
*                                                                               
EMLS261  DC    AL1(EMLS261L)                                                    
EMLS261E DS    CL160                                                            
EMLS261L EQU   *-EMLS261                                                        
*                                                                               
EMLS262  DC    AL1(EMLS262L)                                                    
EMLS262E DS    CL160                                                            
EMLS262L EQU   *-EMLS262                                                        
*                                                                               
EMLS270  DC    AL1(EMLS270L)                                                    
         DC    C'<p style="color:#4E4D4C; font-size:13px; '                     
         DC    C'text-decoration:none; line-height:16px;">'                     
EMLS270L EQU   *-EMLS270                                                        
*                                                                               
EMLS285  DC    AL1(EMLS285L)                                                    
EMLS285E DS    CL80                                                             
EMLS285L EQU   *-EMLS285                                                        
*                                                                               
EMLS290  DC    AL1(EMLS290L)                                                    
         DC    C'</a>'                                                          
EMLS290L EQU   *-EMLS290                                                        
*                                                                               
EMLS295  DC    AL1(EMLS295L)                                                    
         DC    C'</p>'                                                          
EMLS295L EQU   *-EMLS295                                                        
*                                                                               
EMLS300  DC    AL1(EMLS300L)                                                    
         DC    C'</td>'                                                         
EMLS300L EQU   *-EMLS300                                                        
*                                                                               
EMLS305  DC    AL1(EMLS305L)                                                    
         DC    C'<td width="20">'                                               
EMLS305L EQU   *-EMLS305                                                        
*                                                                               
EMLS310  DC    AL1(EMLS310L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
EMLS310L EQU   *-EMLS310                                                        
*                                                                               
EMLS315  DC    AL1(EMLS315L)                                                    
         DC    C'</td>'                                                         
EMLS315L EQU   *-EMLS315                                                        
*                                                                               
EMLS320  DC    AL1(EMLS320L)                                                    
         DC    C'</tr>'                                                         
EMLS320L EQU   *-EMLS320                                                        
*                                                                               
EMLS325  DC    AL1(EMLS325L)                                                    
         DC    C'<tr>'                                                          
EMLS325L EQU   *-EMLS325                                                        
EMLS330  DC    AL1(EMLS330L)                                                    
         DC    C'<td height="30" colspan="3" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS330L EQU   *-EMLS330                                                        
*                                                                               
EMLS335  DC    AL1(EMLS335L)                                                    
         DC    C'<img height="30" width="20" border="0" alt="spacer" '          
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
EMLS335L EQU   *-EMLS335                                                        
*                                                                               
EMLS340  DC    AL1(EMLS340L)                                                    
         DC    C'</td>'                                                         
EMLS340L EQU   *-EMLS340                                                        
*                                                                               
EMLS345  DC    AL1(EMLS345L)                                                    
         DC    C'</tr>'                                                         
EMLS345L EQU   *-EMLS345                                                        
*                                                                               
EMLS350  DC    AL1(EMLS350L)                                                    
         DC    C'</table>'                                                      
EMLS350L EQU   *-EMLS350                                                        
*                                                                               
EMLS355  DC    AL1(EMLS355L)                                                    
         DC    C'<!-- no background module -->'                                 
EMLS355L EQU   *-EMLS355                                                        
*                                                                               
EMLS360  DC    AL1(EMLS360L)                                                    
         DC    C'<table width="800" bgcolor="#ffffff" border="0" '              
         DC    C'cellspacing="0" cellpadding="0" style="font-family: '          
EMLS360L EQU   *-EMLS360                                                        
*                                                                               
EMLS365  DC    AL1(EMLS365L)                                                    
         DC    C'Calibri, sans-serif; font-size:10px; color:#4E4D4C; '          
         DC    C'background-color:#ffffff; -webkit-border-radius: 3px;'         
         DC    C' -moz-border-radius: 3px; border-radius: 3px;">'               
EMLS365L EQU   *-EMLS365                                                        
*                                                                               
EMLS370  DC    AL1(EMLS370L)                                                    
         DC    C'<tr>'                                                          
EMLS370L EQU   *-EMLS370                                                        
*                                                                               
EMLS375  DC    AL1(EMLS375L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS375L EQU   *-EMLS375                                                        
*                                                                               
EMLS380  DC    AL1(EMLS380L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS380L EQU   *-EMLS380                                                        
*                                                                               
EMLS385  DC    AL1(EMLS385L)                                                    
         DC    C'</td>'                                                         
EMLS385L EQU   *-EMLS385                                                        
*                                                                               
EMLS390  DC    AL1(EMLS390L)                                                    
         DC    C'<td>'                                                          
EMLS390L EQU   *-EMLS390                                                        
*                                                                               
EMLS395  DC    AL1(EMLS395L)                                                    
         DC    X'50'                                                            
         DC    C'nbsp;'                                                         
EMLS395L EQU   *-EMLS395                                                        
*                                                                               
EMLS400  DC    AL1(EMLS400L)                                                    
         DC    C'</td>'                                                         
EMLS400L EQU   *-EMLS400                                                        
*                                                                               
EMLS405  DC    AL1(EMLS405L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS405L EQU   *-EMLS405                                                        
*                                                                               
EMLS410  DC    AL1(EMLS410L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS410L EQU   *-EMLS410                                                        
*                                                                               
EMLS415  DC    AL1(EMLS415L)                                                    
         DC    C'</td>'                                                         
EMLS415L EQU   *-EMLS415                                                        
*                                                                               
EMLS420  DC    AL1(EMLS420L)                                                    
         DC    C'</tr>'                                                         
EMLS420L EQU   *-EMLS420                                                        
*                                                                               
EMLS425  DC    AL1(EMLS425L)                                                    
         DC    C'<tr>'                                                          
EMLS425L EQU   *-EMLS425                                                        
*                                                                               
EMLS430  DC    AL1(EMLS430L)                                                    
         DC    C'<td style="vertical-align:top;">'                              
EMLS430L EQU   *-EMLS430                                                        
*                                                                               
EMLS435  DC    AL1(EMLS435L)                                                    
         DC    C'<p>'                                                           
EMLS435L EQU   *-EMLS435                                                        
*                                                                               
EMLS440  DC    AL1(EMLS440L)                                                    
EMAILS1  DS    CL80                                                             
EMAILS2  DS    CL80                                                             
EMLS440L EQU   *-EMLS440                                                        
*                                                                               
EMLS445  DC    AL1(EMLS445L)                                                    
EMAILS3  DS    CL80                                                             
EMAILS4  DS    CL80                                                             
EMLS445L EQU   *-EMLS445                                                        
*                                                                               
EMLS450  DC    AL1(EMLS450L)                                                    
EMAILS5  DS    CL80                                                             
         DC    C'</p>'                                                          
EMLS450L EQU   *-EMLS450                                                        
*                                                                               
EMLS455  DC    AL1(EMLS455L)                                                    
         DC    C'<p>'                                                           
EMLS455L EQU   *-EMLS455                                                        
*                                                                               
EMLS460  DC    AL1(EMLS460L)                                                    
EMAILS6  DS    CL80                                                             
EMAILS7  DS    CL80                                                             
EMLS460L EQU   *-EMLS460                                                        
*                                                                               
EMLS465  DC    AL1(EMLS465L)                                                    
EMAILS8  DS    CL80                                                             
EMAILS9  DS    CL80                                                             
EMLS465L EQU   *-EMLS465                                                        
*                                                                               
EMLS468  DC    AL1(EMLS468L)                                                    
EMAILSA  DS    CL80                                                             
EMLS468L EQU   *-EMLS468                                                        
*                                                                               
EMLS470  DC    AL1(EMLS470L)                                                    
         DC    C'</p>'                                                          
EMLS470L EQU   *-EMLS470                                                        
*                                                                               
EMLS475  DC    AL1(EMLS475L)                                                    
         DC    C'</td>'                                                         
EMLS475L EQU   *-EMLS475                                                        
*                                                                               
EMLS480  DC    AL1(EMLS480L)                                                    
         DC    C'</tr>'                                                         
EMLS480L EQU   *-EMLS480                                                        
*                                                                               
EMLS485  DC    AL1(EMLS485L)                                                    
         DC    C'<tr>'                                                          
EMLS485L EQU   *-EMLS485                                                        
*                                                                               
EMLS490  DC    AL1(EMLS490L)                                                    
         DC    C'<td height="20" style="font-size:1px; '                        
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS490L EQU   *-EMLS490                                                        
*                                                                               
EMLS495  DC    AL1(EMLS495L)                                                    
         DC    C'<img height="20" border="0" alt=spacer" '                      
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS495L EQU   *-EMLS495                                                        
*                                                                               
EMLS500  DC    AL1(EMLS500L)                                                    
         DC    C'</td>'                                                         
EMLS500L EQU   *-EMLS500                                                        
*                                                                               
EMLS505  DC    AL1(EMLS505L)                                                    
         DC    C'</tr>'                                                         
EMLS505L EQU   *-EMLS505                                                        
*                                                                               
EMLS510  DC    AL1(EMLS510L)                                                    
         DC    C'</table>'                                                      
EMLS510L EQU   *-EMLS510                                                        
*                                                                               
EMLS515  DC    AL1(EMLS515L)                                                    
         DC    C'<!-- end: body -->'                                            
EMLS515L EQU   *-EMLS515                                                        
*                                                                               
EMLS520  DC    AL1(EMLS520L)                                                    
         DC    C'<!-- Footer -->'                                               
EMLS520L EQU   *-EMLS520                                                        
*                                                                               
EMLS525  DC    AL1(EMLS525L)                                                    
         DC    C'<table width="800" cellspacing="0" cellpadding="0" '           
         DC    C'style="height:78px; font-family: Calibri, '                    
EMLS525L EQU   *-EMLS525                                                        
*                                                                               
EMLS530  DC    AL1(EMLS530L)                                                    
         DC    C'sans-serif; font-size:10px; color:#ffffff; '                   
         DC    C'background-color:#4E4D4C; text-align:center; '                 
EMLS530L EQU   *-EMLS530                                                        
*                                                                               
EMLS535  DC    AL1(EMLS535L)                                                    
         DC    C'border-bottom:1px solid #4E4D4C; '                             
         DC    C'-webkit-border-radius: 3px; -moz-border-radius: 3px; '         
         DC    C'border-radius: 3px;">'                                         
EMLS535L EQU   *-EMLS535                                                        
*                                                                               
EMLS540  DC    AL1(EMLS540L)                                                    
         DC    C'<tr>'                                                          
EMLS540L EQU   *-EMLS540                                                        
*                                                                               
EMLS545  DC    AL1(EMLS545L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS545L EQU   *-EMLS545                                                        
*                                                                               
EMLS550  DC    AL1(EMLS550L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS550L EQU   *-EMLS550                                                        
*                                                                               
EMLS555  DC    AL1(EMLS555L)                                                    
         DC    C'</td>'                                                         
EMLS555L EQU   *-EMLS555                                                        
*                                                                               
EMLS560  DC    AL1(EMLS560L)                                                    
         DC    C'<td colspan="2" height="15" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS560L EQU   *-EMLS560                                                        
*                                                                               
EMLS565  DC    AL1(EMLS565L)                                                    
         DC    C'<img height="15" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS565L EQU   *-EMLS565                                                        
*                                                                               
EMLS570  DC    AL1(EMLS570L)                                                    
         DC    C'</td>'                                                         
EMLS570L EQU   *-EMLS570                                                        
*                                                                               
EMLS575  DC    AL1(EMLS575L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS575L EQU   *-EMLS575                                                        
*                                                                               
EMLS580  DC    AL1(EMLS580L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS580L EQU   *-EMLS580                                                        
*                                                                               
EMLS585  DC    AL1(EMLS585L)                                                    
         DC    C'</td>'                                                         
EMLS585L EQU   *-EMLS585                                                        
*                                                                               
EMLS590  DC    AL1(EMLS590L)                                                    
         DC    C'</tr>'                                                         
EMLS590L EQU   *-EMLS590                                                        
*                                                                               
EMLS595  DC    AL1(EMLS595L)                                                    
         DC    C'<tr style="vertical-align:bottom;">'                           
EMLS595L EQU   *-EMLS595                                                        
*                                                                               
EMLS600  DC    AL1(EMLS600L)                                                    
         DC    C'<td style="text-align:left;">'                                 
EMLS600L EQU   *-EMLS600                                                        
*                                                                               
EMLS605  DC    AL1(EMLS605L)                                                    
         DC    C'<div>'                                                         
EMLS605L EQU   *-EMLS605                                                        
*                                                                               
EMLS610  DC    AL1(EMLS610L)                                                    
EMLS610E DS    CL80                                                             
EMLS610L EQU   *-EMLS610                                                        
*                                                                               
EMLS615  DC    AL1(EMLS615L)                                                    
         DC    C'<br />'                                                        
EMLS615L EQU   *-EMLS615                                                        
*                                                                               
EMLS620  DC    AL1(EMLS620L)                                                    
         DC    X'50'                                                            
         DC    C'copy'                                                          
EMLS620E DS    CL4                                                              
         DC    C' MEDIAOCEAN'                                                   
EMLS620L EQU   *-EMLS620                                                        
*                                                                               
EMLS625  DC    AL1(EMLS625L)                                                    
         DC    C'| '                                                            
EMLS625E DS    CL18                                                             
         DC    C':'                                                             
EMLS625L EQU   *-EMLS625                                                        
*                                                                               
EMLS630  DC    AL1(EMLS630L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://www.facebook.com/team.mediaocean">'              
         DC    C'FACEBOOK'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
EMLS630L EQU   *-EMLS630                                                        
*                                                                               
EMLS635  DC    AL1(EMLS635L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://twitter.com/teammediaocean">'                    
         DC    C'TWITTER'                                                       
         DC    C'</a>'                                                          
         DC    C' |'                                                            
EMLS635L EQU   *-EMLS635                                                        
*                                                                               
EMLS640  DC    AL1(EMLS640L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://www.linkedin.com/company/mediaocean">'            
         DC    C'LINKEDIN'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
EMLS640L EQU   *-EMLS640                                                        
*                                                                               
EMLS642  DC    AL1(EMLS642L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://instagram.com/teammediaocean">'                   
         DC    C'INSTAGRAM'                                                     
         DC    C'</a>'                                                          
EMLS642L EQU   *-EMLS642                                                        
*                                                                               
EMLS645  DC    AL1(EMLS645L)                                                    
         DC    C'</div>'                                                        
EMLS645L EQU   *-EMLS645                                                        
*                                                                               
EMLS650  DC    AL1(EMLS650L)                                                    
         DC    C'</td>'                                                         
EMLS650L EQU   *-EMLS650                                                        
*                                                                               
EMLS655  DC    AL1(EMLS655L)                                                    
         DC    C'<td style="text-align:right;">'                                
EMLS655L EQU   *-EMLS655                                                        
*                                                                               
EMLS660  DC    AL1(EMLS660L)                                                    
         DC    C'<a href="http://www.mediaocean.com">'                          
EMLS660L EQU   *-EMLS660                                                        
*                                                                               
EMLS665  DC    AL1(EMLS665L)                                                    
         DC    C'<img src="http://info.mediaocean.com/rs/331-XPM-231/'          
         DC    C'images/MO-aura-footer-logo.png" alt="MediaOcean" />'           
EMLS665L EQU   *-EMLS665                                                        
*                                                                               
EMLS670  DC    AL1(EMLS670L)                                                    
         DC    C'</a>'                                                          
EMLS670L EQU   *-EMLS670                                                        
*                                                                               
EMLS675  DC    AL1(EMLS675L)                                                    
         DC    C'</td>'                                                         
EMLS675L EQU   *-EMLS675                                                        
*                                                                               
EMLS680  DC    AL1(EMLS680L)                                                    
         DC    C'</tr>'                                                         
EMLS680L EQU   *-EMLS680                                                        
*                                                                               
EMLS685  DC    AL1(EMLS685L)                                                    
         DC    C'</table>'                                                      
EMLS685L EQU   *-EMLS685                                                        
*                                                                               
EMLS690  DC    AL1(EMLS690L)                                                    
         DC    C'<!-- end: Footer -->'                                          
EMLS690L EQU   *-EMLS690                                                        
*                                                                               
EMLS695  DC    AL1(EMLS695L)                                                    
         DC    C'</td>'                                                         
EMLS695L EQU   *-EMLS695                                                        
*                                                                               
EMLS700  DC    AL1(EMLS700L)                                                    
         DC    C'</tr>'                                                         
EMLS700L EQU   *-EMLS700                                                        
*                                                                               
EMLS705  DC    AL1(EMLS705L)                                                    
         DC    C'</table>'                                                      
EMLS705L EQU   *-EMLS705                                                        
*                                                                               
EMLS710  DC    AL1(EMLS710L)                                                    
         DC    C'<!-- End Inner Wrap -->'                                       
EMLS710L EQU   *-EMLS710                                                        
*                                                                               
EMLS715  DC    AL1(EMLS715L)                                                    
         DC    C'</td>'                                                         
EMLS715L EQU   *-EMLS715                                                        
*                                                                               
EMLS720  DC    AL1(EMLS720L)                                                    
         DC    C'</tr>'                                                         
EMLS720L EQU   *-EMLS720                                                        
*                                                                               
EMLS725  DC    AL1(EMLS725L)                                                    
         DC    C'</table>'                                                      
EMLS725L EQU   *-EMLS725                                                        
*                                                                               
EMLS730  DC    AL1(EMLS730L)                                                    
         DC    C'<!-- End Outer Wrap -->'                                       
EMLS730L EQU   *-EMLS730                                                        
*                                                                               
EMLS735  DC    AL1(EMLS735L)                                                    
         DC    C'</body>'                                                       
EMLS735L EQU   *-EMLS735                                                        
*                                                                               
EMLS740  DC    AL1(EMLS740L)                                                    
         DC    C'</html>'                                                       
EMLS740L EQU   *-EMLS740                                                        
*                                                                               
EMLSTMLL EQU   *-EMLSUM                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055ACBRA20   05/17/20'                                      
         END                                                                    
