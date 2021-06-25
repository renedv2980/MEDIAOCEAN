*          DATA SET SRUPD60    AT LEVEL 084 AS OF 02/09/21                      
*PHASE T10D60A                                                                  
* NSHE 084 08Feb21 Further fix for day narrative update      SPEC-53700         
* MPEN 083 11Nov20 Fix for day narrative time update         DSRD-27923         
* DDRA 082 11Dec20 Relink for new MELDCMPT/MELDCPTRI         DSPCM-6048         
* GSUM 082 01Dec20 Handle ACRTTRN via MELNK17 to un/hold trn SPEC-51940         
* DDRA 081 27Nov20 Relink for new MELDCMPT/MELDCPTRI         DSPCM-5891         
* DDRA 080 19Nov20 Relink for new MELDCMPT/MELDCPTRI         DSPCM-5891         
* ABID 079 11Nov20 Relink for new MELDCPTRI                    PCM05852         
* MPEN 078 07AUG20 Time comments by day                      DSRD-27177         
* NMAL 077 10MAY20 Relink for new MELDCMPT/MELDCPTRI         DSTVS-4844         
* ABID 076 02APR20 RELINK FOR ACLDCPTR CHANGES               DSPCA-3076         
* PWES 075 09MAR20 Relink for new MELDCMPT/MELDCPTRI          DSMX-2835         
* ABID 074 27FEB20 RELINK FOR ACLDCPTR CHANGES               DSPCA-3076         
* ABID 073 11FEB20 RELINK BOOKS RELEASED IN 2020.1 RELEASE   DSRD-25377         
* ABID 072 08FEB20 FIX DUMP ISSUE DUE TO DUPLICATE RECORD    ITMF-42166         
* NSHE 071 03Oct19 Lock SJ ledger for all runner uploads     DSRD-24043         
* ABID 069 27SEP19 RUNNER FAILURE WHEN READING ARCHIVED 198  SPEC-39198         
*                  TRANSACTIONS                              SPEC-39198         
* ABID 069 27SEP19 LOCK 1R ACCOUNT IN OFFLINE PASS TO AVOID  DSRD-23960         
*                  DUPLICATE CPRRECD RECORD                  DSRD-23960         
* NSHE 069 19Sep19 Fix TRSPASD logic in AMST to avoid dupes  ITMF-39452         
* NSHE     26Jul19 Remove UK only ledger read                DSRD-23225         
* MPEN 068 29APR19 Save timeoff id                           DSRD-22248         
* GSUM 067 07JUN19 Relink for new MELDCMPT/MELDCPTRI          DSMX-2483         
* GSUM 066 16MAY19 CAMREC campaign bucket: keep even if zero ITMF-36340         
* ABID 065 01MAR19  IMPROVE TIMESHEET - STOP DEADLY EMBRACES DSRD-21662         
* DDRA 064 27Mar19 Relink for new MELDCMPT/MELDCPTRI         DSPCM-5002         
* MPEN 063 21Nov18 Fix for serial number passive             DSRD-20817         
* TKLU 061 30Oct18 CAMREC campaign bucket updates (+GSUM)    DSPCM-4700         
* NSHE 060 24Sep18 Change group invoice seq lookup           DSRD-20292         
* TKLU 051 18Aug18 RNSPASD adjustments (ACLDCPTR)            DSPCA-2844         
* TKLU 056 16Aug18 ACBRA1E time upload needs BLDPAS 1R lvls  DSRD-17832         
* DDRA 055 31Mar18 Relink for new MELDCMPT/MELDCPTRI          DSMX-1879         
* NSHE 054 05Mar18 Deal with fiscal start month correctly      RD015589         
* MPEN 053 12Jul17 Extend timeline narrative to 200 chars      RD015589         
* DDRA 052 02Aug17 Relink for new MELDCMPT/MELDCPTRI         DSPCM-4181         
* WDOW 051 26Jul17 Relink for new MELDCMPT/MELDCPTRI         DSPCM-4128         
* DDRA 051 27May17 Relink for new MELDCMPT/MELDCPTRI         DSPCM-3461         
* MPEN 050 14Dec16 Relink for new ACLDCPTR                     RD013417         
* TKLU 049 14Nov16 DATCON/15 for ADDTRN                        PCA01157         
* GSUM 048 11Oct16 Relink (again) for new MELDCMPT/MELDCPTRI   MXS00267         
* GSUM 047 12Sep16 Relink for new MELDCMPT/MELDCPTRI           MXS00267         
* NSHE 046 03Feb16 don't set date for MELDPCTRI                ITMF5092         
* MPEN 045 21Jan16 Include new ACLDCPTR fix for 2 level appr   BO-1645          
* GSUM 042 09Nov15 Copy exact record length from sort buffer   DSMX-914         
* MPEN 041 25Aug15 Relink for new ACPROCPT                     RD008297         
* NRAK 040 21aug14 <ot80490l> new acldcptr                                      
* TKLU 039 24Jun14 Relink for new ACLDCPTR                     RD000869         
* WDOW 038 17Mar14 Relink for new MELDCMPT                     DSTVS-95         
* MPEN 037 13Feb14 US only code to send email to MQ           DSPCA-578         
* NSHE 036 22Oct13 Addtrn fix readded as previous version in                    
*          April overwrote my changes                          DSBO-405         
* DDRA 035 15Jul13 Relink for new MELDCMPT                     DSPCM-66         
* WDOW 034 28Jun13 Relink for new MELDCMPT and SRUPD60D        PR003607         
* TSMY 031 08Feb13 New MELDCMPT                                BR53961L         
* TKLU 030 05Feb13 ACLDCPTR to support ACFD passives           PR003355         
* CBLA 029 28Jan13 *INCLUDE of PDUMPER for debug MELDCPTRI     BR52475L         
                                                                                
*INCLUDE ACLDCPTR                                                               
*INCLUDE ACPROCPT                                                               
*&&UK                                                                           
*INCLUDE MELDCMPT                                                               
*INCLUDE PDUMPER                                                                
*INCLUDE LOGIO                                                                  
*&&                                                                             
                                                                                
SRUPD60  TITLE 'BO Time Sheet and MX flexible payment uploads'                  
                                                                                
***********************************************************************         
* BrandOcean                                                          *         
* This program is called by ACBRA1B off-line and SRUPD00 on-line - it *         
* reads a FACWRK file created by ACBRA1B and (optionally) updates the *         
* DDS master files.  The input file may contain both logical records  *         
* (those that require special processing) and regular directory/file  *         
* changes (copy records are not passed).                              *         
*                                                                     *         
* MediaExplorer                                                       *         
* This program is called by MELNK17 offline to update the media and   *         
* accounting file/directory from records in a FACWRK file created by  *         
* MELNK17                                                             *         
*                                                                     *         
* On entry the FACWRK input file is assumed to be open and the first  *         
* record read.                                                        *         
*                                                                     *         
* When running off-line the caller will provide the TSAR blocks for   *         
* the TSAR buffers - these blocks are passed back at the end so       *         
* that if the buffers were acquired by calls from this module the     *         
* storage won't be lost.  An I/O trace can be printed when running    *         
* in either draft or live mode.                                       *         
***********************************************************************         
                                                                                
SRUPD60  CSECT                                                                  
         NMOD1 WORKL,**UP60**,CLEAR=YES,RR=RE                                   
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(Local working storage)                  
         ST    RB,ASRUPD60                                                      
         USING ADDTRND,ADDTRNW                                                  
         USING CMPTD,FWCMPT                                                     
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
                                                                                
         ST    RE,RELO                                                          
         A     RE,VLDCMPT                                                       
         ST    RE,LDCMPT           Set V(LDCMPT)                                
         L     RE,RELO                                                          
         A     RE,VLDCPTR                                                       
         ST    RE,LDCPTR           Set V(LDCPTR)                                
                                                                                
         MVC   PLIST(PLISTL),0(R1) Set calling parameters                       
                                                                                
         L     R0,PLABUF           R0=A(16K buffer area)                        
         ST    R0,AIO1             Set A(I/O areas)                             
         AHI   R0,IOLENQ                                                        
         ST    R0,AIO2                                                          
         AHI   R0,IOLENQ                                                        
         ST    R0,AIO3                                                          
         AHI   R0,IOLENQ                                                        
         ST    R0,AIO4                                                          
         AHI   R0,IOLENQ                                                        
         ST    R0,AIO5                                                          
         AHI   R0,IOLENQ                                                        
         ST    R0,AIO6                                                          
         AHI   R0,IOLENQ                                                        
         ST    R0,AIO7                                                          
         AHI   R0,IOLENQ                                                        
         ST    R0,AGENAREA                                                      
                                                                                
         L     R1,PLAFWPAR         Extract FACWRK parameter list                
         MVC   FWDMCB(FWDMCBL),0(R1)                                            
         L     R7,FWAREC                                                        
         USING FW_D,R7             R7=A(FACWRK record)                          
                                                                                
         MVI   TODO,0              Nothing to do                                
         CLI   PLONOFF,PLON        Test on-line                                 
         BE    SRUP0010                                                         
         EJECT                                                                  
***********************************************************************         
* Off-line initialisation                                             *         
***********************************************************************         
                                                                                
         CLI   PLONOFF,PLOFF       Test off-line                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R8,PLALPD                                                        
         USING LP_D,R8             R8=A(LP_D)                                   
                                                                                
         TM    LP_OFLG1,LP_OFDFT   Test draft mode                              
         JNZ   *+8                                                              
         OI    LP_INDS,LP_IRECV    No - set to invoke recovery on abend         
                                                                                
         L     R1,LP_ACOM                                                       
         GOTOR SETFAC              Extract COMFACS addresses                    
         L     RF,LP_ABLK3                                                      
         MVC   TSARPASS,0(RF)      Copy TSAR block for passive buffer           
         L     RF,LP_ABLK4                                                      
         MVC   TSARRECS,0(RF)      Copy TSAR block for record buffer            
         L     RF,LP_ABLK5                                                      
         MVC   TSAROLDT,0(RF)      Copy TSAR block for old time buffer          
         L     RF,LP_ABLK6                                                      
         MVC   TSARNEWT,0(RF)      Copy TSAR block for new time buffer          
         L     RF,LP_ABLK7                                                      
         MVC   TSARTRNK,0(RF)      Copy TSAR block for transaction keys         
         CLI   PLSORT,PLON         Are we using sorter to hold records          
         JNE   SRUP0002            No - must be using FACWRK                    
         L     RF,LP_ABLK8                                                      
         MVC   VSORTER,0(RF)       Copy address of sorter                       
                                                                                
SRUP0002 L     RF,LP_ASECD         address of security block                    
         MVC   AGYSEC,SECAGY-SECD(RF)  Save security agency.                    
                                                                                
         L     RF,LP_ARUNP                                                      
         ICM   RE,7,RUNPARUN-RUNPARMD(RF)                                       
         ST    RE,RUNFACS          Set A(RUNFACS)                               
         USING RUNFACSD,RE                                                      
         MVC   PRINTER,RPRINTER                                                 
         MVC   SMTP,RSMTP          Address of SMTP                              
         MVC   MQI,RMQI            Address of MQ                                
         MVC   AMASTC,RMASTC       Address of MASTC                             
         L     R5,RCPRINT                                                       
         DROP  RE                                                               
         USING DPRINT,R5           R5=A(Printer control block)                  
         L     RF,AMASTC                                                        
         MVC   USRID,MCUSERID-MCBLOCK(RF) Address of User ID                    
         MVC   FACID,MCFACPAK-MCBLOCK(RF) Address of FACPAK ID                  
         ICM   RE,15,MCUTL-MCBLOCK(RF)    Address of UTL                        
         LA    RE,TSYS-UTLD(RE)                                                 
         ST    RE,ATSYS            A(SENUM offline)                             
         MVC   CURRSYS,0(RE)       Current SENUM                                
                                                                                
         CLI   LP_OFTRC,0          Test printing a trace                        
         JE    ACUPFRST            No                                           
         MVC   TITLE(L'TTITLE),TTITLE                                           
         ZAP   LINE,MAXLINE        Print a space line                           
         MVC   P(L'TNEWT),TNEWT    Print start of new trace                     
         GOTOR PRINTER             Print start trace line                       
         TM    LP_OFLG1,LP_OFDFT   Test draft mode                              
         JZ    ACUPFRST                                                         
         MVC   P(L'TDRAFT),TDRAFT                                               
         GOTOR PRINTER             Print draft mode line                        
         B     ACUPFRST                                                         
                                                                                
TB       USING TSARD,TSARRECS      BUFREC TSAR block                            
TP       USING TSARD,TSARPASS      BLDPAS TSAR block                            
TK       USING TSARD,TSARTRNK      BLDTRK TSAR block                            
         EJECT                                                                  
***********************************************************************         
* On-line initialisation                                              *         
***********************************************************************         
                                                                                
SRUP0010 SR    R8,R8               LP_D not available on-line                   
         L     R1,PLASRPAR         Extract S/R parameter list                   
         MVC   SRPARS(SRPARSL),0(R1)                                            
         MVC   ANEXT#,SRPAR2       Set a(record number table)                   
                                                                                
         L     R2,SRPAR1                                                        
         USING SYSFACD,R2          R2=A(SYSFACS)                                
         L     R1,SRPAR4                                                        
         GOTOR SETFAC              Extract COMFACS addresses                    
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* Extract values from FACWRK header record (already read)             *         
***********************************************************************         
                                                                                
ACUPFRST CLC   TSAR,FZERO          Test A(TSAR) resolved                        
         JNE   ACUPF00             Yes                                          
         GOTOR GETPHS,PARM,('QTSAR',TSARF)                                      
                                                                                
ACUPF00  CLI   PLSORT,PLOFF        Test running with FACWRK                     
         JE    ACUPF02             Yes                                          
                                                                                
         GOTO1 VSORTER,DMCB,SORTGET                                             
         ICM   R7,15,4(R1)                                                      
         JNZ   ACUPF02                                                          
         DC    H'0'                                                             
                                                                                
ACUPF02  MVC   TRNPUSER,FW_USER    Set user-ID                                  
         MVC   AGY,FW_AGY          Set agency alpha ID                          
         MVC   CPY,FW_CPY          Set company code                             
         MVC   TRNCPYS1,FW_CPYS1   Set company status bytes                     
         MVC   TRNCPYS2,FW_CPYS2                                                
         MVC   TRNCPYS3,FW_CPYS3                                                
         MVC   TRNCPYS4,FW_CPYS4                                                
         MVC   TRNCPYS5,FW_CPYS5                                                
         MVC   TRNCPYS6,FW_CPYS6                                                
         MVC   TRNCPYS7,FW_CPYS7                                                
         MVC   TRNCPYS8,FW_CPYS8                                                
         MVC   TRNCPYS9,FW_CPYS9                                                
         MVC   TRNCPYSA,FW_CPYSA                                                
*&&UK*&& MVC   TRNCPYSB,FW_CPYSB                                                
*&&UK*&& MVC   TRNCPYSC,FW_CPYSC                                                
         MVC   TRNGLMOA,FW_CPYGL   GLMOA DATE                                   
         MVC   HD_CPSFS,FW_CPYSF                                                
         MVC   ONERLVS(ONERLVL),FW_PERLV                                        
         MVC   SJALVS(SJALVLQ),FW_SJALV                                         
         MVC   CALOVRLY,FW_BOVLY                                                
                                                                                
         MVC   FWFLAG,FW_FLAG      Copy header flags                            
         MVC   FWSENA,FW_HSENA                                                  
         MVC   FWSENM,FW_HSENM                                                  
         MVC   CMPTAGYN,FW_HAGYN                                                
         MVC   CMPTMEDS,FW_HMEDS                                                
         CLI   PLSORT,PLON         Test running with SORTER buff passed         
         JE    ACUPNEXT            Yes                                          
                                                                                
***********************************************************************         
* Build a TSAR buffer containing record keys and sequence numbers so  *         
* that the input file so the input file can be read randomly          *         
***********************************************************************         
                                                                                
S        USING TSARD,TSARSORT      Initialise sort file                         
         MVI   S.TSACTN,TSAINI                                                  
         MVI   S.TSRECI,TSRXTN+TSRTSAB2                                         
         MVI   S.TSKEYL,SORTKLNQ                                                
         LHI   R0,SORTRLNQ                                                      
         STH   R0,S.TSRECL                                                      
         MVC   S.TSACOM,ACOMFACS                                                
         LHI   R0,ONEK             Acquire 1MB for buffer off-line              
         STCM  R0,3,S.TSBUFFL                                                   
         LA    R0,SORTREC                                                       
         ST    R0,S.TSAREC                                                      
         MVC   S.TSBUFFA,ASBUFF    Set A(sort buffer)                           
         GOTOR TSAR,S.TSARD                                                     
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   PLONOFF,PLOFF       Test running off-line                        
         JNE   *+10                                                             
         MVC   ASBUFF,S.TSBUFFA    Yes - save a(sort buffer)                    
         MVI   S.TSACTN,TSAADD     Set action to add                            
         LHI   R0,1                Current record (header) is 1                 
                                                                                
ACUPF04  GOTOR DATAMGR,FWDMCB,(0,READ)                                          
         CLI   8(R1),0             Test record read                             
         JE    ACUPF06                                                          
         TM    8(R1),IOEEOF        Test end of file                             
         JNZ   ACUPF08                                                          
         DC    H'0'                Die if disk/format error                     
                                                                                
ACUPF06  MVC   SORTKEY,FW_KEY      Set record key                               
         AHI   R0,1                Bump record number                           
         STCM  R0,3,SORTSEQ#       Set sequence to make key unique              
         STCM  R0,3,SORTREC#       Set record number (for random read)          
         GOTOR TSAR,S.TSARD        Add record to sort file                      
         JE    ACUPF04             Get next input file record                   
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* On-line - build a list of record numbers to read in the TIA         *         
* (this is because ADDTRN uses the second MINIO buffer)               *         
***********************************************************************         
                                                                                
ACUPF08  XC    SORTKEY,SORTKEY     Clear sort record key for read high          
         MVI   S.TSACTN,TSARDH     Set action to read high                      
         CLI   PLONOFF,PLON        Test running on-line                         
         JNE   ACUPNEXT            No                                           
         L     R2,ANEXT#           R2=A(list of record numbers)                 
                                                                                
ACUPF10  XC    0(2,R2),0(R2)       Set end of record number list                
         GOTOR TSAR,S.TSARD        Read first/next sorted file record           
         TM    S.TSERRS,TSEEOF     Test all input records processed             
         JNZ   ACUPNEXT                                                         
         MVI   S.TSACTN,TSANXT     Set action to read next                      
         MVC   0(2,R2),SORTREC#    Set record number in list                    
         AHI   R2,2                Bump to next record number                   
         J     ACUPF10                                                          
         EJECT                                                                  
***********************************************************************         
* Get next record from sort buffer and read FACWRK record randomly    *         
***********************************************************************         
                                                                                
ACUPNEXT CLI   PLONOFF,PLOFF       Test running off-line                        
         JNE   ACUPN02                                                          
         CLI   PLSORT,PLON         Test running with SORTER buff passed         
         JNE   ACUPN01             No                                           
         GOTO1 VSORTER,DMCB,SORTGET                                             
         ICM   R7,15,4(R1)                                                      
         JNZ   ACUPN04                                                          
         GOTO1 VSORTER,DMCB,SORTEND                                             
         J     ACUPLAST                                                         
                                                                                
ACUPN01  GOTOR TSAR,S.TSARD        Read first/next sorted file record           
         TM    S.TSERRS,TSEEOF     Test all input records processed             
         JNZ   ACUPLAST                                                         
         MVI   S.TSACTN,TSANXT     Set action to read next                      
         L     RE,FWAREC           Build key for random read                    
         XC    0(2,RE),0(RE)                                                    
         MVC   2(2,RE),SORTREC#                                                 
         GOTOR DATAMGR,FWDMCB,(0,RANDOM)                                        
         CLI   8(R1),0             Test for errors                              
         JE    ACUPN04                                                          
         DC    H'0'                Die if disk/format error                     
                                                                                
***********************************************************************         
* Get next record randomly from FACWRK recovery file                  *         
***********************************************************************         
                                                                                
ACUPN02  L     R2,ANEXT#           Read next input record randomly              
         OC    0(2,R2),0(R2)       Test end of record number list               
         JZ    ACUPLAST            Yes                                          
         L     RE,FWAREC           Build key for random read                    
         XC    0(2,RE),0(RE)                                                    
         MVC   2(2,RE),0(R2)                                                    
         AHI   R2,2                Bump to next record number in list           
         ST    R2,ANEXT#           Set A(next record number)                    
         GOTOR DATAMGR,FWDMCB,(0,RANDOM)                                        
         CLI   8(R1),0             Test for errors                              
         JE    ACUPN04                                                          
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* Go to appropriate record handling routine                           *         
***********************************************************************         
                                                                                
ACUPN04  LA    R2,RECTAB           Locate record properties                     
         USING RECTABD,R2                                                       
         LA    RE,RECTABL                                                       
         LA    RF,((RECTABL*RECTABN)-1)(R2)                                     
         CLC   FW_FILE,RECTFIL                                                  
         JE    *+10                                                             
         JXLE  R2,RE,*-10                                                       
         DC    H'0'                                                             
         OC    TODO,RECTTDO                                                     
         CLI   RECTTDO,TODOMEDQ    Media record?                                
         JE    *+12                                                             
         CLI   RECTTDO,TODOLOKQ    Lock record?                                 
         JNE   ACUPN04C                                                         
         TM    TODO,TODOACCQ       Any accounting records?                      
         JZ    ACUPN04A                                                         
         GOTOR ATRNLAST            Last time call to ADDTRN handler             
         GOTOR TIMRLAST            Last time call to TIMREC handler             
*&&UK*&& GOTOR PLDRLAST            Last time call to PLDREC handler             
         GOTOR UPDPAS              Update passive pointers                      
         GOTOR CHKPER              Check person record for locks                
         NI    TODO,FF-TODOACCQ    Account records now done                     
                                                                                
ACUPN04A CLI   RECTTDO,TODOLOKQ    Lock record?                                 
         JNE   ACUPN04C                                                         
*&&UK                                                                           
         TM    TODO,TODOMEDQ       Any media records?                           
         JZ    ACUPN04C                                                         
         GOTOR UPDMPA              Update media passive pointers                
         NI    TODO,FF-TODOMEDQ    Media records now done                       
*&&                                                                             
ACUPN04C MVI   GOTOSYS,0           Clear go to system                           
         SR    R1,R1                                                            
         ICM   R1,3,RECTDSY        SENUM displacement or 0                      
         JZ    ACUPN22                                                          
         LA    R1,WORKD(R1)                                                     
         MVC   GOTOSYS,0(R1)       Go to this system                            
                                                                                
         CLI   PLONOFF,PLOFF       Test online/offline                          
         JE    ACUPN10                                                          
         CLC   GOTOSYS,CURRSYS     Online - already switched?                   
         JE    ACUPN22                                                          
         GOTOR SWITCH,PARM,(GOTOSYS,AEFFS),0                                    
         CLI   4(R1),0                                                          
         JE    ACUPN06                                                          
         CLI   4(R1),2             Can't switch to system                       
         JNE   *+6                                                              
         DC    H'0'                                                             
         GOTOR SWITCH,PARM,(1,AEFFS),0                                          
         DC    H'0'                                                             
ACUPN06  L     R2,SRPAR1                                                        
         L     RE,VSELIST-SYSFACD(R2)                                           
         LH    R0,0(RE)                                                         
         L     R1,2(RE)                                                         
         LA    RE,6(RE)                                                         
         USING SELISTD,RE                                                       
         CLC   SESYS,GOTOSYS       Match system number to list                  
         JE    *+10                                                             
         JXLE  RE,R0,*-10                                                       
         DC    H'0'                Invalid system                               
         TM    SEIND,SEISETRO+SEIRONLY                                          
         JZ    ACUPN20             System is up and updative                    
         DC    H'0'                System in read-only mode                     
                                                                                
ACUPN10  L     R1,ATSYS            Set SENUM                                    
         MVC   0(L'GOTOSYS,R1),GOTOSYS                                          
                                                                                
ACUPN20  MVC   CURRSYS,GOTOSYS     Remember latest system                       
ACUPN22  LLH   RE,RECTROU                                                       
         A     RE,ASRUPD60                                                      
         BR    RE                  Now process record                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Last time code                                                      *         
***********************************************************************         
                                                                                
ACUPLAST TM    TODO,TODOACCQ       Any accounting records?                      
         JZ    ACUPL010                                                         
         GOTOR ATRNLAST            Last time call to ADDTRN handler             
         GOTOR TIMRLAST            Last time call to TIMREC handler             
*&&UK*&& GOTOR PLDRLAST            Last time call to PLDREC handler             
         GOTOR UPDPAS              Update passive pointers                      
         GOTOR CHKPER              Check person record for locks                
         NI    TODO,FF-TODOACCQ    Account records now done                     
ACUPL010 TM    FWFLAG,FW_FKFWK     Test don't purge =FWK file                   
         JNZ   EXITY                                                            
         CLI   PLSORT,PLON         Test running with SORTER buff passed         
         JE    EXITY               Yes                                          
         GOTOR DATAMGR,FWDMCB,PURGE                                             
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ADDTRN record handler                                               *         
***********************************************************************         
                                                                                
ATRN     CLI   FW_ACT,FW_AAHDR     Test header record                           
         JNE   ATRN0020                                                         
         TM    ATRNIND,ATRNIFST    Test first time                              
         JNZ   *+2                 No - record out of sequence                  
         OI    ATRNIND,ATRNIFST    Set first time flag                          
*                                                                               
K        USING LDGRECD,DMKEY                                                    
         MVC   K.LDGKEY,SPACES     Read and lock directory                      
         MVC   K.LDGKCPY,CPY           for ledger SJ                            
         MVC   K.LDGKUNT(2),=C'SJ'       for all updative runners               
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR GODMGR,DMCB,(X'80',DMRDHI),ACCDIR,K.LDGKEY,K.LDGKEY              
         CLC   K.LDGKEY(LDGKEND),DMKEYSAV                                       
         JNE   *+2                                                              
         MVC   DMKEYSAV,K.LDGKEY   Save key of record we read                   
*                                                                               
         CLI   CALOVRLY,FW_BINVS   Invoices upload                              
         JE    ATRN0002                                                         
         CLI   CALOVRLY,FW_WRKRF   Worker file update                           
         JE    ATRN0002                                                         
         CLI   CALOVRLY,FW_MXBRV   Billing reversals                            
         JNE   ATRN0010                                                         
         DROP  K                                                                
*                                                                               
K        USING LDGRECD,DMKEY                                                    
ATRN0002 MVC   K.LDGKEY,SPACES     Build key of supplier ledger                 
         MVC   K.LDGKCPY,CPY                                                    
         MVC   K.LDGKUNT(L'CPYSUPP),FW_CPYSU                                    
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR GODMGR,DMCB,DMRDHI,ACCDIR,K.LDGKEY,K.LDGKEY                      
         CLC   K.LDGKEY(LDGKEND),DMKEYSAV                                       
         JNE   *+2                                                              
         MVC   DMKEYSAV,K.LDGKEY   Save key of record we read                   
                                                                                
         MVC   DMDA,K.LDGKDA                                                    
         DROP  K                                                                
                                                                                
         XC    IN_GINSQ,IN_GINSQ                                                
         XC    IN_GIHSQ,IN_GIHSQ                                                
         L     R2,AIO7                                                          
         USING LDGRECD,R2          R2=A(ledger record)                          
         GOTOR GODMGR,DMCB,('RUP',GETREC),ACCMST,DMDA,LDGRECD,DMWORK            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    RF,LDGRFST                                                       
         USING FFTELD,RF                                                        
ATRN0004 CLI   FFTEL,0             Test end of record                           
         JE    ATRN0010                                                         
         CLI   FFTEL,FFTELQ                                                     
         JE    ATRN0008                                                         
ATRN0006 LLC   R0,FFTLN                                                         
         AR    RF,R0                                                            
         J     ATRN0004                                                         
                                                                                
ATRN0008 CLI   FFTTYPE,FFTTHGIN                                                 
         JNE   ATRN0006                                                         
         ST    RF,SAVEELE                                                       
         ICM   R1,15,FFTDATA       Highest allocation group number              
         C     R1,=X'7FFFFFFF'                                                  
         JL    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,IN_GINSQ      Save highest group inv number                
         DROP  RF                                                               
*                                                                               
ATRN0010 MVC   TRNCOMF,ACOMFACS    Set A(COMFACS)                               
         MVC   TRNACC,AIO1         Set A(Account buffer)                        
         MVC   TRNOFA,AIO2         Set A(Office buffer)                         
         MVC   TRNCAC,AIO3         Set A(Contra-account buffer)                 
         MVC   TRNBUK,AIO4         Set A(Bucket buffer)                         
         MVC   TRNREC,AIO5         Set A(transaction record)                    
         MVI   TRNMODE,TRNMONLN                                                 
         CLI   PLONOFF,PLON        Test on-line                                 
         JE    *+8                                                              
         MVI   TRNMODE,TRNMOFLN                                                 
         OI    TRNMODE,TRNMRNER    Running under Runner                         
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL        Set A(P&L buffer)                            
         MVI   TRNINDS,TRNICONV    Set converted records                        
         OI    TRNINDS2,TRNIADDG                                                
         GOTOR DATCON,DMCB,(15,0),(2,TRNEFDT)                                   
         CLC   ADDTRN,FZERO        Test A(ADDTRN) resolved                      
         JNE   ACUPNEXT            Yes                                          
         GOTOR GETPHS,PARM,('QADDTRN',ADDTRNF)                                  
         J     ACUPNEXT                                                         
                                                                                
ATRN0020 TM    ATRNIND,ATRNIFST    Can't be first time                          
         JZ    *+2                                                              
         CLI   FW_ACT,FW_AATRN     Test transaction record                      
         JNE   ATRN0050                                                         
         MVC   TRNINDS,FW_ATTI     Set transaction values                       
         OI    TRNINDS,TRNICONV    Set converted records                        
         MVC   TRNINDS1,FW_ATTI1                                                
         MVC   TRNINDS2,FW_ATTI2                                                
         OI    TRNINDS2,TRNIADDG                                                
         MVC   TRNCACNM,FW_ATCAN                                                
         MVC   TRNBMOS,FW_ATMOA                                                 
         MVC   WORK(L'FW_RKEY),FW_RKEY                                          
         LA    RE,FW_D+FW_ATOLQ                                                 
         MVC   0(L'FW_RKEY,RE),WORK                                             
         L     R0,AIO5             Copy sorted record to IO area 5              
*        LA    R1,IOLENQ           !May extend past end of sort buffer!         
         XR    R1,R1                                                            
         ICM   R1,3,TRNRLEN-TRNKEY(RE)                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   CALOVRLY,FW_BINVS   Invoices upload                              
         JE    ATRN0022                                                         
         CLI   CALOVRLY,FW_WRKRF   Worker file update                           
         JE    ATRN0022                                                         
         CLI   CALOVRLY,FW_MXBRV   Billing reversals                            
         JNE   ATRN0030                                                         
                                                                                
         USING TRNRECD,R2                                                       
ATRN0022 L     R2,AIO5                                                          
         LA    RF,TRNRFST                                                       
         USING GINELD,RF                                                        
ATRN0024 CLI   GINEL,0             Die if you don't find GINELD                 
         JE    *+2                                                              
         CLI   GINEL,GINELQ                                                     
         JE    ATRN0026                                                         
         LLC   R0,GINLN                                                         
         AR    RF,R0                                                            
         J     ATRN0024                                                         
                                                                                
ATRN0026 ICM   R1,15,GININV        Add existing group inv number                
         ICM   R0,15,IN_GINSQ       to sequence on the transaction              
         AR    R1,R0                                                            
         STCM  R1,15,GININV        Save new group inv number on trans           
         CLM   R1,15,IN_GIHSQ                                                   
         JNH   ATRN0030                                                         
         STCM  R1,15,IN_GIHSQ      Save highest group inv used                  
                                                                                
ATRN0030 GOTOR GOATRN,ADDTRND      Call ADDTRN interface                        
         JNE   *+2                                                              
         CLI   CALOVRLY,FW_BEXPN   Expenses upload                              
         JE    ATRN0045                                                         
         CLI   CALOVRLY,FW_MXPAY   Media payments                               
         JE    ATRN0045                                                         
         CLI   CALOVRLY,FW_BSALP   Salary posting upload                        
         JNE   ACUPNEXT                                                         
ATRN0045 L     R1,TRNREC                                                        
         MVC   WORK+L'FW_RKEY(L'FW_RKEY),0(R1) Save real key                    
         GOTOR BLDTRK              Build transaction keys buffer                
         J     ACUPNEXT            Get next FACWRK record                       
                                                                                
ATRN0050 CLI   FW_ACT,FW_AAEND     Test end record                              
         JNE   *+2                                                              
         TM    ATRNIND,ATRNIPUT    Test any records put                         
         JZ    ATRN0060                                                         
         MVI   TRNINDS,TRNILAST+TRNICONV                                        
         OI    TRNINDS2,TRNIUPDG   UPDATE UNIT G                                
         GOTOR ADDTRN,ADDTRND      Yes - call ADDTRN for last time              
         CLI   TRNERRS,0                                                        
         JE    ATRN0060                                                         
         DC    H'0'                                                             
                                                                                
ATRN0060 MVI   ATRNIND,0           Reset indicators                             
                                                                                
         CLI   CALOVRLY,FW_BINVS   Invoices upload                              
         JE    ATRN0065                                                         
         CLI   CALOVRLY,FW_WRKRF   Worker file update                           
         JE    ATRN0065                                                         
         CLI   CALOVRLY,FW_MXBRV   Billing reversals                            
         JNE   ACUPNEXT            Get next FACWRK record                       
                                                                                
ATRN0065 L     R2,AIO7                                                          
         USING LDGRECD,R2          R2=A(ledger record)                          
                                                                                
         OC    IN_GINSQ,IN_GINSQ   Did element exist previously                 
         JZ    ATRN0070            No                                           
         L     RF,SAVEELE          Yes - recall element and update              
         USING FFTELD,RF                                                        
         ICM   R1,15,IN_GIHSQ      Save back the highest number used            
         STCM  R1,15,FFTDATA                                                    
         J     ATRN0075                                                         
                                                                                
N        USING FFTELD,ELEMENT                                                   
ATRN0070 XC    ELEMENT,ELEMENT                                                  
         MVI   N.FFTEL,FFTELQ                                                   
         MVI   N.FFTTYPE,FFTTHGIN                                               
         ICM   R1,15,IN_GIHSQ      Save back the highest number used            
         STCM  R1,15,N.FFTDATA                                                  
         LA    RF,L'IN_GINSQ                                                    
         STC   RF,N.FFTDLEN                                                     
         LA    RF,FFTLN1Q+1(RF)                                                 
         STC   RF,N.FFTLN                                                       
         GOTO1 HELLO,DMCB,(C'P',ACCMST),LDGRECD,N.FFTELD,0                      
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ATRN0075 GOTOR GODMGR,DMCB,PUTREC,ACCMST,DMDA,LDGRECD,DMWORK                    
         JE    ACUPNEXT            Get next FACWRK record                       
         DC    H'0'                                                             
                                                                                
ATRNLAST CLI   ATRNIND,0           Test all done with ADDTRN records            
         BER   RE                                                               
         DC    H'0'                No - 'End' record is missing                 
         EJECT                                                                  
***********************************************************************         
* CAMREC record handler                                               *         
***********************************************************************         
*&&UK                                                                           
                                                                                
CAMR     DS    0H                                                               
                                                                                
         USING M_GEND,R2           R2=A(Record to be put/added)                 
         L     R2,AIO1             Media file record                            
                                                                                
         MVC   M_KEY,FW_RKEY       ??? required ???                             
         XR    R1,R1                                                            
         ICM   R1,B'0011',FW_RLEN                                               
         SHI   R1,FW_RECHL                                                      
         LA    R0,M_RECLEN                                                      
         LA    RE,FW_RREC                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   DMDA,FW_RDA         Set disk address (for GETREC/PUTREC)         
                                                                                
         CLI   FW_ACT,FW_APUTR     Ensure PUTREC requested                      
         JNE   *+2                                                              
                                                                                
         L     R6,AIO6             Init campaign bucket local table             
         XC    0(L'CBVMONTH,R6),0(R6)                                           
                                                                                
* Get ACCELs from MELNK18 campaign change record and add to the buffer          
                                                                                
         USING DACC,R1                                                          
         LA    R1,M_DUMEL                                                       
CAMR0020 LLC   R0,ACCLEN                                                        
         AR    R1,R0                                                            
         CLI   ACCEL,0                                                          
         JE    CAMR0040                                                         
         CLI   ACCEL,ACCELQ                                                     
         JNE   CAMR0020                                                         
         JAS   RE,CAMBA            Add ACCEL values to buffer                   
         JE    CAMR0020                                                         
         DC    H'0'                Already in buffer - shouldn't happen         
         DROP  R1                                                               
                                                                                
O        USING M_GEND,R3                                                        
CAMR0040 L     R3,AIO3                                                          
         GOTOR GODMGR,DMCB,('RUD',GETREC),MEDFIL,DMDA,O.M_GEND,DMWORK           
         JNE   *+2                 Can't get record                             
         CLC   M_KEY,O.M_KEY       Test keys are the same for                   
         JNE   *+2                 PUTREC key change                            
         DROP  R2,O                                                             
                                                                                
         USING M_GEND,R3                                                        
                                                                                
* Get ACCELs from media file campaign record and merge with those               
* from MELNK18 ones in buffer if necessary.  If not in buffer, skip.            
                                                                                
         USING DACC,R1                                                          
         LA    R1,M_DUMEL                                                       
         MVI   BYTE1,0                                                          
CAMR0060 LLC   R0,ACCLEN                                                        
         AR    R1,R0                                                            
         CLI   ACCEL,0                                                          
         JE    CAMR0080                                                         
         CLI   ACCEL,ACCELQ                                                     
         JNE   CAMR0060                                                         
         JAS   RE,CAMBM            Merge ACCEL values (if in buffer)            
         JNE   CAMR0060            Not in buffer, no need to del/add            
         MVI   ACCEL,FF            Will add merged one later, so delete         
         MVI   BYTE1,FF            Remember delete required                     
         J     CAMR0060                                                         
         DROP  R1                                                               
                                                                                
* Delete merged ACCELs from file record                                         
                                                                                
CAMR0080 CLI   BYTE1,FF            Any delete required?                         
         JNE   CAMR0100                                                         
                                                                                
         GOTOR HELLO,DMCB,(C'D',MEDFIL),('FF',M_GEND),0                         
         CLI   12(R1),0                                                         
         JNE   *+2                 Hello/Delete failure (?)                     
                                                                                
* Now loop through the buffer and add a new ACCEL to the file record            
* for each entry (unless costs are all zero)                                    
                                                                                
CAMR0100 L     R6,AIO6                                                          
         USING CAMPVALD,R6                                                      
                                                                                
CAMR0120 OC    CBVMONTH,CBVMONTH   EoT?                                         
         JZ    CAMR0320                                                         
                                                                                
* See ITMF-36340 - missing ACCEL causes DFA job to give high RC from            
* BUCKUP step.  Therefore we must add the new merged element even if            
* the merged values all cancel out to zero.                                     
*                                                                               
*        LA    R1,CBVORNT          Check for NULL scenario                      
*        LHI   R0,CBVNUMQ                                                       
*                                                                               
*AMR0140 CP    0(L'CBVORNT,R1),=P'0'                                            
*        JNE   CAMR0160                                                         
*        AHI   R1,L'CBVORNT                                                     
*        JCT   R0,CAMR0140                                                      
*        J     CAMR0300            Skip if so                                   
                                                                                
CAMR0160 XC    ELEMENT,ELEMENT     Build new ACCEL element                      
         LA    R4,ELEMENT                                                       
         USING DACC,R4                                                          
         MVI   ACCEL,ACCELQ                                                     
         MVC   ACCYRMON,CBVMONTH                                                
         LA    R2,CBVORNT          Values to store from buffer                  
         LHI   R0,ACCNBUCK         Number of fields                             
         LA    R1,ACCBUCKS         Binary values                                
         LA    RF,ACCXTD           Extended values  (also current EoE)          
                                                                                
CAMR0180 DS    0H                  Loop through all 8 values                    
         ZAP   0(8,R2),0(8,R2)     Check value will fit binary field            
         JZ    CAMR0280            Zero anyway, skip to next value              
         JM    CAMR0200                                                         
         ZAP   DUB,=P'2147483647'  Biggest positive value                       
         CP    0(8,R2),DUB                                                      
         JH    CAMR0240            Won't fit, need extended value               
         J     CAMR0220                                                         
                                                                                
CAMR0200 ZAP   DUB,=P'-2147483648' Biggest negative value                       
         CP    0(8,R2),DUB                                                      
         JL    CAMR0240            Won't fit, need extended value               
                                                                                
CAMR0220 ZAP   DUB,0(8,R2)         Save to binary field                         
         CVB   RE,DUB                                                           
         STCM  RE,B'1111',0(R1)    Binary field                                 
         J     CAMR0280                                                         
                                                                                
CAMR0240 ZAP   0(L'ACCXTD,RF),0(8,R2)  Save to extended field (decimal)         
         LA    RE,ACCNBUCK+1                                                    
         SR    RE,R0               RE=field#                                    
         STC   RE,BYTE1            Squeeze it into high-order nibble            
         PACK  BYTE1,BYTE1                                                      
         MVZ   0(1,RF),BYTE1                                                    
         LA    RF,L'ACCXTD(RF)     This next bit below is nonsense!             
         XI    DUB+7,X'01'         Reverse sign                                 
         AP    DUB,0(8,R2)         Get remainder to store (why oh why?)         
         SR    RE,RE               Stores 0.00 if not within XL4 range!         
         CP    DUB,=P'2147483647'                                               
         JH    CAMR0260                                                         
         CP    DUB,=P'-2147483648'                                              
         JL    CAMR0260                                                         
         CVB   RE,DUB                                                           
                                                                                
CAMR0260 STCM  RE,B'1111',0(R1)    Rest of decimal in binary field too          
                                                                                
CAMR0280 AHI   R1,4                Bump to next value                           
         AHI   R2,L'CBVORNT                                                     
         JCT   R0,CAMR0180                                                      
                                                                                
         SR    RF,R4               Set element length (RF=EoL)                  
         STC   RF,ACCLEN                                                        
                                                                                
         GOTO1 HELLO,DMCB,(C'P',MEDFIL),M_GEND,ACCEL,0                          
         CLI   12(R1),0                                                         
         JNE   *+2                 Hello/Put failure - too big ?                
         DROP  R4                                                               
                                                                                
CAMR0300 AHI   R6,CAMPVALQ                                                      
         J     CAMR0120                                                         
         DROP  R6                                                               
                                                                                
* Finally put back the campaign record                                          
                                                                                
CAMR0320 GOTOR GODMGR,DMCB,PUTREC,MEDFIL,DMDA,M_KEY,DMWORK                      
         JNE   *+2                 Can't put record                             
                                                                                
CAMRX    DS    0H                                                               
         J     ACUPNEXT            Get next FACWRK record                       
         DROP  R3                                                               
                                                                                
* -----------                                                                   
* Subroutine:                                                                   
* -----------                                                                   
* Unpack campaign ACCEL and add to buffer OR merge with existing entry          
* R1=A(campaign ACCEL to add)      Buffer is in AIO6                            
* CAMBA - add, MELNK18 rec, exits NE if already there (caller dies)             
* CAMBM - merge, file rec, exits NE if NOT already there (caller skips)         
                                                                                
CAMBA    MVI   WORK,C'A'           Call to add                                  
         J     *+8                                                              
CAMBM    MVI   WORK,C'M'           Call to merge with existing entry            
         NTR1  ,                                                                
                                                                                
* Unpack campaign ACCEL element into WORK+1 (CAMPVALD)                          
                                                                                
         LR    R4,R1                                                            
         USING DACC,R4                                                          
         XC    WORK+1(L'WORK-1),WORK+1   Leave call flag in WORK(1)             
W        USING CAMPVALD,WORK+1                                                  
                                                                                
         CLI   ACCEL,ACCELQ                                                     
         JNE   *+2                                                              
                                                                                
         MVC   W.CBVMONTH,ACCYRMON   Set year/month                             
         LA    RE,W.CBVORNT                                                     
         LHI   R0,CBVNUMQ                                                       
         LA    RF,ACCBUCKS                                                      
                                                                                
CAMB0020 ICM   R1,B'1111',0(RF)                                                 
         CVD   R1,DUB                                                           
         ZAP   0(L'CBVORNT,RE),DUB                                              
         AHI   RE,L'CBVORNT                                                     
         AHI   RF,L'ACCBUCKS                                                    
         JCT   R0,CAMB0020                                                      
                                                                                
         LLC   R0,ACCLEN           Extended values                              
         SHI   R0,ACCLENQ                                                       
         JNP   CAMB0060            None present                                 
         LA    R1,ACCXTD                                                        
                                                                                
CAMB0040 LLC   RE,0(R1)                                                         
         SRL   RE,4                Field#                                       
         BCTR  RE,0                                                             
         SLL   RE,3                x8 for disp into table (CBVORNT)             
         LA    RE,W.CBVORNT(RE)                                                 
         ZAP   DUB,0(L'ACCXTD,R1)  PL7    #NNNNNNNNNNNNS                        
         NI    DUB+1,X'0F'         PL8  000NNNNNNNNNNNNS                        
         ZAP   0(L'CBVORNT,RE),DUB Replace table val with extended val          
         AHI   R1,L'ACCXTD         Bump to next extended value                  
         SHI   R0,L'ACCXTD                                                      
         JP    CAMB0040            Next extended field if any                   
         DROP  R4                                                               
                                                                                
* Insert WORK entry into buffer or add to existing buffer entry                 
                                                                                
CAMB0060 L     R6,AIO6             Look for WORK element in buffer              
         LA    R0,IOLENQ-CAMPVALQ(R6)  Max EoT                                  
B        USING CAMPVALD,R6                                                      
CAMB0080 OC    B.CBVMONTH,B.CBVMONTH                                            
         JZ    CAMB0100            Not found                                    
         CLC   B.CBVMONTH,W.CBVMONTH                                            
         JE    CAMB0120            Found                                        
         AHI   R6,CAMPVALQ                                                      
         CR    R6,R0                                                            
         JL    CAMB0080                                                         
         DC    H'0'                Blown IOLENQ (can't happen but...)           
                                                                                
CAMB0100 CLI   WORK,C'A'           Not found - only add if an add call          
         JNE   EXITN                                                            
         MVC   B.CBVMONTH(CAMPVALQ),W.CBVMONTH      Else add new entry          
         XC    B.CBVMONTH+CAMPVALQ(CAMPVALQ),B.CBVMONTH+CAMPVALQ  term          
         J     EXITY                                                            
                                                                                
CAMB0120 CLI   WORK,C'M'           Found - only merge if a merge call           
         JNE   EXITN                                                            
         LA    RE,B.CBVORNT        Add to existing entry                        
         LA    RF,W.CBVORNT                                                     
         LHI   R0,CBVNUMQ                                                       
CAMB0140 AP    0(L'CBVORNT,RE),0(L'CBVORNT,RF)                                  
         AHI   RE,L'CBVORNT                                                     
         AHI   RF,L'CBVORNT                                                     
         JCT   R0,CAMB0140                                                      
         J     EXITY                                                            
                                                                                
         DROP  B,W                                                              
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* TIMREC handler                                                      *         
***********************************************************************         
                                                                                
         USING TT_D,R6             R6=A(Time buffer record)                     
TIMR     L     R6,AIO1                                                          
                                                                                
         CLI   FW_TRREC,FW_TRHDR   Test header record                           
         JNE   TIMR0010                                                         
         TM    TIMRIND,TIMRIFST    Test first time                              
         JNZ   *+2                 No - record out of sequence                  
         OI    TIMRIND,TIMRIFST                                                 
*                                  YES, READ 1R RECORD FOR UPDATE               
K        USING LDGRECD,DMKEY                                                    
         MVC   K.LDGKEY,SPACES     BUILD KEY OF SUPPLIER LEDGER                 
         MVC   K.LDGKCPY,CPY       company code                                 
         MVC   K.LDGKUNT(L'FW_TRULA),FW_TRULA  1R unit ledger                   
         MVC   DMKEYSAV,DMKEY      save key details                             
         GOTOR GODMGR,DMCB,(X'80',DMRDHI),ACCDIR,K.LDGKEY,K.LDGKEY              
         CLC   K.LDGKEY(LDGKEND),DMKEYSAV                                       
         JNE   *+2                                                              
         MVC   DMKEYSAV,K.LDGKEY   Save key of record we read                   
         DROP  K                                                                
*                                                                               
TIMR0002 MVC   HD_1RULA,FW_TRULA   Set time header values                       
         MVC   HD_1RPER,FW_TRPER                                                
         MVC   HD_PEDT,FW_TRPED                                                 
         SR    R0,R0                                                            
         ICM   R0,7,HD_PEDT                                                     
         LNR   R0,R0                                                            
         STCM  R0,7,HD_PEDTC                                                    
         MVC   HD_TSSTO,FW_TRSTO                                                
         MVC   HD_TSSTN,FW_TRSTN                                                
         MVC   HD_PPID#,FW_TRPID                                                
         MVC   HD_SUBLM,FW_TRLDT                                                
         MVC   HD_SUBCL,FW_TRCDT                                                
         MVC   HD_EDHRS,FW_TREDT                                                
         MVC   HD_IND,FW_HDIND                                                  
         GOTOR BUFTIM,DMCB,('TSAINI',OLDBUF)                                    
         GOTOR BUFTIM,DMCB,('TSAINI',NEWBUF)                                    
         LTR   R8,R8               Test on-line                                 
         JZ    ACUPNEXT            Yes - get next FACWRK record                 
         L     RF,LP_ABLK5         Return TSAR blocks to caller                 
         MVC   0(L'TSAROLDT,RF),TSAROLDT                                        
         L     RF,LP_ABLK6                                                      
         MVC   0(L'TSARNEWT,RF),TSARNEWT                                        
         J     ACUPNEXT            Get next FACWRK record                       
                                                                                
TIMR0010 TM    TIMRIND,TIMRIFST    Test first time                              
         JZ    *+2                 Yes - record out of sequence                 
         CLI   FW_TRREC,FW_TROLD   Test old time buffer record                  
         JE    *+12                                                             
         CLI   FW_TRREC,FW_TRNEW   Test new time buffer record                  
         JNE   TIMR0020                                                         
         TM    TIMRIND,TIMRISTA+TIMRIDEL                                        
         JNZ   *+2                                                              
         OI    TIMRIND,TIMRITIM    Set we have time records                     
         LA    R0,TT_D             Copy TT_D                                    
         LHI   R1,TT_RECL                                                       
         LA    RE,FW_TRDTT                                                      
         LHI   RF,TT_LN1Q                                                       
         MVCL  R0,RE                                                            
         GOTOR BLDCLS              Build time cluster                           
         LHI   R0,OLDBUF           Set old time buffer                          
         CLI   FW_TRREC,FW_TROLD                                                
         JE    *+8                                                              
         LHI   R0,NEWBUF           Set new time buffer                          
         GOTOR BUFTIM,DMCB,('TSAADD',(R0))                                      
         J     ACUPNEXT            Get next FACWRK record                       
                                                                                
TIMR0020 CLI   FW_TRREC,FW_TRSTA   Test changing time sheet status              
         JNE   TIMR0030                                                         
         TM    TIMRIND,TIMRITIM+TIMRIDEL                                        
         JNZ   *+2                                                              
         OI    TIMRIND,TIMRISTA    Set to delete time records                   
         J     ACUPNEXT            Get next FACWRK record                       
                                                                                
TIMR0030 CLI   FW_TRREC,FW_TRDEL   Test deleting time records                   
         JNE   TIMR0040                                                         
         TM    TIMRIND,TIMRITIM+TIMRISTA                                        
         JNZ   *+2                                                              
         OI    TIMRIND,TIMRIDEL    Set to delete time records                   
         J     ACUPNEXT            Get next FACWRK record                       
                                                                                
TIMR0040 CLI   FW_TRREC,FW_TREND   Test end of time records                     
         JNE   *+2                                                              
         MVI   TIMRIND,0           Reset indicators                             
         GOTOR UPDTIM              Update time records                          
         MVI   BUFRIND,0           Clear buffer                                 
         J     ACUPNEXT            Get next FACWRK record                       
                                                                                
TIMRLAST CLI   TIMRIND,0           Test all done with time records              
         BER   RE                                                               
         DC    H'0'                No - 'End' record is missing                 
         EJECT                                                                  
***********************************************************************         
* Last time for Time Records                                          *         
***********************************************************************         
                                                                                
CHKPER   NTR1  LABEL=*                                                          
         TM    LP_OFLG1,LP_OFDFT   Test draft mode                              
         JNZ   EXITY                                                            
                                                                                
         GOTOR DATCON,DMCB,(5,0),(1,TODAYP)                                     
         GOTOR DATCON,DMCB,(5,0),(0,WORK)                                       
         GOTOR ADDAY,DMCB,(C'D',WORK),WORK+6,F'-1'                              
         GOTOR DATCON,DMCB,(0,WORK+6),(1,DOVRDTE)                               
                                                                                
         TM    HD_IND,FW_APRQ                 Approval mode?                    
         JO    CKPER300                       Read for approvals                
                                                                                
         TM    HD_IND,FW_SLOCK                Any Submitter Locks?              
         JZ    EXITY                          No - done                         
                                                                                
*                                  Submitter section                            
                                                                                
         MVC   SUB1RACT,HD_1RACT   Save off account for later                   
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,DMKEY            READ FOR PERSON RECORD                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CPY                                                      
         MVC   PERKCODE,HD_1RPER                                                
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY                           
         CLC   PERKEY,DMKEYSAV                                                  
         JNE   CKPERX              Record not found                             
                                                                                
         MVC   DMDA,PERKDA         Set disk address (for GETREC/PUTREC)         
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,DMDA,AIO5,DMWORK                      
         JNE   CKPERX                                                           
                                                                                
         L     R2,AIO5                                                          
         LA    R2,PERRFST-PERRECD(R2)                                           
         USING LOCELD,R2                                                        
CKPER010 CLI   LOCEL,0             End of record                                
         JE    CKPER040                                                         
         CLI   LOCEL,LOCELQ                                                     
         JE    CKPER030                                                         
CKPER020 LLC   RE,LOCLN                                                         
         AR    R2,RE                                                            
         J     CKPER010                                                         
                                                                                
         USING LOCELD,R2                                                        
CKPER030 CLC   TODAYP,LOCSTART   FIND LOCATION FOR CURRENT DATE                 
         JL    CKPER020                                                         
         OC    LOCEND,LOCEND                                                    
         JZ    *+14                                                             
         CLC   TODAYP,LOCEND                                                    
         JH    CKPER020                                                         
         MVC   QSTART,LOCSTART                                                  
         MVC   QEND,LOCEND                                                      
         J     CKPER020                                                         
         DROP  R2                                                               
                                                                                
CKPER040 GOTOR CHKMCS           Get brandocean switch on and off dates          
         JNE   CKPERX                                                           
                                                                                
         LA    R4,BRATAB                                                        
CKPER050 OC    0(L'GDADATE*2,R4),0(R4)                                          
         JZ    CKPER080                                                         
         CLC   0(L'GDADATE,R4),L'GDADATE(R4)                                    
         JNE   CKPER070                                                         
CKPER060 LA    R4,L'GDADATE2+L'GDADATE(R4)                                      
         J     CKPER050                                                         
                                                                                
CKPER070 CLC   0(L'GDADATE,R4),QSTART    Brndocn on date after loc strt         
         JL    CKPER060            Yes                                          
         MVC   QSTART,0(R4)        Set location start date as start             
         J     CKPER080                                                         
                                                                                
CKPER075 CLC   QSTART(1),TODAYP    Do not exceed this year.                     
         JNL   CKPER900            If we've gone thru all years-Done            
         GOTOR DATCON,DMCB,(1,QSTART),(0,WORK)                                  
         GOTOR ADDAY,DMCB,(C'Y',WORK),WORK+6,F'1'                               
         GOTOR DATCON,DMCB,(0,WORK+6),(1,QSTART)                                
         MVC   QSTART+1(2),=X'0101'                                             
                                                                                
CKPER080 GOTOR CALPRDS             Get period end dates into Genarea            
         JNE   CKPERX                                                           
         GOTOR RDEDT               Read Daily Edit Hours                        
                                                                                
         USING CALTABD,R4                                                       
         L     R4,AGENAREA                                                      
CKPER090 CLI   0(R4),0             Any dates left                               
         JE    CKPER200            No - you are done                            
         MVI   RUNINDS,0           Init Runidicators                            
         USING TSWRECD,R3                                                       
         LA    R3,DMKEY            Build key for reading timesheets             
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CPY                                                      
         MVC   TSWKPER,HD_1RPER    Add person code                              
         XR    RF,RF                                                            
         ICM   RF,7,CALENDT                                                     
         LNR   RF,RF                                                            
         STCM  RF,7,TSWKEND                                                     
         LLC   RE,ONERL3L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   TSWKODS(0),HD_1RACT Add office dept sub dept                     
         OC    TSWKODS,SPACES                                                   
         MVC   DMKEYSAV(TSWKULC-TSWRECD),TSWRECD Save key                       
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY                           
         JE    CKPER110                                                         
         J     CKPERX                                                           
                                                                                
CKPER100 LA    R3,DMKEY                                                         
         MVC   DMKEY,DMKEYSAV                                                   
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,DMKEY,DMKEY                           
CKPER105 GOTOR DATAMGR,DMCB,DMRSEQ,ACCDIR,DMKEY,DMKEY                           
CKPER110 CLC   DMKEYSAV(TSWKULC-TSWRECD),DMKEY                                  
         JNE   CKPER190            Record not found                             
         CLC   TSWKULC,SPACES      Any contra?                                  
         JNH   CKPER105                                                         
                                                                                
         TM    RUNINDS,RUNEDT      did we already init CALDAYS?                 
         JO    CKPER115                                                         
         GOTOR GETEDT,(R4)         Set up CALDAYS with DAY EDIT HOURS           
         OI    RUNINDS,RUNEDT                                                   
                                                                                
CKPER115 CLC   DOVRDTE,CALSTRT     Check if we are in this period               
         JL    CKPER196                                                         
         CLC   DOVRDTE,CALENDT                                                  
         JH    CKPER196                                                         
                                                                                
         MVC   DMDA,TSWKDA         Set disk address (for GETREC/PUTREC)         
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,DMDA,AIO5,DMWORK                      
         JNE   CKPERX                                                           
                                                                                
         USING TIMRECD,R3                                                       
         L     R3,AIO5                                                          
         USING TIMELD,R2                                                        
         LA    R2,TIMRFST                                                       
         MVI   BYTE1,0                                                          
CKPER120 CLI   TIMEL,0                                                          
         JE    CKPER100            Else - check next line                       
         CLI   TIMEL,TIMELQ        X'8B' - Time Element                         
         JNE   CKPER130                                                         
         CLI   TIMETYP,TIMETIME    TIMETIME type                                
         JE    CKPER140                                                         
CKPER130 SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R2,R1                                                            
         J     CKPER120                                                         
                                                                                
CKPER140 LR    R0,R2                                                            
         SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R0,R1               Get end of ELM                               
         LA    R1,TIMEDAY                                                       
         USING TIMEDAY,R1                                                       
CKPER150 CR    R0,R1               At the end of the ELM?                       
         JNH   CKPER130                                                         
                                                                                
         USING CALDAYD,RE                                                       
         LA    RE,CALDAYS                                                       
CKPER160 CLI   CALDAY#,FF          End of table?                                
         JE    CKPER180                                                         
         CLI   CALDAY#,0                                                        
         JE    CKPER180                                                         
         CLC   CALDAY,TIMETDTE     Find right dates                             
         JNE   CKPER170                                                         
         SP    CALHRS,TIMEHRS                                                   
         JP    *+10                                                             
         ZAP   CALHRS,=P'0'                                                     
         J     CKPER180                                                         
CKPER170 LA    RE,CALLNQ(RE)                                                    
         J     CKPER160                                                         
                                                                                
CKPER180 LA    R1,L'TIMEDAY(R1)                                                 
         J     CKPER150                                                         
         DROP  R1,RE                                                            
                                                                                
         USING CALDAYD,RE                                                       
CKPER190 LA    RE,CALDAYS                                                       
         CLI   CALDAY#,0           Anything in table?                           
         JE    CKPER200            missing period                               
CKPER192 CP    CALHRS,=P'0'                                                     
         JE    CKPER194                                                         
         CLC   CALDAY,DOVRDTE      Do we have hours left?                       
         JNH   CKPERX              Yes - do not unlock                          
CKPER194 LA    RE,CALLNQ(RE)                                                    
         CLI   CALDAY#,FF          End of Table?                                
         JE    CKPER196                                                         
         CLI   CALDAY#,0                                                        
         JNE   CKPER192                                                         
                                                                                
CKPER196 LA    R4,CALTABL(R4)      Bump to next period                          
         J     CKPER090            Build key for next period                    
         DROP  R2,R3,RE                                                         
                                                                                
CKPER200 CLI   0(R4),0                                                          
         JE    *+14                End of periods - must be good                
         CLC   CALENDT,DOVRDTE     Were we processing a timesheet?              
         JL    CKPERX                                                           
         J     CKPER075                                                         
                                                                                
*  APPROVER PID SECTION                                                         
                                                                                
CKPER300 XC    APPRPID,APPRPID                                                  
                                                                                
         USING DPAPASD,R3                                                       
         LA    R3,DMKEY                                                         
         XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,CPY                                                      
         MVI   DPAPAPPL,DPAPATIM                                                
         MVC   DPAP1RAC,HD_1RACT                                                
         ZAP   DPAPXVAL,=P'0'                                                   
         MVC   DMKEYSAV,DMKEY                                                   
*                                                                               
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY,0                         
         JE    CKPER310                                                         
         DC    H'0'                                                             
*                                                                               
CKPER310 CLC   DPAPAS(DPAPPIDB-DPAPASD),DMKEYSAV                                
         JNE   CKPER320                                                         
         MVC   APPRPID,DPAPPIDB                                                 
         J     CKPER380                                                         
*                                                                               
CKPER320 XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,CPY                                                      
         MVC   DPAPAPPL,DMKEYSAV+DPAPAPPL-DPAPAS                                
         LLC   RE,ONERL3L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   DPAP1RAC(0),HD_1RACT                                             
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,=P'0'                                                   
         MVC   DMKEYSAV,DMKEY                                                   
*                                                                               
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY,0                         
         JE    CKPER330                                                         
         DC    H'0'                                                             
*                                                                               
CKPER330 CLC   DPAPAS(DPAPPIDB-DPAPASD),DMKEYSAV                                
         JNE   CKPER340                                                         
         MVC   APPRPID,DPAPPIDB                                                 
         J     CKPER380                                                         
*                                                                               
CKPER340 XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,CPY                                                      
         MVC   DPAPAPPL,DMKEYSAV+DPAPAPPL-DPAPAS                                
         LLC   RE,ONERL2L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   DPAP1RAC(0),HD_1RACT                                             
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,=P'0'                                                   
         MVC   DMKEYSAV,DMKEY                                                   
*                                                                               
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY,0                         
         JE    CKPER350                                                         
         DC    H'0'                                                             
*                                                                               
CKPER350 CLC   DPAPAS(DPAPPIDB-DPAPASD),DMKEYSAV                                
         JNE   CKPER360                                                         
         MVC   APPRPID,DPAPPIDB                                                 
         J     CKPER380                                                         
*                                                                               
CKPER360 XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,CPY                                                      
         MVC   DPAPAPPL,DMKEYSAV+DPAPAPPL-DPAPAS                                
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   DPAP1RAC(0),HD_1RACT                                             
         OC    DPAP1RAC,SPACES                                                  
         ZAP   DPAPXVAL,=P'0'                                                   
         MVC   DMKEYSAV,DMKEY                                                   
*                                                                               
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY,0                         
         JE    CKPER370                                                         
         DC    H'0'                                                             
*                                                                               
CKPER370 CLC   DPAPAS(DPAPPIDB-DPAPASD),DMKEYSAV                                
         JNE   CKPERX                                                           
         MVC   APPRPID,DPAPPIDB                                                 
                                                                                
         USING PIDRECD,R3                                                       
CKPER380 LA    R3,DMKEY                                                         
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CPY                                                      
         MVC   PIDKPID,APPRPID                                                  
         MVI   PIDKSTYP,PIDKPERQ                                                
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY                           
         JNE   CKPERX                                                           
         CLC   PIDKEY(PIDKPER-PIDKEY),DMKEYSAV                                  
         JNE   CKPERX              Record not found                             
                                                                                
         MVC   DMDA,PIDKDA         Set disk address (for GETREC/PUTREC)         
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,DMDA,AIO5,DMWORK                      
         JNE   CKPERX                                                           
                                                                                
         USING PERRECD,R3                                                       
         L     R3,AIO5                                                          
         LA    R2,PERRFST                                                       
         USING LOCELD,R2                                                        
CKPER390 CLI   LOCEL,0                                                          
         JE    CKPER420                                                         
         CLI   LOCEL,LOCELQ                                                     
         JE    CKPER410                                                         
CKPER400 XR    R1,R1                                                            
         IC    R1,LOCLN                                                         
         AR    R2,R1                                                            
         J     CKPER390                                                         
                                                                                
CKPER410 CLC   LOCSTART,TODAYP     Find current location                        
         JH    CKPER400                                                         
         OC    LOCEND,LOCEND                                                    
         JZ    *+14                                                             
         CLC   LOCEND,TODAYP                                                    
         JH    CKPER400                                                         
         MVC   APP1RACT,SPACES                                                  
                                                                                
         LA    RF,APP1RACT         Fill out APProvers 1R account                
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),LOCOFF      Move in Office                               
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LLC   R1,ONERL1L                                                       
         LLC   RE,ONERL2L                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),LOCDEPT     Move in Department                           
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LLC   R1,ONERL2L                                                       
         LLC   RE,ONERL3L                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),LOCSUB      Move in Sub-Department                       
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         LLC   R1,ONERL3L                                                       
         LLC   RE,ONERL4L                                                       
         SR    RE,R1                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   0(0,RF),PERKCODE    Move in person                               
         J     CKPER400                                                         
         DROP  R2,R3                                                            
                                                                                
         USING ACTRECD,R2                                                       
CKPER420 LA    R2,DMKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CPY                                                      
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,APP1RACT                                                 
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,DMKEY,DMKEY                           
         JNE   CKPERX                                                           
         MVC   DMDA,ACTKDA         Set disk address (for GETREC/PUTREC)         
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,DMDA,AIO5,DMWORK                      
         JNE   CKPERX                                                           
                                                                                
         L     R2,AIO5                                                          
         LA    R4,ACTRFST                                                       
         USING RSTELD,R4                                                        
         XR    R1,R1                                                            
CKPER430 CLI   RSTEL,0                                                          
         JE    CKPER460                                                         
         CLI   RSTEL,RSTELQ                                                     
         JE    CKPER450                                                         
CKPER440 IC    R1,RSTLN                                                         
         AR    R4,R1                                                            
         J     CKPER430                                                         
                                                                                
CKPER450 CLI   RSTLN,RSTLN3Q       Make sure that element is big enough         
         JL    CKPER440                                                         
         TM    RSTSTAT7,RSTLCKAP   Is the approver locked?                      
         JNO   CKPERX                                                           
                                                                                
         USING APPRECD,R3                                                       
CKPER460 LA    R3,DMKEY                                                         
         XC    APPKEY,APPKEY                                                    
         MVC   APPKCPY,CPY                                                      
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKPIDB,APPRPID                                                 
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY                           
         JNE   CKPERX                                                           
         CLC   DMKEYSAV(APPKSEQ-APPRECD),DMKEY                                  
         JNE   CKPERX              Record not found                             
                                                                                
         MVC   DMDA,APPKDA         Set disk address (for GETREC/PUTREC)         
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,DMDA,AIO5,DMWORK                      
         JNE   CKPERX                                                           
                                                                                
         LA    R1,PERTAB                                                        
         XC    0(PERTABX-PERTAB,R1),0(R1)                                       
         LA    R1,SJATAB                                                        
         XC    0(SJATABX-SJATAB,R1),0(R1)                                       
         LA    R1,NCATAB                                                        
         XC    0(NCATABX-NCATAB,R1),0(R1)                                       
                                                                                
         L     R3,AIO5                                                          
         LA    R2,APPRFST                                                       
         USING LIDELD,R2                                                        
CKPER470 CLI   LIDEL,0                                                          
         JE    CKPER510                                                         
         CLI   LIDEL,LIDELQ                                                     
         JE    CKPER490                                                         
CKPER480 XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R2,R1                                                            
         J     CKPER470                                                         
                                                                                
CKPER490 SR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         AR    R1,R2                                                            
         ST    R1,AENDLID          End of LIDELD                                
                                                                                
         LA    R1,NCATAB                                                        
         CLI   LIDTYPE,LIDTAP1N    1N Account Approver?                         
         JE    CKPER504                                                         
         LA    R1,SJATAB                                                        
         CLI   LIDTYPE,LIDTAPSJ    SJ Account Approver?                         
         JE    CKPER504                                                         
         LA    R1,PERTAB                                                        
         CLI   LIDTYPE,LIDTAP1R    Do we have people to approve?                
         JNE   CKPER480                                                         
                                                                                
CKPER500 LA    R4,LIDDATA          Start of list                                
         USING LIDDATA,R4                                                       
                                                                                
CKPER501 TM    LIDAPDTY,LIDAPDTI   Is this entry for TIME                       
         JNO   CKPER503            No                                           
                                                                                
CKPER502 CLI   0(R1),FF                                                         
         JE    CKPER480                                                         
         CLC   0(L'LIDAPACC,R1),SPACES                                          
         JNH   *+12                                                             
         LA    R1,L'LIDAPACC(R1)                                                
         J     CKPER502                                                         
         MVC   0(L'LIDAPACC,R1),LIDAPACC Move in account                        
         OC    0(L'LIDAPACC,R1),SPACES                                          
CKPER503 SR    R0,R0                                                            
         IC    R0,LIDITLN                                                       
         AR    R4,R0                                                            
         C     R4,AENDLID                                                       
         JNL   CKPER480            End of list                                  
         J     CKPER501                                                         
         DROP  R4                                                               
                                                                                
CKPER504 LA    R4,LIDDATA          Start of list                                
         USING LIDDATA,R4                                                       
                                                                                
CKPER505 TM    LIDAPDTY,LIDAPDTI   Is this entry for TIME                       
         JNO   CKPER507            No                                           
                                                                                
CKPER506 CLI   0(R1),FF                                                         
         JE    CKPER480                                                         
         CLC   0(L'LIDASJAC,R1),SPACES                                          
         JNH   *+12                                                             
         LA    R1,L'LIDASJAC(R1)                                                
         J     CKPER506                                                         
         LA    RE,LIDASJAC         Displacement for SJ and 1N are same          
         MVC   0(L'LIDASJAC,R1),0(RE)    Move in account                        
         OC    0(L'LIDASJAC,R1),SPACES                                          
CKPER507 SR    R0,R0                                                            
         IC    R0,LIDITLN                                                       
         AR    R4,R0                                                            
         C     R4,AENDLID                                                       
         JNL   CKPER480            End of list                                  
         J     CKPER505                                                         
         DROP  R4                                                               
                                                                                
CKPER510 LA    R4,NCATAB                                                        
         MVC   CURUL,=C'1N'                                                     
         J     CKPER530                                                         
CKPER520 LA    R4,SJATAB                                                        
         MVC   CURUL,=C'SJ'                                                     
CKPER530 CLI   0(R4),FF                                                         
         JE    *+14                                                             
         CLC   0(L'LIDASJAC,R4),SPACES                                          
         JH    CKPER540                                                         
         CLC   CURUL,=C'SJ'        Are we already doing SJ?                     
         JNE   CKPER520            Not Yet - Do it now                          
         J     CKPER660                                                         
                                                                                
CKPER540 CLC   CURUL,=C'SJ'        Are we doing SJ?                             
         JNE   CKPER580                                                         
         MVC   SVCLIOFF,SPACES     Always init saved area for office            
                                                                                
         USING ACTRECD,R3                                                       
         LA    R3,DMKEY            Read for client office                       
         MVC   ACTKEY,SPACES       CLEAR KEY                                    
         MVC   ACTKCPY,CPY                                                      
         MVC   ACTKUNT(2),=C'SJ'                                                
         LA    RE,SJALVS+SJALVLQ-1                                              
         ST    RE,FULL1            Start at the lowest and go higher            
CKPER545 L     RE,FULL1                                                         
         LA    R1,SJALVS                                                        
         CR    R1,RE                                                            
         JH    CKPERX              Somethings wrong.  Exit                      
         SR    R1,R1                                                            
         IC    R1,0(RE)            Read Current Level length                    
         AHI   R1,-1                                                            
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         MVC   ACTKACT(0),0(R4)                                                 
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY                           
         JNE   CKPERX                                                           
         CLC   DMKEYSAV(ACTKEND),DMKEY                                          
         JNE   CKPERX              Record not found                             
                                                                                
         MVC   DMDA,ACTKDA         Set disk address (for GETREC/PUTREC)         
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,DMDA,AIO5,DMWORK                      
         JNE   CKPERX                                                           
                                                                                
         L     R3,AIO5                                                          
         LA    R2,ACTRFST                                                       
CKPER550 CLI   0(R2),0             Look for submit date                         
         JE    CKPER570                                                         
         CLI   0(R2),PPRELQ        X'24' - Production Profile Element           
         JE    CKPER560                                                         
CKPER555 XR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     CKPER550                                                         
                                                                                
         USING PPRELD,R2                                                        
CKPER560 MVC   SVCLIOFF,PPRGAOFF                                                
         J     CKPER555                                                         
         DROP  R2                                                               
                                                                                
CKPER570 CLC   SVCLIOFF,SPACES     Did we find an office?                       
         JH    CKPER580                                                         
         L     RE,FULL1                                                         
         AHI   RE,-1               Back up a level                              
         ST    RE,FULL1                                                         
         J     CKPER545                                                         
                                                                                
         USING TSJPASD,R3                                                       
CKPER580 LA    R3,DMKEY                                                         
         XC    TSJPAS,TSJPAS       CLEAR KEY                                    
         MVI   TSJPTYP,TSJPTYPQ    X'3E'                                        
         MVI   TSJPSUB,TSJPSUBQ    x'19'                                        
         MVC   TSJPCPY,CPY                                                      
         MVI   TSJPVIEW,TSJP1NAQ   Assume 1N                                    
         CLC   CURUL,=C'1N'                                                     
         JE    *+8                                                              
         MVI   TSJPVIEW,TSJPSJAQ   Else SJ                                      
         MVC   TSJPCOFF,SVCLIOFF   SJ Client Office                             
         MVC   TSJPACT,0(R4)       Move in 1N/SJ Account                        
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY                           
         JE    CKPER610                                                         
         J     CKPERX                                                           
                                                                                
CKPER590 LA    R3,DMKEY                                                         
         MVC   DMKEY,DMKEYSAV                                                   
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,DMKEY,DMKEY                           
CKPER600 GOTOR DATAMGR,DMCB,DMRSEQ,ACCDIR,DMKEY,DMKEY                           
CKPER610 CLC   DMKEYSAV(TSJPACT-TSJPAS),DMKEY                                   
         JNE   CKPER650            Record not found                             
         LA    RE,DMKEYSAV+(TSJPMED-TSJPASD)-1                                  
         LR    R1,RE                                                            
         CLI   0(RE),C' '                                                       
         JH    *+12                                                             
         AHI   RE,-1                                                            
         J     *-12                                                             
         SR    R1,RE                                                            
         AHI   R1,-1                                                            
         BASR  RE,0                                                             
         EX    R1,4(RE)                                                         
         CLC   DMKEYSAV+(TSJPACT-TSJPASD)(0),DMKEY+(TSJPACT-TSJPASD)            
         JNE   CKPER650                                                         
         TM    TSJPSTAT,TIMSAWAP   Is this awaiting approval?                   
         JNO   CKPER600            No - skip                                    
                                                                                
         MVC   DMDA,TSJPDA         Set disk address (for GETREC/PUTREC)         
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,DMDA,AIO5,DMWORK                      
         JNE   CKPERX                                                           
                                                                                
         USING TIMRECD,R3                                                       
         L     R3,AIO5                                                          
         LA    R2,TIMRFST                                                       
         USING GDAELD,R2                                                        
CKPER620 CLI   GDAEL,0             Look for submit date                         
         JE    CKPER590                                                         
         CLI   GDAEL,GDAELQ                                                     
         JNE   CKPER630                                                         
         CLI   GDATYPE,GDACLSUB    for client approvals                         
         JE    CKPER640                                                         
CKPER630 XR    R1,R1                                                            
         IC    R1,GDALN                                                         
         AR    R2,R1                                                            
         J     CKPER620                                                         
                                                                                
CKPER640 CLC   GDADATE,DOVRDTE     Compare date to overdue                      
         JNH   CKPERX              Noncompliant - do not unlock                 
         J     CKPER630                                                         
                                                                                
CKPER650 LA    R4,L'LIDASJAC(R4)                                                
         J     CKPER530                                                         
         DROP  R2,R3                                                            
                                                                                
*                                  Read for submitted time                      
*                                  1R Now                                       
                                                                                
         USING TAPPASD,R3                                                       
CKPER660 LA    R3,DMKEY                                                         
         XC    TAPPAS,TAPPAS       CLEAR KEY                                    
         MVI   TAPPTYP,TAPPTYPQ    X'3E'                                        
         MVI   TAPPSUB,TAPPSUBQ    X'16'                                        
         MVC   TAPPCPY,CPY                                                      
         MVI   TAPPKYST,TAPSAWPQ   Only concerned w/awaiting app                
         MVI   TAPPCAT,TAPPMAN     Only interested in Mngr view                 
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY                           
         JE    CKPER690                                                         
         J     CKPERX                                                           
                                                                                
CKPER670 LA    R3,DMKEY                                                         
         MVC   DMKEY,DMKEYSAV                                                   
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,DMKEY,DMKEY                           
CKPER680 GOTOR DATAMGR,DMCB,DMRSEQ,ACCDIR,DMKEY,DMKEY                           
CKPER690 CLC   DMKEYSAV(TAPPPEDT-TAPPAS),DMKEY                                  
         JNE   CKPER900            Record not found                             
                                                                                
         LA    R4,PERTAB                                                        
CKPER700 CLI   0(R4),FF                                                         
         JE    CKPER680                                                         
         OC    0(L'LIDASJAC,R4),0(R4)                                           
         JZ    CKPER680                                                         
         CLC   TAPMODSP,0(R4)                                                   
         JE    *+12                                                             
         LA    R4,L'LIDASJAC(R4)                                                
         J     CKPER700                                                         
                                                                                
         MVC   DMDA,TAPPDA         Set disk address (for GETREC/PUTREC)         
         MVC   DMKEYSAV,DMKEY                                                   
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,DMDA,AIO5,DMWORK                      
         JNE   CKPERX                                                           
                                                                                
         USING TIMRECD,R3                                                       
         L     R3,AIO5                                                          
         LA    R2,TIMRFST                                                       
         USING GDAELD,R2                                                        
CKPER710 CLI   GDAEL,0             Look for submit date                         
         JE    CKPER670                                                         
         CLI   GDAEL,GDAELQ                                                     
         JNE   CKPER720                                                         
         CLI   GDATYPE,GDALMSUB    Line Manager Approvals                       
         JE    CKPER730                                                         
CKPER720 XR    R1,R1                                                            
         IC    R1,GDALN                                                         
         AR    R2,R1                                                            
         J     CKPER710                                                         
                                                                                
CKPER730 CLC   GDADATE,DOVRDTE     Compare date to overdue                      
         JNH   CKPERX              Noncompliant - do not unlock                 
         J     CKPER720                                                         
         DROP  R2,R3                                                            
                                                                                
*                                  UNLOCK Person Record                         
                                                                                
         USING ACTRECD,R2                                                       
CKPER900 LA    R2,DMKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CPY                                                      
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,SUB1RACT                                                 
         GOTOR DATAMGR,DMCB,('RUP',DMREAD),ACCDIR,DMKEY,DMKEY                   
         JNE   CKPERX                                                           
         MVC   DMDA,ACTKDA         Set disk address (for GETREC/PUTREC)         
         GOTOR DATAMGR,DMCB,('RUP',GETREC),ACCMST,DMDA,AIO5,DMWORK              
         JNE   CKPERX                                                           
                                                                                
         L     R2,AIO5                                                          
         LA    R4,ACTRFST                                                       
         USING RSTELD,R4                                                        
         XR    R1,R1                                                            
CKPER910 CLI   RSTEL,0                                                          
         JE    CKPER940                                                         
         CLI   RSTEL,RSTELQ                                                     
         JE    CKPER930                                                         
CKPER920 IC    R1,RSTLN                                                         
         AR    R4,R1                                                            
         J     CKPER910                                                         
                                                                                
CKPER930 CLI   RSTLN,RSTLN3Q       Make sure that element is big enough         
         JL    CKPER920                                                         
         TM    HD_IND,FW_APRQ                 Approval mode?                    
         JNO   *+12                           Read for approvals                
         NI    RSTSTAT7,X'FF'-RSTLCKAP   Turn off LOCKED bit for appr           
         J     *+8                                                              
         NI    RSTSTAT7,X'FF'-RSTLCKTS   Turn off LOCKED bit for subr           
         MVC   BYTE1,RSTSTAT7                                                   
         J     CKPER920                                                         
                                                                                
CKPER940 GOTOR DATAMGR,DMCB,PUTREC,ACCMST,DMDA,AIO5,DMWORK                      
                                                                                
         TM    BYTE1,RSTLCKTS+RSTLCKAP Are there any locks left?                
         JNZ   CKPERX                                                           
                                                                                
*&&US*&& GOTOR SENDMQ        send notification that pid is unlocked             
                                                                                
CKPERX   DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* Send notification that person has been unlocked                    *          
**********************************************************************          
                                                                                
SENDMQ   NTR1  LABEL=*                                                          
                                                                                
SENDM20  MVC   MQUID,USRID         Connected User ID                            
                                                                                
         USING MAILTBD,RE                                                       
         LA    RE,MAILTAB          Find Principle ID override                   
SENDM22  CLI   0(RE),X'FF'         EOT?                                         
         JE    SENDM24                                                          
         CLC   MAILAGY,AGY         Match on Agency                              
         JE    *+12                                                             
         LA    RE,MAILNQ(RE)                                                    
         J     SENDM22                                                          
         MVC   MQUID,SPACES                                                     
         MVC   MQUID(L'MAILUID),MAILUID   Move in Principle ID                  
         DROP  RE                                                               
                                                                                
SENDM24  MVC   MQFACPAK,FACID      Connected FACPAK                             
         GOTOR DATCON,DMCB,(5,0),(5,MQDATE)                                     
         TIME  DEC                                                              
         ST    R0,DUB                                                           
         MVI   DUB+4,X'0F'                                                      
         UNPK  WORK(9),DUB(5)                                                   
         MVC   MQHR,WORK                                                        
         MVC   MQMIN,WORK+2                                                     
         MVC   MQSEC,WORK+4                                                     
                                                                                
* Get 8 char PID from 2 char PID, Uses AIO1.                                    
                                                                                
         USING SA0REC,R2                                                        
         LA    R2,DMKEY                                                         
         XC    SA0KEY,SA0KEY       Build key to read                            
         MVI   SA0KTYP,SA0KTYPQ                                                 
         OC    SA0KAGY,AGYSEC      Use security agency if present               
         JNZ   *+10                                                             
         MVC   SA0KAGY,AGY         Else native agency                           
         MVC   SA0KNUM,HD_PPID#    Assume Submitters PID                        
         TM    HD_IND,FW_APRQ      Approval mode?                               
         JNO   *+10                                                             
         MVC   SA0KNUM,APPRPID     Use Approvers PID                            
         MVC   WORK,SPACES                                                      
         GOTOR DATAMGR,DMCB,DMREAD,CTFILE,DMKEY,AIO1                            
         JNE   EXITN                                                            
         L     R2,AIO1                                                          
         LA    R3,SA0DATA                                                       
         USING SAPALD,R3                                                        
         XR    R0,R0                                                            
                                                                                
SENDM60  CLI   SAPALEL,SAPALELQ                                                 
         JE    SENDM70                                                          
         CLI   SAPALEL,0                                                        
         JE    SENDM80                                                          
         IC    R0,SAPALLN                                                       
         AR    R3,R0                                                            
         J     SENDM60                                                          
SENDM70  MVC   MQPID(8),SAPALPID     Pass 8 char PID                            
                                                                                
SENDM80  GOTOR MQI,DMCB,MQSTR,MQSTRL                                            
                                                                                
SENDMX   J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* MAIL TABLE DSECT                                                   *          
**********************************************************************          
                                                                                
MAILTBD  DSECT                                                                  
MAILAGY  DS    CL2                 Agency alpha ID                              
MAILUID  DS    CL8                 Connect ID                                   
MAILADD  DS    CL45                E-MAIL Address comma separated list          
MAILNQ   EQU   *-MAILTBD           Length                                       
SRUPD60  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK MCS TIMESHEET USER                                            *         
* ON NTRY PARM1 BYTE 1-3 ADDRESS OF 1R ACCOUNT                        *         
***********************************************************************         
         DS    0H                                                               
CHKMCS   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*CHKMCS*'                                                    
         LA    R4,BRATAB                                                        
         XC    0(BRATABX-BRATAB,R4),0(R4)                                       
         LA    R2,ONERL1L                                                       
         ST    R2,FULL1                                                         
         LA    R3,5                4 level + U/L                                
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,DMKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CPY                                                      
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   DMKEYSAV,DMKEY                                                   
CKMCS02  GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY                           
         JNE   EXITN                                                            
         MVC   DMDA,ACTKDA         Set disk address (for GETREC)                
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,DMDA,AIO5,DMWORK                      
         GOTOR CHKE5EL                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CPY                                                      
         MVC   ACTKUNT(2),=C'1R'                                                
         L     RE,FULL1                                                         
         XR    R1,R1                                                            
         IC    R1,0(RE)                                                         
         SHI   R1,1                                                             
         LA    RE,1(RE)                                                         
         ST    RE,FULL1                                                         
         BASR  RB,0                                                             
         MVC   ACTKACT(0),HD_1RACT                                              
         EX    R1,0(RB)                                                         
         MVC   DMKEYSAV,DMKEY                                                   
         JCT   R3,CKMCS02                                                       
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK E5 ELEMENT                                                    *         
***********************************************************************         
                                                                                
         DS    0H                                                               
CHKE5EL  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*CHKE5L*'                                                    
         L     R3,AIO5                                                          
         USING ACTRECD,R3                                                       
         LA    R3,ACTRFST                                                       
         USING GDAELD,R3                                                        
         SR    R0,R0                                                            
CKE5L002 CLI   GDAEL,0                                                          
         JE    CKE5L030                                                         
         CLI   GDAEL,GDAELQ                                                     
         JE    CKE5L006                                                         
CKE5L004 IC    R0,GDALN                                                         
         AR    R3,R0                                                            
         J     CKE5L002                                                         
                                                                                
CKE5L006 CLI   GDATYPE,GDATMCST                                                 
         JNE   CKE5L004                                                         
                                                                                
CKE5L012 LA    R5,BRATAB                                                        
         LHI   R0,MAXBRAQ                                                       
CKE5L014 OC    0(L'GDADATE2,R5),0(R5)                                           
         JZ    CKE5L016                                                         
         CLC   0(L'GDADATE,R5),GDADATE                                          
         JE    CKE5L016                                                         
         LA    R5,L'GDADATE2+L'GDADATE(R5)                                      
         J     CKE5L014                                                         
*                                                                               
CKE5L016 MVC   0(L'GDADATE,R5),GDADATE                                          
         MVC   L'GDADATE(L'GDADATE2,R5),GDADATE2                                
         J     CKE5L004                                                         
*                                                                               
CKE5L030 J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Calendar periods retrieval                                          *         
*   This routine is used to build a list of time periods using the    *         
*   company calendar                                                  *         
***********************************************************************         
                                                                                
CALPRDS  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*CALPRDS'                                                      
                                                                                
         USING CALTABD,R4                                                       
         L     R4,AGENAREA                                                      
         XC    CALENDT,CALENDT                                                  
                                                                                
         XC    CALENDT,CALENDT                                                  
         XR    RE,RE                                                            
         IC    RE,HD_CPSFS          Company Fiscal Start Month                  
         CHI   RE,X'F0'                                                         
         JH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'0F'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
         MVC   DCDAT,QSTART                                                     
         MVC   DCDAT+1(1),BYTE1                                                 
         CLC   QSTART+1(1),BYTE1                                                
         JNL   CALP002                                                          
         GOTOR DATCON,DMCB,(1,DCDAT),(0,WORK)                                   
         GOTOR ADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                              
         GOTOR DATCON,DMCB,(0,WORK+6),(1,DCDAT)                                 
                                                                                
CALP002  GOTOR DATCON,DMCB,(1,DCDAT),(0,WORK)                                   
         GOTOR ADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                              
         GOTOR DATCON,DMCB,(0,WORK+6),(1,DCEND)                                 
                                                                                
CALP004  GOTOR GTCAL                                                            
         JNE   CALPRDSN                                                         
                                                                                
         USING CASRECD,R2                                                       
         USING TMPELD,R3                                                        
         MVI   BYTE1,0                                                          
         L     R2,AIO5                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
CALP006  CLI   TMPEL,0                                                          
         JE    CALP018                                                          
         CLI   TMPEL,TMPELQ                                                     
         JE    CALP010                                                          
         CLI   TMPEL,TMRELQ                                                     
         JNE   CALP008                                                          
         ST    R3,FULL1                                                         
                                                                                
CALP008  IC    R0,TMPLN                                                         
         AR    R3,R0                                                            
         J     CALP006                                                          
                                                                                
CALP010  CLC   TMPSTART,QSTART     IS START DATE LOWER THAN PERIOD DATE         
*&&UK*&& JNL   CALP012             NO - CHECK END DATE                          
*&&US*&& JNL   CALP016             NO - CHECK END DATE                          
         CLC   TMPEND,QSTART       IS START DATE LOWER THAN PERIOD END          
         JL    CALP008             NO - NOT INTERESTED                          
         J     CALP016                                                          
CALP012  CLC   TMPEND,DCEND        IS PERIOD END DATE LOWER THAN END            
         JNH   CALP016             YES                                          
         CLC   TMPSTART,DCEND      IS PERIOD START DATE HIGHER THAN END         
         JH    CALP008             YES - NOT INTERESTED                         
*                                                                               
CALP016  CLC   TMPSTART,DOVRDTE    NOTHING HIGHER THAN OVERDUE                  
         JH    CALP008                                                          
         MVC   CALENDT,TMPEND                                                   
         MVC   CALSTRT,TMPSTART                                                 
         LA    R4,CALTABL(R4)                                                   
         XC    CALENDT,CALENDT                                                  
         J     CALP008                                                          
*                                                                               
CALP018  L     R3,FULL1                                                         
         USING TMRELD,R3                                                        
         CLC   DCEND(2),TMREND                                                  
         JH    CALPRDSN                                                         
*                                                                               
CALPRDSY J     EXITY                                                            
                                                                                
CALPRDSN J     EXITN                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* READ DAILY EDIT HOURS                                               *         
***********************************************************************         
                                                                                
RDEDT    NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*RDEDT**'                                                      
                                                                                
         USING EDTRECD,R3                                                       
         LA    R3,DMKEY                                                         
         MVC   EDTKEY,SPACES       YES READ FOR DAY HOURS                       
         MVI   EDTKTYP,EDTKTYPQ                                                 
         MVI   EDTKSUB,EDTKSUBQ                                                 
         MVC   EDTKCPY,CPY                                                      
         LA    RF,HD_1RACT                                                      
         LLC   R1,ONERL1L                                                       
         AHI   R1,-1                                                            
         BASR  RB,0                                                             
         MVC   EDTKOFC(0),0(RF)                                                 
         EX    R1,0(RB)                                                         
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in HD_1RACT                
         LLC   R2,ONERL2L                                                       
         LLC   R1,ONERL1L                                                       
         SR    R2,R1                                                            
         LR    R1,R2                                                            
         AHI   R1,-1                                                            
         BASR  RB,0                                                             
         MVC   EDTKDPT(0),0(RF)                                                 
         EX    R1,0(RB)                                                         
         AHI   R1,1                                                             
         AR    RF,R1               Bump to next spot in HD_1RACT                
         LLC   R2,ONERL3L                                                       
         LLC   R1,ONERL2L                                                       
         SR    R2,R1                                                            
         LR    R1,R2                                                            
         AHI   R1,-1                                                            
         BASR  RB,0                                                             
         MVC   EDTKSBD(0),0(RF)                                                 
         EX    R1,0(RB)                                                         
         MVC   EDTKPER,HD_1RPER                                                 
         MVC   EDTKYR,TODAYP                                                    
         MVI   EDTKSEQ,0                                                        
         MVI   EDTKKSTA,EDTKSDAY                                                
RDEDT010 MVC   DMKEYSAV,EDTKEY                                                  
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DMKEY,DMKEY                           
         CLC   EDTKEY,DMKEYSAV                                                  
         JE    RDEDT030                                                         
         MVC   EDTKEY,DMKEYSAV                                                  
         LA    R2,EDTTAB                                                        
RDEDT020 CLI   0(R2),FF            No Day edit hours                            
         JE    RDEDTX                                                           
         LA    RE,EDTKEY                                                        
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)                                                       
         AR    RE,RF                                                            
         SR    R1,R1                                                            
         IC    R1,2(R2)                                                         
         AHI   R1,-1                                                            
         BASR  RB,0                                                             
         EX    R1,8(RB)                                                         
         J     *+10                                                             
         CLC   0(0,RE),SPACES                                                   
         JH    *+12                                                             
         LA    R2,3(R2)                                                         
         J     RDEDT020                                                         
                                                                                
         BASR  RB,0                                                             
         MVC   0(0,RE),SPACES                                                   
         EX    R1,0(RB)                                                         
         J     RDEDT010                                                         
                                                                                
RDEDT030 GOTOR DATAMGR,DMCB,GETREC,ACCMST,EDTKDA,AIO5,DMWORK                    
                                                                                
RDEDTX   J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET DAILY EDIT HOURS AND PROCESS CALDAY TABLE                       *         
***********************************************************************         
                                                                                
         USING CALTABD,R4                                                       
GETEDT   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*GETEDT*'                                                      
*                                                                               
         LR    R4,R1                                                            
         XC    CALDAYS,CALDAYS     Clear calendar days                          
*                                                                               
         USING EDTRECD,R2                                                       
         L     R2,AIO5                                                          
         CLI   EDTKTYP,EDTKTYPQ    Make sure we have a EDT Hour rec             
         JNE   GETEDTX                                                          
         CLI   EDTKSUB,EDTKSUBQ                                                 
         JNE   GETEDTX                                                          
         TM    EDTKKSTA,EDTKSDAY   By Day                                       
         JNO   GETEDTX                                                          
*                                                                               
         GOTOR DATCON,DMCB,(1,CALSTRT),(0,WORK)                                 
         GOTOR GETDAY,DMCB,WORK,DUB1                                            
         CLC   DUB1(3),SPACES                                                   
         JE    GETEDTX                                                          
         USING DAYTABD,RF                                                       
         LA    RF,DAYTAB                                                        
GTEDT040 CLI   0(RF),FF                                                         
         JE    GETEDTX                                                          
         CLC   DAY#,0(R1)                                                       
         JE    GTEDT050                                                         
         LA    RF,DAYTABLN(RF)                                                  
         J     GTEDT040                                                         
*                                                                               
GTEDT050 SR    R3,R3                                                            
         IC    R3,DAYEDT#          Get Matching Edit hour Day #                 
         STC   R3,BYTE1                                                         
         SR    R0,R0                                                            
         DROP  RF                                                               
*                                                                               
         USING CALDAYD,R3                                                       
         LA    R3,CALDAYS                                                       
GTEDT060 CLI   CALDAY#,FF          Are we at the end of the table?              
         JE    GTEDT070                                                         
         GOTOR ADDAY,DMCB,(C'D',WORK),WORK+6,(R0)                               
         GOTOR DATCON,DMCB,(0,WORK+6),(1,DUB)                                   
         CLC   DUB,TODAYP                                                       
         JH    GTEDT070                                                         
         SR    R1,R1                                                            
         IC    R1,BYTE1                                                         
         STC   R1,CALDAY#                                                       
         OI    CALDAY#,X'F0'                                                    
         MVC   CALDAY,DUB                                                       
         ZAP   CALHRS,=P'0'                                                     
         AHI   R1,1                                                             
         CHI   R1,7                7 is the highest day                         
         JNH   *+8                                                              
         LA    R1,1                reset back to 1                              
         STC   R1,BYTE1                                                         
         LA    R3,CALLNQ(R3)                                                    
         AHI   R0,1                                                             
         J     GTEDT060                                                         
         DROP  R3                                                               
*                                                                               
GTEDT070 LA    R3,EDTRFST                                                       
GTEDT080 CLI   0(R3),0                                                          
         JE    GETEDTX                                                          
         CLI   0(R3),DEDELQ        X'54' - DAILY BUCKET ELEMENT                 
         JE    GTEDT100                                                         
GTEDT090 SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         J     GTEDT080                                                         
*                                                                               
         USING DEDELD,R3                                                        
         USING CALDAYD,R1                                                       
GTEDT100 LA    R1,CALDAYS                                                       
GTEDT110 CLI   CALDAY#,FF          End of table?                                
         JE    GETEDTX                                                          
         CLI   CALDAY#,0                                                        
         JE    GTEDT090                                                         
         CLC   DEDIND,CALDAY#      Match on #                                   
         JNE   GTEDT120                                                         
         TM    DEDSTAT,DEDSPHOL+DEDSSHOL    Is this an exception?               
         JZ    *+14                                                             
         CLC   DEDDATE,CALDAY      Match on Date.                               
         JNE   GTEDT090                                                         
         ZAP   CALHRS,DEDHRS                                                    
         J     GTEDT090                                                         
GTEDT120 LA    R1,CALLNQ(R1)                                                    
         J     GTEDT110                                                         
*                                                                               
GETEDTX  J     EXIT                                                             
         DROP  R1,R2,R3,R4                                                      
         EJECT                                                                  
***********************************************************************         
* GET CALENDAR                                                        *         
***********************************************************************         
         DS    0H                                                               
GTCAL    NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*GTCAL**'                                                    
                                                                                
         USING CASRECD,R2                                                       
         LA    R2,DMKEY                                                         
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKCPY,CPY                                                      
         MVC   CASKEMOA,DCEND                                                   
         MVC   CASKSMOA,DCDAT                                                   
         LA    RF,HD_1RACT                                                      
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         EX    RE,4(R1)                                                         
         MVC   CASKOFC(0),0(RF)                                                 
         MVC   DMKEYSAV,CASKEY                                                  
                                                                                
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,DMKEY,DMKEY                           
         JE    GETCAL20                                                         
                                                                                
         MVC   CASKEY,DMKEYSAV                                                  
         MVC   CASKOFC,SPACES                                                   
                                                                                
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,DMKEY,DMKEY                           
         JNE   EXITN                                                            
                                                                                
GETCAL20 GOTOR DATAMGR,DMCB,GETREC,ACCMST,CASKDA,AIO5,DMWORK                    
         JE    EXITY                                                            
         J     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Build time cluster at TT_DATA and set TT_DLEN to its length         *         
***********************************************************************         
                                                                                
BLDCLS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDCLS*'                                                      
                                                                                
         LA    R2,TT_DATA                                                       
         USING TIMELD,R2           R2=A(time cluster in buffer record)          
                                                                                
         CLI   TT_TTYP,TT_TMATR    Test materials                               
         JE    BLDCLS16                                                         
         OC    TT_NAR#,TT_NAR#     Day narrative                                
         JNZ   BLDCLS20                                                         
                                                                                
         MVI   TIMEL,TIMELQ        Build TIMEINP element                        
         MVI   TIMETYP,TIMEINP                                                  
         MVC   TIMACC,TT_AULA                                                   
         MVC   TIMTSK,TT_TSK                                                    
         MVC   TIMOFF,TT_OFF                                                    
         MVC   TIMMED,TT_MED                                                    
         MVC   TIMTTYP,TT_TTYP                                                  
         MVC   TIMIND,TT_IND                                                    
         MVC   TIMMOA,TT_MOA                                                    
         MVC   TIMSTAT,TT_STAT                                                  
         MVC   TIMLINE#,TT_TIME#                                                
         MVC   TIMADAT,TT_ADAT                                                  
         MVC   TIMHRS,TT_HRS                                                    
         LHI   R0,TIMILN1Q                                                      
         CLI   TT_TTYP,TIMTNC                                                   
         JE    BLDCLS02                                                         
         MVC   TIMRATE,TT_RATE                                                  
         MVC   TIMRBSTA,TT_RBSTA                                                
         MVC   TIMREFF,TT_REFF                                                  
         MVC   TIMINC,TT_INULA                                                  
*&&UK*&& MVC   TIMCRATE,TT_CRATE                                                
*&&UK*&& MVC   TIMCREFF,TT_CREFF                                                
         MVC   TIMAMNT,TT_AMNT                                                  
         LHI   R0,TIMILN2Q                                                      
BLDCLS02 STC   R0,TIMLN                                                         
         AR    R2,R0                                                            
                                                                                
         SR    R1,R1               Build TIMENAR element                        
         ICM   R1,1,TT_NARRL                                                    
         JZ    BLDCLS04                                                         
         BCTR  R1,0                                                             
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMENAR                                                  
         BASR  RE,0                                                             
         MVC   TIMNARR(0),TT_NARR                                               
         EX    R1,0(RE)                                                         
         LA    R0,TIMNARR-TIMELD+1(R1)                                          
         STC   R0,TIMLN                                                         
         AR    R2,R0                                                            
                                                                                
BLDCLS04 CLC   TT_ORD,SPACES                                                    
         JNH   BLDCLS06                                                         
         MVI   TIMEL,TIMELQ        Build TIMEORDR element                       
         MVI   TIMETYP,TIMEORDR                                                 
         MVC   TIMOIDNO,TT_TIME#                                                
         MVC   TIMOORDR,TT_ORD                                                  
         LHI   R0,TIMOLNQ                                                       
         STC   R0,TIMLN                                                         
         AR    R2,R0                                                            
                                                                                
BLDCLS06 CLC   TT_INTRF,SPACES                                                  
         JNH   BLDCLS08                                                         
         MVI   TIMEL,TIMELQ        Build TIMEINRF element                       
         MVI   TIMETYP,TIMEINRF                                                 
         MVC   TIMJIDNO,TT_TIME#                                                
         MVC   TIMJINRF,TT_INTRF                                                
         LHI   R0,TIMJLNQ                                                       
         STC   R0,TIMLN                                                         
         AR    R2,R0                                                            
                                                                                
BLDCLS08 CLC   TT_EST,SPACES                                                    
         JNH   BLDCLS09                                                         
         MVI   TIMEL,TIMELQ        Build TIMEINRF element                       
         MVI   TIMETYP,TIMEEST                                                  
         MVC   TIMJIDNO,TT_TIME#                                                
         MVC   TIMJINRF,TT_EST                                                  
         LHI   R0,TIMSLNQ                                                       
         STC   R0,TIMLN                                                         
         AR    R2,R0                                                            
                                                                                
BLDCLS09 LLC   RF,TT_TOFFL                                                      
         LTR   RF,RF                                                            
         JZ    BLDCLS10                                                         
         MVI   TIMEL,TIMELQ        Build Timeoff id element                     
         MVI   TIMETYP,TIMETOFF                                                 
         MVC   TIMFLID,TT_TIME#                                                 
         BCTR  RF,0                                                             
         BASR  R1,0                                                             
         MVC   TIMFIDN(0),TT_TOFFI                                              
         EX    RF,0(R1)                                                         
         AHI   RF,1+(TIMFIDN-TIMELD)                                            
         STC   RF,TIMLN                                                         
         AR    R2,RF                                                            
                                                                                
BLDCLS10 MVI   TIMEL,TIMELQ        Build TIMETIME element                       
         MVI   TIMETYP,TIMETIME                                                 
         MVC   TIMEIDNO,TT_TIME#                                                
         MVC   TIMEPST1,TT_EPST1                                                
         MVC   TIMEPIDC,TT_CLIAP                                                
         MVC   TIMETPDT,TT_ETPDT                                                
         LA    RE,TT_DHVAL                                                      
         LA    R1,TIMEDAY                                                       
BLDCLS12 OC    0(L'TIMETDTE,RE),0(RE)  Is there a date in the buffer            
         JZ    BLDCLS14                                                         
         MVC   0(L'TIMEDAY,R1),0(RE)   Copy date and hours to element           
         LA    R1,L'TIMEDAY(R1)    Bump along timel element                     
         LA    RE,L'TIMEDAY(RE)    Bump along buffer record                     
         J     BLDCLS12                                                         
                                                                                
BLDCLS14 SR    R1,R2                                                            
         STC   R1,TIMLN                                                         
         LR    R0,R1                                                            
         J     BLDCLS22                                                         
                                                                                
BLDCLS16 MVI   TIMEL,TIMELQ        Build TIMEITMS element                       
         MVI   TIMETYP,TIMEITMS                                                 
         MVC   TIMIIDNO,TT_TIME#                                                
         MVC   TIMIIIDN,TT_ITEM#                                                
         MVC   TIMIMOA,TT_MOA                                                   
         MVC   TIMIULA,TT_AULA                                                  
         MVC   TIMITSK,TT_TSK                                                   
         MVC   TIMIOFF,TT_OFF                                                   
         MVC   TIMINULA,TT_INULA                                                
         MVC   TIMISTAT,TT_EPST1                                                
         MVC   TIMIPIDC,TT_CLIAP                                                
         MVC   TIMISEQ,TT_ISEQ                                                  
         MVC   TIMINUM,TT_INUM                                                  
         MVC   TIMIMULT,TT_IMULT                                                
         MVC   TIMIPRCE,TT_IPRCE                                                
         MVC   TIMIIND,TT_IIND                                                  
         MVC   TIMITOT,TT_ITOT                                                  
         LHI   R0,TIMITLNQ                                                      
         SR    R1,R1                                                            
         ICM   R1,1,TT_NARRL                                                    
         JZ    BLDCLS18                                                         
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   TIMITEXT(0),TT_NARR                                              
         EX    R1,0(RE)                                                         
         LA    R0,TIMITLNQ+1(R1)                                                
BLDCLS18 STC   R0,TIMLN                                                         
         J     BLDCLS22                                                         
*                                  Day narrative                                
BLDCLS20 MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMEDNAR                                                 
         MVC   TIMDIDNO,TT_TIME#                                                
         MVC   TIMDNIDN,TT_NAR#                                                 
         MVC   TIMDTDT1,TT_DDTE                                                 
         LLC   RF,TT_DNARL                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   TIMDNARR(0),TT_DNAR                                              
         EX    RF,0(RE)                                                         
         LA    R0,1+TIMDNLNQ(RF)                                                
         STC   R0,TIMLN                                                         
                                                                                
BLDCLS22 AR    R2,R0               Add length of last element added             
         LA    R0,TT_DATA                                                       
         SR    R2,R0                                                            
         STCM  R2,3,TT_DLEN        Set length of time cluster                   
         J     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PLDREC handler                                                      *         
***********************************************************************         
*&&UK                                                                           
         USING PLDRECD,PLDREC                                                   
W        USING PLDRECD,DMKEYSAV                                                 
PLDR     MVC   W.PLDKEY,FW_RKEY                                                 
         MVC   W.PLDKSTA(L'FW_DREC),FW_DREC                                     
         OC    PLDKEY,PLDKEY       Test already have a key saved                
         JZ    PLDR0020                                                         
         CLC   PLDKEY(PLDKSTA-PLDKEY),W.PLDKEY                                  
         JNE   PLDR0010                                                         
         AP    PLDKAMT,W.PLDKAMT   Add in the value if the same key             
         J     ACUPNEXT            Get next FACWRK record                       
                                                                                
PLDR0010 GOTOR PLDPUT              Add/Write previous PLDREC                    
                                                                                
PLDR0020 MVC   PLDKEY(ACCKLEN),W.PLDKEY                                         
         J     ACUPNEXT            Get next FACWRK record                       
         DROP  W                                                                
                                                                                
PLDRLAST NTR1  LABEL=NO            Handle last time call                        
         OC    PLDKEY,PLDKEY       Test any PLDREC update pending               
         JZ    EXIT                                                             
         GOTOR PLDPUT              Add/Write last PLDREC                        
         J     EXIT                                                             
                                                                                
K        USING PLDRECD,DMKEY                                                    
PLDPUT   NTR1  LABEL=NO            Put record to directory                      
         GOTOR GODMGR,DMCB,('RUD',DMREAD),ACCDIR,PLDKEY,K.PLDKEY                
         TM    8(R1),FF-(IOERNF+IOEDEL)                                         
         JZ    *+6                                                              
         DC    H'0'                                                             
         TM    8(R1),IOERNF        Test record not found                        
         JZ    PLDPUT02                                                         
         GOTOR GODMGR,DMCB,DMADD,ACCDIR,PLDKEY,PLDKEY                           
         JE    EXIT                                                             
         DC    H'0'                                                             
                                                                                
PLDPUT02 TM    8(R1),IOEDEL        Test record is deleted                       
         JNZ   *+10                                                             
         AP    PLDKAMT,K.PLDKAMT   Add existing amount into record              
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,PLDKEY,PLDKEY                           
         JE    EXIT                                                             
         DC    H'0'                                                             
         DROP  K                                                                
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ACCDIR record handler                                               *         
***********************************************************************         
                                                                                
K        USING ACCRECD,DMKEY                                                    
ADIR     MVC   K.ACCKEY,FW_RKEY    Reconstruct directory record                 
         MVC   K.ACCKSTA(L'FW_DREC),FW_DREC                                     
                                                                                
         CLI   FW_ACT,FW_AADDD     Test add new directory record                
         JE    ADIR0010                                                         
         CLI   FW_ACT,FW_AWRTD     Test write directory record                  
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GODMGR,DMCB,('RUD',DMREAD),ACCDIR,FW_RKEY,DMKEYSAV               
         TM    8(R1),FF-(IOEDEL)                                                
         JZ    *+6                                                              
         DC    H'0'                                                             
         CLC   K.ACCKEY(ACCKLEN),DMKEYSAV                                       
         JE    ACUPNEXT            No change - get next FACWRK record           
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,K.ACCKEY,K.ACCKEY                       
         JE    ACUPNEXT            Get next FACWRK record                       
         DC    H'0'                                                             
                                                                                
ADIR0010 GOTOR GODMGR,DMCB,DMADD,ACCDIR,K.ACCKEY,K.ACCKEY                       
         JE    ACUPNEXT            Get next FACWRK record                       
         DC    H'0'                                                             
         DROP  K                                                                
         EJECT                                                                  
***********************************************************************         
* ACCMST record handler                                               *         
*                                                                     *         
* Passive pointers will be generated only when FW_RTYP is set in the  *         
* input record (see ACRECEQUS).  Note that ledger level lengths may   *         
* be required to create passives - these can be passed on the FACWRK  *         
* header record if necessary                                          *         
***********************************************************************         
                                                                                
AMST     L     R2,AIO1                                                          
         USING ACCRECD,R2          R2=A(Record to be put/added)                 
*                                                                               
         MVC   ACCKEY,FW_RKEY      Reconstruct ACCDIR/ACCMST record             
         SR    R1,R1                                                            
         ICM   R1,3,FW_RLEN                                                     
         SHI   R1,FW_RECHL                                                      
         LA    R0,ACCRLEN                                                       
         LA    RE,FW_RREC                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   DMDA,FW_RDA         Set disk address (for GETREC/PUTREC)         
*                                                                               
         CLI   FW_FILE,FW_FMST     Test ACCMST ?                                
         JNE   *+2                 no, abend                                    
*                                                                               
AMST0010 CLI   FW_ACT,FW_AADDR     Test ADDREC                                  
         JE    AMST0040                                                         
*&&UK                              yes                                          
         CLI   FW_ACT,FW_AADFR     Test update Archive record ?                 
         JE    AMST0100            yes, continue                                
*&&                                no                                           
         CLI   FW_ACT,FW_APUTR     Test PUTREC                                  
         JNE   *+2                                                              
*                                                                               
         L     R3,AIO3                                                          
O        USING ACCRECD,R3                                                       
         GOTOR GODMGR,DMCB,('RUD',GETREC),ACCMST,DMDA,O.ACCRECD,DMWORK          
         JNE   *+2                 Can't get record                             
         CLC   ACCKEY,O.ACCKEY     Test keys are the same                       
         JNE   *+2                 PUTREC key change                            
         CLI   FW_RTYP,0           Test record type given                       
         JE    AMST0020                                                         
         LA    RF,ONERLVS                                                       
         CLI   FW_RTYP,ACRTTIM                                                  
         JE    AMST0015                                                         
         XR    RF,RF                                                            
*&&UK                                                                           
         CLI   CALOVRLY,FW_MXPAY   Media payments (MELNK17)                     
         JNE   AMST0015                                                         
         CLI   FW_RTYP,ACRTTRN     Transaction (SF, only held status..          
         JNE   AMST0015                                   ..is updated)         
N        USING TRNELD,ACCRECD+(TRNRFST-TRNRECD)                                 
         MVC   BYTE1,N.TRNSTAT     New TRNEL TRNSTAT from MELNK17               
         NI    BYTE1,TRNSHOLD      Preserve only TRNSHOLD bit in case..         
         LA    RE,O.ACCRECD            ..ADDTRN has already updated rec         
         LA    R0,ACCRECD          Copy original over new version               
         SR    RF,RF                                                            
         ICM   RF,3,O.ACCRLEN                                                   
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         NI    N.TRNSTAT,FF-TRNSHOLD    Re-insert TRNSHOLD                      
         OC    N.TRNSTAT,BYTE1                                                  
         J     AMST0020            No passives affected                         
         DROP  N                                                                
*&&                                                                             
                                                                                
AMST0015 GOTOR BLDPAS,DMCB,('PP_ADEL',O.ACCRECD),(FW_RTYP,(RF))                 
                                                                                
AMST0020 GOTOR GODMGR,DMCB,PUTREC,ACCMST,DMDA,ACCRECD,DMWORK                    
         JNE   *+2                 Can't put record                             
         CLI   FW_RTYP,0           Test record type given                       
         JE    AMST0030            Get next FACWRK record                       
         LA    RF,ONERLVS                                                       
         CLI   FW_RTYP,ACRTTIM                                                  
         JE    AMST0025                                                         
         XR    RF,RF                                                            
*&&UK                                                                           
         CLI   CALOVRLY,FW_MXPAY   Media payments (MELNK17)                     
         JNE   AMST0025                                                         
         CLI   FW_RTYP,ACRTTRN     Transaction                                  
         JE    AMST0030            No passives affected                         
*&&                                                                             
                                                                                
AMST0025 GOTOR BLDPAS,DMCB,('PP_AADD',ACCRECD),(FW_RTYP,(RF))                   
                                                                                
K        USING ACCRECD,DMKEY                                                    
AMST0030 CLC   ACCRSTA,O.ACCRSTA   Test change of record status                 
         JE    ACUPNEXT            No - get next FACWRK record                  
         GOTOR GODMGR,DMCB,('RUD',DMREAD),ACCDIR,ACCKEY,K.ACCKEY                
         TM    8(R1),FF-(IOEDEL)                                                
         JNZ   *+2                                                              
         CLC   K.ACCKDA,DMDA       Ensure disk addresses match                  
         JNE   *+2                                                              
         MVC   K.ACCKSTA,ACCRSTA   Set new status and write back                
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,K.ACCKEY,K.ACCKEY                       
         JE    ACUPNEXT            Get next FACWRK record                       
         DC    H'0'                                                             
                                                                                
AMST0040 CLI   FW_RTYP,0           Test record type given                       
         JE    AMST0050            No - get next FACWRK record                  
         GOTOR UPDTRK,DMCB,ACCRECD,(FW_RTYP,0)  Rebuild batch record            
                                                                                
AMST0050 CLI   FW_RTYP,ACRTTRN     Are we processing a order trans              
         JNE   AMST0058            No                                           
         TM    TRNCPYS9,CPYSSRNM   Accent cashflow in use                       
         JZ    AMST0058            No                                           
         LA    R4,ACCRFST                                                       
         USING SERELD,R4                                                        
AMST0052 CLI   SEREL,0             EOR - Shouldn't happen so die                
         JE    *+2                                                              
         CLI   SEREL,SERELQ        Is it the TX serial element                  
         JE    AMST0056                                                         
         LLC   RF,SERLN                                                         
         AR    R4,RF                                                            
         J     AMST0052                                                         
                                                                                
AMST0056 LA    R3,DMKEY            Build key for serial passive pointer         
         USING TRSPASD,R3                                                       
         XC    TRSPKEY,TRSPKEY                                                  
         MVI   TRSPTYP,TRSPTYPQ                                                 
         MVI   TRSPSUB,TRSPSUBQ                                                 
         MVC   TRSPCPY,ACCKEY                                                   
         GOTOR GODMGR,DMCB,('RUD',DMRDHI),ACCDIR,TRSPKEY,TRSPKEY                
         ICM   RE,15,TRSPSER                                                    
         LPR   RE,RE                                                            
         AHI   RE,1                Increment by 1                               
         STCM  RE,15,SERNM         Store new number in element                  
         LNR   RE,RE                                                            
         STCM  RE,15,TRSPSER                                                    
         MVC   TRSPSTA,ACCRSTA                                                  
         MVC   DMKEYSAV,DMKEY                                                   
                                                                                
AMST0058 CLI   FW_RTYP,ACRTCPR   - for Add Costing personal rates rec?          
         JNE   AMST0060            NO , CONTINUE                                
*                                                                               
K        USING ACCRECD,DMKEY     ## READ DIR RECORD FOR THE KEY ##              
         GOTOR GODMGR,DMCB,('RUD',DMREAD),ACCDIR,ACCKEY,K.ACCKEY                
         JE    ACUPNEXT            FOUND , GET NEXT RECORD                      
*                                                                               
         TM    8(R1),IOEDEL        IS THIS RECORD ALREADY DELETED?              
         JZ    AMST0060            NO, ADD NEW RECORD                           
*                                                                               
         NI    K.ACCKSTA,X'FF'-DELRECQ   UNSET THE DELETED STATUS               
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,K.ACCKEY,K.ACCKEY                       
         JE    *+6                 SUCCESSFUL, CONTINUE                         
         DC    H'00'                                                            
*                                                                               
         MVC   DMDA,K.ACCKDA       SAVE DISK ADDRESS FOR MASTER READ            
         L     R3,AIO3             ADDRESS WORK AREA                            
O        USING ACCRECD,R3          READ DELETED MASTER RECORD                   
*                                                                               
* O.ACCRECD - OLD MASTER RECORD READ FROM THE DELETED DIR RECORD                
* ACCRECD   - NEW MASTER RECORD WHICH WE WANTED TO ADD                          
*                                                                               
         GOTOR GODMGR,DMCB,('RUP',GETREC),ACCMST,DMDA,O.ACCRECD,DMWORK          
         JNE   *+2                                                              
*                                                                               
         GOTOR GODMGR,DMCB,PUTREC,ACCMST,DMDA,ACCRECD,DMWORK                    
         JE    ACUPNEXT            FOUND , GET NEXT RECORD                      
         DC    H'00'                                                            
*                                                                               
AMST0060 MVC   DMDA,FW_RDA         Set disk address (for GETREC/PUTREC)         
         GOTOR GODMGR,DMCB,ADDREC,ACCMST,DMDA,ACCRECD,DMWORK                    
         JE    AMST0062            SUCCESSFUL, CONTINUE                         
         DC    H'00'                                                            
*                                                                               
AMST0062 CLI   FW_RTYP,ACRTTRN     Are we processing a order trans              
         JNE   AMST0070            No                                           
         TM    TRNCPYS9,CPYSSRNM   Accent cashflow in use                       
         JZ    AMST0070            No                                           
                                                                                
         MVC   DMKEY,DMKEYSAV                                                   
         MVC   DMKEY+TRSPDA-TRSPASD(L'TRSPDA),DMDA                              
         GOTOR GODMGR,DMCB,DMADD,ACCDIR,DMKEY,DMKEY                             
         JNE   *+2                 Die if we can't add new passive              
AMST0070 CLI   FW_RTYP,0           Test record type given                       
         JE    ACUPNEXT            No - get next FACWRK record                  
         LA    RF,ONERLVS                                                       
         CLI   FW_RTYP,ACRTTIM                                                  
         JE    AMST0080                                                         
         XR    RF,RF                                                            
                                                                                
AMST0080 GOTOR BLDPAS,DMCB,('PP_AADD',ACCRECD),(FW_RTYP,(RF))                   
         J     ACUPNEXT            Get next FACWRK record                       
         DROP  R2,K,O                                                           
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ACCARC record handler                                               *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*&&UK                                                                           
*                                  yes, Continue                                
         USING TRNRECD,R2          R2=A(transaction record for Archive)         
AMST0100 L     R3,AIO3             address work area                            
O        USING TRNRECD,R3          map with record layout                       
         GOTOR GODMGR,DMCB,GETREC,ACCARC,DMDA,O.TRNRECD,DMWORK                  
         JNE   *+2                 Can't get record                             
*                                                                               
         CLC   TRNKEY,O.TRNKEY     Test keys are the same                       
         JNE   *+2                 no, abend                                    
*                                                                               
         MVI   FW_RTYP,ACRTTRNA    init record type as archive trans            
*                                                                               
* Delete passive for the archive associated with old archive record             
*                                                                               
AMST0115 GOTOR BLDPAS,DMCB,('PP_ADEL',O.TRNRECD),(FW_RTYP,0)                    
*                                                                               
* Remove archive status and promote archive record to master record             
*                                                                               
AMST0120 NI    O.TRNRSTAT,X'FF'-TRNSARCH Remove archive from old rec            
         NI    TRNRSTAT,X'FF'-TRNSARCH Remove archive from new record           
         GOTOR GODMGR,DMCB,ADFREC,ACCMST,DMDA,O.TRNRECD,DMWORK                  
         JNE   *+2                 Can't pomote record                          
*                                                                               
* Read DIR record for the key to update master record addrs and status          
*                                                                               
DIR      USING TRNRECD,DMKEY                                                    
         GOTOR GODMGR,DMCB,('RUD',DMREAD),ACCDIR,TRNKEY,DIR.TRNKEY              
         TM    8(R1),FF-(IOEDEL)                                                
         JNZ   *+2                                                              
*                                                                               
         MVC   DIR.TRNKSTA,TRNRSTA Set new status for dir update                
         MVC   DIR.TRNKDA,DMDA     Save Disk address for dir update             
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,DIR.TRNKEY,DIR.TRNKEY                   
         JNE   *+2                 Get next FACWRK record                       
*                                                                               
* Read master record to update status and record. We read old record            
* from master file and replace it with updated record                           
*                                                                               
* O.TRNRECD  - Old Archive record promoted to master file                       
* TRNRECD    - New record which was updated by ACBRA** module                   
*                                                                               
         GOTOR GODMGR,DMCB,('RUP',GETREC),ACCMST,DMDA,O.TRNRECD,DMWORK          
         JNE   *+2                                                              
*                                                                               
         GOTOR GODMGR,DMCB,PUTREC,ACCMST,DMDA,TRNRECD,DMWORK                    
         JNE   *+2                                                              
*                                                                               
* Add new passive pointers for the master record                                
*                                                                               
AMST0130 GOTOR BLDPAS,DMCB,('PP_AADD',TRNRECD),(FW_RTYP,0)                      
         J     ACUPNEXT            Get next FACWRK record                       
*                                                                               
         DROP  R2,DIR,O                                                         
*&&                                                                             
***********************************************************************         
* MEDDIR record handler                                               *         
***********************************************************************         
*&&UK                                                                           
MDIR     XC    DMKEY,DMKEY         Media directory record                       
K        USING M_GEND,DMKEY                                                     
         MVC   K.M_KEY,FW_RKEY                                                  
         MVC   K.M_DST(L'FW_DREC),FW_DREC                                       
                                                                                
         CLI   FW_ACT,FW_AADDD     Test add new directory record                
         JE    MDIR0010                                                         
         CLI   FW_ACT,FW_AWRTD     Test write directory record                  
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GODMGR,DMCB,('RUD',DMREAD),MEDDIR,FW_RKEY,DMKEYSAV               
         TM    8(R1),FF-(IOEDEL)                                                
         JZ    *+6                                                              
         DC    H'0'                                                             
         CLC   K.M_KEY(M_DIRLQ),DMKEYSAV                                        
         JE    ACUPNEXT            No change - get next FACWRK record           
         GOTOR GODMGR,DMCB,DMWRT,MEDDIR,K.M_KEY,K.M_KEY                         
         JE    ACUPNEXT            Get next FACWRK record                       
         DC    H'0'                                                             
                                                                                
MDIR0010 GOTOR GODMGR,DMCB,DMADD,MEDDIR,K.M_KEY,K.M_KEY                         
         JE    ACUPNEXT            Get next FACWRK record                       
         DC    H'0'                                                             
         DROP  K                                                                
         EJECT                                                                  
***********************************************************************         
* MEDFIL record handler                                               *         
***********************************************************************         
                                                                                
MFIL     L     R2,AIO1             Media file record                            
         USING M_GEND,R2           R2=A(Record to be put/added)                 
                                                                                
         MVC   M_KEY,FW_RKEY                                                    
         SR    R1,R1                                                            
         ICM   R1,3,FW_RLEN                                                     
         SHI   R1,FW_RECHL                                                      
         LA    R0,M_RECLEN                                                      
         LA    RE,FW_RREC                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   DMDA,FW_RDA         Set disk address (for GETREC/PUTREC)         
                                                                                
         CLI   FW_FILE,FW_FMFI     Test MEDFIL                                  
         JE    MFIL0010                                                         
         DC    H'0'                                                             
                                                                                
MFIL0010 CLI   FW_ACT,FW_AADDR     Test ADDREC                                  
         JE    MFIL0020                                                         
         CLI   FW_ACT,FW_APUTR     Test PUTREC                                  
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,AIO3                                                          
O        USING M_GEND,R3                                                        
         GOTOR GODMGR,DMCB,('RUD',GETREC),MEDFIL,DMDA,O.M_GEND,DMWORK           
         JE    *+6                                                              
         DC    H'0'                Can't get record                             
         CLC   M_KEY,O.M_KEY       Test keys are the same                       
         JE    *+6                                                              
         DC    H'0'                PUTREC key change                            
         GOTOR BLDMPA,DMCB,('PM_ADEL',O.M_KEY)                                  
                                                                                
         GOTOR GODMGR,DMCB,PUTREC,MEDFIL,DMDA,M_KEY,DMWORK                      
         JE    *+6                                                              
         DC    H'0'                Can't put record                             
         GOTOR BLDMPA,DMCB,('PM_AADD',M_KEY)                                    
                                                                                
K        USING M_GEND,DMKEY                                                     
         CLC   M_FST,O.M_FST       Test change of record status                 
         JE    ACUPNEXT            No - get next FACWRK record                  
         GOTOR GODMGR,DMCB,('RUD',DMREAD),MEDDIR,M_KEY,K.M_KEY                  
         TM    8(R1),FF-(IOEDEL)                                                
         JZ    *+6                                                              
         DC    H'0'                                                             
         CLC   K.M_DDA,DMDA        Ensure disk addresses match                  
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   K.M_DST,M_FST       Set new status and write back                
         GOTOR GODMGR,DMCB,DMWRT,MEDDIR,K.M_KEY,K.M_KEY                         
         JE    ACUPNEXT            Get next FACWRK record                       
         DC    H'0'                                                             
                                                                                
MFIL0020 GOTOR GODMGR,DMCB,ADDREC,MEDFIL,DMDA,M_GEND,DMWORK                     
         JE    *+6                                                              
         DC    H'0'                Can't add record                             
         GOTOR BLDMPA,DMCB,('PM_AADD',M_GEND)                                   
         J     ACUPNEXT            Get next FACWRK record                       
         DROP  R2,K,O                                                           
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* Handle LOCKET records - build table of keys                         *         
***********************************************************************         
                                                                                
LOCK     CLC   FW_LKKEY,LLKEY      No - Test already unlocked                   
         JE    ACUPNEXT            Yes - ignore                                 
         MVC   LLKEY,FW_LKKEY                                                   
         LHI   R0,LLKUNLKQ         On-line unlock action                        
         CLI   PLONOFF,PLON                                                     
         JE    *+8                                                              
         LHI   R0,LLKUNGLQ         Off-line - running as global                 
         GOTOR LOCKET,DMCB,((R0),LLKEY),ACOMFACS                                
         JE    ACUPNEXT                                                         
         DC    H'0'                Can't unlock                                 
         EJECT                                                                  
***********************************************************************         
* Update time records                                                 *         
***********************************************************************         
                                                                                
         USING TT_D,R6             R6=A(Input time record)                      
UPDTIM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDTIM*'                                                      
                                                                                
         LARL  R0,UTHOOK1          Point to BUFREC hook routines                
         ST    R0,AUTHOOK1                                                      
         LARL  R0,UTHOOK2                                                       
         ST    R0,AUTHOOK2                                                      
         LARL  R0,UTHOOK3                                                       
         ST    R0,AUTHOOK3                                                      
                                                                                
         L     R2,AIO2                                                          
         USING TIMRECD,R2          R2=A(Time record)                            
         TM    TIMRIND,TIMRISTA    Test changing time sheet status              
         JNZ   UTIM0370                                                         
         TM    TIMRIND,TIMRIDEL    Test deleting all time records               
         JNZ   UTIM0410                                                         
                                                                                
         TM    HD_IND,FW_INOPR     If no previous time existed                  
         JNZ   UTIM0150            don't need worry about existing time         
                                                                                
***********************************************************************         
* Delete deleted time elements from time records                      *         
***********************************************************************         
                                                                                
         GOTOR BUFTIM,DMCB,('TSASRT',OLDBUF),SORTLOC                            
                                                                                
         XC    DMDA,DMDA           Clear disk address                           
         LA    R0,TT_KEY           Clear TT_KEY                                 
         LHI   R1,TT_LN1Q                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR BUFTIM,DMCB,('TSARDH',OLDBUF)                                    
         J     UTIM0020                                                         
UTIM0010 GOTOR BUFTIM,DMCB,('TSANXT',OLDBUF)                                    
UTIM0020 TM    TSARERRS,TSEEOF     Test end of file                             
         JNZ   UTIM0030                                                         
                                                                                
         TM    TT_BSTAT,TT_BSROB   Does time line still exist?                  
         JNZ   UTIM0010            Yes                                          
         CLC   TT_DA,DMDA          Test same record as previous                 
         JE    UTIM0050                                                         
                                                                                
UTIM0030 OC    DMDA,DMDA           Test first time                              
         JZ    UTIM0040                                                         
         GOTOR BUFREC,DMCB,('BUFPUT',TIMRECQ),('BUFIELD',TIMRECD),0             
                                                                                
UTIM0040 TM    TSARERRS,TSEEOF     Test end of file                             
         JNZ   UTIM0060                                                         
                                                                                
         MVC   DMDA,TT_DA          Set disk address                             
         GOTOR BUFREC,DMCB,('BUFGET+RUD',TIMRECQ),TIMRECD,AUTHOOK1              
                                                                                
UTIM0050 GOTOR DELCLS              Delete time cluster from record              
         J     UTIM0010                                                         
                                                                                
***********************************************************************         
* Delete changed/approved time elements from time records             *         
***********************************************************************         
                                                                                
UTIM0060 GOTOR BUFTIM,DMCB,('TSASRT',NEWBUF),SORTOFA                            
                                                                                
         XC    DMDA,DMDA           Clear disk address                           
         LA    R0,TT_KEY           Clear TT_KEY                                 
         LHI   R1,TT_LN1Q                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF)                                    
         J     UTIM0080                                                         
UTIM0070 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF)                                    
UTIM0080 TM    TSARERRS,TSEEOF     Test end of file                             
         JNZ   UTIM0090                                                         
                                                                                
         TM    TT_BSTAT,TT_BSRNR   Ignore new time lines                        
         JNZ   UTIM0070                                                         
         TM    TT_BSTAT,TT_BSRCR   Has this line changed?                       
         JNZ   *+12                                                             
         TM    TT_BSTAT,TT_BSRIA   or been approved?                            
         JZ    UTIM0070                                                         
         CLC   TT_DA,DMDA          Test same record as previous                 
         JE    UTIM0110                                                         
                                                                                
UTIM0090 OC    DMDA,DMDA           Test first time                              
         JZ    UTIM0100                                                         
         GOTOR BUFREC,DMCB,('BUFPUT',TIMRECQ),                         +        
               ('BUFIELD+BUFIPAD',TIMRECD)                                      
                                                                                
UTIM0100 TM    TSARERRS,TSEEOF     Test end of file                             
         JNZ   UTIM0120                                                         
                                                                                
         MVC   DMDA,TT_DA          Set disk address                             
         GOTOR BUFREC,DMCB,('BUFGET+RUD',TIMRECQ),TIMRECD,AUTHOOK2              
                                                                                
UTIM0110 GOTOR DELCLS              Delete cluster from record                   
         J     UTIM0070                                                         
                                                                                
UTIM0120 LA    R0,TT_KEY           Clear TT_KEY                                 
         LHI   R1,TT_LN1Q                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR BUFREC,DMCB,('BUFRDH',TIMRECQ),TIMRECD                           
         J     UTIM0140                                                         
UTIM0130 GOTOR BUFREC,DMCB,('BUFNXT',TIMRECQ),TIMRECD                           
UTIM0140 JNE   UTIM0150                                                         
         TM    DMFLAG,BUFIELD      Test record has deleted elements             
         JZ    UTIM0130                                                         
         GOTOR HELLO,DMCB,(C'D',ACCMST),('FF',TIMRECD),0                        
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR BUFREC,DMCB,('BUFPUT',TIMRECQ),TIMRECD                           
         J     UTIM0130                                                         
                                                                                
***********************************************************************         
* Add changed time and new time to time records                       *         
***********************************************************************         
                                                                                
UTIM0150 XC    DMDA,DMDA           Clear disk address                           
         LA    R0,TT_KEY           Clear TT_KEY                                 
         LHI   R1,TT_LN1Q                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR BUFTIM,DMCB,('TSARDH',NEWBUF)                                    
         J     UTIM0170                                                         
UTIM0160 GOTOR BUFTIM,DMCB,('TSANXT',NEWBUF)                                    
UTIM0170 TM    TSARERRS,TSEEOF     Test end of file                             
         JNZ   UTIM0300                                                         
                                                                                
         OC    SVTCAULA,SVTCAULA   Have we saved a contra                       
         JZ    UTIM0172                                                         
         CLC   SVTCAULA,TT_CAULA   Does contra match                            
         JNE   UTIM0172                                                         
         CLC   SVTCOFF,TT_COFF     Does contra office match                     
         JE    UTIM0174                                                         
UTIM0172 MVC   SVTCAULA,TT_CAULA   Save latest contra                           
         MVC   SVTCOFF,TT_COFF     Save latest contra office                    
         MVI   UPDTIND,0           Initialise flag                              
         MVI   TIMSEQ#,0           Initialise record sequence                   
                                                                                
UTIM0174 TM    TT_BSTAT,TT_BSRNR   Test new time                                
         JNZ   UTIM0180                                                         
         TM    TT_BSTAT,TT_BSRCR   Have we amended time line?                   
         JNZ   UTIM0180            Yes                                          
         TM    TT_BSTAT,TT_BSRIA   Are we approving now?                        
         JNZ   UTIM0180            Yes                                          
         MVC   TIMSEQ#,TT_CKSBR    keep sequence of last unchanged              
         J     UTIM0160             time record                                 
                                                                                
K        USING TIMRECD,DMKEY                                                    
UTIM0180 MVC   K.TIMKEY,BLANKS     Locate a record to add this time to          
         MVC   K.TIMKCPY,CPY                                                    
         MVC   K.TIMKULA,HD_1RULA                                               
         MVC   K.TIMKOFF,TT_COFF                                                
         MVC   K.TIMKCCPY,CPY                                                   
         MVC   K.TIMKULC,TT_CAULA                                               
         MVC   K.TIMKPEDT,HD_PEDT                                               
         MVC   K.TIMKREF,TIMEREF                                                
         MVC   K.TIMKSBR,TIMSEQ#                                                
                                                                                
         MVC   DMKEYSAV,K.TIMKEY   Save key of time record                      
                                                                                
***********************************************************************         
* First look to see if we have buffered a time record (this will      *         
* happen when there are multiple adds or changes to the same time     *         
* time record in an upload)                                           *         
***********************************************************************         
                                                                                
         MVI   TIMMODE,TIMMBUFF    Set reading buffer mode                      
         GOTOR BUFREC,DMCB,('BUFRDH',TIMRECQ),TIMRECD                           
         J     UTIM0200                                                         
                                                                                
UTIM0190 GOTOR BUFREC,DMCB,('BUFNXT',TIMRECQ),TIMRECD                           
                                                                                
UTIM0200 JE    UTIM0240            Found in buffer - try adding time            
                                                                                
***********************************************************************         
* Buffer record not found - look on the file to find a record to add  *         
* the data to                                                         *         
***********************************************************************         
                                                                                
UTIM0210 MVI   TIMMODE,TIMMFILE    Set reading file mode                        
         MVC   K.TIMKEY,DMKEYSAV   Reset time record key                        
         TM    UPDTIND,UPDTIFND    Was a record found                           
         JZ    UTIM0215                                                         
         IC    R0,TIMSEQ#          Yes - bump sequence number                   
         AHI   R0,1                                                             
         STC   R0,TIMSEQ#                                                       
         CLC   K.TIMKSBR,TIMSEQ#                                                
         JNL   *+10                                                             
         MVC   K.TIMKSBR,TIMSEQ#   Set current sequence number                  
         MVC   DMKEYSAV,K.TIMKEY                                                
                                                                                
UTIM0215 GOTOR GODMGR,DMCB,('RUD',DMRDHI),ACCDIR,K.TIMKEY,K.TIMKEY              
*        J     UTIM0230                                                         
*TIM0220 GOTOR GODMGR,DMCB,('RUD',DMRSEQ),ACCDIR,K.TIMKEY,K.TIMKEY              
         TM    8(R1),IOEDEL        Test record is deleted                       
         JNZ   UTIM0240                                                         
         CLI   8(R1),0             Test any other errors                        
         JE    UTIM0240                                                         
         DC    H'0'                                                             
                                                                                
UTIM0240 CLC   K.TIMKEY(TIMKSBR-TIMKEY),DMKEYSAV                                
         JE    UTIM0250                                                         
         CLI   TIMMODE,TIMMBUFF    Test buffer reading mode                     
         JE    UTIM0190            Yes - get next buffer record                 
         OI    UPDTIND,UPDTIADD    Set to add new record                        
         J     UTIM0260                                                         
                                                                                
UTIM0250 CLC   K.TIMKEY,DMKEYSAV   Does the sequence match                      
         JL    UTIM0255             don't allow to be lower                     
         OI    UPDTIND,UPDTIFND    Set found a record                           
         MVC   TIMSEQ#,K.TIMKSBR   Set current sequence number                  
         MVC   DMDA,K.TIMKDA       Set disk address                             
         GOTOR BUFREC,DMCB,('BUFGET+RUD',TIMRECQ),TIMRECD,AUTHOOK2              
         TM    DMRETN,IOEDEL       Is this record deleted?                      
         JZ    *+12                                                             
         OI    UPDTIND,UPDTIDEL    Set re-using deleted record                  
         J     UTIM0260            Create new time record                       
                                                                                
         GOTOR ADDCLS              Add cluster to time record                   
         JE    UTIM0290            Data fits - update record in buffer          
UTIM0255 CLI   TIMMODE,TIMMBUFF    Test reading mode                            
         JE    UTIM0190            Get next record in buffer                    
         J     UTIM0210            Get next file record                         
                                                                                
***********************************************************************         
* No records found to add the new data to - create a new time record  *         
***********************************************************************         
                                                                                
UTIM0260 XC    TIMRECD(255),TIMRECD                                             
         MVC   TIMKEY,DMKEYSAV                                                  
         MVC   TIMKSBR,TIMSEQ#                                                  
         MVC   TIMRSTAT,HD_TSSTN                                                
         MVI   TIMRRI2,TIMKRI2Q                                                 
         MVI   TIMRRI3,TIMKRI3Q                                                 
         MVI   TIMRRI4,TIMKRI4Q                                                 
                                                                                
E        USING PIDELD,TIMRFST                                                   
         MVI   E.PIDEL,PIDELQ      Build PIDEL at front of time record          
         MVI   E.PIDLN,PIDLNQ                                                   
         MVC   E.PIDNO,HD_PPID#                                                 
         DROP  E                                                                
                                                                                
         LA    RE,TIMRFST+PIDLNQ   RE=A(Where to add time elements)             
         USING GDAELD,RE                                                        
         MVI   GDAEL,GDAELQ        Yes - build general date element             
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDACLSUB                                                 
         MVC   GDADATE,HD_SUBCL                                                 
         LA    RE,GDALNQ(RE)                                                    
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDALMSUB                                                 
         MVC   GDADATE,HD_SUBLM                                                 
         LA    RE,GDALNQ(RE)                                                    
         TM    TRNCPYS9,CPYSEDHO                                                
         JZ    UTIM0270                                                         
         USING SCIELD,RE                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITEDTH                                                 
         ZAP   SCIAMNT,HD_EDHRS                                                 
         LA    RE,SCILN1Q(RE)                                                   
         DROP  RE                                                               
                                                                                
UTIM0270 LA    RF,TIMRECD                                                       
         LR    R0,RE                                                            
         AHI   R0,1                                                             
         SR    R0,RF                                                            
         SR    RF,RF                                                            
         ICM   RF,3,TT_DLEN                                                     
         AR    R0,RF                                                            
         CHI   R0,TIMMAXLN         Test data will fit on this record            
         JNH   *+6                 Yes                                          
         DC    H'0'                This shouldn't happen                        
         STCM  R0,3,TIMRLEN        Set record length                            
*                                                                               
         LA    R0,TT_DATA                                                       
         LR    R1,RF                                                            
         AHI   RF,1                Pad with 0 at end of record                  
         MVCL  RE,R0               Move data to record                          
*                                                                               
         GOTOR SETSEQ              Set sequence numbers                         
*                                                                               
UTIM0280 TM    UPDTIND,UPDTIDEL    Test re-using a deleted record               
         JNZ   UTIM0290                                                         
                                                                                
         L     R0,DMADD#           Bump ADDREC sequence number                  
         AHI   R0,1                                                             
         ST    R0,DMADD#                                                        
         STCM  R0,15,DMDA          and return as disk address                   
         XC    DMDA,EFFS                                                        
         GOTOR BUFREC,DMCB,('BUFADD',TIMRECQ),('BUFINEW',TIMRECD),0             
         J     UTIM0160                                                         
                                                                                
UTIM0290 GOTOR BUFREC,DMCB,('BUFPUT',TIMRECQ),('BUFIELA',TIMRECD)               
         NI    UPDTIND,X'FF'-UPDTIDEL                                           
         J     UTIM0160                                                         
                                                                                
***********************************************************************         
* Update time records for real and create passive pointers - delete   *         
* any empty time records                                              *         
***********************************************************************         
                                                                                
UTIM0300 LA    R0,TT_KEY           Clear TT_KEY                                 
         LHI   R1,TT_LN1Q                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR BUFREC,DMCB,('BUFSRT',TIMRECQ),TIMRECD,0,SORTSRF                 
         GOTOR BUFREC,DMCB,('BUFRDH',TIMRECQ),TIMRECD                           
         J     UTIM0320                                                         
UTIM0310 GOTOR BUFREC,DMCB,('BUFNXT',TIMRECQ),TIMRECD                           
UTIM0320 JNE   UTIM0370                                                         
                                                                                
         LHI   R0,TIMRFST-TIMRECD+PIDLNQ+GDALNQ+GDALNQ+SCILN1Q+1                
         CLM   R0,3,TIMRLEN        Test record is empty                         
         JL    UTIM0330                                                         
                                                                                
         OI    TIMRSTAT,TIMSDELT   Yes - delete it                              
         GOTOR GODMGR,DMCB,PUTREC,ACCMST,DMDA,TIMRECD,DMWORK                    
         JE    *+6                                                              
         DC    H'0'                                                             
K        USING TIMRECD,DMKEY                                                    
         OI    K.TIMKSTAT,TIMSDELT                                              
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,K.TIMKEY,K.TIMKEY                       
         JE    UTIM0310                                                         
         DC    H'0'                                                             
                                                                                
UTIM0330 TM    DMFLAG,BUFIELA+BUFIPAD+BUFISTA+BUFINEW                           
         JZ    UTIM0360                                                         
                                                                                
         MVC   TIMRSTAT,HD_TSSTN   Set status                                   
         MVC   K.TIMKSTAT,HD_TSSTN   Set status in directory                    
                                                                                
*        GOTOR SETSEQ              Set sequence numbers                         
         GOTOR SETMOS              Set activity months                          
         GOTOR SETGDA              Set General Date Element                     
                                                                                
         MVC   K.TIMKLMOS,TIMRLMOS Set months in directory record               
         MVC   K.TIMKHMOS,TIMRHMOS                                              
                                                                                
         TM    DMFLAG,BUFINEW      Test ADDREC pending                          
         JZ    UTIM0340            No                                           
         GOTOR GODMGR,DMCB,ADDREC,ACCMST,DMDA,TIMRECD,DMWORK                    
         JE    UTIM0360                                                         
         DC    H'0'                                                             
                                                                                
UTIM0340 GOTOR GODMGR,DMCB,PUTREC,ACCMST,DMDA,TIMRECD,DMWORK                    
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GODMGR,DMCB,('RUD',DMREAD),ACCDIR,K.TIMRECD,DMKEYSAV             
         JE    UTIM0350                                                         
         TM    8(R1),IOEDEL        Is this record deleted?                      
         JNZ   UTIM0350            Yes - ignore                                 
         DC    H'0'                                                             
                                                                                
UTIM0350 CLC   K.TIMRECD(ACCKLEN),DMKEYSAV                                      
         JE    UTIM0360                                                         
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,K.TIMRECD,K.TIMRECD                     
         JE    UTIM0360                                                         
         DC    H'0'                                                             
                                                                                
UTIM0360 TM    DMFLAG,BUFIELD+BUFIELA+BUFIPAD+BUFINEW+BUFISTA                   
         JZ    UTIM0310                                                         
         GOTOR BLDPAS,DMCB,('PP_AADD',TIMRECD),('ACRTTIM',ONERLVS)              
         J     UTIM0310                                                         
                                                                                
***********************************************************************         
* Read TSW passives and change any time record that doesn't have the  *         
* new time sheet status                                               *         
***********************************************************************         
                                                                                
UTIM0370 GOTOR BUFREC,DMCB,('BUFSRT',TIMRECQ),TIMRECD,0,SORTORI                 
         CLC   HD_TSSTN,HD_TSSTO   Test change of time sheet status             
         JE    EXITY               No - exit                                    
                                                                                
PP       USING TSWRECD,DMKEY       Build key of TSW record                      
         GOTOR BLDTSW              Build TSW key/set DMKEYSAV                   
                                                                                
         GOTOR GODMGR,DMCB,DMRDHI,ACCDIR,PP.TSWKEY,PP.TSWKEY                    
         J     UTIM0390                                                         
UTIM0380 GOTOR GODMGR,DMCB,DMRSEQ,ACCDIR,PP.TSWKEY,PP.TSWKEY                    
UTIM0390 CLC   PP.TSWKEY(TSWKULC-TSWKEY),DMKEYSAV                               
         JNE   EXITY                                                            
         MVC   DMKEYSAV,PP.TSWKEY  Save key of record we read                   
         MVC   DMDA,PP.TSWKDA      Set disk address                             
                                                                                
         GOTOR BUFREC,DMCB,('BUFGET+RUP',TIMRECQ),TIMRECD,AUTHOOK3              
         JNE   UTIM0400            No change to this record                     
                                                                                
         GOTOR SETGDA              Set General Date Element                     
                                                                                
         GOTOR GODMGR,DMCB,PUTREC,ACCMST,DMDA,TIMRECD,DMWORK                    
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,DMKEY,DMKEY                             
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR BLDPAS,DMCB,('PP_AADD',TIMRECD),('ACRTTIM',ONERLVS)              
                                                                                
UTIM0400 GOTOR GODMGR,DMCB,DMREAD,ACCDIR,DMKEYSAV,PP.TSWKEY                     
         JE    UTIM0380            Get next TSW passive key                     
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* Read TSW passives and delete all time records (only used when a     *         
* time sheet is deleted)                                              *         
***********************************************************************         
                                                                                
UTIM0410 GOTOR BLDTSW              Build TSW key/set DMKEYSAV                   
                                                                                
         GOTOR GODMGR,DMCB,DMRDHI,ACCDIR,PP.TSWKEY,PP.TSWKEY                    
         J     UTIM0430                                                         
UTIM0420 GOTOR GODMGR,DMCB,DMRSEQ,ACCDIR,PP.TSWKEY,PP.TSWKEY                    
UTIM0430 CLC   PP.TSWKEY(TSWKULC-TSWKEY),DMKEYSAV                               
         JNE   EXITY                                                            
         MVC   DMKEYSAV,PP.TSWKEY  Save key of record we read                   
                                                                                
         MVC   DMDA,PP.TSWKDA                                                   
         GOTOR GODMGR,DMCB,('RUP',GETREC),ACCMST,DMDA,TIMRECD,DMWORK            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR BLDPAS,DMCB,('PP_ADEL',TIMRECD),('ACRTTIM',ONERLVS)              
                                                                                
         OI    TIMRSTAT,TIMSDELT   Delete file record                           
         GOTOR GODMGR,DMCB,PUTREC,ACCMST,DMDA,TIMRECD,DMWORK                    
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GODMGR,DMCB,('RUP',DMREAD),ACCDIR,TIMKEY,K.TIMKEY                
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    K.TIMKSTAT,TIMSDELT Delete directory record                      
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,K.TIMKEY,K.TIMKEY                       
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GODMGR,DMCB,DMREAD,ACCDIR,DMKEYSAV,PP.TSWKEY                     
         JE    UTIM0420            Get next TSW passive key                     
         DC    H'0'                                                             
         DROP  K                                                                
         EJECT                                                                  
***********************************************************************         
* Build key of TSW record and copy into DMKEYSAV                      *         
***********************************************************************         
                                                                                
BLDTSW   XC    PP.TSWKEY,PP.TSWKEY                                              
         MVI   PP.TSWKTYP,TSWKTYPQ                                              
         MVI   PP.TSWKSUB,TSWKSUBQ                                              
         MVC   PP.TSWKCPY,CPY                                                   
         MVC   PP.TSWKPER,HD_1RPER                                              
         MVC   PP.TSWKEND,HD_PEDTC                                              
         LLC   RF,ONERL3L                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   PP.TSWKODS(0),HD_1RACT                                           
         EX    RF,0(R1)                                                         
         OC    PP.TSWKODS,BLANKS                                                
         MVC   DMKEYSAV,PP.TSWKEY                                               
         BR    RE                                                               
         DROP  PP                                                               
         EJECT                                                                  
***********************************************************************         
* Add time cluster to a time record if it will fit else exit with     *         
* CC=Not equal                                                        *         
***********************************************************************         
                                                                                
ADDCLS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDCLS*'                                                      
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,TIMRLEN        R1=Current record length                     
         SR    RF,RF                                                            
         ICM   RF,3,TT_DLEN                                                     
         LA    R0,0(R1,RF)         R0=Total record length                       
         CHI   R0,TIMMAXLN         Test data will fit on this record            
         JH    EXITN               No - look for another record                 
                                                                                
         MVI   BYTE1,0                                                          
                                                                                
         CLI   TIMRFST,PIDELQ                                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TIMRFST,1           Set PIDEL to low value                       
         CLI   TIMRFST+PIDLNQ,GDAELQ                                            
         JNE   ADDCLS01                                                         
         MVI   TIMRFST+PIDLNQ,2                                                 
         CLI   TIMRFST+PIDLNQ+GDALNQ,GDAELQ                                     
         JNE   ADDCLS01                                                         
         MVI   TIMRFST+PIDLNQ+GDALNQ,2                                          
                                                                                
ADDCLS01 SR    R0,R0                                                            
         LA    R3,TIMRFST                                                       
         USING TIMELD,R3           R3=A(first element on time record)           
ADDCLS02 CLI   TIMEL,0             Test end of record                           
         JE    ADDCLS10                                                         
         CLI   TIMEL,TIMELQ        Test time element                            
         JNE   ADDCLS08                                                         
         MVI   BYTE1,1              Set found some TIMELDs                      
         MVC   TIMESEQ#,TIMSEQ     Set sequence number of last                  
         CLI   TIMETYP,TIMETIME    Test time status element                     
         JNE   ADDCLS04                                                         
         CLC   TIMEIDNO,TT_TIME#   Test same time line                          
         JE    ADDCLS12                                                         
         J     ADDCLS08                                                         
ADDCLS04 CLI   TIMETYP,TIMEITMS    Test item element                            
         JNE   ADDCLS06                                                         
         CLC   TIMIIDNO,TT_TIME#   Test same time line                          
         JE    ADDCLS12                                                         
         J     ADDCLS08                                                         
ADDCLS06 CLI   TIMETYP,TIMEDNAR    Test day narrative element                   
         JNE   ADDCLS08                                                         
         CLC   TIMDIDNO,TT_TIME#   Test same time line                          
         JE    ADDCLS12                                                         
ADDCLS08 IC    R0,TIMLN            Bump to next element on record               
         AR    R3,R0                                                            
         J     ADDCLS02                                                         
*                                                                               
ADDCLS10 OC    TT_ITEM#,TT_ITEM#   For items that span multiple records         
         JNZ   ADDCLS12                                                         
         OC    TT_NAR#,TT_NAR#     For narrative always go to next rec          
         JNZ   ADDCLS12                                                         
         CLI   BYTE1,1             Any TIMELDs found?                           
         JNE   ADDCLS11            No - reset sequence as TIMEINP               
         IC    R0,TIMESEQ#         Use next sequence number if no tim           
         AHI   R0,1                found with same time line number             
         STC   R0,TIMESEQ#                                                      
         J     ADDCLS12                                                         
*                                                                               
ADDCLS11 MVI   TIMESEQ#,1          Set sequence as 1 for first timel            
*                                    on record                                  
ADDCLS12 LA    R3,TT_DATA          Point to time cluster to add                 
         USING TIMELD,R3                                                        
ADDCLS14 CLI   TIMEL,0             Test end of time cluster                     
         JE    ADDCLS16                                                         
         MVC   TIMSEQ,TIMESEQ#     Set sequence number                          
         GOTOR HELLO,DMCB,(C'P',ACCMST),TIMRECD,TIMELD,0                        
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         IC    R0,TIMLN            Bump to next element                         
         AR    R3,R0                                                            
         J     ADDCLS14                                                         
                                                                                
ADDCLS16 MVI   TIMRFST,PIDELQ      Reset PID element code                       
         CLI   TIMRFST+PIDLNQ,2                                                 
         JNE   EXITY                                                            
         MVI   TIMRFST+PIDLNQ,GDAELQ                                            
         CLI   TIMRFST+PIDLNQ+GDALNQ,2                                          
         JNE   EXITY                                                            
         MVI   TIMRFST+PIDLNQ+GDALNQ,GDAELQ                                     
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Set all elements for a time cluster to be deleted                   *         
***********************************************************************         
                                                                                
DELCLS   SR    R1,R1                                                            
         ICM   R1,3,TT_SCLST                                                    
         LA    R1,TIMRECD(R1)      R1=A(Start of cluster)                       
         USING TIMELD,R1                                                        
         CLI   TIMEL,TIMELQ                                                     
         JE    *+6                                                              
         DC    H'0'                Bad start of cluster displacement            
         SR    RF,RF                                                            
         ICM   RF,3,TT_ECLST                                                    
         LA    RF,TIMRECD(RF)      RF=A(End of cluster)                         
                                                                                
         XR    R0,R0               Delete all elements for cluster              
                                                                                
DELCLS02 CLI   TIMETYP,TIMEDNAR    Is it day narrative TIMELD?                  
         JNE   DELCLS04                                                         
         CLI   TT_TTYP,TT_TDNAR    Do we have day narrative                     
         JNE   DELCLS06            Then ok to delete                            
         CLC   TIMDNIDN,TT_NAR#    Match sequence                               
         JE    DELCLS04                                                         
         J     DELCLS06            Otherwise skip deleting it                   
*                                                                               
DELCLS04 MVI   TIMEL,FF            Set to delete this element                   
*                                                                               
DELCLS06 IC    R0,TIMLN                                                         
         AR    R1,R0                                                            
         CR    R1,RF               Test at end of cluster                       
         BER   RE                  Yes                                          
         JNH   DELCLS02            No - bump to next element                    
         DC    H'0'                Bad end of cluster displacement              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Set lowest and highest activity month for time on a time record     *         
***********************************************************************         
                                                                                
SETMOS   LA    RF,TIMRFST                                                       
         USING TIMELD,RF                                                        
         XC    HALF2,HALF2                                                      
         MVC   HALF1,EFFS                                                       
         SR    R0,R0                                                            
                                                                                
SETMOS02 CLI   TIMEL,0             Test end of record                           
         JE    SETMOSX                                                          
         CLI   TIMEL,TIMELQ        Test time element                            
         JNE   SETMOS06                                                         
                                                                                
         CLI   TIMETYP,TIMEITMS    Is it materials?                             
         JNE   SETMOS04                                                         
         CLC   HALF2,TIMIMOA                                                    
         JNL   *+10                                                             
         MVC   HALF2,TIMIMOA                                                    
         CLC   HALF1,TIMIMOA                                                    
         JNH   *+10                                                             
         MVC   HALF1,TIMIMOA                                                    
         J     SETMOS06                                                         
                                                                                
SETMOS04 CLI   TIMETYP,TIMEINP     Is it time?                                  
         JNE   SETMOS06                                                         
         CLC   HALF2,TIMMOA                                                     
         JNL   *+10                                                             
         MVC   HALF2,TIMMOA                                                     
         CLC   HALF1,TIMMOA                                                     
         JNH   SETMOS06                                                         
         MVC   HALF1,TIMMOA                                                     
                                                                                
SETMOS06 IC    R0,TIMLN            Bump to next element                         
         AR    RF,R0                                                            
         J     SETMOS02                                                         
                                                                                
SETMOSX  MVC   TIMRLMOS,HALF1                                                   
         MVC   TIMRHMOS,HALF2                                                   
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* Add Date to the GDAELD elements                                     *         
***********************************************************************         
                                                                                
SETGDA   LA    R1,TIMRFST          Point to first element on record             
         USING GDAELD,R1                                                        
         MVI   BYTE1,0                                                          
         SR    R0,R0                                                            
         ST    RE,FULL1            Save off the return address                  
SETGDA02 CLI   GDAEL,0             Test end of record                           
         JE    SETGDA10                                                         
         CLI   GDAEL,GDAELQ        Is this a General date element?              
         JNE   SETGDA04                                                         
         CLI   GDATYPE,GDACLSUB    Client Approver?                             
         JE    SETGDA06                                                         
         CLI   GDATYPE,GDALMSUB    Line Manager Approver?                       
         JE    SETGDA08                                                         
SETGDA04 IC    R0,GDALN            Bump to next element                         
         AR    R1,R0                                                            
         J     SETGDA02                                                         
                                                                                
SETGDA06 OI    BYTE1,X'80'         Show that weve updated a SUBCL elem          
         OC    HD_SUBCL,HD_SUBCL                                                
         JZ    SETGDA04                                                         
         MVC   GDADATE,HD_SUBCL                                                 
         J     SETGDA04                                                         
                                                                                
SETGDA08 OI    BYTE1,X'40'         Show that weve updated a SUBLM elem          
         OC    HD_SUBLM,HD_SUBLM                                                
         JZ    SETGDA04                                                         
         MVC   GDADATE,HD_SUBLM                                                 
         J     SETGDA04                                                         
                                                                                
SETGDA10 TM    BYTE1,X'80'         Did we update an SUBCL element?              
         JO    SETGDA12                                                         
         LA    R1,ELEM                                                          
         OC    HD_SUBCL,HD_SUBCL   Do we have client apprvr submit date         
         JZ    SETGDA12            No                                           
         XC    ELEM,ELEM                                                        
         MVI   GDAEL,GDAELQ        Yes - build general date element             
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDACLSUB                                                 
         MVC   GDADATE,HD_SUBCL                                                 
         GOTOR HELLO,DMCB,(C'P',ACCMST),TIMRECD,ELEM,0                          
SETGDA12 TM    BYTE1,X'40'         Did we update an SUBLM element?              
         JO    SETGDAX                                                          
         OC    HD_SUBLM,HD_SUBLM   Do we have line manager submit date          
         JZ    SETGDAX             No                                           
         LA    R1,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDALMSUB                                                 
         MVC   GDADATE,HD_SUBLM                                                 
         GOTOR HELLO,DMCB,(C'P',ACCMST),TIMRECD,ELEM,0                          
                                                                                
SETGDAX  L     RE,FULL1                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Re-sequence all time elements in a time record                      *         
***********************************************************************         
                                                                                
SETSEQ   LA    R1,TIMRFST          Point to first element on record             
         USING TIMELD,R1                                                        
         XR    R0,R0                                                            
         XR    RF,RF               RF=Sequence number                           
         MVI   BYTE1,0                                                          
*                                                                               
SETSEQ02 CLI   TIMEL,0             Test end of record                           
         BER   RE                                                               
         CLI   TIMEL,TIMELQ        Is this a time element?                      
         JNE   SETSEQ20                                                         
         CLI   TIMETYP,TIMEINP     Test start of time cluster                   
         JNE   SETSEQ04                                                         
         MVI   BYTE1,1                                                          
         AHI   RF,1                Yes - bump sequence number                   
         STC   RF,TIMESEQ#         save sequence                                
SETSEQ04 CLI   TIMETYP,TIMEITMS                                                 
         JE    SETSEQ06                                                         
         CLI   TIMETYP,TIMEDNAR                                                 
         JNE   SETSEQ08                                                         
                                                                                
SETSEQ06 CLI   BYTE1,1             Did we find TIMEINP first                    
         JE    SETSEQ08             if not use sequence from last               
         LLC   RF,TIMESEQ#           record                                     
                                                                                
SETSEQ08 STC   RF,TIMSEQ           Set sequence number                          
SETSEQ20 IC    R0,TIMLN            Bump to next element                         
         AR    R1,R0                                                            
         J     SETSEQ02                                                         
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* BUFREC hook routines for time records                               *         
***********************************************************************         
                                                                                
         USING BR_D,R2                                                          
K        USING TIMRECD,DMKEY                                                    
R        USING TIMRECD,BR_REC                                                   
                                                                                
***********************************************************************         
* Cluster deletion - delete passives (once only)                      *         
***********************************************************************         
                                                                                
UTHOOK1  LR    R2,R1               Point to BR_D                                
         TM    BR_FLAG,BUFIPAD     Test already deleted passives                
         JNZ   EXITY                                                            
         GOTOR BLDPAS,DMCB,('PP_ADEL',R.TIMRECD),('ACRTTIM',ONERLVS)            
         OI    BR_FLAG,BUFIPAD     Set passives deleted                         
         J     EXITY               Buffer this record                           
                                                                                
***********************************************************************         
* Status change on uploaded time records                              *         
***********************************************************************         
                                                                                
UTHOOK2  LR    R2,R1               Point to BR_D                                
         TM    BR_FLAG,BUFIPAD     Test passives were deleted                   
         JNZ   UTHOOK22                                                         
         GOTOR BLDPAS,DMCB,('PP_ADEL',R.TIMRECD),('ACRTTIM',ONERLVS)            
         OI    BR_FLAG,BUFIPAD     Set passives deleted                         
UTHOOK22 CLC   R.TIMRSTAT,HD_TSSTN Test change of status                        
         JE    EXITY               No - buffer this record only                 
         OI    BR_FLAG,BUFISTA     Set status change                            
         MVC   R.TIMRSTAT,HD_TSSTN Set new status                               
         J     EXITY               Buffer this record                           
                                                                                
***********************************************************************         
* Status change on non-uploaded time                                  *         
***********************************************************************         
                                                                                
UTHOOK3  LR    R2,R1               Point to BR_D                                
         CLI   TB.TSACTN,TSAPUT    Has record already been buffered             
         JE    EXITN               Yes - ignore this one                        
         CLC   R.TIMRSTAT,HD_TSSTN Test change of status                        
         JE    EXITN               No - ignore this record                      
         GOTOR BLDPAS,DMCB,('PP_ADEL',R.TIMRECD),('ACRTTIM',ONERLVS)            
         OI    BR_FLAG,BUFISTA+BUFIPAD                                          
         MVC   R.TIMRSTAT,HD_TSSTN Set new status in file                       
         MVC   K.TIMKSTAT,HD_TSSTN Set new status in directory                  
         J     EXITY               Buffer this record                           
         DROP  R2,K                                                             
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR for old and new time records                      *         
*                                                                     *         
* Note that when running off-line the TSAR buffers are acquired only  *         
* once - the XC(TSPNEWL) below is intentional as the first time       *         
* through the code TSAR will issue the GETMAIN for the buffer and     *         
* set its address in the TSAR block                                   *         
***********************************************************************         
                                                                                
NEWBUF   EQU   1                   Use TSAROLDT (new time buffer)               
OLDBUF   EQU   2                   Use TSAROLDT (old time buffer)               
                                                                                
BUFTIM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BUFTIM*'                                                      
                                                                                
         LR    R2,R1               Point to parameter list                      
         LA    R3,TSAROLDT         Point to correct TSAR block                  
         CLI   3(R2),NEWBUF                                                     
         JNE   *+8                                                              
         LA    R3,TSARNEWT                                                      
                                                                                
         USING TSARD,R3            R3=A(TSAR block)                             
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   BUFTIM02                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVC   TSACTN,0(R2)                                                     
         MVC   TSACOM,ACOMFACS                                                  
         LA    R0,TT_D                                                          
         ST    R0,TSAREC           Set A(record)                                
         LHI   R0,TWOK                                                          
         OC    TSBUFFL,TSBUFFL                                                  
         JNZ   *+8                                                              
         STCM  R0,3,TSBUFFL        Set require 2MB off-line                     
         MVI   TSRECI,TSRMINB1     Set which buffer to use                      
         CLI   3(R2),OLDBUF                                                     
         JNE   *+8                                                              
         MVI   TSRECI,TSRMINB2                                                  
         OI    TSRECI,TSRXTN                                                    
         MVI   TSKEYL,TT_SORTL     Set key length                               
         LHI   R0,TT_RECL                                                       
         STCM  R0,3,TSRECL         Set record length                            
         MVI   TSINDS,TSINODSK     Set no disk writes (save/restore)            
         GOTOR TSAR,TSARD          Call TSAR                                    
         TM    TSINDS,TSIINIOK                                                  
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
BUFTIM02 TM    TSINDS,TSIINIOK     Test initialised                             
         JNZ   *+6                                                              
         DC    H'0'                No - invalid action sequence                 
                                                                                
         MVC   TSACTN,0(R2)        Set action                                   
                                                                                
         CLI   TSACTN,TSASRT       Test sorting buffer                          
         JNE   BUFTIM04                                                         
         L     R1,4(R2)                                                         
         MVC   TSRTPARM,0(R1)      Yes - set sort parameters                    
                                                                                
BUFTIM04 GOTOR TSAR,TSARD          Call TSAR                                    
         MVC   TSARERRS,TSERRS     Set error condition if any                   
         CLI   TSARERRS,0          Set condition code for caller                
         J     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* Manage a TSAR buffer of ACCMST records                              *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows or zero if last     *         
*        time call                                                    *         
*                                                                     *         
*        P1/0   - Action/read flags                                   *         
*        P1/3   - Record type                                         *         
*        P2/0   - User flags                                          *         
*        P2/1-3 - A(Record)                                           *         
*        P3     - A(Caller hook routine for buffer add)               *         
*        P4     - A(Sort parameters)                                  *         
***********************************************************************         
                                                                                
BUFGET   EQU   1                   Get/create a buffer entry                    
BUFPUT   EQU   2                   Update a buffer entry                        
BUFADD   EQU   3                   Add a buffer entry                           
BUFRDH   EQU   4                   Read high for a record type                  
BUFNXT   EQU   5                   Get next record for a record type            
BUFSRT   EQU   6                   Sort the buffer                              
                                                                                
TIMRECQ  EQU   1                   Time record equate                           
                                                                                
BUFREC   NTR1  WORK=(RC,BRWORKL)                                                
         J     *+12                                                             
         DC    C'*BUFREC*'                                                      
         LR    R2,R1               Point to parameter list                      
                                                                                
         USING BRWORKD,RC          RC=A(local working storage)                  
         MVC   BRACTN,0(R2)                                                     
         MVC   BRINDS,0(R2)                                                     
         MVC   BRFLAG,4(R2)        Set caller flags                             
         NI    BRACTN,B'00000111'  Isolate action number                        
         NI    BRINDS,B'11111000'  Isolate read flags                           
         MVC   BRRECTY,3(R2)       Set record type                              
                                                                                
         TM    BUFRIND,BUFRIFST    Test first time                              
         JNZ   BUFREC02                                                         
         MVI   TB.TSACTN,TSAINI    Yes - initialise TSAR buffer                 
         MVI   TB.TSRECI,TSRXTN+TSRTSAB1                                        
         MVI   TB.TSKEYL,BR_KL                                                  
         LHI   R0,BR_LNQ                                                        
         STCM  R0,3,TB.TSRECL                                                   
         LHI   R0,TWOK             Default to 2MB off-line                      
         OC    TB.TSBUFFL,TB.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TB.TSBUFFL     Set buffer length if not set                 
         MVC   TB.TSACOM,ACOMFACS                                               
         GOTOR TSAR,TB.TSARD       Initialise TSAR buffer                       
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    BUFRIND,BUFRIFST    Set not first time                           
         LTR   R8,R8               Test off-line                                
         JZ    BUFREC02                                                         
         L     RF,LP_ABLK4         Return block to caller                       
         MVC   0(L'TSARRECS,RF),TSARRECS                                        
                                                                                
BUFREC02 L     R4,4(R2)            R4=A(Record)                                 
         SHI   R4,BR_HL            Back up by header length                     
         ST    R4,TB.TSAREC        Set A(TSAR record)                           
R        USING BR_D,R4                                                          
         MVC   BRSAVE(BR_HL),0(R4) Save what's there                            
         XC    R.BR_D(BR_HL),R.BR_D                                             
         MVC   R.BR_RECTY,BRRECTY  Set record type                              
         MVI   DMRETN,0            Clear return flags                           
         MVI   BRTSARER,0          Clear TSAR return flags                      
                                                                                
         CLI   BRACTN,BUFGET       Test get record from buffer                  
         JNE   BUFREC10                                                         
         MVC   R.BR_DA,DMDA        Set disk address                             
         MVI   TB.TSACTN,TSARDH                                                 
         GOTOR TSAR,TB.TSARD       See if we have record already                
         MVI   TB.TSACTN,TSAPUT    Set action to put record back                
         TM    TB.TSERRS,TSEEOF    End of file                                  
         JNZ   BUFREC04            Yes                                          
         CLC   R.BR_DA,DMDA        Is the disk address the same                 
         JE    BUFREC06            Yes - do put                                 
BUFREC04 MVI   TB.TSACTN,TSAADD    Set action to add record                     
         MVC   R.BR_RECTY,BRRECTY  Set record type                              
         MVC   R.BR_DA,DMDA        Set disk address                             
         MVC   R.BR_INDS,BRINDS    Set read flags                               
         MVC   R.BR_FLAG,BRFLAG    Set user flags                               
                                                                                
         GOTOR GODMGR,PARM,(R.BR_INDS,GETREC),ACCMST,R.BR_DA,          +        
               R.BR_REC,R.BR_WORK                                               
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   R.BR_KEY,R.BR_REC                                                
         GOTOR GODMGR,(R1),(R.BR_INDS,DMREAD),ACCDIR,R.BR_REC,DMKEY             
         TM    8(R1),FF-(IOEDEL)                                                
         JZ    *+6                                                              
         DC    H'0'                                                             
         MVC   DMRETN,8(R1)        Save directory return                        
                                                                                
BUFREC06 ICM   RF,15,8(R2)         Test caller hook routine provided            
         JZ    BUFREC08                                                         
         LARL  RE,BUFREC08                                                      
         NTR1  LABEL=NO                                                         
         GOTOR (RF),R.BR_D         Yes - call it                                
                                                                                
BUFREC08 JNE   EXIT                Caller doesn't want to buffer record         
         GOTOR TSAR,TB.TSARD       Add or put record to TSAR buffer             
         JE    BUFREC20                                                         
         DC    H'0'                                                             
                                                                                
BUFREC10 CLI   BRACTN,BUFADD       Test add a new record                        
         JE    *+12                                                             
         CLI   BRACTN,BUFPUT       Test put record                              
         JNE   BUFREC12                                                         
         MVC   R.BR_DA,DMDA        Set disk address                             
         MVC   R.BR_WORK,DMWORK    Set PUTREC work area                         
         MVC   R.BR_FLAG,DMFLAG    Set current user flags                       
         OC    R.BR_FLAG,BRFLAG    'OR' in new user flags                       
         MVC   R.BR_KEY,R.BR_REC   Copy record to key                           
         MVI   TB.TSACTN,TSAADD    Set action to add                            
         CLI   BRACTN,BUFADD       Test adding a new record                     
         JE    *+8                                                              
         MVI   TB.TSACTN,TSAPUT    No - set action to put                       
         GOTOR TSAR,TB.TSARD                                                    
         JE    BUFREC22                                                         
         DC    H'0'                                                             
                                                                                
BUFREC12 CLI   BRACTN,BUFRDH       Test read high for a record type             
         JNE   BUFREC14                                                         
         MVI   TB.TSACTN,TSARDH    Set action to 'read high'                    
         J     BUFREC18                                                         
                                                                                
BUFREC14 CLI   BRACTN,BUFNXT       Get next record for a record type            
         JNE   BUFREC16                                                         
         MVI   TB.TSACTN,TSANXT    Set action to 'get next'                     
         J     BUFREC18                                                         
                                                                                
BUFREC16 CLI   BRACTN,BUFSRT       Sort buffer a record type                    
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TB.TSACTN,TSASRT    Set action to 'sort'                         
         L     R1,12(R2)                                                        
         MVC   TB.TSRTPARM,0(R1)   Yes - set sort parameters                    
                                                                                
BUFREC18 GOTOR TSAR,TB.TSARD       Read high/get next                           
         MVC   BRTSARER,TB.TSERRS                                               
         TM    BRTSARER,TSEEOF     Test end of file                             
         JNZ   BUFREC22                                                         
         MVI   BRTSARER,0                                                       
         CLC   R.BR_RECTY,BRRECTY  Test same record type                        
         JE    BUFREC20                                                         
         MVI   BRTSARER,TSEEOF     Set EOF if not same record type              
         J     BUFREC22                                                         
                                                                                
BUFREC20 MVC   DMDA,R.BR_DA        Set disk address                             
         MVC   DMWORK,R.BR_WORK    Set PUTREC work area                         
         MVC   DMINDS,R.BR_INDS    Set read flags                               
         MVC   DMFLAG,R.BR_FLAG    Set user flags                               
K        USING ACCRECD,DMKEY       Build directory record                       
         MVC   K.ACCKEY,R.BR_REC                                                
         MVC   K.ACCKSTA,R.BR_REC+(ACCRSTA-ACCRECD)                             
         MVC   K.ACCKDA,DMDA                                                    
                                                                                
BUFREC22 MVC   R.BR_D(BR_HL),BRSAVE                                             
                                                                                
         CLI   BRTSARER,0          Set condition code for caller                
         J     EXIT                                                             
         DROP  RC,R,K                                                           
                                                                                
BRWORKD  DSECT                     ** BUFREC local working storage **           
BRACTN   DS    X                   Action                                       
BRINDS   DS    X                   Read flags                                   
BRFLAG   DS    X                   User flags                                   
BRRECTY  DS    X                   Record type                                  
BRTSARER DS    XL(L'TSARERRS)      TSAR error                                   
BRSAVE   DS    XL(BR_HL)           Save area                                    
BRWORKL  EQU   *-BRWORKD                                                        
                                                                                
BR_D     DSECT                     ** Record buffer **                          
BR_RECTY DS    X                   Record type                                  
BR_DA    DS    XL(L'DMDA)          Disk address                                 
BR_KEY   DS    XL(L'TIMKEY)        Record key                                   
BR_KL    EQU   *-BR_D                                                           
BR_WORK  DS    XL(L'DMWORK)        PUTREC work area                             
BR_INDS  DS    X                   Read flags                                   
BR_FLAG  DS    X                   User flags                                   
BR_HL    EQU   *-BR_D              Length of header                             
BR_REC   DS    XL(IOLENQ)          ACCMST record                                
BR_LNQ   EQU   *-BR_D                                                           
SRUPD60  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Get transaction key and add supposed and real key to TSAR buffer    *         
***********************************************************************         
                                                                                
TR       USING TR_D,WORK                                                        
BLDTRK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*TRKPAS*'                                                      
         TM    TRKIND,TRKIINIT     Test TSAR buffer initialised                 
         JNZ   BLDTRK04                                                         
         OI    TRKIND,TRKIINIT                                                  
         MVI   TK.TSACTN,TSAINI    No - initialise TSAR buffer                  
         MVI   TK.TSRECI,TSRXTN+TSRMINB1                                        
         MVI   TK.TSKEYL,TR_KLQ                                                 
         LA    R0,TR.TR_D                                                       
         ST    R0,TK.TSAREC                                                     
         LHI   R0,TR_LNQ                                                        
         STCM  R0,3,TK.TSRECL                                                   
         LHI   R0,TWOK             Default to 2MB off-line                      
         OC    TK.TSBUFFL,TK.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TK.TSBUFFL     Set buffer length if not set                 
         MVC   TK.TSACOM,ACOMFACS                                               
         GOTOR TSAR,TK.TSARD       Call TSAR to initialise                      
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TK.TSACTN,TSAADD    Set TSAR action to 'add'                     
         LTR   R8,R8               Test off-line                                
         JZ    BLDTRK04                                                         
         L     RF,LP_ABLK7         Return block to caller                       
         MVC   0(L'TSARTRNK,RF),TSARTRNK                                        
                                                                                
BLDTRK04 GOTOR TSAR,TK.TSARD       Add trans keys to TSAR buffer                
         JE    EXITY                                                            
         DC    H'0'                Transaction key buffer full                  
         EJECT                                                                  
***********************************************************************         
* Read transaction key buffer and replace key on askel if different   *         
***********************************************************************         
                                                                                
TR       USING TR_D,WORK                                                        
UPDTRK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDTRK*'                                                      
         LR    R2,R1               Point to parameter list                      
         CLI   4(R2),ACRTNBT       Is this a batch record                       
         JNE   EXITY               No - not interested in record                
*        TM    TRKIND,TRKIINIT     Test any trans keys added to buffer          
*        JZ    EXITY               No - exit                                    
         L     R2,0(R2)                                                         
         USING TBARECD,R2                                                       
         OC    TBAKTSEQ,TBAKTSEQ   Is it batch header                           
         JZ    EXITY               Yes - not interested in this rec             
                                                                                
         AHI   R2,TBARFST-TBARECD                                               
         USING ASKELD,R2                                                        
UPDTRK02 CLI   ASKEL,0                                                          
         JE    EXITY                                                            
         CLI   ASKEL,ASKELQ                                                     
         JE    UPDTRK06                                                         
         CLI   ASKEL,GINELQ                                                     
         JE    UPDTRK08                                                         
UPDTRK04 LLC   RF,ASKLN                                                         
         AR    R2,RF                                                            
         J     UPDTRK02                                                         
                                                                                
UPDTRK06 XC    TR.TR_KEY(TR_LNQ),TR.TR_KEY                                      
         MVC   TR.TR_KEY(L'TR_KEY),ASKKEY                                       
         MVI   TK.TSACTN,TSARDH    Read for key                                 
         GOTOR TSAR,TK.TSARD                                                    
         JE    *+6                 Should always find key                       
         DC    H'0'                                                             
         MVC   ASKKEY,TR.TR_KEYRL  Replace key for real key                     
         J     UPDTRK04                                                         
         DROP  TR                                                               
                                                                                
         USING GINELD,R2                                                        
UPDTRK08 ICM   R1,15,GININV        Add existing group invoice number            
         ICM   R0,15,IN_GINSQ       to sequence on the batch record             
         AR    R1,R0                                                            
         STCM  R1,15,GININV        Save new group inv number on trans           
         J     EXITY                                                            
                                                                                
TR_D     DSECT                     ** transaction key buffer **                 
                                                                                
TR_KEY   DS    XL(L'ACCKEY)        Record key - supposed                        
TR_KLQ   EQU   *-TR_KEY            Length of TSAR key                           
TR_KEYRL DS    XL(L'ACCKEY)        Record key real                              
TR_LNQ   EQU   *-TR_D                                                           
SRUPD60  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Get passive pointers for a record and add to TSAR buffer            *         
***********************************************************************         
                                                                                
PP       USING PP_D,WORK                                                        
BLDPAS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDPAS*'                                                      
         LR    R2,R1               Point to parameter list                      
                                                                                
         TM    PASIND,PASIINIT     Test TSAR buffer initialised                 
         JNZ   BLDPAS04                                                         
         OI    PASIND,PASIINIT                                                  
         MVI   TP.TSACTN,TSAINI    No - initialise TSAR buffer                  
         MVI   TP.TSRECI,TSRXTN+TSRWSSVR                                        
         MVI   TP.TSKEYL,PP_KLQ                                                 
         LA    R0,PP.PP_D                                                       
         ST    R0,TP.TSAREC                                                     
         LHI   R0,PP_LNQ                                                        
         STCM  R0,3,TP.TSRECL                                                   
         LHI   R0,TWOK             Default to 2MB off-line                      
         OC    TP.TSBUFFL,TP.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TP.TSBUFFL     Set buffer length if not set                 
         MVC   TP.TSACOM,ACOMFACS                                               
         GOTOR TSAR,TP.TSARD       Call TSAR to initialise                      
         JNE   *+2                                                              
         MVI   TP.TSACTN,TSAADD    Set TSAR action to 'add'                     
         LTR   R8,R8               Test off-line                                
         JZ    BLDPAS04                                                         
         L     RF,LP_ABLK3         Return block to caller                       
         MVC   0(L'TSARPASS,RF),TSARPASS                                        
                                                                                
BLDPAS04 XC    LDGLVALN(ONERLVL),LDGLVALN                                       
         ICM   RF,15,4(R2)         Set ledger level lengths if given            
         JZ    *+10                                                             
         MVC   LDGLVALN(ONERLVL),0(RF)                                          
         MVC   CPYSTA1,TRNCPYS1                                                 
         MVC   CPYSTA2,TRNCPYS2                                                 
         MVC   CPYSTA3,TRNCPYS3                                                 
         MVC   CPYSTA4,TRNCPYS4                                                 
         MVC   CPYSTA5,TRNCPYS5                                                 
         MVC   CPYSTA6,TRNCPYS6                                                 
         MVC   CPYSTA7,TRNCPYS7                                                 
         MVC   CPYSTA9,TRNCPYS9                                                 
         MVC   CPTCPY,CPY          Set company code                             
         MVC   CPTREC,4(R2)        Set record type                              
         L     R0,0(R2)            R0=A(Record)                                 
         GOTOR LDCPTR,PARM,(R0),PASLST,CPTRBLK,0,DMDA,ACOMFACS                  
                                                                                
         LA    R3,PASLST           Point to first key                           
L        USING ACCRECD,R3                                                       
BLDPAS06 AHI   R3,ACCKLEN          Bump to first/next passive key               
         CLI   L.ACCKEY,FF         Test end of passive list                     
         JE    EXITY                                                            
         MVC   PP.PP_KEY,L.ACCKEY  Set key                                      
         LH    R0,PASSEQ           Bump sequence number                         
         AHI   R0,1                                                             
         STH   R0,PASSEQ                                                        
         MVC   PP.PP_SEQ,PASSEQ    Set sequence number                          
         MVC   PP.PP_STA,L.ACCKSTA Set status                                   
         MVC   PP.PP_DA,DMDA       Set disk address                             
         MVC   PP.PP_ACT,0(R2)     Set action (add or delete)                   
         GOTOR TSAR,TP.TSARD       Add passive to TSAR buffer                   
         JE    BLDPAS06                                                         
         DC    H'0'                Passive buffer full                          
         DROP  L                                                                
         EJECT                                                                  
***********************************************************************         
* Read TSAR passive pointer buffer and add/change ACCDIR passives     *         
***********************************************************************         
                                                                                
UPDPAS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDPAS*'                                                      
                                                                                
         TM    PASIND,PASIINIT     Test any passives added to buffer            
         JZ    EXITY               No - exit                                    
                                                                                
K        USING ACCKEY,DMKEY                                                     
         XC    PP.PP_KEY(PP_LNQ),PP.PP_KEY                                      
         MVI   TP.TSACTN,TSARDH    Read high first time                         
                                                                                
UPDPAS02 GOTOR TSAR,TP.TSARD       Get first/next passive                       
         TM    TP.TSERRS,TSEEOF    Test end of file                             
         JNZ   UPDPAS08            Yes - all done                               
                                                                                
         MVI   TP.TSACTN,TSANXT    (set action for next call)                   
         MVC   K.ACCKEY,PP.PP_KEY                                               
                                                                                
         CLI   PP.PP_ACT,PP_ADEL   Test deleting a passive                      
         JNE   UPDPAS04                                                         
         GOTOR GODMGR,DMCB,('RUD',DMREAD),ACCDIR,K.ACCKEY,K.ACCKEY              
         TM    8(R1),FF-(IOEDEL+IOERNF)   Was there an I/O error                
         JNZ   *+2                                                              
         TM    8(R1),IOERNF        Does record exist                            
         JNZ   UPDPAS02            No - ignore                                  
         TM    8(R1),IOEDEL        Is this record already deleted?              
         JNZ   UPDPAS02            Yes - ignore                                 
         OI    K.ACCKSTA,DELRECQ   Set deleted status                           
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,K.ACCKEY,K.ACCKEY                       
         JE    UPDPAS02                                                         
         DC    H'0'                                                             
                                                                                
UPDPAS04 CLI   PP.PP_ACT,PP_AADD   Test adding a passive                        
         JNE   *+2                                                              
         GOTOR GODMGR,DMCB,('RUD',DMREAD),ACCDIR,K.ACCKEY,K.ACCKEY              
         TM    8(R1),FF-(IOEDEL+IOERNF)                                         
         JNZ   *+2                                                              
         TM    8(R1),IOEDEL        Test found but deleted                       
         JNZ   UPDPAS06                                                         
         TM    8(R1),IOERNF        Test record not found                        
         JZ    UPDPAS06                                                         
         TM    PP.PP_STA,DELRECQ   Test adding as deleted                       
         JNZ   UPDPAS02                                                         
         MVC   K.ACCKEY,PP.PP_KEY                                               
         MVC   K.ACCKSTA,PP.PP_STA                                              
         MVC   K.ACCKDA,PP.PP_DA                                                
         GOTOR GODMGR,DMCB,DMADD,ACCDIR,K.ACCKEY,K.ACCKEY                       
         JE    UPDPAS02                                                         
         DC    H'0'                                                             
                                                                                
UPDPAS06 TM    8(R1),IOEDEL        Test found but deleted                       
         JZ    *+12                                                             
         TM    PP.PP_STA,DELRECQ   Test changing as deleted                     
         JNZ   UPDPAS02            Yes - ignore                                 
         CLC   K.ACCKSTA,PP.PP_STA Test status or disk address changed          
         JNE   *+14                                                             
         CLC   K.ACCKDA,PP.PP_DA                                                
         JE    UPDPAS02            No - don't write this one                    
         MVC   K.ACCKEY,PP.PP_KEY                                               
         MVC   K.ACCKSTA,PP.PP_STA                                              
         MVC   K.ACCKDA,PP.PP_DA                                                
         GOTOR GODMGR,DMCB,DMWRT,ACCDIR,K.ACCKEY,K.ACCKEY                       
         JE    UPDPAS02                                                         
         DC    H'0'                                                             
                                                                                
UPDPAS08 MVI   PASIND,0            Reset indicators                             
         J     EXITY                                                            
         DROP  PP,K                                                             
                                                                                
PP_D     DSECT                     ** Passive pointer buffer **                 
                                                                                
PP_KEY   DS    XL(L'ACCKEY)        Record key                                   
PP_SEQ   DS    XL(L'PASSEQ)        Sequence number                              
PP_KLQ   EQU   *-PP_KEY            Length of TSAR key                           
                                                                                
PP_STA   DS    XL(L'ACCKSTA)       Record status                                
PP_DA    DS    XL(L'ACCRLNK)       Record disk address                          
                                                                                
PP_ACT   DS    X                   ** Passive pointer action **                 
PP_ADEL  EQU   1                   Delete passive                               
PP_AADD  EQU   2                   Add passive                                  
PP_LNQ   EQU   *-PP_D                                                           
SRUPD60  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Get passive pointers for a media record and add to TSAR buffer      *         
***********************************************************************         
*&&UK                                                                           
PM       USING PM_D,WORK                                                        
BLDMPA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDMPA*'                                                      
         LR    R2,R1               Point to parameter list                      
                                                                                
         TM    MPAIND,MPAIINIT     Test TSAR buffer initialised                 
         JNZ   BLDMPA04                                                         
         OI    MPAIND,MPAIINIT                                                  
         MVI   TP.TSACTN,TSAINI    No - initialise TSAR buffer                  
         MVI   TP.TSRECI,TSRXTN                                                 
         MVI   TP.TSKEYL,PM_KLQ                                                 
         LA    R0,PM.PM_D                                                       
         ST    R0,TP.TSAREC                                                     
         LHI   R0,PM_LNQ                                                        
         STCM  R0,3,TP.TSRECL                                                   
         LHI   R0,TWOK             Default to 2MB off-line                      
         OC    TP.TSBUFFL,TP.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TP.TSBUFFL     Set buffer length if not set                 
         MVC   TP.TSACOM,ACOMFACS                                               
         GOTOR TSAR,TP.TSARD       Call TSAR to initialise                      
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TP.TSACTN,TSAADD    Set TSAR action to 'add'                     
         LTR   R8,R8               Test off-line                                
         JZ    BLDMPA04                                                         
         L     RF,LP_ABLK3         Return block to caller                       
         MVC   0(L'TSARPASS,RF),TSARPASS                                        
                                                                                
BLDMPA04 L     R0,0(R2)            R0=A(Record)                                 
         GOTOR LDCMPT,PARM,(R0),PASLST,('FF',0),FWCMPT,0,              +        
               (C'S',ACOMFACS)                                                  
         LA    R3,PASLST           Point to first key                           
L        USING M_GEND,R3                                                        
BLDMPA06 AHI   R3,M_DIRLQ          Bump to first/next passive key               
         CLI   L.M_KEY,FF          Test end of passive list                     
         JE    EXITY                                                            
         CLI   CALOVRLY,FW_MXPAY   Skip most buy passives for payments          
         JNE   BLDMPA08            (see BR52475L)                               
*        CLI   L.M_RECTYP,PPBYTYPQ                                              
*        JE    BLDMPA06                                                         
*        CLI   L.M_RECTYP,PPMSTYPQ                                              
*        JE    BLDMPA06                                                         
*        CLI   L.M_RECTYP,PPSPTYPQ                                              
*        JE    BLDMPA06                                                         
         CLI   L.M_RECTYP,PPOUTYPQ                                              
         JE    BLDMPA06                                                         
         CLI   L.M_RECTYP,PPCCTYPQ                                              
         JE    BLDMPA06                                                         
         CLI   L.M_RECTYP,PPTRTYPQ                                              
         JE    BLDMPA06                                                         
         CLI   L.M_RECTYP,PPPATYPQ                                              
         JE    BLDMPA06                                                         
         CLI   L.M_RECTYP,PPPBTYPQ                                              
         JE    BLDMPA06                                                         
         CLI   L.M_RECTYP,PPLTTYPQ                                              
         JE    BLDMPA06                                                         
*        CLI   L.M_RECTYP,SUDATYPQ                                              
*        JE    BLDMPA06                                                         
         CLI   L.M_RECTYP,PPBNTYPQ                                              
         JE    BLDMPA06                                                         
BLDMPA08 MVC   PM.PM_KEY,L.M_KEY   Set key                                      
         LH    R0,MPASEQ           Bump sequence number                         
         AHI   R0,1                                                             
         STH   R0,MPASEQ                                                        
         MVC   PM.PM_SEQ,MPASEQ    Set sequence number                          
         MVC   PM.PM_STA,L.M_DST   Set status                                   
         MVC   PM.PM_DA,DMDA       Set disk address                             
         MVC   PM.PM_ACT,0(R2)     Set action (add or delete)                   
         GOTOR TSAR,TP.TSARD       Add passive to TSAR buffer                   
         JE    BLDMPA06                                                         
         DC    H'0'                Passive buffer full                          
         DROP  L                                                                
         EJECT                                                                  
***********************************************************************         
* Read TSAR passive pointer buffer and add/change MEDDIR passives     *         
***********************************************************************         
                                                                                
UPDMPA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDMPA*'                                                      
                                                                                
         TM    MPAIND,MPAIINIT     Test any passives added to buffer            
         JZ    EXITY               No - exit                                    
                                                                                
K        USING M_GEND,DMKEY                                                     
         XC    PM.PM_KEY(PM_LNQ),PM.PM_KEY                                      
         MVI   TP.TSACTN,TSARDH    Read high first time                         
                                                                                
UPDMPA02 GOTOR TSAR,TP.TSARD       Get first/next passive                       
         TM    TP.TSERRS,TSEEOF    Test end of file                             
         JNZ   UPDMPA08            Yes - all done                               
                                                                                
         MVI   TP.TSACTN,TSANXT    (set action for next call)                   
         MVC   K.M_KEY,PM.PM_KEY                                                
                                                                                
         CLI   PM.PM_ACT,PM_ADEL   Test deleting a passive                      
         JNE   UPDMPA04                                                         
         GOTOR GODMGR,DMCB,('RUD',DMREAD),MEDDIR,K.M_KEY,K.M_KEY                
         TM    8(R1),FF-(IOEDEL+IOERNF)   Was there an I/O error                
         JZ    *+6                                                              
         DC    H'0'                                                             
         TM    8(R1),IOERNF        Does record exist                            
         JNZ   UPDMPA02            No - ignore                                  
         TM    8(R1),IOEDEL        Is this record already deleted?              
         JNZ   UPDMPA02            Yes - ignore                                 
         OI    K.M_DSTAT,M_DELQ    Set deleted status                           
         GOTOR GODMGR,DMCB,DMWRT,MEDDIR,K.M_KEY,K.M_KEY                         
         JE    UPDMPA02                                                         
         DC    H'0'                                                             
                                                                                
UPDMPA04 CLI   PM.PM_ACT,PM_AADD   Test adding a passive                        
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GODMGR,DMCB,('RUD',DMREAD),MEDDIR,K.M_KEY,K.M_KEY                
         TM    8(R1),FF-(IOEDEL+IOERNF)                                         
         JZ    *+6                                                              
         DC    H'0'                                                             
         TM    8(R1),IOEDEL        Test found but deleted                       
         JNZ   UPDMPA06                                                         
         TM    8(R1),IOERNF        Test record not found                        
         JZ    UPDMPA06                                                         
         TM    PM.PM_STA,M_DELQ    Test adding as deleted                       
         JNZ   UPDMPA02                                                         
         MVC   K.M_KEY,PM.PM_KEY                                                
         MVC   K.M_DST,PM.PM_STA                                                
         MVC   K.M_DDA,PM.PM_DA                                                 
         GOTOR GODMGR,DMCB,DMADD,MEDDIR,K.M_KEY,K.M_KEY                         
         JE    UPDMPA02                                                         
         DC    H'0'                                                             
                                                                                
UPDMPA06 TM    8(R1),IOEDEL        Test found but deleted                       
         JZ    *+12                                                             
         TM    PM.PM_STA,M_DELQ    Test changing as deleted                     
         JNZ   UPDMPA02            Yes - ignore                                 
         CLC   K.M_DST,PM.PM_STA   Test status or disk address changed          
         JNE   *+14                                                             
         CLC   K.M_DDA,PM.PM_DA                                                 
         JE    UPDMPA02            No - don't write this one                    
         MVC   K.M_KEY,PM.PM_KEY                                                
         MVC   K.M_DST,PM.PM_STA                                                
         MVC   K.M_DDA,PM.PM_DA                                                 
         GOTOR GODMGR,DMCB,DMWRT,MEDDIR,K.M_KEY,K.M_KEY                         
         JE    UPDMPA02                                                         
         DC    H'0'                                                             
                                                                                
UPDMPA08 MVI   MPAIND,0            Reset indicators                             
         J     EXITY                                                            
         DROP  PM,K                                                             
                                                                                
PM_D     DSECT                     ** Passive pointer buffer **                 
                                                                                
PM_KEY   DS    XL(L'M_KEY)         Record key                                   
PM_SEQ   DS    XL(L'MPASEQ)        Sequence number                              
PM_KLQ   EQU   *-PM_KEY            Length of TSAR key                           
                                                                                
PM_STA   DS    XL(L'M_DST)         Record status                                
PM_DA    DS    XL(L'M_DDA)         Record disk address                          
                                                                                
PM_ACT   DS    X                   ** Passive pointer action **                 
PM_ADEL  EQU   1                   Delete passive                               
PM_AADD  EQU   2                   Add passive                                  
PM_LNQ   EQU   *-PM_D                                                           
*&&                                                                             
                                                                                
SRUPD60  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Load a core resident phase and set its address                      *         
*                                                                     *         
* Ntry:- R1=AL1(Phase number),AL3(Phase address)                      *         
***********************************************************************         
                                                                                
GETPHS   NTR1  LABEL=NO                                                         
         ICM   R0,B'1110',T00A                                                  
         ICM   R0,B'0001',0(R1)                                                 
         ICM   R2,7,1(R1)          R2=A(Phase address)                          
         GOTOR CALLOV,PARM,0,(R0),0                                             
         CLI   4(R1),FF            Test phase found                             
         JNE   *+6                                                              
         DC    H'0'                                                             
         GOTOR PROTOFF             Turn off storage protection                  
         MVC   0(4,R2),0(R1)       Set phase address                            
         GOTOR PROTON              Turn on storage protection                   
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Set ACOMFACS and extract addresses of called routines               *         
***********************************************************************         
                                                                                
         USING COMFACSD,R1                                                      
SETFAC   ST    R1,ACOMFACS         Set A(COMFACS)                               
         MVC   SWITCH,CSWITCH      Extract COMFACS addresses                    
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   CALLOV,CCALLOV                                                   
         MVC   PROTOFF,CPROTOFF                                                 
         MVC   PROTON,CPROTON                                                   
         MVC   LOCKET,CLOCKET                                                   
         MVC   ADDAY,CADDAY                                                     
         MVC   GETDAY,CGETDAY                                                   
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Data manager interface and off-line trace facility                  *         
***********************************************************************         
                                                                                
GODMGR   NTR1  LABEL=NO                                                         
         LR    R2,R1               Point to parameter list                      
         MVI   8(R2),0                                                          
                                                                                
         LTR   R8,R8               Test off-line (LP_D set)                     
         JZ    GODMGR20            No                                           
         LM    R3,R4,0(R2)         R3=A(action),R4=A(file)                      
                                                                                
         MVC   THISFILE,0(R4)      Set file                                     
                                                                                
         MVI   TFILE,TFILEDIR      Set file type                                
         CLC   ACCDIR,0(R4)        Test ACCDIR I/O                              
         JE    GODMGR00                                                         
         CLC   MEDDIR,0(R4)        Test MEDDIR I/O                              
         JE    GODMGR00                                                         
         MVI   TFILE,TFILEFIL      No - assume ACCMST/ACCARC/MEDFIL             
                                                                                
GODMGR00 TM    LP_OFTRC,X'40'      Test want DMGR trace                         
         JZ    GODMGR06                                                         
         CLI   TFILE,TFILEFIL      Test ACCMST/ACCARC/MEDFIL I/O                
         JNE   GODMGR04                                                         
         CLC   PUTREC,0(R3)        Test PUTREC                                  
         JE    GODMGR01                                                         
         CLC   ADFREC,0(R3)        Test ADFREC                                  
         JE    GODMGR01                                                         
         CLC   ADDREC,0(R3)        or ADDREC                                    
         JNE   GODMGR06                                                         
                                                                                
GODMGR01 GOTOR PRINTER             Print a space line                           
         MVC   PDMGRLIT,TDMGR      Build record description                     
         MVC   PDMGRACT,0(R3)      Set action                                   
         MVC   PDMGRFIL,0(R4)      Set file                                     
         CLC   ADDREC,0(R3)        Test PUTREC                                  
         JE    GODMGR02                                                         
         MVC   PDMGRDAL,TDA        Set disk address                             
         L     RF,8(R2)                                                         
         MVC   WORK(L'ACCKDA),0(RF)                                             
         GOTOR HEXOUT,TDMCB,WORK,PDMGRDAD,L'ACCKDA,0                            
GODMGR02 GOTOR GOPRNT              Print description line                       
         L     R1,12(R2)           Point to record                              
         GOTOR PRTREC              Print file record                            
         J     GODMGR06                                                         
                                                                                
GODMGR04 L     RF,8(R2)            RF=A(directory key)                          
         MVC   TDIR,0(RF)                                                       
                                                                                
GODMGR06 CLI   TFILE,TFILEDIR      Test directory I/O                           
         JE    GODMGR14                                                         
         TM    LP_OFLG1,LP_OFDFT   Test 'draft' run                             
         JZ    GODMGR20                                                         
         CLC   PUTREC,0(R3)        No updative I/Os allowed if true             
         JE    GODMGRX                                                          
         CLC   ADDREC,0(R3)        ADDREC requires special handling             
         JNE   GODMGR20                                                         
         L     R0,DMADD#           Bump ADDREC sequence number                  
         AHI   R0,1                                                             
         ST    R0,DMADD#                                                        
         L     RF,8(R2)                                                         
         ST    R0,0(RF)            and return as disk address                   
         XC    0(L'DMDA,RF),EFFS                                                
         J     GODMGRX                                                          
                                                                                
GODMGR14 CLC   DMREAD(3),0(R3)     Test updative I/O                            
         JE    GODMGR20            No                                           
         TM    LP_OFLG1,LP_OFDFT   Test 'draft' run                             
         JZ    GODMGR20                                                         
         LTR   R8,R8               Test off-line (LP_D set)                     
         JZ    GODMGRX                                                          
         TM    LP_OFTRC,X'40'      Test want I/O trace                          
         JZ    GODMGRX                                                          
         GOTOR PRINTER             Print a space line                           
         MVC   PDMGRLIT,TDMGR      Build record description                     
         MVC   PDMGRACT,0(R3)      Set action                                   
         MVC   PDMGRFIL,0(R4)      Set file                                     
         GOTOR GOPRNT              Print description                            
         GOTOR PRTREC,TDIR         Print directory record                       
         J     GODMGRX                                                          
                                                                                
GODMGR20 GOTOR DATAMGR,(R2)        Call DATAMGR to do the I/O                   
                                                                                
         LTR   R8,R8               Test off-line (LP_D set)                     
         JZ    GODMGRX                                                          
         TM    LP_OFTRC,X'40'      Test want I/O trace                          
         JZ    GODMGRX                                                          
         CLI   TFILE,TFILEDIR      Test directory I/O                           
         JE    GODMGR22                                                         
         TM    LP_OFTRC,X'01'      Test want read-only I/O trace                
         JNO   GODMGRX                                                          
         CLC   GETREC,0(R3)        Test just issued GETREC                      
         JNE   GODMGRX                                                          
         GOTOR PRINTER             Print a space line                           
         MVC   PDMGRLIT,TDMGR      Build record description                     
         MVC   PDMGRACT,0(R3)      Set action                                   
         MVC   PDMGRFIL,0(R4)      Set file                                     
         MVC   PDMGRDAL,TDA        Set disk address                             
         L     RF,8(R2)                                                         
         MVC   WORK(L'ACCKDA),0(RF)                                             
         GOTOR HEXOUT,TDMCB,WORK,PDMGRDAD,L'ACCKDA,0                            
         GOTOR GOPRNT              Print description line                       
         L     R1,12(R2)           Point to record                              
         GOTOR PRTREC              Print the record                             
         J     GODMGRX                                                          
                                                                                
GODMGR22 CLC   DMREAD(3),0(R3)     Test read-only directory I/O                 
         JE    GODMGR24            Yes                                          
         GOTOR PRINTER             Print a space line                           
         MVC   PDMGRLIT,TDMGR      Build record description                     
         MVC   PDMGRACT,0(R3)      Set action                                   
         MVC   PDMGRFIL,0(R4)      Set file                                     
         GOTOR GOPRNT              Print description                            
         GOTOR PRTREC,TDIR         Print directory record                       
         J     GODMGRX             Exit without issuing I/O                     
                                                                                
GODMGR24 TM    LP_OFTRC,X'01'      Test want read-only I/O trace                
         JZ    GODMGRX             No                                           
         CLC   DMRSEQ,0(R3)        Test read sequential                         
         JE    GODMGR26            Yes - only print after key                   
         GOTOR PRINTER             Print a space line                           
         MVC   PDMGRLIT,TDMGR      Build record description                     
         MVC   PDMGRACT,0(R3)      Set action                                   
         MVC   PDMGRFIL,0(R4)      Set file                                     
         MVC   PDMGRBOA,TBEFORE    Set this is the before record                
         GOTOR GOPRNT              Print description                            
         MVI   TFILE,TFILEBEF      Set this is the before key                   
         GOTOR PRTREC,TDIR         Print before directory record                
         MVI   TFILE,TFILEDIR      Reset to full key printing                   
                                                                                
GODMGR26 GOTOR PRINTER             Print a space line                           
         MVC   PDMGRLIT,TDMGR      Build record description                     
         MVC   PDMGRACT,0(R3)      Set action                                   
         MVC   PDMGRFIL,0(R4)      Set file                                     
         MVC   PDMGRBOA,TAFTER     Set this is the after record                 
         MVC   PDMGRRBL,TRET       Set 'Ret=xx'                                 
         GOTOR HEXOUT,TDMCB,8(R2),PDMGRRB,1,0                                   
         GOTOR GOPRNT              Print description                            
         L     R1,12(R2)           Point to actual record read                  
         GOTOR PRTREC              Print after directory record                 
                                                                                
GODMGRX  CLI   8(R2),0             Set condition code for caller                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADDTRN interface and off-line trace facility                        *         
***********************************************************************         
                                                                                
GOATRN   NTR1  LABEL=NO                                                         
         LR    R2,R1               Point to parameter list                      
         MVI   TRNERRS,0                                                        
         LTR   R8,R8               Test off-line                                
         JZ    GOATRN20            No                                           
                                                                                
         TM    LP_OFTRC,X'80'      Test want ADDTRN trace                       
         JZ    GOATRN02            No                                           
         GOTOR PRINTER             Print a space line                           
         MVC   P(L'TATRN),TATRN                                                 
         GOTOR GOPRNT              Print description                            
         MVI   TFILE,TFILEFIL      Print full record                            
         L     R1,TRNREC                                                        
         GOTOR PRTREC              Print transaction record                     
                                                                                
GOATRN02 TM    LP_OFLG1,LP_OFDFT   Test 'draft' run                             
         JNZ   GOATRNX             Yes - don't call ADDTRN                      
                                                                                
GOATRN20 GOTOR ADDTRN,(R2)         Call ADDTRN                                  
                                                                                
GOATRNX  OI    ATRNIND,ATRNIPUT    Set record has been put                      
         CLI   TRNERRS,0           Set condition code for caller                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Build 'I/O Count=nnnnnn' on trace line and call PRINTER             *         
***********************************************************************         
                                                                                
GOPRNT   NTR1  LABEL=NO                                                         
         MVC   PDMGRIOL,TIOC                                                    
         L     RF,RUNFACS                                                       
         L     R0,RTOTSIO-RUNFACSD(RF)                                          
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PDMGRIO,DUB                                                      
         GOTOR PRINTER                                                          
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Routine to print out a record                                       *         
* Ntry - R1=A(Record)                                                 *         
*        R3=A(Action)                                                 *         
*        R4=A(File)                                                   *         
***********************************************************************         
                                                                                
PRTREC   NTR1  LABEL=NO                                                         
         LR    R7,R1                                                            
         USING ACCRECD,R7                                                       
         MVC   TRECLEN,ACCRLEN                                                  
         CLC   THISFIL4,MEDFIL                                                  
         JNE   *+10                                                             
         MVC   TRECLEN,ACCRECD+(M_RECLEN-M_GEND)                                
         GOTOR DISDIR              Display key details                          
         CLI   TFILE,TFILEFIL      Test file record                             
         JNE   EXIT                                                             
         GOTOR DISELS              Display record elements                      
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Routine to display directory details                                *         
***********************************************************************         
                                                                                
DISDIR   NTR1  LABEL=NO                                                         
         XC    TPLIST,TPLIST                                                    
         LA    R1,TPLIST                                                        
         ST    R7,0(R1)                                                         
         LHI   R0,ACCRFST-ACCRECD  R0=File record length                        
         CLI   TFILE,TFILEFIL      Test ACCMST/ACCARC/MEDFIL                    
         JE    *+8                                                              
         LHI   R0,ACCKLEN          R0=Directory record length                   
         CLI   TFILE,TFILEBEF      Test 'before' I/O key                        
         JNE   *+8                                                              
         LHI   R0,L'ACCKEY         Yes - no D/A or status                       
         CLC   THISFIL3,MEDDIR     If this is MEDDIR/MEDFIL                     
         JNE   *+8                                                              
         SHI   R0,(L'ACCKEY-L'M_KEY)  Key is shorter                            
         ST    R0,4(R1)                                                         
         GOTOR PUTSTR              Output directory info                        
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Display record in element format                                    *         
***********************************************************************         
                                                                                
DISELS   NTR1  LABEL=NO                                                         
         LA    R2,ACCRECD          Check elements are intact                    
         SR    R3,R3                                                            
         ICM   R3,3,ACCRLEN                                                     
         CLC   THISFIL4,MEDFIL                                                  
         JNE   *+8                                                              
         ICM   R3,3,ACCRECD+(M_RECLEN-M_GEND)                                   
         LA    R3,0(R2,R3)                                                      
                                                                                
DISELS02 CR    R2,R3               Past end of record?                          
         JH    DISELS08            No                                           
         CLI   0(R2),0             Reached end of record?                       
         JE    DISELS04            Yes                                          
         ICM   RF,1,1(R2)          Next element                                 
         JZ    DISELS08                                                         
         BRXH  R2,RF,DISELS02                                                   
                                                                                
DISELS04 CR    R2,R3               Record can have a 0 on its end               
         JE    DISELS06            that can be included in length               
         CLI   0(R2),0             of record - have to check for                
         JNE   DISELS08            this condition                               
         AHI   R2,1                                                             
         CR    R2,R3                                                            
         JNE   DISELS08                                                         
                                                                                
DISELS06 GOTOR DMPELS              Elements are ok - dump them                  
         J     EXIT                                                             
                                                                                
DISELS08 LA    R1,TPLIST           Elements are bad - do all you can            
         LA    R2,ACCRFST                                                       
         LHI   R3,ACCRFST-ACCRECD                                               
         CLC   THISFIL4,MEDFIL                                                  
         JNE   DISELS10                                                         
         LA    R2,ACCRECD+(M_DUMEL-M_GEND)                                      
         LHI   R3,M_DUMEL-M_GEND                                                
                                                                                
DISELS10 CLI   0(R2),0             Possible EOR                                 
         JE    DISELS12                                                         
         CH    R3,TRECLEN                                                       
         JH    DISELS12                                                         
         SR    RF,RF                                                            
         ICM   RF,1,1(R2)                                                       
         JZ    DISELS14            Zero length element                          
         ST    R2,0(R1)                                                         
         ST    RF,4(R1)                                                         
         ST    R3,8(R1)                                                         
         GOTOR PUTSTR                                                           
         AR    R2,RF                                                            
         AR    R3,RF                                                            
         J     DISELS10                                                         
                                                                                
DISELS12 CH    R3,TRECLEN          Account for possible zero on end             
         JE    EXIT                                                             
         CLI   0(R2),0             Zero on end?                                 
         JNE   DISELS14            No                                           
         LA    RF,1(R3)                                                         
         CH    RF,TRECLEN          Really the end?                              
         JE    EXIT                Yes                                          
                                                                                
DISELS14 MVC   P(L'ERRORT),ERRORT                                               
         MVC   P+L'ERRORT(L'BADELEM),BADELEM                                    
         GOTOR PRINTER                                                          
         SH    RF,TRECLEN                                                       
         JNP   EXIT                                                             
         LA    R1,TPLIST                                                        
         ST    R2,0(R1)                                                         
         ST    RF,4(R1)                                                         
         ST    R3,8(R1)                                                         
         GOTOR PUTSTR                                                           
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Routine to hex dump record data in element format                   *         
***********************************************************************         
                                                                                
DMPELS   NTR1  LABEL=NO                                                         
         SR    RF,RF                                                            
         LA    R1,TPLIST                                                        
         LA    R2,ACCRECD                                                       
         AHI   R2,ACCRFST-ACCRECD                                               
         LHI   R3,ACCRFST-ACCRECD                                               
         CLC   THISFIL4,MEDFIL                                                  
         JNE   DMPELS02                                                         
         LA    R2,ACCRECD+(M_DUMEL-M_GEND)                                      
         LHI   R3,M_DUMEL-M_GEND                                                
                                                                                
DMPELS02 CLI   0(R2),0                                                          
         JE    DMPELS04                                                         
         CH    R3,TRECLEN                                                       
         JH    DMPELS04                                                         
                                                                                
         IC    RF,1(R2)                                                         
         ST    R2,0(R1)                                                         
         ST    RF,4(R1)                                                         
         ST    R3,8(R1)                                                         
         GOTOR PUTSTR                                                           
         AR    R2,RF                                                            
         AR    R3,RF                                                            
         J     DMPELS02                                                         
                                                                                
DMPELS04 CH    RF,TRECLEN          Account for possible zero on end             
         JNL   EXIT                                                             
         ST    R2,0(R1)                                                         
         LHI   RF,1                                                             
         ST    RF,4(R1)                                                         
         ST    R3,8(R1)                                                         
         GOTOR PUTSTR                                                           
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Routine to put out an area of storage for a length                  *         
*                                                                     *         
* Ntry:- P1 = A(storage)                                              *         
*        P2 = L'storage                                               *         
*        P3 = Start displacement                                      *         
***********************************************************************         
                                                                                
PUTSTR   NTR1  LABEL=NO                                                         
         LM    R2,R4,0(R1)         Set parameters                               
         MVI   TFLG,0                                                           
                                                                                
PUTSTR02 LTR   R6,R3               Anything left to output?                     
         JNP   EXIT                No                                           
         CHI   R6,32                                                            
         JL    *+8                                                              
         LHI   R6,32                                                            
         CVD   R4,DUB              Output start displacement                    
         OI    DUB+7,X'0F'                                                      
         UNPK  P(4),DUB                                                         
         CLI   TFLG,0                                                           
         JNE   *+8                                                              
         MVI   P+5,C'='                                                         
         MVI   TFLG,255                                                         
         GOTOR HEXOUT,TDMCB,(R2),P+6,(R6),0                                     
         LHI   R1,32               Move out hex data                            
         SLL   R1,1                                                             
         LA    R1,P+8(R1)                                                       
         LR    RF,R6                                                            
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),0(R2)                                                    
         EX    RF,0(RE)                                                         
         TR    P,TRTAB             Translate line                               
         GOTOR PRINTER             and print it                                 
         AR    R2,R6               Next data                                    
         SR    R3,R6                                                            
         J     PUTSTR02                                                         
         EJECT                                                                  
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
ONEK     EQU   1024                                                             
TWOK     EQU   ONEK*2                                                           
FF       EQU   X'FF'                                                            
FE       EQU   X'FE'                                                            
                                                                                
TIMMAXLN EQU   1950                Maximum length of time record                
                                                                                
IOLENQ   EQU   2*ONEK              Maximum length of ACCMST record              
IOEEOF   EQU   X'80'               DATAMGR end of file                          
IOEDUP   EQU   X'20'               DUPLICATE KEY ON ADD                         
IOERNF   EQU   X'10'               DATAMGR record not found                     
IOEDEL   EQU   X'02'               DATAMGR record is deleted                    
                                                                                
DELRECQ  EQU   TIMSDELT            Deleted record status                        
                                                                                
RUP      EQU   X'80'               Read-for-update                              
RDR      EQU   X'08'               Read deleted records                         
RUD      EQU   RUP+RDR             Read-for-update/Read deleted records         
                                                                                
BUFIELD  EQU   X'80'               Element deleted from buffer record           
BUFIELA  EQU   X'40'               Element added to buffer record               
BUFISTA  EQU   X'20'               Change of time status                        
BUFIPAD  EQU   X'10'               Buffer record passives deleted               
BUFINEW  EQU   X'08'               New record to be added                       
         EJECT                                                                  
GLOBALS  DS    0D                  ** Global literals **                        
                                                                                
         LTORG                                                                  
                                                                                
VLDCPTR  DC    V(LDCPTR)                                                        
VLDCMPT  DC    V(LDCMPT)                                                        
ADDTRNF  DS    0F                                                               
ADDTRN   DC    A(0)                A(ADDTRN)                                    
TSARF    DS    0F                                                               
TSAR     DC    A(0)                A(TSAR)                                      
ASBUFF   DC    A(0)                A(sort buffer)                               
                                                                                
T00A     DC    C'R',X'000A'        For loading T00A phases                      
                                                                                
READ     DC    C'REA'              FACWRK read command                          
RANDOM   DC    C'RAN'              FACWRK random read command                   
PURGE    DC    C'PUR'              FACWRK purge command                         
                                                                                
SORTEND  DC    C'END'                                                           
SORTGET  DC    C'GET'                                                           
SORTPUT  DC    C'PUT'                                                           
                                                                                
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ACCARC   DC    C'ACCARC  '                                                      
CTFILE   DC    C'CTFILE  '                                                      
MEDDIR   DC    C'MEDDIR  '                                                      
MEDFIL   DC    C'MEDFILE '                                                      
                                                                                
DMREAD   DC    C'DMREAD  '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
DMADD    DC    C'DMADD   '                                                      
                                                                                
GETREC   DC    C'GETREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
ADDREC   DC    C'ADDREC  '                                                      
ADFREC   DC    C'ADFREC  '                                                      
                                                                                
TIMEREF  DC    C'*TIME*'           Reference for time records                   
                                                                                
BLANKS   DC    CL64' '                                                          
FZERO    DC    F'0'                                                             
AEFFS    DS    0A                                                               
EFFS     DC    X'FFFFFFFF'                                                      
                                                                                
TTITLE   DC    C'BrandOcean eTime/MX Upload Trace'                              
TNEWT    DC    C'****  Start of new trace ****'                                 
TDRAFT   DC    C'****  Running in draft mode ****'                              
TATRN    DC    C'****  Transaction record'                                      
TDMGR    DC    C'****  Datamgr I/O - '                                          
TDA      DC    C'D/A='                                                          
TBEFORE  DC    CL(L'PDMGRBOA)'Input key'                                        
TAFTER   DC    CL(L'PDMGRBOA)'Output key'                                       
TRET     DC    C'Ret='                                                          
TIOC     DC    C'I/O Count='                                                    
                                                                                
SORTSRF  DC    AL1(0,BR_KEY-BR_D,L'TIMKEY)                                      
SORTORI  DC    AL1(0,0,BR_KL)                                                   
SORTLOC  DC    AL1(0,TT_LOC-TT_KEY,TT_LOCL)                                     
SORTOFA  DC    AL1(0,TT_CVAL-TT_KEY,TT_CVALL)                                   
                                                                                
ERRORT   DC    CL8'*ERROR* '                                                    
BADELEM  DC    CL60'Element structure invalid - HEX dump follows'               
                                                                                
DAYTAB   DS    0X                                                               
         DC    AL1(1),AL1(2)                                                    
         DC    AL1(2),AL1(3)                                                    
         DC    AL1(3),AL1(4)                                                    
         DC    AL1(4),AL1(5)                                                    
         DC    AL1(5),AL1(6)                                                    
         DC    AL1(6),AL1(7)                                                    
         DC    AL1(7),AL1(1)                                                    
         DC    X'FF'                                                            
                                                                                
CALDAYS  DC    XL128'00'           Days/Hours per period                        
         DC    X'FF'                                                            
                                                                                
EDTTAB   DC    AL2(EDTKPER-EDTKEY),AL1(L'EDTKPER)                               
         DC    AL2(EDTKSBD-EDTKEY),AL1(L'EDTKSBD)                               
         DC    AL2(EDTKDPT-EDTKEY),AL1(L'EDTKDPT)                               
         DC    AL2(EDTKOFC-EDTKEY),AL1(L'EDTKOFC)                               
         DC    X'FF'                                                            
                                                                                
BRATAB   DS    0X                                                               
         DC    (MAXBRAQ)XL3'00'                                                 
BRATABX  DC    X'FF'                                                            
MAXBRAQ  EQU   20                                                               
                                                                                
PERTAB   DS    0X                  1R Account Table                             
         DC    (MAXPERQ)XL12'00'                                                
PERTABX  DC    X'FF'                                                            
MAXPERQ  EQU   20                                                               
                                                                                
SJATAB   DS    0X                  SJ Account Table                             
         DC    (MAXSJAQ)XL12'00'                                                
SJATABX  DC    X'FF'                                                            
MAXSJAQ  EQU   20                                                               
                                                                                
SVCLIOFF DC    CL2' '                                                           
                                                                                
MQSTR    DC    CL16'UNLOCKEDUSERS***'                                           
MQUID    DC    CL10' '                                                          
         DC    CL1' '                                                           
MQPID    DC    CL8' '                                                           
         DC    CL2' '                                                           
MQFACPAK DC    CL1' '                                                           
         DC    CL2' '                                                           
MQDATE   DC    CL8' '              Today's date                                 
         DC    CL3' / '                                                         
MQTIME   DC    0CL8                                                             
MQHR     DC    CL2' '                                                           
         DC    C':'                                                             
MQMIN    DC    CL2' '                                                           
         DC    C':'                                                             
MQSEC    DC    CL2' '                                                           
MQSTRL   EQU   *-MQSTR                                                          
                                                                                
JESMAIL  DC    CL8'JESMAIL '                                                    
COLON    EQU   C':'                                                             
                                                                                
TOWHO    DC    CL45' '                                                          
                                                                                
BODYLIN1 DC    0CL80                                                            
SUBJDSC  DC    0CL72                                                            
         DC    C'BrandOcean - Unlock :'                                         
SUBJUID  DC    CL8' '                                                           
         DC    CL1' '                                                           
SUBJPID  DC    CL8' '                                                           
         DC    CL2' '                                                           
SUBJDATE DC    CL8' '              Today's date                                 
         DC    CL3' / '                                                         
SUBJTIME DC    0CL8                                                             
SUBJHR   DC    CL2' '                                                           
         DC    C':'                                                             
SUBJMIN  DC    CL2' '                                                           
         DC    C':'                                                             
SUBJSEC  DC    CL2' '                                                           
         DC    CL(L'BODYLIN1-(*-BODYLIN1))' '     SPARE SPACES                  
                                                                                
BODYLIN2 DC    0CL80                                                            
         DC    C'User Pid has completed necessary Brandocean'                   
         DC    C' work to be unlocked - '                                       
BODYDATE DC    CL8' '              Today's date                                 
         DC    CL(L'BODYLIN2-(*-BODYLIN2))' '     SPARE SPACES                  
                                                                                
MAILTAB  DS    0CL(MAILNQ)                                                      
                                                                                
         DC    CL2'*B'             Alpha id DDSB                                
         DC    CL8'DDSB'           Userid                                       
         DC    CL45'RGUP,JSHA,MBLAU'  Comma separated E-Mail Addresses          
                                                                                
         DC    CL2'MA'             Alpha id MCST                                
         DC    CL8'MCST'           Userid                                       
         DC    CL45'RGUP,JSHA,MBLAU'  Comma separated E-Mail Addresses          
                                                                                
         DC    CL2'H3'             Alpha id GHGCOR                              
         DC    CL8'GHGCOR'         Userid                                       
         DC    CL45'UNLOCK@GREY.COM'  Comma separated E-Mail Addresses          
                                                                                
         DC    CL2'GP'             Alpha id GGCOR                               
         DC    CL8'GGCOR'          Userid                                       
         DC    CL45'RGUP,JSHA'        Comma separated E-Mail Addresses          
                                                                                
         DC    CL2'UM'             Alpha id GGCOR                               
         DC    CL8'GGCOR'          Userid                                       
         DC    CL45'RGUP,JSHA'        Comma separated E-Mail Addresses          
                                                                                
         DC    CL2'UW'             Alpha id UGHGNY                              
         DC    CL8'UGHGNY'         Userid                                       
         DC    CL45'UNLOCK@GREY.COM'  Comma separated E-Mail Addresses          
                                                                                
         DC    AL1(FF)                                                          
                                                                                
NCATAB   DS    0X                  1N Account Table                             
         DC    (MAXNCAQ)XL12'00'                                                
NCATABX  DC    X'FF'                                                            
MAXNCAQ  EQU   20                                                               
                                                                                
TRTAB    DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 00-0F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 10-1F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 20-2F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 30-3F                     
         DC    XL16'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F' 4B-4F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B5A5B5C5D5E5F' 50-5F                     
         DC    XL16'60614B4B4B4B4B4B4B4B4B4B6C6D6E6F' 60-6F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B797A7B7C7D7E7F' 70-7F                     
         DC    XL16'4B8182838485868788894B4B4B4B4B4B' 80-8F                     
         DC    XL16'4B9192939495969798994B4B4B4B4B4B' 90-9F                     
         DC    XL16'4BA1A2A3A4A5A6A7A8A94B4B4B4B4B4B' A0-AF                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' B0-BF                     
         DC    XL16'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B' C0-CF                     
         DC    XL16'D0D1D2D3D4D5D6D7D8D94B4B4B4B4B4B' D0-DF                     
         DC    XL16'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B' F0-FF                     
                                                                                
                                                                                
         DS    0A                                                               
RECTAB   DC    AL1(FW_FTRN$)       ADDTRN handler                               
         DC    AL1(TODOACCQ)                                                    
         DC    AL2(FWSENA-WORKD)                                                
         DC    AL2(ATRN-SRUPD60)                                                
                                                                                
         DC    AL1(FW_FTIM$)       TIMREC handler                               
         DC    AL1(TODOACCQ)                                                    
         DC    AL2(FWSENA-WORKD)                                                
         DC    AL2(TIMR-SRUPD60)                                                
                                                                                
*&&UK*&& DC    AL1(FW_FCAM$)       CAMREC handler                               
*&&UK*&& DC    AL1(TODOMEDQ)                                                    
*&&UK*&& DC    AL2(FWSENM-WORKD)                                                
*&&UK*&& DC    AL2(CAMR-SRUPD60)                                                
                                                                                
*&&UK*&& DC    AL1(FW_FPLD$)       PLDREC handler                               
*&&UK*&& DC    AL1(TODOACCQ)                                                    
*&&UK*&& DC    AL2(FWSENA-WORKD)                                                
*&&UK*&& DC    AL2(PLDR-SRUPD60)                                                
                                                                                
         DC    AL1(FW_FDIR)        ACCDIR record handler                        
         DC    AL1(TODOACCQ)                                                    
         DC    AL2(FWSENA-WORKD)                                                
         DC    AL2(ADIR-SRUPD60)                                                
                                                                                
         DC    AL1(FW_FMST)        ACCMST record handler                        
         DC    AL1(TODOACCQ)                                                    
         DC    AL2(FWSENA-WORKD)                                                
         DC    AL2(AMST-SRUPD60)                                                
*&&UK                                                                           
*                                                                               
         DC    AL1(FW_FMDI)        MEDDIR record handler                        
         DC    AL1(TODOMEDQ)                                                    
         DC    AL2(FWSENM-WORKD)                                                
         DC    AL2(MDIR-SRUPD60)                                                
                                                                                
         DC    AL1(FW_FMFI)        MEDFIL record handler                        
         DC    AL1(TODOMEDQ)                                                    
         DC    AL2(FWSENM-WORKD)                                                
         DC    AL2(MFIL-SRUPD60)                                                
*&&                                                                             
         DC    AL1(FW_FLOK$)       LOKREC record handler                        
         DC    AL1(TODOLOKQ)                                                    
         DC    AL2(0)                                                           
         DC    AL2(LOCK-SRUPD60)                                                
                                                                                
RECTABN  EQU   (*-RECTAB)/RECTABL                                               
         EJECT                                                                  
SSB      CSECT                                                                  
         DC    256X'00'                                                         
         EJECT                                                                  
WORKD    DSECT                     ** Local working storage **                  
                                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
                                                                                
RELO     DS    A                   Program relocation factor                    
                                                                                
ASRUPD60 DS    A                   A(This program)                              
                                                                                
PLIST    DS    0X                  ** Calling parameter list **                 
                                                                                
PLONOFF  DS    0X                  ** On-line/Off-line flag **                  
PLON     EQU   X'00'               Running on-line  (under SRUPD00)             
PLOFF    EQU   X'FF'               Running off-line (under ACBRA1B)             
                                                                                
PLALPD   DS    0A                  A(LP_D)   - off-line or                      
PLASRPAR DS    A                   A(SRPARS) - on-line                          
PLSORT   DS    0X                  ** Records in SORTER not FACWRK **           
PLAFWPAR DS    A                   A(FWDMCB)                                    
PLABUF   DS    A                   A(18K buffer)                                
PLISTL   EQU   *-PLIST                                                          
                                                                                
SRPARS   DS    0X                  ** Service request parameters **             
SRPAR1   DS    A                   A(SYSFACS)                                   
SRPAR2   DS    A                   A(TIA)                                       
SRPAR3   DS    A                   A(UTL)                                       
SRPAR4   DS    A                   A(COMFACS)                                   
SRPARSL  EQU   *-SRPARS                                                         
                                                                                
FWDMCB   DS    0X                  ** FACWRK parameter list **                  
FWAACT   DS    A                   A(Action)                                    
FWAFIL   DS    A                   A(File)                                      
FWANDX   DS    A                   A(Index record)                              
FWAREC   DS    A                   A(I/O area)                                  
FWABUF   DS    A                   A(Buffer)                                    
FWDMCBL  EQU   *-FWDMCB                                                         
                                                                                
ACOMFACS DS    A                   A(COMFACS)                                   
LDCPTR   DS    A                   A(LDCPTR)                                    
LDCMPT   DS    A                   A(LDCMPT)                                    
                                                                                
CALLOV   DS    A                                                                
DATAMGR  DS    A                                                                
DATCON   DS    A                                                                
HELLO    DS    A                                                                
HEXOUT   DS    A                                                                
LOCKET   DS    A                                                                
PROTOFF  DS    A                                                                
PROTON   DS    A                                                                
RUNFACS  DS    A                                                                
SWITCH   DS    A                                                                
PRINTER  DS    A                                                                
SMTP     DS    A                   A(SMTP)                                      
MQI      DS    A                   A(MQ interface)                              
ADDAY    DS    A                                                                
GETDAY   DS    A                                                                
AMASTC   DS    A                                                                
                                                                                
AIO1     DS    A                   A(I/O area 1)                                
AIO2     DS    A                   A(I/O area 2)                                
AIO3     DS    A                   A(I/O area 3)                                
AIO4     DS    A                   A(I/O area 4)                                
AIO5     DS    A                   A(I/O area 5)                                
AIO6     DS    A                   A(I/O area 6)                                
AIO7     DS    A                   A(I/O area 7)                                
                                                                                
AGENAREA DS    A                   General Storage area                         
                                                                                
ANEXT#   DS    A                   A(next record number to read)                
                                                                                
AUTHOOK1 DS    A                   A(UPDTIM BUFREC hook routine 1)              
AUTHOOK2 DS    A                   A(UPDTIM BUFREC hook routine 2)              
AUTHOOK3 DS    A                   A(UPDTIM BUFREC hook routine 3)              
                                                                                
ATSYS    DS    A                   A(SENUM in offline UTL)                      
                                                                                
AENDLID  DS    A                   A(END OF LIST ELEMENT)                       
VSORTER  DS    V                   A(SORTER)                                    
SAVEELE  DS    A                                                                
                                                                                
TSAROLDT DS    XL(TSPXTNL)         TSAR block for old time buffer               
TSARNEWT DS    XL(TSPXTNL)         TSAR block for new time buffer               
TSARRECS DS    XL(TSPXTNL)         TSAR block for virtual I/O                   
TSARPASS DS    XL(TSPXTNL)         TSAR block for passive pointers              
TSARSORT DS    XL(TSPXTNL)         TSAR block for sorting input file            
TSARTRNK DS    XL(TSPXTNL)         TSAR block for transaction keys              
                                                                                
FWFLAG   DS    XL(L'FW_FLAG)       Header record flag byte                      
FWSENA   DS    XL(L'FW_HSENA)      Header record account SENUM                  
FWSENM   DS    XL(L'FW_HSENM)      Header record media SENUM                    
FWCMPT   DS    XL(CMPTDLQ)         Block for MELDCMPT                           
                                                                                
SORTREC  DS    0X                  ** Sort file record **                       
SORTKEY  DS    XL(FW_KLNQ)         Key from input file                          
SORTSEQ# DS    XL2                 Sequence number (makes key unique)           
SORTKLNQ EQU   *-SORTKEY                                                        
SORTREC# DS    XL2                 Input file record sequence number            
SORTRLNQ EQU   *-SORTREC                                                        
                                                                                
AGY      DS    XL(L'FW_AGY)        Agency alpha ID                              
CPY      DS    XL(L'FW_CPY)        Company code                                 
AGYSEC   DS    XL(L'SECAGY)        Agency used for security                     
                                                                                
USRID    DS    CL10                User ID                                      
FACID    DS    C                   Facpak ID                                    
                                                                                
RUNINDS  DS    XL1                 Run Indicators                               
RUNEDT   EQU   X'80'               Already got Edit hours                       
                                                                                
ELEMENT  DS    XL255               Storage to build elements                    
                                                                                
CURUL    DS    CL2                 Current Unit/Ledger for CHKPER               
DCDAT    DS    PL3                 WORKING DATE - START                         
DCEND    DS    PL3                 WORKING DATE - END                           
DOVRDTE  DS    PL3                 OVERDUE DATE                                 
TODAYP   DS    PL3                 TODAY DATE                                   
QEND     DS    PL3                                                              
QSTART   DS    PL3                                                              
APPRPID  DS    XL(L'PIDNO)         Approver PID number                          
APP1RACT DS    0CL12               Approvers Account                            
SUB1RACT DS    CL12                Submitters Account                           
TIMEOUT  DS    PL8                                                              
IN_GINSQ DS    XL(L'GININV)        Group invoice number                         
IN_GIHSQ DS    XL(L'GININV)        Group invoice highest number                 
SVTCAULA DS    CL(L'ACTKULA)       Saved contra account                         
SVTCOFF  DS    CL2                 Saved contra office                          
                                                                                
HD_1RPER DS    CL(L'PERKCODE)      Person PID code                              
HD_PPID# DS    XL(L'PIDNO)         Person PID number                            
HD_SUBLM DS    XL3                 Submitted date - line manager apprvr         
HD_SUBCL DS    XL3                 Submitted date - client approver             
HD_EDHRS DS    PL3                 Edit hours                                   
HD_PEDT  DS    XL3                 Period end date                              
HD_PEDTC DS    XL3                 Period end date complemented                 
HD_TSSTO DS    XL(L'TIMKSTAT)      Previous time sheet status                   
HD_TSSTN DS    XL(L'TIMKSTAT)      Current time sheet status                    
HD_IND   DS    XL(L'FW_HDIND)      Header status                                
                                                                                
HD_1RULA DS    0CL(L'ACTKULA)      ** Person 1R account **                      
HD_1RUNT DS    CL(L'ACTKUNT)       Unit                                         
HD_1RLDG DS    CL(L'ACTKLDG)       Ledger                                       
HD_1RACT DS    CL(L'ACTKACT)       Account code                                 
HD_CPSFS DS    CL(L'CPYSFST)       Company Fiscal Start                         
                                                                                
ONERLVS  DS    0X                  ** 1R ledger level lengths **                
ONERL1L  DS    X                   Length of office                             
ONERL2L  DS    X                   Length of department                         
ONERL3L  DS    X                   Length of sub-department                     
ONERL4L  DS    X                   Length of person                             
ONERLVL  EQU   *-ONERLVS                                                        
                                                                                
SJALVS   DS    0X                  ** SJ ledger level lengths **                
SJLEV1L  DS    X                   Length of office                             
SJLEV2L  DS    X                   Length of department                         
SJLEV3L  DS    X                   Length of sub-department                     
SJALVLQ  EQU   *-SJALVS                                                         
                                                                                
CALOVRLY DS    X                   Calling overlay                              
                                                                                
TIMMODE  DS    X                   ** Time reading mode **                      
TIMMBUFF EQU   1                   Reading buffer                               
TIMMFILE EQU   2                   Reading file                                 
                                                                                
TIMSEQ#  DS    X                   Sequence number of time record               
TIMESEQ# DS    X                   TIMSEQ seq number                            
                                                                                
TSARERRS DS    XL(L'TSERRS)        TSAR error condition                         
                                                                                
PASSEQ   DS    H                   Passive pointer sequence number              
                                                                                
MPASEQ   DS    H                   Media passive pointer sequence#              
                                                                                
CURRSYS  DS    X                   Current SENUM                                
GOTOSYS  DS    X                   Go to SENUM                                  
                                                                                
TODO     DS    X                   Control break processing to do               
TODOACCQ EQU   X'80'               Account record(s)                            
TODOMEDQ EQU   X'40'               Media record(s)                              
TODOLOKQ EQU   X'20'               Lock record(s)                               
                                                                                
PASIND   DS    X                   ** BLDPAS routine flags **                   
PASIINIT EQU   X'80'               Passive buffer initialised                   
                                                                                
MPAIND   DS    X                   ** BLDMPA routine flags **                   
MPAIINIT EQU   X'80'               Passive buffer initialised                   
                                                                                
TRKIND   DS    X                   ** BLDTRK routine flags **                   
TRKIINIT EQU   X'80'               Trans key buffer initialised                 
                                                                                
UPDTIND  DS    X                   ** UPDTIM routine flags **                   
UPDTIFND EQU   X'80'               Time record found                            
UPDTIADD EQU   X'40'               Add a new time record                        
UPDTIDEL EQU   X'20'               Re-use deleted time record                   
                                                                                
TIMRIND  DS    X                   ** TIMREC handler indicators **              
TIMRIFST EQU   X'80'               First time flag                              
TIMRITIM EQU   X'40'               Time buffer records passed                   
TIMRIDEL EQU   X'20'               Delete time records                          
TIMRISTA EQU   X'10'               Change status of time records                
                                                                                
LLKEY    DS    XL(L'FW_LKKEY)      Last LOCKET key                              
                                                                                
ATRNIND  DS    X                   ** ADDTRN handler indicators **              
ATRNIFST EQU   X'80'               First time flag                              
ATRNIPUT EQU   X'40'               ADDTRN called                                
                                                                                
BUFRIND  DS    X                   ** BUFREC indicator byte **                  
BUFRIFST EQU   X'80'               First time flag                              
*&&UK                                                                           
PLDREC   DS    XL(ACCKLEN)         Current PLDREC                               
*&&                                                                             
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
FULL1    DS    F                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
WORK     DS    XL256                                                            
BYTE1    DS    XL1                                                              
ELEM     DS    XL255                                                            
                                                                                
DMADD#   DS    F                   Draft ADDREC sequence number                 
DMINDS   DS    X                                                                
DMFLAG   DS    X                                                                
DMRETN   DS    X                                                                
DMDA     DS    XL4                                                              
DMWORK   DS    XL48                                                             
DMKEY    DS    XL64                                                             
DMKEYSAV DS    XL64                                                             
                                                                                
ADDTRNW  DS    XL(TRNBLKL)         ADDTRN control block                         
PALAREA  DS    XL32                ADDTRN P&L control block                     
                                                                                
TDMCB    DS    6F                  DMCB for PRTREC                              
TPLIST   DS    XL24                Address/length list for PRTREC               
TRECLEN  DS    H                   Record length for PRTREC                     
TFLG     DS    X                   Flag for PRTREC                              
TDIR     DS    XL(ACCKLEN)         Read key                                     
                                                                                
TFILE    DS    C                   ** File type **                              
TFILEDIR EQU   C'D'                ACCDIR/MEDDIR                                
TFILEFIL EQU   C'F'                ACCMST/ACCARC/MEDFIL                         
TFILEBEF EQU   C'B'                Before key (key only)                        
                                                                                
THISFIL3 DS    0CL3                MED, etc.                                    
THISFIL4 DS    0CL4                MEDF, MEDD etc.                              
THISFILE DS    CL(L'ACCMST)        File                                         
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACLDCPTRD                                                      
         PRINT ON                                                               
                                                                                
PASLST   DS    XL(6*ONEK)          Passive pointers built here                  
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DAY TABLE DSECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
DAYTABD  DSECT                                                                  
DAY#     DS    XL1                 GETDAY Day #                                 
DAYEDT#  DS    XL1                 EDIT Hrs Day #                               
DAYTABLN EQU   *-DAYTABD                                                        
                                                                                
***********************************************************************         
* CALENDAR TABLE DSECT                                                *         
***********************************************************************         
         SPACE 1                                                                
CALTABD  DSECT                                                                  
CALENDT  DS    PL3                 Period end date 2's complement               
CALSTRT  DS    PL3                 Period start date 2's complement             
CALTABL  EQU   *-CALTABD                                                        
                                                                                
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
* CAMPAIGN ACCEL TABLE                                                *         
***********************************************************************         
                                                                                
*&&UK                                                                           
CAMPVALD DSECT                     Campaign bucket values                       
CBVMONTH DS    XL2                                                              
CBVORNT  DS    PL8                                                              
CBVBLBLE DS    PL8                                                              
CBVPAID  DS    PL8                                                              
CBVORGR  DS    PL8                                                              
CBVMED   DS    PL8                                                              
CBVCOMM  DS    PL8                                                              
CBVBILL  DS    PL8                                                              
CBVVOL   DS    PL8                                                              
CBVNUMQ  EQU   8                                                                
CAMPVALQ EQU   *-CAMPVALD                                                       
*&&                                                                             
                                                                                
***********************************************************************         
* RECORD TABLE DSECT                                                  *         
***********************************************************************         
         SPACE 1                                                                
RECTABD  DSECT                                                                  
RECTFIL  DS    XL1                                                              
RECTTDO  DS    XL1                                                              
RECTDSY  DS    XL2                                                              
RECTROU  DS    XL2                                                              
RECTABL  EQU   *-RECTABD                                                        
                                                                                
       ++INCLUDE SRUPD60D                                                       
       ++INCLUDE ACTRAND                                                        
       ++INCLUDE ACTIMED                                                        
TT_D     DSECT                     ** TT_D extension **                         
         ORG   TT_D+TT_LN1Q                                                     
TT_DLEN  DS    AL2                 Length of time data that follows             
TT_DATA  DS    0X                  Time elements created here                   
TT_RECL  EQU   ONEK                Time records are 1k long                     
         EJECT                                                                  
* Other included books follow                                                   
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         ORG   P                                                                
PDMGR    DS    0CL(L'P)            ** DATAMGR trace line **                     
PDMGRLIT DS    CL(L'TDMGR)                                                      
PDMGRACT DS    CL(L'PUTREC)        Action                                       
         DS    C                                                                
PDMGRFIL DS    CL(L'ACCMST)        File                                         
         DS    C                                                                
PDMGRDAL DS    CL(L'TDA)           'D/A='                                       
PDMGRDAD DS    CL8                 Disk address                                 
         ORG   PDMGRDAL                                                         
PDMGRBOA DS    CL12                'Input key'/'Output key'                     
         DS    C                                                                
PDMGRRBL DS    CL(L'TRET)          'Ret='                                       
PDMGRRB  DS    CL2                 XX                                           
         DS    C                                                                
PDMGRIOL DS    CL(L'TIOC)          'I/O Count='                                 
PDMGRIO  DS    CL6                 999999                                       
       ++INCLUDE DDLINKD                                                        
       ++INCLUDE DDRUNNERD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDTSARD                                                        
*PREFIX=L                                                                       
       ++INCLUDE FALOCKETD                                                      
*PREFIX=                                                                        
       ++INCLUDE ACRECEQUS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
ADDTRND  DSECT                                                                  
       ++INCLUDE ACADDTRND                                                      
* DDSMTPD                                                                       
       ++INCLUDE DDSMTPD                                                        
* FASECRETD                                                                     
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
* FAUTL                                                                         
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* MEFILGEND                                                                     
         PRINT OFF                                                              
       ++INCLUDE MEFILGEND                                                      
         PRINT ON                                                               
*&&UK                                                                           
* MEFILPASD                                                                     
* MEFILCACEL                                                                    
         PRINT OFF                                                              
       ++INCLUDE MEFILPASD                                                      
       ++INCLUDE MEFILCACEL                                                     
         PRINT ON                                                               
*&&                                                                             
* MELDCMPTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE MELDCMPTD                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084SRUPD60   02/09/21'                                      
         END                                                                    
