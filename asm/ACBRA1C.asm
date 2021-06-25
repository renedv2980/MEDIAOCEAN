*          DATA SET ACBRA1C    AT LEVEL 024 AS OF 04/24/15                      
*PHASE T6241CA                                                                  
*                                                                               
*                                                                               
ACBRA1C  TITLE '- BRA File Upload download server'                              
                                                                                
***********************************************************************         
* Download server: Downloads definitions of upload request maps to    *         
* enable caller to make upload requests dynamically.                  *         
*                                                                     *         
***********************************************************************         
                                                                                
* Level change comments                                                         
* ---------------------                                                         
* JFOS 28MAR11 <PR000031> First version                                         
* SMAN 15NOV11 <BR18745D> Fix length of posting item number                     
* MPEN 021 17MAR14 <DSBO-675> Additional fields for account upload              
* MPEN 022 29AUG14 <DSMHUB-40> Additional fields for account upload             
* MPEN 023 27OCT14 <DSMHUB-37> Fix for media filters                            
* NSHE 024 24APR15 <DSBO-1406> Remove SETFAC routine                            
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           +        
               SLOWLIST=SLOWS,FACS=FACS,WORKERKEY=ACBO,ABENDLIST=FAILS,+        
               SYSPHASE=SYSPHASE,SYSTEM=ACCSYSQ,APPEND=Y,              +        
               SERVERTYPE=TSTACBO,SEGMENT=Y,LOADFACSOFF=Y,             +        
               BLOCKS=(B#WORKD,WORKD,                                  +        
               B#SAVED,SAVED,                                          +        
               B#TWA,TWAD,                                             +        
               B#LP_D,LP_D,                                            +        
               B#UPVTAB,UPVTHDD)                                                
*                                                                               
SLOWS    DC    C':'                                                             
FAILS    DC    C':'                                                             
*                                                                               
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO1C**,CLEAR=YES,RR=RE                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R7                                                      
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6                                                      
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         L     R9,LP_ABLK1         ONLINE - ROOT PROVIDES WORKD/SAVED           
         ICM   R8,15,RSVRSAVE                                                   
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         MVC   ATWA,LP_ATWA                                                     
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ABUFFRIN,RBUFFRIN                                                
         DROP  R6,R7                                                            
                                                                                
         MVI   TWAMODE,0                                                        
         USING TWAD,R7                                                          
         L     R7,ATWA             RA=A(ON/OFFLINE TWA)                         
                                                                                
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
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
*                                                                               
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
*                                                                               
RUNSTR   GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
*                                                                               
RUNSTR04 OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
         LA    R0,UPVTAB                                                        
         ST    R0,LP_BLKS+((B#UPVTAB-1)*L'LP_BLKS)                              
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
         MVC   LP_BLKS+((B#LP_D-1)*L'LP_BLKS)(L'LP_BLKS),ALP                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   XC    QVALUES(QVALUESL),QVALUES     Initialise local                   
         XC    DVALUES(DVALUESL),DVALUES     storage, first for run             
         LA    R0,OVALUES                                                       
         LHI   R1,OVALUESL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   MVI   GIND2,GI2INAP                                                    
         GOTOR (#CPYINI,ACPYINI)   Initialise company values                    
         GOTO1 VDICTATE,DMCB,C'LL  ',DCDICTL,DSDICTL                            
         MVC   AGENCY,CUXCPY                                                    
         MVC   USRID,CUUSER                                                     
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
                                                                                
RUNREQ02 L     RE,LP_AWMP          build default range elements in WMP          
         USING LW_D,RE                                                          
         MVI   LW_TYPE,LW_TALLQ    all values                                   
         STCM  RE,7,AALL                                                        
         AHI   RE,LW_LN1Q                                                       
         MVI   LW_TYPE,LW_TNZRQ    non-zero values                              
         STCM  RE,7,ANZR                                                        
         AHI   RE,LW_LN1Q                                                       
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
RUNREQ04 XC    MAPI,MAPI           initialise map indicators                    
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JNE   *+14                                                             
         MVC   MAPI,L'LP_QMAPN(RF)                                              
         J     *+10                                                             
         AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
*                                                                               
RUNREQX  GOTOR LP_APUTO,LP_D       call DDLINK output processor                 
         J     EXITY               exit back to DDLINK                          
         DROP  RB                                                               
                                                                                
***********************************************************************         
* File Upload Maps download                                           *         
***********************************************************************         
                                                                                
REQFUMD  LKREQ H,A#FUMDL,OUTFUMD,NEXTREQ=REQUESTX                               
                                                                                
Dummy    LKREQ F,01,(D,B#SAVED,QDUMMY),CHAR,OLEN=L'QDUMMY,             +        
               MAXLEN=L'QDUMMY,TEXT=AC#NOORD,COL=*                              
                                                                                
         LKREQ E                                                                
                                                                                
OUTFUMD  LKOUT H                                                                
                                                                                
         LKOUT R,R#FUMDL                                                        
Array    LKOUT C,1,(A,ARYFUMD)                                                  
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYFUMD  LKOUT A,(R,NXTFUM),MULTIROW=Y,ROWNAME=UPVTHDD                          
Uplcod   LKOUT C,01,UPVTHUC,CHAR                                                
Prout    LKOUT P,UPVTHFL,SETTNML                                                
Uplnam   LKOUT C,02,UPVTHFN,(R,EDTTNM)                                          
Aswupc   LKOUT C,03,UPVTHAS,CHAR                                                
Prout    LKOUT P,UPVTHAL,SETTNML                                                
Uplnam   LKOUT C,04,UPVTHAN,(R,EDTDNM)                                          
Upllvl   LKOUT C,05,UPVTHLV,CBIN                                                
Uplaet   LKOUT C,06,UPVTHET,CHAR                                                
Upltrno  LKOUT C,07,UPVTHTN,CHAR                                                
Array    LKOUT C,01,(A,ARYFLD)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
ARYFLD   LKOUT A,(D,B#UPVTAB,UPVTDFST),EOT=EOR,NEWEL=B,                +        
               ROWID=(UPVTDAD,UPVTDELQ),ROWWIDTH=(V,UPVTDLN)                    
FldID    LKOUT C,01,UPVTDFI,CBIN                                                
Prout    LKOUT P,UPVTDFL,SETTNML                                                
Fldname  LKOUT C,02,UPVTDFN,(R,EDTTNM)                                          
Fldtype  LKOUT C,03,UPVTDDT,CBIN                                                
Fldlowc  LKOUT C,04,UPVTDLC,CHAR                                                
Fldmaxl  LKOUT C,05,UPVTDML,CBIN                                                
Fldaray  LKOUT C,06,UPVTDAR,CHAR                                                
Fldlist  LKOUT C,07,UPVTDLI,CHAR                                                
Fldnntr  LKOUT C,08,UPVTDEN,CBIN                                                
Fldmxrp  LKOUT C,09,UPVTDMR,CBIN                                                
Fldpctk  LKOUT C,10,UPVTDPT,CHAR                                                
Fldrtrn  LKOUT C,11,UPVTDRT,CHAR                                                
Flderro  LKOUT C,12,UPVTDER,CHAR                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Get first/next Upload map table                                     *         
***********************************************************************         
                                                                                
         USING UPVTHDD,R2                                                       
NXTFUM   CLI   LP_RMODE,LP_RFRST   test first time                              
         JNE   NFUM04              no                                           
         LARL  R2,UPVTAB           Set A(First table)                           
         SR    R3,R3                                                            
NFUM00   MVC   LBYTE1,UPVTHRC                                                   
         GOTOR VSECRET,DMCB,('SECPRACT+SECPOSP',ASECBLK),              +        
               (UPVTHRC,UPVTHRA),SECPROG                                        
         JE    NFUM02                                                           
NFUM01   ICM   R3,3,UPVTLEN        L'Current table                              
         AR    R2,R3               bump to next table                           
         OC    UPVTLEN,UPVTLEN     test EOT                                     
         JZ    NFUM10                                                           
         CLC   UPVTHRC,LBYTE1                                                   
         JE    NFUM01                                                           
         J     NFUM00                                                           
                                                                                
NFUM02   ST    R2,LP_ADATA                                                      
         ST    R2,AUPVTCUR                                                      
         ST    R2,LP_BLKS+((B#UPVTAB-1)*L'LP_BLKS)                              
         MVI   LP_RMODE,LP_RNEXT                                                
         J     EXITY                                                            
                                                                                
NFUM04   DS    0H                                                               
         L     R2,AUPVTCUR         RF=A(Current table)                          
         SR    R3,R3                                                            
NFUM05   ICM   R3,3,UPVTLEN                                                     
         AR    R2,R3                                                            
         OC    UPVTLEN,UPVTLEN     EOT?                                         
         JZ    NFUM10              Yes                                          
                                                                                
NFUM06   MVC   LBYTE1,UPVTHRC                                                   
         GOTOR VSECRET,DMCB,('SECPRACT+SECPOSP',ASECBLK),              +        
               (UPVTHRC,UPVTHRA),SECPROG                                        
         JE    NFUM08                                                           
NFUM07   ICM   R3,3,UPVTLEN                                                     
         AR    R2,R3                                                            
         OC    UPVTLEN,UPVTLEN     EOT?                                         
         JZ    NFUM10              Yes                                          
         CLC   UPVTHRC,LBYTE1                                                   
         JE    NFUM07                                                           
         J     NFUM06                                                           
                                                                                
NFUM08   ST    R2,LP_ADATA         set A(Next table)                            
         ST    R2,AUPVTCUR                                                      
         ST    R2,LP_BLKS+((B#UPVTAB-1)*L'LP_BLKS)                              
         J     EXITY                                                            
                                                                                
NFUM10   MVI   LP_RMODE,LP_RLAST                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
REQUESTX LKREQ X                                                                
                                                                                
***********************************************************************         
* Local routines                                                      *         
***********************************************************************         
                                                                                
SETTNML  L     R1,LP_AINP                                                       
         MVC   UPVLITL,0(R1)       Save L'literal                               
         J     EXIT                                                             
                                                                                
EDTDNM   LM    R2,R4,LP_AINP                                                    
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)          disp into DSDICTL                            
         LA    RE,DSDICTL(RE)      RE=A(literal)                                
         LLC   RF,UPVLITL                                                       
         ST    RF,LP_OLEN                                                       
         AHI   RF,-1                                                            
         BASR  R1,0                                                             
         MVC   0(0,R4),0(RE)                                                    
         EX    RF,0(R1)                                                         
         J     EXITY                                                            
                                                                                
EDTTNM   LM    R2,R4,LP_AINP                                                    
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)          disp into UPVLITS                            
         LA    RE,UPVLITS(RE)      RE=A(literal)                                
         LLC   RF,UPVLITL                                                       
         ST    RF,LP_OLEN                                                       
         AHI   RF,-1                                                            
         BASR  R1,0                                                             
         MVC   0(0,R4),0(RE)                                                    
         EX    RF,0(R1)                                                         
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Clear work area (RC=A(Work area), R1=L'Work area)                   *         
***********************************************************************         
                                                                                
CLRWRK   STM   RE,R1,12(RD)                                                     
         LR    R0,RC                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT AND DECLARATIONS                                      *          
**********************************************************************          
                                                                                
SETCCC   JE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               NOT EQUAL TO EQUAL/NOT ZERO TO ZERO          
         BR    RE                                                               
         LTR   RE,RE               EQUAL TO NOT EQUAL/ZERO TO NOT ZERO          
         BR    RE                                                               
                                                                                
MORE     MVI   LP_RMODE,LP_RMORE   SET MORE TO COME                             
         J     EXITY                                                            
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
         J     EXITY                                                            
                                                                                
QERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         J     EXITY                                                            
                                                                                
XERROR   MVI   LP_RMODE,LP_RERRR   SET REQUEST ERROR & EXIT                     
         MVI   LP_EMSYS,6                                                       
         J     EXITN                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         J     EXITY                                                            
                                                                                
EXITH    LHI   RE,2                                                             
         J     EXITCC                                                           
EXITL    DS    0H                                                               
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LIST OF ACCOUNT FILES TO OPEN IN ALL SYSTEMS                        *         
***********************************************************************         
                                                                                
FILES    DS    0X                 ** FILE INFO **                               
         DC    C'ACCOUNT'         SYSTEM NAME FOR OPEN                          
                                                                                
         DC    C'X'               No files required                             
                                                                                
                                                                                
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
                                                                                
FACS     DS    0X                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* UPLOAD VALUES TABLE - CURRENT DEFINITIONS:                          *         
*                                                                     *         
* X'64' History/Salary Header Upload                                  *         
* X'65' History/Salary Element Upload                                 *         
* X'66' History/Salary Trailer Upload                                 *         
* X'75' Posting Header Upload                                         *         
* X'76' Posting Item Upload                                           *         
* X'77' Posting Workcode Upload                                       *         
* X'78' Posting Trailer Upload                                        *         
* X'82' Account Upload                                                *         
*                                                                     *         
***********************************************************************         
                                                                                
UPVTAB   DS    0X                                                               
                                                                                
* History/Salary Upload                                                         
*                                                                               
** History/Salary Header fields    (R#HISLHDR)                                  
*                                                                               
UPVHSH   DS    0X                                                               
         DC    AL2(UPVHSHLQ)                                                    
         DC    AL1(RECHIST)                                                     
         DC    AL1(HISUPLD)                                                     
         DC    CL4'64'           History/Salary header Upload                   
         DC    AL1(L'HISSALH)                                                   
         DC    AL2(HISSALH-UPVLITS)                                             
         DC    CL4'64'                                                          
         DC    AL1(L'AC@HSUPL)                                                  
         DC    AL2(AC@HSUPL-DSDICTL)                                            
         DC    AL2(01)                                                          
         DC    AL1(YESQ)                                                        
         DC    CL4'66'                                                          
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           1R Account code                              
         DC    AL1(L'ACCOD1R)                                                   
         DC    AL2(ACCOD1R-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0003)           Month of activity                            
         DC    AL1(L'MOA)                                                       
         DC    AL2(MOA-UPVLITS)                                                 
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
* Return values                                                                 
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Errors                                       
         DC    AL1(L'ERRORS)                                                    
         DC    AL2(ERRORS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(255)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
*                                                                               
         DC    X'0000'             End of this table                            
*                                                                               
UPVHSHLQ EQU   *-UPVHSH                                                         
*                                                                               
** History/Salary Element fields   (R#HSLELE)                                   
*                                                                               
UPVHSE   DS    0X                                                               
         DC    AL2(UPVHSELQ)                                                    
         DC    AL1(RECHIST)                                                     
         DC    AL1(HISUPLD)                                                     
         DC    CL4'65'           History/Salary Element Upload                  
         DC    AL1(L'HISSALE)                                                   
         DC    AL2(HISSALE-UPVLITS)                                             
         DC    CL4'64'                                                          
         DC    AL1(L'AC@HSUPL)                                                  
         DC    AL2(AC@HSUPL-DSDICTL)                                            
         DC    AL2(02)                                                          
         DC    AL1(YESQ)                                                        
         DC    CL4'66'                                                          
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Date of salary                               
         DC    AL1(L'DATESAL)                                                   
         DC    AL2(DATESAL-UPVLITS)                                             
         DC    AL2(UPVDATEQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(10)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
*        DC    AL1(UPVTDELQ)                                                    
*        DC    AL1(UPVTDLNQ)                                                    
*        DC    AL3(0003)           Method                                       
*        DC    AL1(L'METHOD)                                                    
*        DC    AL2(METHOD-UPVLITS)                                              
*        DC    AL2(UPVCHARQ)                                                    
*        DC    AL1(NOQ)                                                         
*        DC    AL3(03)                                                          
*        DC    AL1(NULQ)                                                        
*        DC    AL1(NOQ)                                                         
*        DC    AL3(00)                                                          
*        DC    AL3(00)                                                          
*        DC    AL1(NOQ)                                                         
*        DC    AL1(NOQ)                                                         
*        DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0003)           Salary/payroll code                          
         DC    AL1(L'SALPCON)                                                   
         DC    AL2(SALPCON-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(05)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0004)           Amount                                       
         DC    AL1(L'AMOUNT)                                                    
         DC    AL2(AMOUNT-UPVLITS)                                              
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(10)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
* Return values                                                                 
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Errors                                       
         DC    AL1(L'ERRORS)                                                    
         DC    AL2(ERRORS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(255)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
*                                                                               
         DC    X'0000'             End of this table                            
*                                                                               
UPVHSELQ EQU   *-UPVHSE                                                         
*                                                                               
** History/Salary Trailer fields   (R#HSLTRL)                                   
*                                                                               
UPVHST   DS    0X                                                               
         DC    AL2(UPVHSTLQ)                                                    
         DC    AL1(RECHIST)                                                     
         DC    AL1(HISUPLD)                                                     
         DC    CL4'66'           History/Salary Trailer Upload                  
         DC    AL1(L'HISSALT)                                                   
         DC    AL2(HISSALT-UPVLITS)                                             
         DC    CL4'64'                                                          
         DC    AL1(L'AC@HSUPL)                                                  
         DC    AL2(AC@HSUPL-DSDICTL)                                            
         DC    AL2(03)                                                          
         DC    AL1(YESQ)                                                        
         DC    CL4'66'                                                          
*                                                                               
* Return values                                                                 
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Errors                                       
         DC    AL1(L'ERRORS)                                                    
         DC    AL2(ERRORS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(255)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
*                                                                               
         DC    X'0000'             End of this table                            
*                                                                               
UPVHSTLQ EQU   *-UPVHST                                                         
*                                                                               
* Posting Upload                                                                
*                                                                               
** Posting Header fields   (R#POHD)                                             
*                                                                               
UPVPOH   DS    0X                                                               
         DC    AL2(UPVPOHLQ)                                                    
         DC    AL1(RECSUPD)                                                     
         DC    AL1(SPOUPLD)                                                     
         DC    CL4'75'           Posting header Upload                          
         DC    AL1(L'POSHUP)                                                    
         DC    AL2(POSHUP-UPVLITS)                                              
         DC    CL4'75'                                                          
         DC    AL1(L'AC@PSUPL)                                                  
         DC    AL2(AC@PSUPL-DSDICTL)                                            
         DC    AL2(01)                                                          
         DC    AL1(YESQ)                                                        
         DC    CL4'78'                                                          
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Batch type                                   
         DC    AL1(L'BATTYP)                                                    
         DC    AL2(BATTYP-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0003)           Batch reference                              
         DC    AL1(L'BATREF)                                                    
         DC    AL2(BATREF-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0004)           Batch MOA                                    
         DC    AL1(L'BATMOA)                                                    
         DC    AL2(BATMOA-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0005)           Batch name                                   
         DC    AL1(L'BATNAM)                                                    
         DC    AL2(BATNAM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(15)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0006)           Batch comments                               
         DC    AL1(L'BATCMT)                                                    
         DC    AL2(BATCMT-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(50)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
* Return values                                                                 
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Errors                                       
         DC    AL1(L'ERRORS)                                                    
         DC    AL2(ERRORS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(255)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
*                                                                               
         DC    X'0000'             End of this table                            
*                                                                               
UPVPOHLQ EQU   *-UPVPOH                                                         
*                                                                               
** Posting Item fields   (R#PITM)                                               
*                                                                               
UPVPOI   DS    0X                                                               
         DC    AL2(UPVPOILQ)                                                    
         DC    AL1(RECSUPD)                                                     
         DC    AL1(SPOUPLD)                                                     
         DC    CL4'76'           Posting Item Upload                            
         DC    AL1(L'POSIUP)                                                    
         DC    AL2(POSIUP-UPVLITS)                                              
         DC    CL4'75'                                                          
         DC    AL1(L'AC@PSUPL)                                                  
         DC    AL2(AC@PSUPL-DSDICTL)                                            
         DC    AL2(02)                                                          
         DC    AL1(YESQ)                                                        
         DC    CL4'78'                                                          
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Posting item number                          
         DC    AL1(L'POSINO)                                                    
         DC    AL2(POSINO-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(05)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0003)           Debit a/c including unit + ledger            
         DC    AL1(L'DRACUL)                                                    
         DC    AL2(DRACUL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0004)           Credit a/c including unit + ledger           
         DC    AL1(L'CRACUL)                                                    
         DC    AL2(CRACUL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0005)           Reference                                    
         DC    AL1(L'REFNCE)                                                    
         DC    AL2(REFNCE-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(20)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0006)           Date                                         
         DC    AL1(L'DATE)                                                      
         DC    AL2(DATE-UPVLITS)                                                
         DC    AL2(UPVDATEQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0007)           Office                                       
         DC    AL1(L'OFFICE)                                                    
         DC    AL2(OFFICE-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0008)           Vat code                                     
         DC    AL1(L'VATCOD)                                                    
         DC    AL2(VATCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0009)           VAT Account code                             
         DC    AL1(L'VATACOD)                                                   
         DC    AL2(VATACOD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0010)           Order Number                                 
         DC    AL1(L'ORDNUM)                                                    
         DC    AL2(ORDNUM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0011)           Order Date                                   
         DC    AL1(L'ORDDAT)                                                    
         DC    AL2(ORDDAT-UPVLITS)                                              
         DC    AL2(UPVDATEQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0012)           Foreign Currency Code                        
         DC    AL1(L'FORCURC)                                                   
         DC    AL2(FORCURC-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0013)           Exchange Rate                                
         DC    AL1(L'EXCHRAT)                                                   
         DC    AL2(EXCHRAT-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0014)           Client code                                  
         DC    AL1(L'CLICOD)                                                    
         DC    AL2(CLICOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0015)           Product code                                 
         DC    AL1(L'PROCOD)                                                    
         DC    AL2(PROCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0016)           Job code                                     
         DC    AL1(L'JOBCOD)                                                    
         DC    AL2(JOBCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0017)           2D Account (Department)                      
         DC    AL1(L'ACCOD2D)                                                   
         DC    AL2(ACCOD2D-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0018)           2P Account (Person)                          
         DC    AL1(L'ACCOD2P)                                                   
         DC    AL2(ACCOD2P-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0019)           Miles                                        
         DC    AL1(L'MILES)                                                     
         DC    AL2(MILES-UPVLITS)                                               
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(13)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0020)           Estimate Number                              
         DC    AL1(L'ESTNUM)                                                    
         DC    AL2(ESTNUM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0021)           Due Date                                     
         DC    AL1(L'DUEDAT)                                                    
         DC    AL2(DUEDAT-UPVLITS)                                              
         DC    AL2(UPVDATEQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
                                                                                
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0022)           Internal reference                           
         DC    AL1(L'INTREF)                                                    
         DC    AL2(INTREF-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(07)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0023)           Cheque Number                                
         DC    AL1(L'CHQNUM)                                                    
         DC    AL2(CHQNUM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(07)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0024)           Cheque date                                  
         DC    AL1(L'CHQDAT)                                                    
         DC    AL2(CHQDAT-UPVLITS)                                              
         DC    AL2(UPVDATEQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0025)           Bank Account                                 
         DC    AL1(L'BNKACOD)                                                   
         DC    AL2(BNKACOD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0026)           Discount Account                             
         DC    AL1(L'DSCACOD)                                                   
         DC    AL2(DSCACOD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0027)           Discount Rate                                
         DC    AL1(L'DSCRAT)                                                    
         DC    AL2(DSCRAT-UPVLITS)                                              
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0028)           KSV Rate                                     
         DC    AL1(L'KSVRAT)                                                    
         DC    AL2(KSVRAT-UPVLITS)                                              
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0029)           KSV Billable                                 
         DC    AL1(L'KSVBLB)                                                    
         DC    AL2(KSVBLB-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0030)           Net or Gross                                 
         DC    AL1(L'NETOGRS)                                                   
         DC    AL2(NETOGRS-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0031)          Narrative                                     
         DC    AL1(L'NARRTV)                                                    
         DC    AL2(NARRTV-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(200)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
* Return values                                                                 
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Errors                                       
         DC    AL1(L'ERRORS)                                                    
         DC    AL2(ERRORS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(255)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
*                                                                               
         DC    X'0000'             End of this table                            
*                                                                               
UPVPOILQ EQU   *-UPVPOI                                                         
*                                                                               
** Posting Workcode fields   (R#PWKC)                                           
*                                                                               
UPVPOW   DS    0X                                                               
         DC    AL2(UPVPOWLQ)                                                    
         DC    AL1(RECSUPD)                                                     
         DC    AL1(SPOUPLD)                                                     
         DC    CL4'77'           Posting Workcode Upload                        
         DC    AL1(L'POSWUP)                                                    
         DC    AL2(POSWUP-UPVLITS)                                              
         DC    CL4'75'                                                          
         DC    AL1(L'AC@PSUPL)                                                  
         DC    AL2(AC@PSUPL-DSDICTL)                                            
         DC    AL2(03)                                                          
         DC    AL1(YESQ)                                                        
         DC    CL4'78'                                                          
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Item number                                  
         DC    AL1(L'ITEMNO)                                                    
         DC    AL2(ITEMNO-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(05)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0003)           Workcode sequence number                     
         DC    AL1(L'WCSEQNO)                                                   
         DC    AL2(WCSEQNO-UPVLITS)                                             
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0004)           Workcode                                     
         DC    AL1(L'WORKCOD)                                                   
         DC    AL2(WORKCOD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0005)           Amount                                       
         DC    AL1(L'AMOUNT)                                                    
         DC    AL2(AMOUNT-UPVLITS)                                              
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(13)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0006)           Foreign Currency Amount                      
         DC    AL1(L'FORCURA)                                                   
         DC    AL2(FORCURA-UPVLITS)                                             
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(13)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0010)           Narrative                                    
         DC    AL1(L'NARRTV)                                                    
         DC    AL2(NARRTV-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(200)                                                         
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL3(03)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
* Return values                                                                 
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Errors                                       
         DC    AL1(L'ERRORS)                                                    
         DC    AL2(ERRORS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(255)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
*                                                                               
         DC    X'0000'             End of this table                            
*                                                                               
UPVPOWLQ EQU   *-UPVPOW                                                         
*                                                                               
** Posting Trailer fields   (R#POTL)                                            
*                                                                               
UPVPOT   DS    0X                                                               
         DC    AL2(UPVPOTLQ)                                                    
         DC    AL1(RECSUPD)                                                     
         DC    AL1(SPOUPLD)                                                     
         DC    CL4'78  '           Posting Trailer Upload                       
         DC    AL1(L'POSTUP)                                                    
         DC    AL2(POSTUP-UPVLITS)                                              
         DC    CL4'75  '                                                        
         DC    AL1(L'AC@PSUPL)                                                  
         DC    AL2(AC@PSUPL-DSDICTL)                                            
         DC    AL2(04)                                                          
         DC    AL1(YESQ)                                                        
         DC    CL4'78  '                                                        
*                                                                               
* Return values                                                                 
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC/Web token                                 
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Errors                                       
         DC    AL1(L'ERRORS)                                                    
         DC    AL2(ERRORS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(255)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
*                                                                               
         DC    X'0000'             End of this table                            
*                                                                               
UPVPOTLQ EQU   *-UPVPOT                                                         
*                                                                               
*                                                                               
* Account Upload fields   (R#xxxx)                                              
*                                                                               
UPVACC   DS    0X                                                               
         DC    AL2(UPVACCLQ)                                                    
         DC    AL1(RECACCT)                                                     
         DC    AL1(ACCUPLD)                                                     
         DC    CL4'82'           Account Upload                                 
         DC    AL1(L'ACCLOAD)                                                   
         DC    AL2(ACCLOAD-UPVLITS)                                             
         DC    CL4'82'                                                          
         DC    AL1(L'AC@ACUPL)                                                  
         DC    AL2(AC@ACUPL-DSDICTL)                                            
         DC    AL2(01)                                                          
         DC    AL1(NOQ)                                                         
         DC    CL4'  '                                                          
*                                                                               
* Return values                                                                 
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           Unit                                         
         DC    AL1(L'UNIT)                                                      
         DC    AL2(UNIT-UPVLITS)                                                
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Ledger                                       
         DC    AL1(L'LEDGER)                                                    
         DC    AL2(LEDGER-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0003)           Account code                                 
         DC    AL1(L'ACCCOD)                                                    
         DC    AL2(ACCCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0004)           Request type                                 
         DC    AL1(L'REQTYP)                                                    
         DC    AL2(REQTYP-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0005)           Approval status                              
         DC    AL1(L'APPSTS)                                                    
         DC    AL2(APPSTS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0006)           Approval comments                            
         DC    AL1(L'APPCMT)                                                    
         DC    AL2(APPCMT-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(50)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0007)           Name                                         
         DC    AL1(L'NAME)                                                      
         DC    AL2(NAME-UPVLITS)                                                
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0008)           Override name for timesheets                 
         DC    AL1(L'OVNMTS)                                                    
         DC    AL2(OVNMTS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0009)           Foreign name                                 
         DC    AL1(L'FORNAM)                                                    
         DC    AL2(FORNAM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0010)           Long name                                    
         DC    AL1(L'LNGNAM)                                                    
         DC    AL2(LNGNAM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(100)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0011)           Media short name                             
         DC    AL1(L'MDSNAM)                                                    
         DC    AL2(MDSNAM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0012)           Close date                                   
         DC    AL1(L'CLODAT)                                                    
         DC    AL2(CLODAT-UPVLITS)                                              
         DC    AL2(UPVDATEQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0013)           Open date                                    
         DC    AL1(L'OPNDAT)                                                    
         DC    AL2(OPNDAT-UPVLITS)                                              
         DC    AL2(UPVDATEQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0014)           Job is Master Job                            
         DC    AL1(L'JOBMJOB)                                                   
         DC    AL2(JOBMJOB-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0015)           BrandOcean estimate job                      
         DC    AL1(L'BROEJOB)                                                   
         DC    AL2(BROEJOB-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0016)           PC Token                                     
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0017)           Account Address                              
         DC    AL1(L'ACCADD)                                                    
         DC    AL2(ACCADD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(26)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL3(06)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0018)           Filter Number                                
         DC    AL1(L'FLTNUM)                                                    
         DC    AL2(FLTNUM-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(05)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0019)           Filter Value                                 
         DC    AL1(L'FLTVAL)                                                    
         DC    AL2(FLTVAL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(ARYLSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(05)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0020)           Locked status                                
         DC    AL1(L'LCKSTS)                                                    
         DC    AL2(LCKSTS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0021)           Closed status                                
         DC    AL1(L'CLOSTS)                                                    
         DC    AL2(CLOSTS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0022)           Departmental analysis                        
         DC    AL1(L'DEPANL)                                                    
         DC    AL2(DEPANL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0023)           Miles analysis                               
         DC    AL1(L'MILANL)                                                    
         DC    AL2(MILANL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0024)           Client analysis                              
         DC    AL1(L'CLIANL)                                                    
         DC    AL2(CLIANL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0025)           Staff analysis                               
         DC    AL1(L'STFANL)                                                    
         DC    AL2(STFANL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0026)           Person is executive                          
         DC    AL1(L'PEREXC)                                                    
         DC    AL2(PEREXC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0027)           Project control                              
         DC    AL1(L'PRJCON)                                                    
         DC    AL2(PRJCON-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0028)           Balance sheet                                
         DC    AL1(L'BALSHT)                                                    
         DC    AL2(BALSHT-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0029)           Profit Loss                                  
         DC    AL1(L'PRFLSS)                                                    
         DC    AL2(PRFLSS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0030)           Job analysis                                 
         DC    AL1(L'JOBANL)                                                    
         DC    AL2(JOBANL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0031)           Provisional vendor                           
         DC    AL1(L'PRVVEN)                                                    
         DC    AL2(PRVVEN-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0032)           Merge invoices                               
         DC    AL1(L'MRGINVS)                                                   
         DC    AL2(MRGINVS-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0033)           Payee locked                                 
         DC    AL1(L'PAYLCKD)                                                   
         DC    AL2(PAYLCKD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0034)           Peel/Close reconciled Credits                
         DC    AL1(L'PCRECCR)                                                   
         DC    AL2(PCRECCR-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0035)           Peel/Close reconciled Debits                 
         DC    AL1(L'PCRECDR)                                                   
         DC    AL2(PCRECDR-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0036)           Security level                               
         DC    AL1(L'SECLVL)                                                    
         DC    AL2(SECLVL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0037)           Client cost                                  
         DC    AL1(L'CLICST)                                                    
         DC    AL2(CLICST-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0038)           Job locks estimate                           
         DC    AL1(L'JOBLEST)                                                   
         DC    AL2(JOBLEST-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0039)           Job locks orders                             
         DC    AL1(L'JOBLORD)                                                   
         DC    AL2(JOBLORD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0040)           Job locks billing                            
         DC    AL1(L'JOBLBIL)                                                   
         DC    AL2(JOBLBIL-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0041)           Job locks timesheets                         
         DC    AL1(L'JOBLTIM)                                                   
         DC    AL2(JOBLTIM-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0042)           Job locks adjustments                        
         DC    AL1(L'JOBLADJ)                                                   
         DC    AL2(JOBLADJ-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0043)           Job locks externals                          
         DC    AL1(L'JOBLEXT)                                                   
         DC    AL2(JOBLEXT-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0044)           Exclude from timesheets                      
         DC    AL1(L'EXFRTIM)                                                   
         DC    AL2(EXFRTIM-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0045)           Desktop priority                             
         DC    AL1(L'DSKTPR)                                                    
         DC    AL2(DSKTPR-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0046)           Input VAT account                            
         DC    AL1(L'INPVAC)                                                    
         DC    AL2(INPVAC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0047)           Sundry creditor                              
         DC    AL1(L'SUNCRD)                                                    
         DC    AL2(SUNCRD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0048)           Invoice register                             
         DC    AL1(L'INVREG)                                                    
         DC    AL2(INVREG-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0049)           Force product on timesheet                   
         DC    AL1(L'FPROTIM)                                                   
         DC    AL2(FPROTIM-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0050)           Force job on timesheet                       
         DC    AL1(L'FJOBTIM)                                                   
         DC    AL2(FJOBTIM-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0051)           Default workcode                             
         DC    AL1(L'DEFWC)                                                     
         DC    AL2(DEFWC-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0052)           General ledger office                        
         DC    AL1(L'GLOFF)                                                     
         DC    AL2(GLOFF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0053)           General ledger account code                  
         DC    AL1(L'GLACOD)                                                    
         DC    AL2(GLACOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0054)           VAT number                                   
         DC    AL1(L'VATNUM)                                                    
         DC    AL2(VATNUM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(09)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0055)           NIC status profile                           
         DC    AL1(L'NICSTSP)                                                   
         DC    AL2(NICSTSP-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0056)           Foreign language user                        
         DC    AL1(L'FORLNGU)                                                   
         DC    AL2(FORLNGU-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0057)           Account currency                             
         DC    AL1(L'ACCCUR)                                                    
         DC    AL2(ACCCUR-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0058)           Local currency                               
         DC    AL1(L'LOCCUR)                                                    
         DC    AL2(LOCCUR-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0059)           KSV type                                     
         DC    AL1(L'KSVTYP)                                                    
         DC    AL2(KSVTYP-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0060)           VAT code 13B                                 
         DC    AL1(L'VATC13B)                                                   
         DC    AL2(VATC13B-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0061)           VAT code                                     
         DC    AL1(L'VATCOD)                                                    
         DC    AL2(VATCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0062)           Tax/VAT registration number                  
         DC    AL1(L'TAXREGN)                                                   
         DC    AL2(TAXREGN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(18)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0063)           Email address                                
         DC    AL1(L'EMAILAD)                                                   
         DC    AL2(EMAILAD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(50)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0064)           Desktop extra comments                       
         DC    AL1(L'DSKTXC)                                                    
         DC    AL2(DSKTXC-UPVLITS)                                              
         DC    AL2(UPVVSTRQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(60)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0065)           Telephone                                    
         DC    AL1(L'TELPHN)                                                    
         DC    AL2(TELPHN-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(26)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0066)           Main contact                                 
         DC    AL1(L'MNCONT)                                                    
         DC    AL2(MNCONT-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(50)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0067)           FAX number                                   
         DC    AL1(L'FAXNUM)                                                    
         DC    AL2(FAXNUM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(26)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0068)           Reference number                             
         DC    AL1(L'REFNUM)                                                    
         DC    AL2(REFNUM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0069)           Bank authorisation ID                        
         DC    AL1(L'BNKAID)                                                    
         DC    AL2(BNKAID-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0070)           Entity                                       
         DC    AL1(L'ENTITY)                                                    
         DC    AL2(ENTITY-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0071)           Building Society roll number                 
         DC    AL1(L'BSOCRN)                                                    
         DC    AL2(BSOCRN-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(18)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0072)           Credit card number                           
         DC    AL1(L'CCDNUM)                                                    
         DC    AL2(CCDNUM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(16)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0073)           Online memo                                  
         DC    AL1(L'ONLMEM)                                                    
         DC    AL2(ONLMEM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(54)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0074)           Credit limit external                        
         DC    AL1(L'CRLMEX)                                                    
         DC    AL2(CRLMEX-UPVLITS)                                              
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(13)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0075)           Credit limit internal                        
         DC    AL1(L'CRLMIN)                                                    
         DC    AL2(CRLMIN-UPVLITS)                                              
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(13)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0076)           Fee budget amount                            
         DC    AL1(L'FEEBDGA)                                                   
         DC    AL2(FEEBDGA-UPVLITS)                                             
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(16)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0077)           Discount rate                                
         DC    AL1(L'DSCRAT)                                                    
         DC    AL2(DSCRAT-UPVLITS)                                              
         DC    AL2(UPVAMTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0078)           VAT rate                                     
         DC    AL1(L'VATRAT)                                                    
         DC    AL2(VATRAT-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0079)           Due date formula                             
         DC    AL1(L'DUEDATF)                                                   
         DC    AL2(DUEDATF-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0080)           Job description                              
         DC    AL1(L'JOBDSC)                                                    
         DC    AL2(JOBDSC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(200)                                                         
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL3(04)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0081)           Comment                                      
         DC    AL1(L'COMMNT)                                                    
         DC    AL2(COMMNT-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(50)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL3(03)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0082)           User field code                              
         DC    AL1(L'UFLDCD)                                                    
         DC    AL2(UFLDCD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(02)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(10)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0083)           User field data                              
         DC    AL1(L'UFLDDA)                                                    
         DC    AL2(UFLDDA-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(30)                                                          
         DC    AL1(ARYLSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(10)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0084)           Team role number                             
         DC    AL1(L'TMRNUM)                                                    
         DC    AL2(TMRNUM-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(02)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL3(20)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0085)           Team PID binary                              
         DC    AL1(L'TMPBIN)                                                    
         DC    AL2(TMPBIN-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(ARYELEQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL3(20)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0086)           Team level                                   
         DC    AL1(L'TMLEV)                                                     
         DC    AL2(TMLEV-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(ARYLSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL3(20)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0087)           Client PO number                             
         DC    AL1(L'CLIPON)                                                    
         DC    AL2(CLIPON-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(20)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(12)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0088)           Client PO amount                             
         DC    AL1(L'CLIPOA)                                                    
         DC    AL2(CLIPOA-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(16)                                                          
         DC    AL1(ARYLSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(12)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0089)           Client code                                  
         DC    AL1(L'CLICOD)                                                    
         DC    AL2(CLICOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL3(300)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0090)           Product code                                 
         DC    AL1(L'PROCOD)                                                    
         DC    AL2(PROCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(ARYELEQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL3(300)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0091)           Job code                                     
         DC    AL1(L'JOBCOD)                                                    
         DC    AL2(JOBCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(ARYLSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL3(300)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0092)           Other info                                   
         DC    AL1(L'OTHINF)                                                    
         DC    AL2(OTHINF-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(50)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL3(03)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0093)           Office code (SJ)                             
         DC    AL1(L'OFFCODSJ)                                                  
         DC    AL2(OFFCODSJ-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0094)           Billing group                                
         DC    AL1(L'BILGRP)                                                    
         DC    AL2(BILGRP-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0095)           Print on bill                                
         DC    AL1(L'PRNBIL)                                                    
         DC    AL2(PRNBIL-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(49)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0096)           Debtor account                               
         DC    AL1(L'DEBACC)                                                    
         DC    AL2(DEBACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0097)           Debtor account name                          
         DC    AL1(L'DEBACCN)                                                   
         DC    AL2(DEBACCN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0098)           Debtor account level                         
         DC    AL1(L'DEBACCL)                                                   
         DC    AL2(DEBACCL-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0099)           Costing account                              
         DC    AL1(L'COSACC)                                                    
         DC    AL2(COSACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0100)           Costing account name                         
         DC    AL1(L'COSACCN)                                                   
         DC    AL2(COSACCN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0101)           Costing account level                        
         DC    AL1(L'COSACCL)                                                   
         DC    AL2(COSACCL-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0102)           Bank charges account                         
         DC    AL1(L'BKCACC)                                                    
         DC    AL2(BKCACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0103)           Bank charges account name                    
         DC    AL1(L'BKCACCN)                                                   
         DC    AL2(BKCACCN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0104)           Exchange difference account                  
         DC    AL1(L'EXDACC)                                                    
         DC    AL2(EXDACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0105)           Exchange difference account name             
         DC    AL1(L'EXDACCN)                                                   
         DC    AL2(EXDACCN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0106)           Analysis bank account                        
         DC    AL1(L'ABKACC)                                                    
         DC    AL2(ABKACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0107)           Analysis bank account name                   
         DC    AL1(L'ABKACCN)                                                   
         DC    AL2(ABKACCN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0108)           Cash discount account                        
         DC    AL1(L'CDACC)                                                     
         DC    AL2(CDACC-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0109)           Cash discount account name                   
         DC    AL1(L'CDACCN)                                                    
         DC    AL2(CDACCN-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0110)           Master client                                
         DC    AL1(L'MCLCOD)                                                    
         DC    AL2(MCLCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0111)           Master product                               
         DC    AL1(L'MPRCOD)                                                    
         DC    AL2(MPRCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0112)           Master job                                   
         DC    AL1(L'MJBCOD)                                                    
         DC    AL2(MJBCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0113)           Factoring company account                    
         DC    AL1(L'FACACC)                                                    
         DC    AL2(FACACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0114)           Factoring company account name               
         DC    AL1(L'FACACCN)                                                   
         DC    AL2(FACACCN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0115)           2P T&E account                               
         DC    AL1(L'AC2PTE)                                                    
         DC    AL2(AC2PTE-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0116)           2P T&E account name                          
         DC    AL1(L'AC2PTEN)                                                   
         DC    AL2(AC2PTEN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0117)           2D Dept account                              
         DC    AL1(L'AC2DDP)                                                    
         DC    AL2(AC2DDP-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0118)           2D Dept account name                         
         DC    AL1(L'AC2DDPN)                                                   
         DC    AL2(AC2DDPN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0119)           Override income account                      
         DC    AL1(L'OINACC)                                                    
         DC    AL2(OINACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0120)           Override income account name                 
         DC    AL1(L'OINACCN)                                                   
         DC    AL2(OINACCN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0121)           Override write off account                   
         DC    AL1(L'OWOACC)                                                    
         DC    AL2(OWOACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0122)           Override write account name                  
         DC    AL1(L'OWOACCN)                                                   
         DC    AL2(OWOACCN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0123)           Creditor account binary PID                  
         DC    AL1(L'CRABPID)                                                   
         DC    AL2(CRABPID-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0124)           Expense account                              
         DC    AL1(L'EXPACC)                                                    
         DC    AL2(EXPACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(14)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0125)           Expense account name                         
         DC    AL1(L'EXPACCN)                                                   
         DC    AL2(EXPACCN-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0126)           Bank sort code                               
         DC    AL1(L'BKSTCO)                                                    
         DC    AL2(BKSTCO-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(10)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0127)           Bank account code                            
         DC    AL1(L'BKACCO)                                                    
         DC    AL2(BKACCO-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(10)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0128)           Bank account name                            
         DC    AL1(L'BKACNM)                                                    
         DC    AL2(BKACNM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0129)           Bank name                                    
         DC    AL1(L'BKNAM)                                                     
         DC    AL2(BKNAM-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(36)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0130)           Payment method                               
         DC    AL1(L'PAYMETH)                                                   
         DC    AL2(PAYMETH-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0131)           All account addresses                        
         DC    AL1(L'ALACADR)                                                   
         DC    AL2(ALACADR-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0132)           Expense job                                  
         DC    AL1(L'EXPJOB)                                                    
         DC    AL2(EXPJOB-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0133)           New media code                               
         DC    AL1(L'NMEDCOD)                                                   
         DC    AL2(NMEDCOD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0134)           New product code                             
         DC    AL1(L'NPROCOD)                                                   
         DC    AL2(NPROCOD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0135)           New job code                                 
         DC    AL1(L'NJOBCOD)                                                   
         DC    AL2(NJOBCOD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0136)           Using new upload tool                        
         DC    AL1(L'UNWUPLD)                                                   
         DC    AL2(UNWUPLD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0137)           Creditor ISO country                         
         DC    AL1(L'CRISCTR)                                                   
         DC    AL2(CRISCTR-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0138)           IBAN                                         
         DC    AL1(L'IBAN)                                                      
         DC    AL2(IBAN-UPVLITS)                                                
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(34)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0139)           BIC Code                                     
         DC    AL1(L'BICCODE)                                                   
         DC    AL2(BICCODE-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(11)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0140)           EU SG code                                   
         DC    AL1(L'EUSGCOD)                                                   
         DC    AL2(EUSGCOD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0141)           Non EU SG code                               
         DC    AL1(L'NONEUSG)                                                   
         DC    AL2(NONEUSG-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0142)           Media Accrual Account                        
         DC    AL1(L'MACCACC)                                                   
         DC    AL2(MACCACC-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0143)           Send order to supplier                       
         DC    AL1(L'SORDSUP)                                                   
         DC    AL2(SORDSUP-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0144)           Acknowledge/Query                            
         DC    AL1(L'ACKNQRY)                                                   
         DC    AL2(ACKNQRY-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0145)           Future time account                          
         DC    AL1(L'FUTTIMA)                                                   
         DC    AL2(FUTTIMA-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0146)           VAT region                                   
         DC    AL1(L'VATREG)                                                    
         DC    AL2(VATREG-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0147)           Account filter debtor number                 
         DC    AL1(L'ACDFLN)                                                    
         DC    AL2(ACDFLN-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(05)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0148)           Account filter debtor value                  
         DC    AL1(L'ACDFLV)                                                    
         DC    AL2(ACDFLV-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(ARYLSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(05)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0149)           Account filter costing number                
         DC    AL1(L'ACCFLN)                                                    
         DC    AL2(ACCFLN-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(2)                                                           
         DC    AL3(5)                                                           
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0150)           Account filter costing value                 
         DC    AL1(L'ACCFLV)                                                    
         DC    AL2(ACCFLV-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(ARYLSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(2)                                                           
         DC    AL3(5)                                                           
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0151)           Costing type                                 
         DC    AL1(L'COSTTYP)                                                   
         DC    AL2(COSTTYP-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0152)           Equivalent Type                              
         DC    AL1(L'EQUIVTY)                                                   
         DC    AL2(EQUIVTY-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(05)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0153)           Equivalent Type Code                         
         DC    AL1(L'EQUITYC)                                                   
         DC    AL2(EQUITYC-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(20)                                                          
         DC    AL1(ARYLSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(05)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0154)           SR Equivalent Type                           
         DC    AL1(L'SREQUTY)                                                   
         DC    AL2(SREQUTY-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(ARYFSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(05)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0155)           SR Equivalent Type Code                      
         DC    AL1(L'SREQUTYC)                                                  
         DC    AL2(SREQUTYC-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(20)                                                          
         DC    AL1(ARYLSTQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL3(05)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0156)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0157)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0158)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0159)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0160)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0161)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0162)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0163)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0164)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0165)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0166)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0167)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0168)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0169)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0170)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0171)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0172)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0173)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0174)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0175)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0176)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0177)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0178)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0179)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0180)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0181)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0182)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0183)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0184)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0185)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0186)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0187)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0188)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0189)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0190)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0191)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0192)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0193)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0194)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0195)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0196)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0197)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0198)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0199)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0200)           Job Available To Third Party                 
         DC    AL1(L'JOBTHPTY)                                                  
         DC    AL2(JOBTHPTY-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0201)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0202)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0203)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0204)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0205)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0206)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0207)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0208)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0209)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0210)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0211)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0212)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0213)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0214)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0215)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0216)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0217)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0218)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0219)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0220)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0221)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0222)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0223)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0224)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0225)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0226)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0227)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0228)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0229)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0230)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0231)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0232)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0233)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0234)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0235)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0236)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0237)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0238)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0239)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0240)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0241)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0242)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0243)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0244)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0245)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0246)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0247)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0248)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0249)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0250)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0251)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0252)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0253)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0254)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0255)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0256)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0257)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0258)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0259)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0260)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0261)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0262)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0263)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0264)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0265)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0266)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0267)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0268)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0269)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0270)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0271)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0272)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0273)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0274)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0275)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0276)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0277)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0278)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0279)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0280)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0281)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0282)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0283)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0284)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0285)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0286)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0287)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0288)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0289)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0290)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0291)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0292)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0293)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0294)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0295)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0296)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0297)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0298)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0299)           N/D                                          
         DC    AL1(L'NODEF)                                                     
         DC    AL2(NODEF-UPVLITS)                                               
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0300)           Media 1                                      
         DC    AL1(L'MEDIA1)                                                    
         DC    AL2(MEDIA1-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0301)           Media 2                                      
         DC    AL1(L'MEDIA2)                                                    
         DC    AL2(MEDIA2-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0302)           Media 3                                      
         DC    AL1(L'MEDIA3)                                                    
         DC    AL2(MEDIA3-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0303)           Media 4                                      
         DC    AL1(L'MEDIA4)                                                    
         DC    AL2(MEDIA4-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0304)           Media 5                                      
         DC    AL1(L'MEDIA5)                                                    
         DC    AL2(MEDIA5-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0305)           Media 6                                      
         DC    AL1(L'MEDIA6)                                                    
         DC    AL2(MEDIA6-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0306)           Media 7                                      
         DC    AL1(L'MEDIA7)                                                    
         DC    AL2(MEDIA7-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0307)           Media 8                                      
         DC    AL1(L'MEDIA8)                                                    
         DC    AL2(MEDIA8-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0308)           Media 9                                      
         DC    AL1(L'MEDIA9)                                                    
         DC    AL2(MEDIA9-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0309)           Media 10                                     
         DC    AL1(L'MEDIA10)                                                   
         DC    AL2(MEDIA10-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0310)           Media 11                                     
         DC    AL1(L'MEDIA11)                                                   
         DC    AL2(MEDIA11-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0311)           Media 12                                     
         DC    AL1(L'MEDIA12)                                                   
         DC    AL2(MEDIA12-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0312)           Media 13                                     
         DC    AL1(L'MEDIA13)                                                   
         DC    AL2(MEDIA13-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0313)           Media 14                                     
         DC    AL1(L'MEDIA14)                                                   
         DC    AL2(MEDIA14-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0314)           Media 15                                     
         DC    AL1(L'MEDIA15)                                                   
         DC    AL2(MEDIA15-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0315)           Media 16                                     
         DC    AL1(L'MEDIA16)                                                   
         DC    AL2(MEDIA16-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0316)           Media Billing Group                          
         DC    AL1(L'MEDBGRP)                                                   
         DC    AL2(MEDBGRP-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0317)           Media Address 1                              
         DC    AL1(L'MEDADD1)                                                   
         DC    AL2(MEDADD1-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(30)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0318)           Media Address 2                              
         DC    AL1(L'MEDADD2)                                                   
         DC    AL2(MEDADD2-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(30)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0319)           Media Address 3                              
         DC    AL1(L'MEDADD3)                                                   
         DC    AL2(MEDADD3-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(30)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0320)           Media Address 4                              
         DC    AL1(L'MEDADD4)                                                   
         DC    AL2(MEDADD4-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(30)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0321)           Media Commission Rate                        
         DC    AL1(L'MEDCRAT)                                                   
         DC    AL2(MEDCRAT-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(62)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0322)           Media VAT Type                               
         DC    AL1(L'MEDVTYP)                                                   
         DC    AL2(MEDVTYP-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(02)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0323)           Media Due Date Formula                       
         DC    AL1(L'MEDDDF)                                                    
         DC    AL2(MEDDDF-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(15)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0324)           Buying Agency                                
         DC    AL1(L'BUYAGY)                                                    
         DC    AL2(BUYAGY-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0325)           Creative agency                              
         DC    AL1(L'CREAGY)                                                    
         DC    AL2(CREAGY-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0326)           Limit Access 1                               
         DC    AL1(L'LIMACC1)                                                   
         DC    AL2(LIMACC1-UPVLITS)                                             
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0327)           Limit Access 2                               
         DC    AL1(L'LIMACC2)                                                   
         DC    AL2(LIMACC2-UPVLITS)                                             
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0328)           Media Filters                                
         DC    AL1(L'MEDFILT)                                                   
         DC    AL2(MEDFILT-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(09)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(YESQ)                                                        
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0329)           Prevent New Bookings                         
         DC    AL1(L'PREVNBO)                                                   
         DC    AL2(PREVNBO-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0330)           Add Campaign Ref Media Systems               
         DC    AL1(L'ADDCRMS)                                                   
         DC    AL2(ADDCRMS-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0331)           Billing Comments Compulsory                  
         DC    AL1(L'BILLCCM)                                                   
         DC    AL2(BILLCCM-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0332)           Order Comments Compulsory                    
         DC    AL1(L'ORDCCM)                                                    
         DC    AL2(ORDCCM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0333)           Generate ME1B When Booking                   
         DC    AL1(L'GENM1BB)                                                   
         DC    AL2(GENM1BB-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0334)           Formula Record Code                          
         DC    AL1(L'FORMRCC)                                                   
         DC    AL2(FORMRCC-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(10)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0335)           Max Amount For Single Media Bill             
         DC    AL1(L'MAXSNMB)                                                   
         DC    AL2(MAXSNMB-UPVLITS)                                             
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(07)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0336)           Surcharge % To Add To Media Bill             
         DC    AL1(L'SURPMB)                                                    
         DC    AL2(SURPMB-UPVLITS)                                              
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(06)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0337)           Commission Account                           
         DC    AL1(L'COMACC)                                                    
         DC    AL2(COMACC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0338)           Budget Currency                              
         DC    AL1(L'BUDCURR)                                                   
         DC    AL2(BUDCURR-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(03)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0339)           Billing Currency                             
         DC    AL1(L'BILLCUR)                                                   
         DC    AL2(BILLCUR-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0340)           Outlet Scheme Code                           
         DC    AL1(L'OUTSCHC)                                                   
         DC    AL2(OUTSCHC-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0341)           Outlet Scheme Budget 1                       
         DC    AL1(L'OUTSCHB1)                                                  
         DC    AL2(OUTSCHB1-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0342)           Outlet Scheme Budget 2                       
         DC    AL1(L'OUTSCHB2)                                                  
         DC    AL2(OUTSCHB2-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0343)           Outlet Scheme Budget 3                       
         DC    AL1(L'OUTSCHB3)                                                  
         DC    AL2(OUTSCHB3-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0344)           Outlet Scheme Budget 4                       
         DC    AL1(L'OUTSCHB4)                                                  
         DC    AL2(OUTSCHB4-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0345)           Outlet Scheme Budget 5                       
         DC    AL1(L'OUTSCHB5)                                                  
         DC    AL2(OUTSCHB5-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0346)           Outlet Scheme Budget 6                       
         DC    AL1(L'OUTSCHB6)                                                  
         DC    AL2(OUTSCHB6-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0347)           Product Printable Character                  
         DC    AL1(L'PRODPC)                                                    
         DC    AL2(PRODPC-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0348)           Media Comments                               
         DC    AL1(L'MEDIACOM)                                                  
         DC    AL2(MEDIACOM-UPVLITS)                                            
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(LWRCASEQ)                                                    
         DC    AL3(50)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0349)           Cash Discount Basis                          
         DC    AL1(L'CASHDB)                                                    
         DC    AL2(CASHDB-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0350)           Cash Discount %                              
         DC    AL1(L'CASHDP)                                                    
         DC    AL2(CASHDP-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(08)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0351)           Billing=commission option                    
         DC    AL1(L'BILLCOM)                                                   
         DC    AL2(BILLCOM-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0352)           Media contact name                           
         DC    AL1(L'MMNCONT)                                                   
         DC    AL2(MMNCONT-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(30)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0353)           Media telephone no.                          
         DC    AL1(L'TELPHN)                                                    
         DC    AL2(TELPHN-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(20)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0354)           Media fax no.                                
         DC    AL1(L'FAXNUM)                                                    
         DC    AL2(FAXNUM-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(20)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
         DC    AL1(NOQ)                                                         
*                                                                               
* Return values                                                                 
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0001)           PC Token                                     
         DC    AL1(L'PCTOKEN)                                                   
         DC    AL2(PCTOKEN-UPVLITS)                                             
         DC    AL2(UPVNUMQ)                                                     
         DC    AL1(NOQ)                                                         
         DC    AL3(04)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0002)           Account Code                                 
         DC    AL1(L'ACCCOD)                                                    
         DC    AL2(ACCCOD-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(12)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0003)           Status                                       
         DC    AL1(L'APPSTS)                                                    
         DC    AL2(APPSTS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(01)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0004)           Errors                                       
         DC    AL1(L'ERRORS)                                                    
         DC    AL2(ERRORS-UPVLITS)                                              
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(255)                                                         
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(YESQ)                                                        
*                                                                               
         DC    AL1(UPVTDELQ)                                                    
         DC    AL1(UPVTDLNQ)                                                    
         DC    AL3(0005)           Creator email address                        
         DC    AL1(L'EMAILAD)                                                   
         DC    AL2(EMAILAD-UPVLITS)                                             
         DC    AL2(UPVCHARQ)                                                    
         DC    AL1(NOQ)                                                         
         DC    AL3(52)                                                          
         DC    AL1(NULQ)                                                        
         DC    AL1(NOQ)                                                         
         DC    AL3(00)                                                          
         DC    AL3(00)                                                          
         DC    AL1(NOQ)                                                         
         DC    AL1(YESQ)                                                        
         DC    AL1(NOQ)                                                         
*                                                                               
         DC    X'0000'             End of this table                            
*                                                                               
UPVACCLQ EQU   *-UPVACC                                                         
*                                                                               
         DC    X'0000'             End of tables                                
         EJECT                                                                  
***********************************************************************         
* GLOBAL LITERALS                                                     *         
***********************************************************************         
         SPACE 1                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
MAPTAB   DS    0XL4                REQUEST MAP TABLE                            
                                                                                
MAPDISP  DC    AL2(A#FUMDL)        'File uploads available' Download            
         DC    AL1(0,0)                                                         
                                                                                
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
BITLIST  DC    X'8040201008040201'                                              
EZEROS   DC    C'0000000000'                                                    
PZERO    DC    P'0'                                                             
LARE     DC    X'41E0'                                                          
LAREADDR DC    S(0)                                                             
         EJECT                                                                  
                                                                                
ASTERS   DC    16C'*'                                                           
FFS      DC    16X'FF'                                                          
                                                                                
         EJECT                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
                                                                                
ALLOWCHR DC    256XL1'00'          ALLOWABLE PROGRAM CODE CHARACTERS            
         ORG   ALLOWCHR+C'a'                                                    
         DC    C'abcdefghi'                                                     
         ORG   ALLOWCHR+C'j'                                                    
         DC    C'jklmnopqr'                                                     
         ORG   ALLOWCHR+C's'                                                    
         DC    C'stuvwxyz'                                                      
         ORG   ALLOWCHR+C'A'                                                    
         DC    C'ABCDEFGHI'                                                     
         ORG   ALLOWCHR+C'J'                                                    
         DC    C'JKLMNOPQR'                                                     
         ORG   ALLOWCHR+C'S'                                                    
         DC    C'STUVWXYZ'                                                      
         ORG   ALLOWCHR+C'0'                                                    
         DC    C'0123456789'                                                    
         ORG                                                                    
TST#LIT  DC    C'TestTestTest'                                                  
SECPROG  DC    X'06F1'                                                          
*                                                                               
DCDICTL  DS    0X                                                               
         DCDDL AC#HSUPL,L'AC@HSUPL                                              
         DCDDL AC#PSUPL,L'AC@PSUPL                                              
         DCDDL AC#ACUPL,L'AC@ACUPL                                              
DCDICTLX DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*BLOCK*****************************************************************         
* storage area equ's. numbers are indexes into list of storage block            
* addresses. Addresses point at a series of 15 1K blocks (hence 2               
* assigned for an IO area).                              ???                    
***********************************************************************         
B#UPVTAB EQU   3                   Upload map values table                      
B#BUFREC EQU   9                                                                
B#SVRDEF EQU   12                      - SERVER DEFINITION                      
ASERVER  EQU   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)                                 
B#LP_D   EQU   13                      - LP_D                                   
                                                                                
EOR      EQU   0                                                                
         EJECT                                                                  
***********************************************************************         
* Literal Definitions                                                 *         
***********************************************************************         
SVRDEF   CSECT                                                                  
UPVLITS  DS    0X                  Table literals in alphabetical order         
ACCLOAD  DC    C'Account Upload'                                                
ACCADD   DC    C'Account Address'                                               
ACCCOD   DC    C'Account Code'                                                  
ACCCUR   DC    C'Account Currency'                                              
ACCFLN   DC    C'Account Costing Filter Number'                                 
ACCFLV   DC    C'Account Costing Filter Value'                                  
ACDFLN   DC    C'Account Debtors Filter Number'                                 
ACDFLV   DC    C'Account Debtors Filter Value'                                  
ACKNQRY  DC    C'Acknowledge/Query'                                             
ADDCRMS  DC    C'Add Campaign Ref Media Systems'                                
ALACADR  DC    C'All Account Addresses'                                         
AMOUNT   DC    C'Amount'                                                        
ABKACC   DC    C'Analysis Bank Account'                                         
ABKACCN  DC    C'Analysis Bank Account Name'                                    
APPCMT   DC    C'Approval Comments'                                             
APPSTS   DC    C'Approval Status'                                               
BALSHT   DC    C'Balance Sheet'                                                 
BNKACOD  DC    C'Bank Account'                                                  
BKACCO   DC    C'Bank Account Code'                                             
BKACNM   DC    C'Bank Account Name'                                             
BNKAID   DC    C'Bank Authorisation ID'                                         
BKCACC   DC    C'Bank Charges Account'                                          
BKCACCN  DC    C'Bank Charges Account Name'                                     
BKNAM    DC    C'Bank Name'                                                     
BKSTCO   DC    C'Bank Sort Code'                                                
BATCMT   DC    C'Batch comments'                                                
BATMOA   DC    C'Batch MOA'                                                     
BATNAM   DC    C'Batch name'                                                    
BATREF   DC    C'Batch reference'                                               
BATTYP   DC    C'Batch type'                                                    
BICCODE  DC    C'BIC Code'                                                      
BILLCOM  DC    C'Billing=commission option'                                     
BILLCCM  DC    C'Billing Comments Compulsory'                                   
BILLCUR  DC    C'Billing Currency'                                              
BILGRP   DC    C'Billing Group'                                                 
BROEJOB  DC    C'BrandOcean Estimate Job'                                       
BSOCRN   DC    C'Building Society Roll Number'                                  
BUDCURR  DC    C'Budget Currency'                                               
BUYAGY   DC    C'Buying Agency'                                                 
CASHDB   DC    C'Cash Discount Basis'                                           
CASHDP   DC    C'Cash Discount %'                                               
CREAGY   DC    C'Creative Agency'                                               
CDACC    DC    C'Cash Discount Account'                                         
CDACCN   DC    C'Cash Discount Account Name'                                    
CHQDAT   DC    C'Cheque Date'                                                   
CHQNUM   DC    C'Cheque Number'                                                 
CLIANL   DC    C'Client Analysis'                                               
CLICOD   DC    C'Client Code'                                                   
CLICST   DC    C'Client Cost'                                                   
CLIPON   DC    C'Client PO Number'                                              
CLIPOA   DC    C'Client PO Amount'                                              
CLODAT   DC    C'Close Date'                                                    
CLOSTS   DC    C'Closed Status'                                                 
COMACC   DC    C'Commission Account'                                            
COMMNT   DC    C'Comment'                                                       
COSACC   DC    C'Costing Account'                                               
COSACCL  DC    C'Costing Account Level'                                         
COSACCN  DC    C'Costing Account Name'                                          
COSTTYP  DC    C'Costing Type'                                                  
CRACUL   DC    C'Credit Account including unit/ledger'                          
CCDNUM   DC    C'Credit Card Number'                                            
CRLMEX   DC    C'Credit Limit External'                                         
CRLMIN   DC    C'Credit Limit Internal'                                         
CRABPID  DC    C'Creditor Account Binary PID'                                   
CRISCTR  DC    C'Creditor ISO Country'                                          
DATE     DC    C'Date'                                                          
DATESAL  DC    C'Date of salary'                                                
DRACUL   DC    C'Debit Account including unit/ledger'                           
DEBACC   DC    C'Debtor Account'                                                
DEBACCL  DC    C'Debtor Account Level'                                          
DEBACCN  DC    C'Debtor Account Name'                                           
DEFWC    DC    C'Default Workcode'                                              
DEPANL   DC    C'Departmental Analysis'                                         
DSKTXC   DC    C'Desktop Extra Comments'                                        
DSKTPR   DC    C'Desktop Priority'                                              
DSCACOD  DC    C'Discount Account'                                              
DSCRAT   DC    C'Discount Rate'                                                 
DUEDAT   DC    C'Due Date'                                                      
DUEDATF  DC    C'Due Date Formula'                                              
EMAILAD  DC    C'Email Address'                                                 
ENTITY   DC    C'Entity'                                                        
EQUITYC  DC    C'Equivalent Type Code'                                          
EQUIVTY  DC    C'Equivalent Type'                                               
ERRORS   DC    C'Errors'                                                        
ESTNUM   DC    C'Estimate Number'                                               
EUSGCOD  DC    C'EU SG code'                                                    
EXDACC   DC    C'Exchange Difference Account'                                   
EXDACCN  DC    C'Exchange Difference Account Name'                              
EXCHRAT  DC    C'Exchange Rate'                                                 
EXFRTIM  DC    C'Exclude from Timesheets'                                       
EXPACC   DC    C'Expense Account'                                               
EXPACCN  DC    C'Expense Account Name'                                          
EXPJOB   DC    C'Expense Job'                                                   
FACACC   DC    C'Factoring Company Account'                                     
FACACCN  DC    C'Factoring Company Account Name'                                
FAXNUM   DC    C'FAX Number'                                                    
FEEBDGA  DC    C'Fee Budget Amount'                                             
FJOBTIM  DC    C'Force Job on Timesheet'                                        
FLTNUM   DC    C'Filter Number'                                                 
FLTVAL   DC    C'Filter Value'                                                  
FORCURC  DC    C'Foreign Currency Code'                                         
FORCURA  DC    C'Foreign Currency Amount'                                       
FORMRCC  DC    C'Formula Record Code'                                           
FORLNGU  DC    C'Foreign Language User'                                         
FORNAM   DC    C'Foreign Name'                                                  
FPROTIM  DC    C'Force Product on Timesheet'                                    
FUTTIMA  DC    C'Future Time Account'                                           
GENM1BB  DC    C'Generate ME1B When Booking'                                    
GLOFF    DC    C'General Ledger Office'                                         
GLACOD   DC    C'General Ledger Account Code'                                   
HISSALH  DC    C'History/Salary Header Upload'                                  
HISSALE  DC    C'History/Salary Element Upload'                                 
HISSALT  DC    C'History/Salary Trailer Upload'                                 
IBAN     DC    C'IBAN'                                                          
INPVAC   DC    C'Input VAT Account'                                             
INTREF   DC    C'Internal Reference'                                            
INVREG   DC    C'Invoice Register'                                              
ITEMNO   DC    C'Item number'                                                   
JOBANL   DC    C'Job Analysis'                                                  
JOBCOD   DC    C'Job Code'                                                      
JOBDSC   DC    C'Job Description'                                               
JOBLADJ  DC    C'Job Locks Adjustments'                                         
JOBLBIL  DC    C'Job Locks Billing'                                             
JOBLEST  DC    C'Job Locks Estimte'                                             
JOBLEXT  DC    C'Job Locks Externals'                                           
JOBLORD  DC    C'Job Locks Orders'                                              
JOBLTIM  DC    C'Job Locks Timesheets'                                          
JOBMJOB  DC    C'Job is Master Job'                                             
JOBTHPTY DC    C'Job Available To Third Party'                                  
KSVBLB   DC    C'KSV Billable'                                                  
KSVRAT   DC    C'KSV Rate'                                                      
KSVTYP   DC    C'KSV Type'                                                      
LEDGER   DC    C'Ledger'                                                        
LIMACC1  DC    C'Limit Access 1'                                                
LIMACC2  DC    C'Limit Access 2'                                                
LOCCUR   DC    C'Local Currency'                                                
LCKSTS   DC    C'Locked Status'                                                 
LNGNAM   DC    C'Long Name'                                                     
MACCACC  DC    C'Media Accrual Account'                                         
MAXSNMB  DC    C'Maximum Amount For Single Media Bill'                          
MCLCOD   DC    C'Master Client'                                                 
MDSNAM   DC    C'Media Short Name'                                              
MEDADD1  DC    C'Media Address 1'                                               
MEDADD2  DC    C'Media Address 2'                                               
MEDADD3  DC    C'Media Address 3'                                               
MEDADD4  DC    C'Media Address 4'                                               
MEDBGRP  DC    C'Media Billing Group'                                           
MEDCRAT  DC    C'Media Commission Rate'                                         
MEDDDF   DC    C'Media Due Date Formula'                                        
MEDIA1   DC    C'Media 1'                                                       
MEDIA2   DC    C'Media 2'                                                       
MEDIA3   DC    C'Media 3'                                                       
MEDIA4   DC    C'Media 4'                                                       
MEDIA5   DC    C'Media 5'                                                       
MEDIA6   DC    C'Media 6'                                                       
MEDIA7   DC    C'Media 7'                                                       
MEDIA8   DC    C'Media 8'                                                       
MEDIA9   DC    C'Media 9'                                                       
MEDIA10  DC    C'Media 10'                                                      
MEDIA11  DC    C'Media 11'                                                      
MEDIA12  DC    C'Media 12'                                                      
MEDIA13  DC    C'Media 13'                                                      
MEDIA14  DC    C'Media 14'                                                      
MEDIA15  DC    C'Media 15'                                                      
MEDIA16  DC    C'Media 16'                                                      
MEDIACOM DC    C'Media Comments'                                                
MEDFILT  DC    C'Media Filters'                                                 
MEDVTYP  DC    C'Media VAT Type'                                                
METHOD   DC    C'Method'                                                        
MILES    DC    C'Miles'                                                         
MILANL   DC    C'Miles Analysis'                                                
MJBCOD   DC    C'Master Job'                                                    
MNCONT   DC    C'Main Contact'                                                  
MMNCONT  DC    C'Media Main Contact'                                            
MOA      DC    C'Month of Activity'                                             
MPRCOD   DC    C'Master Product'                                                
MRGINVS  DC    C'Merge Invoices'                                                
NAME     DC    C'Name'                                                          
NARRTV   DC    C'Narrative'                                                     
NETOGRS  DC    C'Net or Gross'                                                  
NICSTSP  DC    C'NIC Status Profile'                                            
NJOBCOD  DC    C'New Job Code'                                                  
NMEDCOD  DC    C'New Media Code'                                                
NPROCOD  DC    C'New Product Code'                                              
NONEUSG  DC    C'Non EU SG Code'                                                
OFFICE   DC    C'Office'                                                        
OFFCODSJ DC    C'Office Code (SJ)'                                              
ONLMEM   DC    C'Online Memo'                                                   
OPNDAT   DC    C'Open Date'                                                     
ORDDAT   DC    C'Order Date'                                                    
ORDNUM   DC    C'Order Number'                                                  
ORDCCM   DC    C'Order Comments Compulsory'                                     
OTHINF   DC    C'Other Info'                                                    
OINACC   DC    C'Override Income Account'                                       
OINACCN  DC    C'Override Income Account Name'                                  
OUTSCHC  DC    C'Outlet Scheme Code'                                            
OUTSCHB1 DC    C'Outlet Scheme Budget 1'                                        
OUTSCHB2 DC    C'Outlet Scheme Budget 2'                                        
OUTSCHB3 DC    C'Outlet Scheme Budget 3'                                        
OUTSCHB4 DC    C'Outlet Scheme Budget 4'                                        
OUTSCHB5 DC    C'Outlet Scheme Budget 5'                                        
OUTSCHB6 DC    C'Outlet Scheme Budget 6'                                        
OVNMTS   DC    C'Override Name for Timesheets'                                  
OWOACC   DC    C'Override Write Off Account'                                    
OWOACCN  DC    C'Override Write Off Account Name'                               
PAYLCKD  DC    C'Payee Locked'                                                  
PAYMETH  DC    C'Payment Method'                                                
PCTOKEN  DC    C'PC/Web Token'                                                  
PCRECCR  DC    C'Peel/Close Reconciled Credits'                                 
PCRECDR  DC    C'Peel/Close Reconciled Debits'                                  
PEREXC   DC    C'Person is Executive'                                           
POSHUP   DC    C'Posting Header Upload'                                         
POSINO   DC    C'Posting Item Number'                                           
POSIUP   DC    C'Posting Item Upload'                                           
POSWUP   DC    C'Posting Workcode Upload'                                       
POSTUP   DC    C'Posting Trailer Upload'                                        
PREVNBO  DC    C'Prevent New Bookings'                                          
PRNBIL   DC    C'Print On Bill'                                                 
PROCOD   DC    C'Product Code'                                                  
PRODPC   DC    C'Product Printable Character'                                   
PRFLSS   DC    C'Profit Loss'                                                   
PRJCON   DC    C'Project Control'                                               
PRVVEN   DC    C'Provisional Vendor'                                            
REFNCE   DC    C'Reference'                                                     
REFNUM   DC    C'Reference Number'                                              
REQTYP   DC    C'Request Type'                                                  
SALPCON  DC    C'Salary Payroll code'                                           
SECLVL   DC    C'Security Level'                                                
SORDSUP  DC    C'Send Order To Supplier'                                        
SREQUTY  DC    C'SR Equivalent Type'                                            
SREQUTYC DC    C'SR Equivalent Type Code'                                       
STFANL   DC    C'Staff Analysis'                                                
SUNCRD   DC    C'Sundry Creditor'                                               
SURPMB   DC    C'Surcharge % To Add To Media Bill'                              
TAXREGN  DC    C'Tax/VAT Registration Number'                                   
TELPHN   DC    C'Telephone'                                                     
TMRNUM   DC    C'Team Role Number'                                              
TMPBIN   DC    C'Team PID Binary'                                               
TMLEV    DC    C'Team Level'                                                    
UNIT     DC    C'Unit'                                                          
UFLDCD   DC    C'User Field Code'                                               
UFLDDA   DC    C'User Field Data'                                               
UNWUPLD  DC    C'Using new upload tool'                                         
VATACOD  DC    C'VAT Account Code'                                              
VATCOD   DC    C'VAT Code'                                                      
VATC13B  DC    C'VAT Code 13B'                                                  
VATNUM   DC    C'VAT Number'                                                    
VATRAT   DC    C'VAT Rate'                                                      
VATREG   DC    C'VAT Region'                                                    
WCSEQNO  DC    C'Workcode sequence number'                                      
WORKCOD  DC    C'Workcode'                                                      
ACCOD1R  DC    C'1R Account code'                                               
ACCOD2D  DC    C'2D Account (Department)'                                       
AC2DDP   DC    C'2D Dept Account'                                               
AC2DDPN  DC    C'2D Dept Account Name'                                          
ACCOD2P  DC    C'2P Account (Person)'                                           
AC2PTE   DC    C'2P T&&E Account'                                               
AC2PTEN  DC    C'2P T&&E Account Name'                                          
NODEF    DC    C'N/D'                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST LITERALS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         EJECT                                                                  
SAVED    DSECT                                                                  
LDUB1    DS    D                                                                
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
AUPVTCUR DS    A                   A(current UPVTAB table)                      
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
MAPI1    DS    X                   ** MAP INDICATOR BYTE1 1 **                  
MAPI2    DS    X                   ** MAP INDICATOR BYTE1 2 **                  
MFILUPS  EQU   X'41'               FILE UPLOADS                                 
UPVLITL  DS    XL1                 L'TABLE LITERAL                              
LBYTE1   DS    XL1                                                              
                                                                                
DSDICTL  DS    0C                                                               
AC@HSUPL DS    CL30                                                             
AC@PSUPL DS    CL16                                                             
AC@ACUPL DS    CL14                                                             
                                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
***********************************************************************         
                                                                                
QVALUES  DS    0XL256              ** REQUEST VALUES **                         
QDUMMY   DS    CL1                 Dummy request value                          
         ORG   QVALUES+L'QVALUES                                                
QVALUESL EQU   *-QVALUES                                                        
                                                                                
         DS    0F                                                               
DVALUES  DS    0X                  ** Derived values **                         
                                                                                
DVALUESL EQU   *-DVALUES                                                        
                                                                                
AGENCY   DS    XL(L'CUXCPY)        Company code                                 
USRID    DS    XL(L'CUUSER)        Connected user id                            
*                                                                               
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
AALL     DS    AL3                 All value WMP entry                          
ANZR     DS    AL3                 Non-zero value WMP entry                     
*                                                                               
*                                                                               
OVALUES  DS    0D                  ** OUTPUT VALUES **                          
                                                                                
OVALUESL EQU   *-OVALUES                                                        
                                                                                
EX_BUFF  DS    XL4096              Buffer area                                  
                                                                                
***********************************************************************         
* Upload tables DSECTS                                                *         
***********************************************************************         
*                                                                               
UPVTHDD  DSECT                     Header                                       
UPVTLEN  DS    XL2                 Table length                                 
UPVTHRC  DS    XL1                 Record                                       
UPVTHRA  DS    XL1                 Record action                                
UPVTHUC  DS    CL4                 Upload code                                  
UPVTHFL  DS    XL1                 L'Upload name                                
UPVTHFN  DS    XL2                 Disp to Upload name                          
UPVTHAS  DS    CL4                 Associated with Upload code                  
UPVTHAL  DS    XL1                 L'Associated Upload name                     
UPVTHAN  DS    XL2                 Disp to Associated Upload name               
UPVTHLV  DS    XL2                 Level of this upload request                 
UPVTHET  DS    CL1                 Add empty trailer record to upload           
UPVTHTN  DS    CL4                 Trailer record number                        
UPVTHDL  EQU   *-UPVTHDD                                                        
UPVTDFST EQU   *                                                                
*                                                                               
UPVTDAD  DSECT                     Field data                                   
UPVTDEL  DS    XL1                 Data element code                            
UPVTDELQ EQU   X'F0'                                                            
UPVTDLN  DS    XL1                 Table entry length                           
UPVTDFI  DS    XL3                 Upload Field id                              
UPVTDFL  DS    XL1                 L'Upload field name                          
UPVTDFN  DS    XL2                 Disp to Upload Field name                    
UPVTDDT  DS    XL2                 Upload Field data type                       
UPVCHARQ EQU   X'0001'             Character                                    
UPVDATEQ EQU   X'0002'             Date                                         
UPVNUMQ  EQU   X'0003'             Number                                       
UPVAMTQ  EQU   X'0004'             Amount                                       
UPVVSTRQ EQU   X'0005'             VString                                      
UPVTDLC  DS    CL1                 Lowercase Y/N                                
LWRCASEQ EQU   YESQ                                                             
UPVTDML  DS    XL3                 Upload field max length                      
UPVTDAR  DS    CL1                 Upload field array                           
ARYFSTQ  EQU   C'F'                First array element                          
ARYELEQ  EQU   C'E'                array Element                                
ARYLSTQ  EQU   C'L'                Last array element                           
NULQ     EQU   C' '                (not part of an array)                       
UPVTDLI  DS    CL1                 List Y/N                                     
UPVTDEN  DS    XL3                 Array N'entries                              
UPVTDMR  DS    XL3                 Array max N'entries                          
UPVTDPT  DS    CL1                 Upload field is PC token Y/N                 
UPVTDRT  DS    CL1                 Return field Y/N                             
UPVTDER  DS    CL1                 Error field Y/N                              
UPVTDLNQ EQU   *-UPVTDAD           Default entry length                         
*                                                                               
***********************************************************************         
* Optimisation buffer record layout                                   *         
***********************************************************************         
         SPACE 1                                                                
OB_D     DSECT                                                                  
                                                                                
OB_KEY   DS    XL64                Record key                                   
                                                                                
OB_ERROR DS    XL2                 Error number (Zero=OK)                       
                                                                                
OB_DATA  DS    0X                  Data starts here                             
OB_NAME  DS    CL(L'NAMEREC)       Record name                                  
OB_OTHER DS    0X                  Other data                                   
         ORG   OB_D+256                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
         EJECT                                                                  
***********************************************************************         
* included books                                                      *         
***********************************************************************         
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
ACTRECD  DSECT                                                                  
         ORG   ACTKEY+ACTKEND                                                   
ACTKREM  DS    XL(L'ACTKEY-(*-ACTKEY))                                          
                                                                                
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
* DDCONBLK                                                                      
         PRINT OFF                                                              
CONBLKD  DSECT                                                                  
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACBRA1C   04/24/15'                                      
         END                                                                    
