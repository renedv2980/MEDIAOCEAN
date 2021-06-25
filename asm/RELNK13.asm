*          DATA SET RELNK13    AT LEVEL 006 AS OF 10/30/14                      
*PHASE T82B13A                                                                  
RELNK13  TITLE '- Rep system download - Makegoods'                              
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,REQUEST=*,CODE=CODE,SYSTEM=REPSYSQ,              +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,                    +        
               B#COV,RCOVRECD,B#CMT,RCMTRECD,B#AGY,RAGYRECD,           +        
               B#STA,RSTARECD,B#CON,RCONRECD,B#BUY,RBUYRECD,           +        
               B#MGC,RMKGDCMD,B#MKG,RMKGRECD)                                   
                                                                                
CODE     NMOD1 0,**RL13**,RR=RE                                                 
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(global literals)                        
         USING LP_D,R1                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         L     R9,LP_ABLK1         ROOT PASSES A(WORKD) IN BLOCK1               
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2         ROOT PASSES A(SAVED) IN BLOCK2               
         USING SAVED,R8            R8=A(SAVED W/S)                              
                                                                                
         ST    R1,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Initialing for running                                              *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   First run?                                   
         JNE   RUNREQ                                                           
         MVC   LP_BLKS+((B#MKG-1)*L'LP_BLKS),AIO5                               
         MVC   LP_BLKS+((B#BUY-1)*L'LP_BLKS),AIO6                               
         MVC   AGY,LP_AGY          Set agency code                              
                                                                                
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RREPKEY,RE          Look up master Rep code                      
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGY                                                     
         DROP  RE                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         CLC   IOKEY(L'RREPKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                No Rep record                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO1'                           
         L     RE,AIO1                                                          
         LA    RE,RREPELEM-RREPREC(RE)                                          
         CLI   0(RE),X'01'                                                      
         JE    *+6                                                              
         DC    H'0'                No Rep element                               
                                                                                
         USING RREPELEM,RE                                                      
         MVC   MASTRCOD,AGY        Init master Rep code                         
         CLC   RREPMAST,SPACES     Have master control?                         
         JNH   *+20                                                             
         CLC   RREPMAST,=X'FFFF'   Master control?                              
         JE    *+10                                                             
         MVC   MASTRCOD,RREPMAST   Set master Rep code                          
         DROP  RE                                                               
                                                                                
         MVI   WKMGHSW,NOQ         Init Makegood header data switch             
         MVI   WKMGOSW,NOQ         Init Makegood offer data switch              
         MVI   WKSUMSW,NOQ         Init Makegood summary data switch            
         MVC   CON#CHAR,SPACES     Init char format contract number             
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Run a download request                                              *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   Run request mode?                            
         JNE   EXITY                                                            
                                                                                
         L     R1,ALP                                                           
         GOTOR LP_APUTO                                                         
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NOMORE   MVI   LP_RMODE,LP_RLAST   Set no more data                             
         J     EXITY                                                            
*                                                                               
XCOLEN   XC    LP_OLEN,LP_OLEN     Clear output field length & exit             
         J     EXITY                                                            
*                                                                               
SETOLENX STCM  R0,15,LP_OLEN       Set output field length & exit               
         J     EXITY                                                            
*                                                                               
EXITN    LA    RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
*                                                                               
NXTELEM  SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLC   ELCODE,0(R3)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R3),0                                                          
         JNE   NXTELEM                                                          
         LTR   R3,R3               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
EDHHMMSS UNPK  FULL1(3),0(2,R1)                                                 
         MVC   0(2,RF),FULL1                                                    
         UNPK  FULL1(3),1(2,R1)                                                 
         MVC   2(2,RF),FULL1                                                    
         UNPK  FULL1(3),2(2,R1)                                                 
         MVC   4(2,RF),FULL1                                                    
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Refresh Offer Group request                                         *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
REQMKG   LKREQ H,Q#DLMKG,OUTMKG,NEXTREQ=REQEND                                  
OffCode  LKREQ F,001,(D,B#SAVED,QOFFCODE),CHAR,TEXT=(*,OFFCLIT),COL=*           
Station  LKREQ F,002,(D,B#SAVED,QSTACLET),CHAR,TEXT=(*,SCALLIT),COL=*           
OrdNumb  LKREQ F,003,(D,B#SAVED,QORDRNUM),SPAK,TEXT=(*,ORD#LIT),COL=*           
OffGrpC  LKREQ F,004,(D,B#SAVED,QOFFGCOD),CHAR,TEXT=(*,OFFGLIT),COL=*           
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Refresh Offer Group reply                                           *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
OUTMKG   LKOUT H                                                                
                                                                                
MKGOFF   LKOUT R,R#DLMKG                                                        
Array    LKOUT C,R#DLMKG,(A,ARYMKG)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Array definition for Makegood record download                       *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
ARYMKG   LKOUT A,(R,NXTMKG),MULTIROW=Y,ROWNAME=RMKGRECD                         
OffCode  LKOUT C,010,RMKGKOFF,CHAR,FILTROUT=TSTMGH,SKIPCOLS=MGHSKIPN            
MGHSKIPS DS    0X                  Start of columns to skip                     
StaCLet  LKOUT C,011,RMKGKSTA,CHAR                                              
OrdNumb  LKOUT C,012,(D,B#SAVED,CON#CHAR),CHAR                                  
GrpCode  LKOUT C,013,RMKGKGRP,CHAR                                              
OffStat  LKOUT C,014,(D,B#SAVED,MHOFFSTA),CHAR,ND=Y                             
CreatBy  LKOUT C,015,(D,B#SAVED,MHCREABY),CHAR,ND=Y                             
CreaDat  LKOUT C,016,(D,B#SAVED,MHCREDAT),CDAT,ND=Y                             
CreaTim  LKOUT C,017,(D,B#SAVED,MHCRETIM),CHAR,ND=Y                             
LasChBy  LKOUT C,018,(D,B#SAVED,MHLASUBY),CHAR,ND=Y                             
LasChDt  LKOUT C,019,(D,B#SAVED,MHLCHDAT),CDAT,ND=Y                             
LasChTm  LKOUT C,020,(D,B#SAVED,MHLCHTIM),CHAR,ND=Y                             
GrpCom1  LKOUT C,021,(D,B#SAVED,MHGRCOM1),CHAR,ND=Y                             
GrpCom2  LKOUT C,022,(D,B#SAVED,MHGRCOM2),CHAR,ND=Y                             
GrpCom3  LKOUT C,023,(D,B#SAVED,MHGRCOM3),CHAR,ND=Y                             
OffDARE  LKOUT C,024,(D,B#SAVED,MHOFFDAR),CHAR,ND=Y                             
OfAplCf  LKOUT C,025,(D,B#SAVED,MHOFAPCF),UBIN                                  
MGHSKIPN EQU   (*-MGHSKIPS)/LX_COLSL                                            
*                                                                               
Array    LKOUT C,R#MSSMG,(A,ARYMMS),FILTROUT=TSTMTY                             
*                                                                               
Array    LKOUT C,R#MGOFR,(A,ARYOFR),FILTROUT=TSTMGO                             
*                                                                               
Array    LKOUT C,R#MGX02,(A,ARYDYTM),FILTROUT=TSTMGO                            
Array    LKOUT C,R#MGX03,(A,ARYEFDT),FILTROUT=TSTMGO                            
*                                                                               
Array    LKOUT C,R#SUMMR,(A,ARYSUM),FILTROUT=TSTSUM                             
*                                                                               
                                                                                
         LKOUT E                                                                
*                                                                               
ARYMMS   LKOUT A,(R,NXTMMS),MULTIROW=Y,ROWNAME=RMKGRECD                         
MsLinNum LKOUT C,010,(D,B#SAVED,MMLINNUM),UBIN,ND=Y                             
MsStrDat LKOUT C,011,(D,B#SAVED,MMSTRDAT),BDAT,ND=Y                             
MsEndDat LKOUT C,012,(D,B#SAVED,MMENDDAT),BDAT,ND=Y                             
MsSpotWk LKOUT C,013,(D,B#SAVED,MMSPOTWK),UBIN,ND=Y                             
Ms#ofWks LKOUT C,014,(D,B#SAVED,MM#WKSMS),UBIN,ND=Y                             
Ms#TotSp LKOUT C,015,(D,B#SAVED,MM#TOTSP),UBIN,ND=Y                             
Ms#ByRat LKOUT C,016,(D,B#SAVED,MM#BYRAT),CBIN,ND=Y                             
Ms#DemoV LKOUT C,017,(D,B#SAVED,MM#DEMOV),(R,EDTDEMV),ND=Y                      
Ms#TotCo LKOUT C,018,(D,B#SAVED,MM#TOTCO),CBIN,ND=Y                             
                                                                                
         LKOUT E                                                                
*                                                                               
ARYOFR   LKOUT A,(D,B#SAVED,SAVED),NEWEL=Y,NROWS=1                              
MOOfLin# LKOUT C,001,(D,B#SAVED,MOOFRLN#),UBIN,ND=Y                             
MOOftype LKOUT C,010,(D,B#SAVED,MOOFRTYP),CHAR,ND=Y                             
MOTotSpt LKOUT C,015,(D,B#SAVED,MO#TOTSP),UBIN,ND=Y                             
MOOfRate LKOUT C,016,(D,B#SAVED,MO#RATE_),CBIN,ND=Y                             
MOLength LKOUT C,020,(D,B#SAVED,MO#LENG_),UBIN,ND=Y                             
MOLenInd LKOUT C,021,(D,B#SAVED,MO#LENGI),CHAR,ND=Y                             
MOProgrm LKOUT C,022,(D,B#SAVED,MO#OPRGM),CHAR,ND=Y                             
MOLnCom1 LKOUT C,023,(D,B#SAVED,MO#LCOM1),CHAR,ND=Y                             
MOLncom2 LKOUT C,023,(D,B#SAVED,MO#LCOM2),CHAR,ND=Y                             
MO#DemVa LKOUT C,024,(D,B#SAVED,MO#DEMOV),(R,EDTDEMV),ND=Y                      
MOTotCos LKOUT C,025,(D,B#SAVED,MO#TOTCO),CBIN,ND=Y                             
         LKOUT E                                                                
*                                                                               
ARYSUM   LKOUT A,(D,B#SAVED,SAVED),NEWEL=Y,NROWS=1                              
SuMisSpt LKOUT C,010,(D,B#SAVED,SUMISSPT),UBIN                                  
SuMisCos LKOUT C,011,(D,B#SAVED,SUMISCOS),CBIN                                  
SuOffSpt LKOUT C,012,(D,B#SAVED,SUMSOFSP),UBIN                                  
SuOffCos LKOUT C,013,(D,B#SAVED,SUMSOFCO),CBIN                                  
         LKOUT E                                                                
*                                                                               
ARYDYTM  LKOUT A,(R,NXTDYTM),MULTIROW=Y,ROWNAME=RMKGRECD                        
StrDay   LKOUT C,010,(D,B#SAVED,X02STRDY),UBIN,ND=Y                             
EndDay   LKOUT C,011,(D,B#SAVED,X02ENDDY),UBIN,ND=Y                             
Days     LKOUT C,012,(D,B#SAVED,X02DAYS_),CHAR,ND=Y                             
StrTime  LKOUT C,013,(D,B#SAVED,X02STRTM),CHAR,ND=Y                             
EndTime  LKOUT C,014,(D,B#SAVED,X02ENDTM),CHAR,ND=Y                             
DayTmWF  LKOUT C,015,(D,B#SAVED,X02DAYTW),UBIN,ND=Y                             
EndTmCC  LKOUT C,016,(D,B#SAVED,X02TCCSW),CHAR,ND=Y                             
TimeText LKOUT C,017,(D,B#SAVED,X02TMTXT),CHAR,ND=Y                             
         LKOUT E                                                                
*                                                                               
ARYEFDT  LKOUT A,(R,NXTEFDT),MULTIROW=Y,ROWNAME=RMKGRECD                        
StrDate  LKOUT C,010,(D,B#SAVED,X03STRDT),BDAT,ND=Y                             
EndDate  LKOUT C,011,(D,B#SAVED,X03ENDDT),BDAT,ND=Y                             
NumOfWk  LKOUT C,012,(D,B#SAVED,X03#OFWK),UBIN,ND=Y                             
WeekInd  LKOUT C,013,(D,B#SAVED,X03WEKSW),CHAR,ND=Y                             
SpotWek  LKOUT C,014,(D,B#SAVED,X03SPTWK),UBIN,ND=Y                             
         LKOUT E                                                                
*                                                                               
TSTMGH   CLI   WKMGHSW,YESQ        Makegood header record?                      
         BR    RE                                                               
*                                                                               
TSTMGO   CLI   WKMGOSW,YESQ        Makegood offer record?                       
         BR    RE                                                               
*                                                                               
TSTSUM   CLI   WKSUMSW,YESQ        Makegood summary reply?                      
         BR    RE                                                               
*                                                                               
TSTMTY   CLI   WKMTYSW,YESQ        Makegood offer type?                         
         BR    RE                                                               
*                                                                               
INIMKGF  LR    R0,RE                                                            
         BRAS  RE,SETMKGF                                                       
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
EDTDEMV  LM    R2,R4,LP_AINP                                                    
         OC    0(L'MM#DEMOV,R2),0(R2)                                           
         JZ    XCOLEN                                                           
         CLC   0(L'MM#DEMOV,R2),=X'FFFFFFFF'                                    
         JE    XCOLEN                                                           
         UNPK  0(9,R4),0(5,R2)                                                  
         MVC   BYTE1,7(R4)         Save last digit                              
         MVI   7(R4),C'.'          Decimal point                                
         MVC   8(1,R4),BYTE1       Restore last digit                           
         LHI   R0,9                                                             
         J     SETOLENX                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REQEND   LKREQ X                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                  ** Global literals **                        
*                                                                               
         LTORG                                                                  
OFFCLIT  DC    C'Office Code'                                                   
SCALLIT  DC    C'Station Call Letters'                                          
ORD#LIT  DC    C'Order Number'                                                  
OFFGLIT  DC    C'Offer Group Code'                                              
                                                                                
         DROP  RB,R1                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETMKGF  NTR1  BASE=*,LABEL=*      Init Makegood flags                          
                                                                                
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTMKG   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
*                                                                               
         LA    R0,MKGRPYVS         Init Makegood reply values                   
         LHI   R1,MKGRPYLQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   WKMGHSW,NOQ         Init Makegood header data switch             
         MVI   WKMGOSW,NOQ         Init Makegood offer data switch              
         MVI   WKSUMSW,NOQ         Init Makegood summary data switch            
*                                                                               
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXMKG30                                                          
         XC    WKOFFGRP,WKOFFGRP   Init offer group code for summary            
         L     R0,AIO4             Init contract record area                    
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   WKMTYCT,0           Init makegood type counter                   
*                                                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),QORDRNUM                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   SVCON9CM,WORK                                                    
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R3,IOKEY                                                         
         USING RMKGKEY,R3                                                       
         MVI   RMKGKTYP,X'11'      Makegood offer record                        
         MVC   RMKGKREP,AGY        Rep code                                     
         MVC   RMKGKOFF,QOFFCODE   Office code                                  
         MVC   RMKGKSTA,QSTACLET   Station call letters                         
         GOTOR (RFCONNUM,AREPFACS),DMCB,(2,SVCON9CM),(3,RMKGKCON)               
         GOTOR (RFCONNUM,AREPFACS),DMCB,(2,SVCON9CM),(5,CON#CHAR)               
         CLC   QOFFGCOD,SPACES     Have offer group code?                       
         JNH   *+10                                                             
         MVC   RMKGKGRP,QOFFGCOD                                                
         DROP  R3                                                               
*                                                                               
NXMKG18  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         JNE   NXMKG_N                                                          
         CLC   IOKEY(RMKGKGRP-RMKGKEY),IOKEYSAV                                 
         JNE   NXMKG_N             Record not found, skip                       
         OC    IOKEY+LNMHKEYQ(ENMHKEYQ),IOKEY+LNMHKEYQ                          
         JNZ   NXMKG_N             Not a Makegood header record, skip           
*                                                                               
         CLC   QOFFGCOD,SPACES     Have offer group code?                       
         JNH   *+14                                                             
         CLC   QOFFGCOD,IOKEY+(RMKGKGRP-RMKGKEY)                                
         JNE   NXMKG_N             Offer code not match, skip                   
*                                                                               
NXMKG20  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO5'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* Offer status filter removed to pass back all offers, including                
* applied offers (KWAN Jan23/2013)                                              
*                                                                               
* * * *  BRAS  RE,CKOFRSTA         Check for offer status                       
* * * *  JNE   NXMKG18                                                          
*                                                                               
         MVI   WKMGHSW,YESQ        Need to reply Makegood header data           
         MVI   WKMTYSW,YESQ        Default to send missed data replies          
         XC    SUVALSTR(SUVALLNQ),SUVALSTR   Clear summary values               
*                                                                               
         L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'01'        Makegood header description elem             
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   NXMKG_N             Bad Makegood header record, skip             
         USING RMKGSDEM,R3                                                      
*                                                                               
         CLI   RMKGSCST,0          New?                                         
         JNE   *+8                                                              
         MVI   MHOFFSTA,C'N'                                                    
         TM    RMKGSCST,RMKGSAPQ   Applied?                                     
         JZ    *+8                                                              
         MVI   MHOFFSTA,C'A'                                                    
         TM    RMKGSCST,RMKGSRCQ   Recalled?                                    
         JZ    *+8                                                              
         MVI   MHOFFSTA,C'C'                                                    
         TM    RMKGSCST,RMKGSRJQ   Rejected?                                    
         JZ    *+8                                                              
         MVI   MHOFFSTA,C'J'                                                    
         TM    RMKGSCST,RMKGSCNQ   Cancelled?                                   
         JZ    *+8                                                              
         MVI   MHOFFSTA,C'X'                                                    
         TM    RMKGSCST,RMKGSRVQ   Revised?                                     
         JZ    *+8                                                              
         MVI   MHOFFSTA,C'R'                                                    
*                                                                               
         CLI   RMKGSFG1,0          New?                                         
         JNE   *+8                                                              
         MVI   MHOFFDAR,C'N'                                                    
         TM    RMKGSFG1,RMGF1MER   Error received?                              
         JZ    *+8                                                              
         MVI   MHOFFDAR,C'!'                                                    
         TM    RMKGSFG1,RMGF1MSN   Sent?                                        
         JZ    *+8                                                              
         MVI   MHOFFDAR,C'S'                                                    
         TM    RMKGSFG1,RMGF1MAR   Approval received?                           
         JZ    *+8                                                              
         MVI   MHOFFDAR,C'P'                                                    
         TM    RMKGSFG1,RMGF1MRR   Rejection received?                          
         JZ    *+8                                                              
         MVI   MHOFFDAR,C'R'                                                    
         TM    RMKGSFG1,RMGF1MCF   Applied?                                     
         JZ    *+8                                                              
         MVI   MHOFFDAR,C'A'                                                    
         TM    RMKGSFG1,RMGF1MCN   Cancelled?                                   
         JZ    *+8                                                              
         MVI   MHOFFDAR,C'C'                                                    
         TM    RMKGSFG1,RMGF1MCR   Resent?                                      
         JZ    *+8                                                              
         MVI   MHOFFDAR,C'E'                                                    
         TM    RMKGSFG1,RMGF1MCM   Recalled?                                    
         JZ    *+8                                                              
         MVI   MHOFFDAR,C'L'                                                    
*                                                                               
         MVI   MHOFAPCF,0          Set applied offer is confirmed to N          
         BRAS  RE,CKOFRMOD         Check for offer mod number                   
         JE    *+8                                                              
         MVI   MHOFAPCF,1          Set applied offer is confirmed to Y          
*                                                                               
         MVI   MHCREABY,C'S'       Default to created by station                
         TM    RMKGSCST,RMKGSCRQ                                                
         JZ    *+8                                                              
         MVI   MHCREABY,C'R'       Set to created by rep                        
         MVC   MHCREDAT,RMKGSCRD   Creation date                                
         OC    RMKGSCRT,RMKGSCRT   Have creation time?                          
         JZ    *+16                                                             
         LA    R1,RMKGSCRT                                                      
         LA    RF,MHCRETIM                                                      
         BRAS  RE,EDHHMMSS         Edit HHMMSS                                  
         TM    RMKGSFG2,RMGF2RPQ   Changed by rep?                              
         JZ    *+8                                                              
         MVI   MHLASUBY,C'R'                                                    
         TM    RMKGSFG2,RMGF2STQ   Changed by station?                          
         JZ    *+8                                                              
         MVI   MHLASUBY,C'S'                                                    
         MVC   MHLCHDAT,RMKGSLAD   Last activity date                           
         OC    RMKGSLAT,RMKGSLAT   Have last activity time?                     
         JZ    *+16                                                             
         LA    R1,RMKGSLAT                                                      
         LA    RF,MHLCHTIM                                                      
         BRAS  RE,EDHHMMSS         Edit HHMMSS                                  
         DROP  R3                                                               
*                                                                               
         LA    R2,MHGRCOM1         Point to first comment line                  
         L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'10'        Makegood header comment elem                 
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
NXMKG28  BRAS  RE,NXTELEM                                                       
         JNE   NXMKG_Y                                                          
         USING RMKGGCEM,R3                                                      
         LLC   RE,RMKGGCLN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),RMKGGCCM                                                 
         LA    R2,L'MHGRCOM1(R2)   Point to next line                           
         J     NXMKG28                                                          
         DROP  R3                                                               
*                                                                               
NXMKG30  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO5'                            
         CLC   IOKEY(RMKGKGRP-RMKGKEY),IOKEYSAV                                 
         JNE   NXMKG_N                                                          
         OC    IOKEY+LNMHKEYQ(ENMHKEYQ),IOKEY+LNMHKEYQ                          
         JZ    NXMKG20                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO5'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   WKMGOSW,YESQ        Need to reply Makegood offer data            
         BRAS  RE,FMTMGODA         Format offer data                            
*                                                                               
         LA    R3,IOKEY                                                         
         USING RMKGKEY,R3                                                       
         OC    WKOFFGRP,WKOFFGRP   Have offer group code for summary?           
         JNZ   *+10                                                             
         MVC   WKOFFGRP,RMKGKGRP   Offer group code for next summary            
         MVC   WKIOKEY,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO5'                            
         CLC   IOKEY(RMKGKGRP-RMKGKEY),IOKEYSAV                                 
         JNE   NXMKG36             Different contract #, do summary             
         CLC   WKOFFGRP,RMKGKGRP   Still on same offer?                         
         JE    NXMKG38                                                          
         MVC   WKOFFGRP,RMKGKGRP   Offer group code for next summary            
         MVI   WKMTYCT,0           Init makegood type counter                   
         MVI   WKMTYSW,YESQ        Default to send missed data replies          
NXMKG36  MVI   WKSUMSW,YESQ        Need to reply Makegood summary data          
         J     *+8                                                              
NXMKG38  MVI   WKSUMSW,NOQ         Do not reply Makegood summary data           
         MVC   IOKEY,WKIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         CLC   IOKEY(L'RMKGKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                Makegood record not restored                 
         DROP  R3                                                               
*                                                                               
NXMKG40  DS    0H                                                               
         J     NXMKG_Y                                                          
*                                                                               
NXMKG_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXMKG_Y  MVC   LP_ADATA,ARMKGREC                                                
         J     EXITY                                                            
*                                                                               
NXMKG_N  MVC   LP_ADATA,ARMKGREC                                                
         J     EXITN                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKOFRSTA NTR1  BASE=*,LABEL=*      Check for offer status                       
*                                                                               
         L     R3,AIO5             Point to makegood offer header rec           
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'01'        Makegood header description elem             
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   EXITY                                                            
         USING RMKGSDEM,R3                                                      
         TM    RMKGSCST,RMKGSAPQ   Applied?                                     
         JZ    EXITY                                                            
*                                                                               
         MVC   WKIOKEY,IOKEY       Save original key                            
         J     CKOFRS40            Skip applied offer completely                
*                                                                               
         MVC   BYTE3,RMKGAPMN      Save mod number at apply step                
         DROP  R3                                                               
*                                                                               
         L     R3,AIO4             Point to contract record                     
         CLI   0(R3),X'0C'         Already has contract record?                 
         JE    CKOFRS16                                                         
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    RE,IOKEY                                                         
         USING RCONREC,RE                                                       
         MVI   RCONPTYP,X'8C'      Passive Pointer 1 for Contract rec           
         MVC   RCONPREP,AGY        Rep code                                     
         MVC   RCONPCON,SVCON9CM   Contract number in 9's complement            
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO4'                            
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                Contract not found                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO4'                           
         JE    *+6                                                              
         DC    H'0'                No contract record                           
*                                                                               
CKOFRS16 L     R3,AIO4             Point to contract record                     
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'01'        Contract description elem                    
         CLC   ELCODE,0(R3)                                                     
         JE    CKOFRS20                                                         
         BRAS  RE,NXTELEM                                                       
         JE    CKOFRS20                                                         
         DC    H'0'                Bad contract record                          
         USING RCONELEM,R3                                                      
CKOFRS20 CLC   RCONMOD,BYTE3       Contract's mod # > applied offer?            
         JH    CKOFRS40                                                         
         DROP  R3                                                               
*                                                                               
         MVC   IOKEY,WKIOKEY       Restore key                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         J     EXITY                                                            
*                                                                               
CKOFRS40 XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         MVC   IOKEY(L'RMKGKEY),WKIOKEY                                         
         SR    RE,RE                                                            
         ICM   RE,3,IOKEY+(RMKGKGRP-RMKGKEY)                                    
         AHI   RE,1                Bump to next offer group                     
         STCM  RE,3,IOKEY+(RMKGKGRP-RMKGKEY)                                    
         J     EXITN                                                            
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKOFRMOD NTR1  BASE=*,LABEL=*      Check for offer mod number                   
*                                                                               
         L     R3,AIO5             Point to makegood offer header rec           
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'01'        Makegood header description elem             
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   EXITY                                                            
         USING RMKGSDEM,R3                                                      
         TM    RMKGSCST,RMKGSAPQ   Applied?                                     
         JZ    EXITY                                                            
         MVC   WKIOKEY,IOKEY       Save original key                            
         MVC   BYTE3,RMKGAPMN      Save mod number at apply step                
         DROP  R3                                                               
*                                                                               
         L     R3,AIO4             Point to contract record                     
         CLI   0(R3),X'0C'         Already has contract record?                 
         JE    CKOFRM16                                                         
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    RE,IOKEY                                                         
         USING RCONREC,RE                                                       
         MVI   RCONPTYP,X'8C'      Passive Pointer 1 for Contract rec           
         MVC   RCONPREP,AGY        Rep code                                     
         MVC   RCONPCON,SVCON9CM   Contract number in 9's complement            
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO4'                            
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                Contract not found                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO4'                           
         JE    *+6                                                              
         DC    H'0'                No contract record                           
*                                                                               
CKOFRM16 L     R3,AIO4             Point to contract record                     
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'01'        Contract description elem                    
         CLC   ELCODE,0(R3)                                                     
         JE    CKOFRM20                                                         
         BRAS  RE,NXTELEM                                                       
         JE    CKOFRS20                                                         
         DC    H'0'                Bad contract record                          
         USING RCONELEM,R3                                                      
CKOFRM20 CLC   RCONMOD,BYTE3       Contract's mod # > applied offer?            
         JH    CKOFRM40                                                         
         MVC   IOKEY,WKIOKEY       Restore key                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         J     EXITY                                                            
*                                                                               
CKOFRM40 MVC   IOKEY,WKIOKEY       Restore key                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         J     EXITN                                                            
         LTORG                                                                  
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTMMS   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
*                                                                               
         LA    R0,MMVALSTR         Init Makegood missed values                  
         LHI   R1,MMVALLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXMMS20                                                          
         L     R3,AIO5                                                          
         OC    LNMHKEYQ(ENMHKEYQ,R3),LNMHKEYQ(R3)                               
         JZ    NXMMS_X                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'05'        Makegood missed buys elem                    
         CLC   ELCODE,0(R3)                                                     
         JE    NXMMS30                                                          
NXMMS12  BRAS  RE,NXTELEM                                                       
         JNE   NXMMS_X                                                          
         J     NXMMS30                                                          
                                                                                
NXMMS20  L     R3,ANXTMMSE                                                      
         MVI   ELCODE,X'05'        Makegood missed buys elem                    
         J     NXMMS12                                                          
                                                                                
         USING RMKGMGEL,R3                                                      
NXMMS30  ST    R3,ANXTMMSE         Save address of Makegood missed elem         
         MVC   MMLINNUM,RMKGMGLI   Missed line number                           
         MVC   MMSTRDAT,RMKGMGD1   Missed start date                            
         MVC   MMENDDAT,RMKGMGD2   Missed end date                              
         MVC   MMSPOTWK,RMKGMGSP   Missed spot per week                         
         XC    FULL2,FULL2                                                      
         MVC   FULL2(L'RMKGMGD1),RMKGMGD1                                       
         OC    RMKGMGD2,RMKGMGD2   Have missed end date?                        
         JZ    *+10                                                             
         MVC   FULL2(L'RMKGMGD2),RMKGMGD2                                       
         XC    WORK,WORK                                                        
         GOTOR VDATCON,DMCB,(3,RMKGMGD1),(5,WORK+0)                             
         MVI   WORK+8,C'-'                                                      
         GOTOR VDATCON,DMCB,(3,FULL2),(5,WORK+9)                                
         GOTOR VPERVAL,DMCB,(17,WORK),ELEM                                      
         CLI   DMCB+4,0                                                         
         JNE   NXMMS32                                                          
WKD      USING PERVALD,ELEM                                                     
         LLC   RE,MMSPOTWK                                                      
         STH   RE,HALF1                                                         
         SR    RE,RE                                                            
         ICM   RE,3,WKD.PVALNWKS   Number of weeks missed (rounded up)          
         STC   RE,MM#WKSMS         Number of weeks missed                       
         MH    RE,HALF1                                                         
         STCM  RE,15,MM#TOTSP      Save missed total spots                      
         ICM   RF,15,SUMISSPT      Calc summary - total missed spots            
         AR    RF,RE                                                            
         STCM  RF,15,SUMISSPT                                                   
         DROP  WKD                                                              
         DROP  R3                                                               
*                                                                               
NXMMS32  MVC   WKIOKEY,IOKEY       Save key to read Buy record                  
         XC    IOKEY,IOKEY                                                      
         LA    R2,WKIOKEY                                                       
         USING RMKGKEY,R2                                                       
         LA    R3,IOKEY                                                         
         USING RBUYKEY,R3                                                       
         MVI   RBUYKTYP,RBUYKIDQ   Buy record                                   
         MVC   RBUYKREP,RMKGKREP   Rep code                                     
         MVC   RBUYKCON,RMKGKCON   Contract # in 9's comp reversed              
         MVC   RBUYKLIN,MMLINNUM   Buy line #                                   
*                                                                               
         MVI   BYTE3,NOQ           Set buy line not found                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO6'                            
         J     NXMMS38                                                          
NXMMS36  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO6'                            
NXMMS38  CLC   IOKEY(RBUYKPLN-RBUYKEY),IOKEYSAV                                 
         JNE   NXMMS42                                                          
         CLC   RBUYKPLN,=X'FFFFFF' Default plan code?                           
         JNE   NXMMS36                                                          
         CLC   RBUYKLIN,MMLINNUM   Buy line found?                              
         JNE   NXMMS36                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO6'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   BYTE3,YESQ          Set buy line found                           
         DROP  R2,R3                                                            
*                                                                               
NXMMS42  MVC   IOKEY,WKIOKEY       Restore Makegood record sequence             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         CLC   IOKEY(L'RMKGKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                Makegood record not restored                 
         CLI   BYTE3,YESQ          Buy line found?                              
         JNE   NXMMS52                                                          
*                                                                               
         L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'01'        Buy description element                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   NXMMS52                                                          
         USING RBUYELEM,R3                                                      
         MVC   MM#BYRAT,RBUYCOS    Save buy rate                                
         ICM   RE,15,MM#TOTSP      Number of total missed spots                 
         STH   RE,HALF1                                                         
         ICM   RE,15,MM#BYRAT      Missed rate                                  
         MH    RE,HALF1                                                         
         STCM  RE,15,MM#TOTCO      Save total missed rate                       
         ICM   RF,15,SUMISCOS      Calc summary - total missed rate             
         AR    RF,RE                                                            
         STCM  RF,15,SUMISCOS                                                   
         DROP  R3                                                               
*                                                                               
         L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'0D'        Look for Dare demo value elem                
         USING RBUYDMEL,R3                                                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   *+14                                                             
         MVC   MM#DEMOV,RBUYDMDM                                                
         J     *-14                                                             
         DROP  R3                                                               
*                                                                               
         L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'0E'        Look for Rep demo value elem                 
         USING RBUYRDEL,R3                                                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   *+14                                                             
         MVC   MM#DEMOV,RBUYRDDM                                                
         J     *-14                                                             
*                                                                               
NXMMS52  J     NXMMS_Y                                                          
*                                                                               
NXMMS_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXMMS_Y  MVC   LP_ADATA,ARMKGREC                                                
         J     EXITY                                                            
*                                                                               
NXMMS_N  MVC   LP_ADATA,ARMKGREC                                                
         J     EXITN                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTMGODA NTR1  BASE=*,LABEL=*      Format Makegood offer data                   
*                                                                               
         LA    R0,MOVALSTR         Init Makegood offer reply data               
         LHI   R1,MOVALLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,AIO5             Point to Makegood record                     
         USING RMKGREC,R3                                                       
         MVC   MOOFRLN#,RMKGKRTY   Offer line number                            
         DROP  R3                                                               
*                                                                               
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'01'        Look for main description element            
         USING RMKGELEM,R3                                                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   F_MGOD10                                                         
*                                                                               
         MVI   MOOFRTYP,C'M'       Default to Makegood                          
         TM    RMKGRTS,X'20'                                                    
         JZ    *+8                                                              
         MVI   MOOFRTYP,C'B'       Set to Bonus                                 
         TM    RMKGRTS,X'10'                                                    
         JZ    *+8                                                              
         MVI   MOOFRTYP,C'P'       Set to Preempt/Credit                        
         TM    RMKGRTS,X'08'                                                    
         JZ    *+8                                                              
         MVI   MOOFRTYP,C'L'       Set to Late Run                              
*                                                                               
         MVI   WKMTYSW,YESQ        Default to send missed data replies          
         CLI   MOOFRTYP,C'M'       Makegood offer type?                         
         JNE   F_MGOD08                                                         
         LLC   RE,WKMTYCT                                                       
         AHI   RE,1                Bump up makegood offer type counter          
         STC   RE,WKMTYCT                                                       
         CLI   WKMTYCT,1           More than 1 makegood type replied?           
         JNH   F_MGOD08                                                         
         MVI   WKMTYSW,NOQ         Already send once, suppress rest             
*                                                                               
F_MGOD08 MVC   MO#TOTSP,RMKGTSPT   Offered total spots                          
         SR    RE,RE               Calc summary - total offered spots           
         ICM   RE,03,MO#TOTSP                                                   
         ICM   RF,15,SUMSOFSP                                                   
         AR    RF,RE                                                            
         STCM  RF,15,SUMSOFSP                                                   
         MVC   MO#RATE_,RMKGCOS    Offered rate                                 
         MVC   MO#TOTCO,RMKGTCOS   Offered total cost                           
         ICM   RE,15,MO#TOTCO      Calc summary - total offered cost            
         ICM   RF,15,SUMSOFCO                                                   
         AR    RF,RE                                                            
         STCM  RF,15,SUMSOFCO                                                   
         MVC   MO#LENG_,RMKGDUR    Offered length                               
         OC    MO#LENG_,MO#LENG_   Have length?                                 
         JZ    *+8                                                              
         MVI   MO#LENGI,C'S'       Default to seconds                           
         TM    MO#LENG_,X'80'      Length is in minutes?                        
         JZ    *+12                                                             
         MVI   MO#LENGI,C'M'       Set to minutes                               
         NI    MO#LENG_,X'FF'-X'80'                                             
*                                                                               
F_MGOD10 DS    0H                                                               
*                                                                               
F_MGOD20 DS    0H                                                               
*                                                                               
F_MGOD30 L     R3,AIO5             Point to Makegood record                     
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'21'        Look for Program element                     
         USING RMKGPGEL,R3                                                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   F_MGOD40                                                         
         LLC   RF,RMKGPGLN                                                      
         AHI   RF,-(2+1)           Adjust elem header and EX                    
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   MO#OPRGM(0),RMKGPGM Offered program                              
*                                                                               
F_MGOD40 L     R3,AIO5             Point to Makegood record                     
         LA    R3,(FRSTELEM)(R3)                                                
         LA    R2,MO#LCOM1                                                      
         MVI   ELCODE,X'11'        Look for Detail comment element              
         USING RMKGDCEL,R3                                                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
F_MGOD42 BRAS  RE,NXTELEM                                                       
         JNE   F_MGOD50                                                         
         LLC   RF,RMKGDCLN                                                      
         AHI   RF,-(2+1)           Adjust elem header and EX                    
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),RMKGDCCM                                                 
         LA    R2,L'MO#LCOM1(R2)   Point to next line                           
         J     F_MGOD42                                                         
*                                                                               
F_MGOD50 L     R3,AIO5             Point to Makegood record                     
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,RMKGDMCQ     Look for Rep demo value element              
         USING RMKGDMEL,R3                                                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   F_MGOD60                                                         
         MVC   MO#DEMOV,RMKGDMDM   Offered Rep's program rating                 
*                                                                               
F_MGOD60 DS    0H                                                               
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTDYTM  NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         XC    X02VALST(X02LENGQ),X02VALST             Init output              
*                                                                               
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXDYT20                                                          
         XC    FULL1,FULL1         Init elem pointer                            
         L     R3,AIO5             Point to Makegood record                     
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'02'        Look for Day/Time elements                   
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
NXDYT14  BRAS  RE,NXTELEM                                                       
         JNE   NXDYT_X                                                          
         USING RMKGDYEL,R3                                                      
         LLC   RF,RMKGDYIN                                                      
         SRL   RF,4                                                             
         STC   RF,X02STRDY         Start day                                    
         MVC   X02ENDDY,RMKGDYIN                                                
         NI    X02ENDDY,X'0F'      End day                                      
*                                                                               
         TM    RMKGDAYS,X'80'      End time CC override?                        
         JNZ   NXDYT16                                                          
         MVC   X02DAYS_,=C'NNNNNNN'                                             
         TM    RMKGDAYS,X'40'      Monday?                                      
         JZ    *+8                                                              
         MVI   X02DAYS_+0,C'Y'                                                  
         TM    RMKGDAYS,X'20'      Tuesday?                                     
         JZ    *+8                                                              
         MVI   X02DAYS_+1,C'Y'                                                  
         TM    RMKGDAYS,X'10'      Wednesday?                                   
         JZ    *+8                                                              
         MVI   X02DAYS_+2,C'Y'                                                  
         TM    RMKGDAYS,X'08'      Thursday?                                    
         JZ    *+8                                                              
         MVI   X02DAYS_+3,C'Y'                                                  
         TM    RMKGDAYS,X'04'      Friday?                                      
         JZ    *+8                                                              
         MVI   X02DAYS_+4,C'Y'                                                  
         TM    RMKGDAYS,X'02'      Saturday?                                    
         JZ    *+8                                                              
         MVI   X02DAYS_+5,C'Y'                                                  
         TM    RMKGDAYS,X'01'      Sunday?                                      
         JZ    *+8                                                              
         MVI   X02DAYS_+6,C'Y'                                                  
*                                                                               
NXDYT16  CLC   RMKGDYT1(04),=C'NONE'                                            
         JE    *+14                                                             
         CLC   RMKGDYT1(04),=C'VARY'                                            
         JNE   *+10                                                             
         MVC   X02TMTXT,RMKGDYT1   Save Time Text                               
*                                                                               
         CLC   RMKGDYT1,=C'CC'     Start time CC override?                      
         JE    NXDYT17                                                          
         CLI   RMKGDYT1,X'FF'                                                   
         JE    NXDYT16A                                                         
         EDIT  RMKGDYT1,X02STRTM,FILL=0                                         
*                                                                               
NXDYT16A CLC   RMKGDYT2,=C'CC'     End time CC override?                        
         JE    NXDYT17                                                          
         CLI   RMKGDYT2,X'FF'                                                   
         JE    NXDYT18                                                          
         OC    RMKGDYT2,RMKGDYT2   If single time, skip end time                
         JZ    NXDYT18                                                          
         EDIT  RMKGDYT2,X02ENDTM,FILL=0                                         
         J     NXDYT18                                                          
*                                                                               
NXDYT17  MVI   X02TCCSW,C'Y'       Y=end time CC override                       
*                                                                               
NXDYT18  MVC   X02DAYTW,RMKGDYWT   Save Day/Time Weighting Factor               
*                                                                               
         ST    R3,FULL1            Save current elem pointer                    
         J     NXDYT30                                                          
         DROP  R3                                                               
                                                                                
NXDYT20  L     R3,FULL1            Point to previous elem                       
         J     NXDYT14                                                          
                                                                                
NXDYT30  DS    0H                                                               
         J     NXDYT_Y                                                          
*                                                                               
NXDYT_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXDYT_Y  MVC   LP_ADATA,ARMKGREC                                                
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTEFDT  NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         XC    X03VALST(X03LENGQ),X03VALST             Init output              
*                                                                               
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXEFD20                                                          
         XC    FULL1,FULL1         Init elem pointer                            
         L     R3,AIO5             Point to Makegood record                     
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'03'        Look for Buy Effective date elements         
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
NXEFD14  BRAS  RE,NXTELEM                                                       
         JNE   NXEFD_X                                                          
         USING RMKGDTEL,R3                                                      
         MVC   X03STRDT,RMKGDTST                                                
         MVC   X03ENDDT,RMKGDTED                                                
         MVC   X03#OFWK,RMKGDTWK                                                
         TM    RMKGDTIN,X'80'      Runs every week?                             
         JZ    *+8                                                              
         MVI   X03WEKSW,C'E'       Set to Every week                            
         TM    RMKGDTIN,X'40'      Runs every other week?                       
         JZ    *+8                                                              
         MVI   X03WEKSW,C'A'       Set to Alternating week                      
         MVC   X03SPTWK,RMKGDTNW                                                
         ST    R3,FULL1            Save current elem pointer                    
         J     NXEFD30                                                          
         DROP  R3                                                               
*                                                                               
NXEFD20  L     R3,FULL1            Point to previous elem                       
         J     NXEFD14                                                          
*                                                                               
NXEFD30  DS    0H                                                               
         J     NXEFD_Y                                                          
*                                                                               
NXEFD_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXEFD_Y  MVC   LP_ADATA,ARMKGREC                                                
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NOQ      EQU   C'N'                                                             
YESQ     EQU   C'Y'                                                             
EOR      EQU   0                   End of record element code                   
EFF      EQU   X'FF'               End of table marker                          
*                                                                               
LNMHKEYQ EQU   RMKGKPLN-RMKGKEY    Length of Makegood header key                
ENMHKEYQ EQU   L'RMKGKEY-LNMHKEYQ  Length of remaining parts of key             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SAVED    DSECT                                                                  
*                                                                               
AGY      DS    CL2                 Agency code                                  
MASTRCOD DS    CL2                 Master Rep Code                              
*                                                                               
ANXTMMSE DS    A                   Address of next Makegood missed elem         
*                                                                               
QOFFCODE DS    CL(L'RMKGKOFF)      Office code                                  
QSTACLET DS    CL(L'RMKGKSTA)      Station call letter                          
QORDRNUM DS    CL(L'RMKGKCON+1)    Order number                                 
QOFFGCOD DS    CL(L'RMKGKGRP)      Offer group code                             
*                                                                               
SVCON9CM DS    XL(L'RCONPCON)      Contract # in 9's complement                 
CON#CHAR DS    CL8                 Contract # in character format               
WKIOKEY  DS    XL(L'IOKEY)                                                      
WKMGHSW  DS    CL1                 Makegood header data switch                  
WKMGOSW  DS    CL1                 Makegood offer data switch                   
WKSUMSW  DS    CL1                 Makegood summary data switch                 
WKMTYSW  DS    CL1                 Makegood offer type switch                   
WKMTYCT  DS    XL1                 makegood offer type counter                  
WKOFFGRP DS    CL(L'RMKGKGRP)      Offer group code for summary reply           
*                                                                               
MKGRPYVS DS    0X                  Start of makegood reply values               
*                                                                               
MHVALSTR DS    0X                  Makegood header record values                
MHOFFSTA DS    CL(L'RMKGSCST)      Offer status                                 
MHOFFDAR DS    CL(L'RMKGSFG1)      Current offer status DARE flag               
MHOFAPCF DS    XL1                 Offer applied and confirmed flag             
MHCREABY DS    CL(L'RMKGSCST)      Created by Rep or Station (RMKGSCRQ)         
MHCREDAT DS    XL(L'RMKGSCRD)      Creation date (compressed)                   
MHCRETIM DS    CL6                 Creation time (HHMMSS)                       
MHLASUBY DS    CL(L'RMKGSFG2)      Last update by Rep or Station                
MHLCHDAT DS    XL(L'RMKGSLAD)      Last activity date (compressed)              
MHLCHTIM DS    CL6                 Last activity time (HHMMSS)                  
MHGRCOM1 DS    CL70                Group comment line 1                         
MHGRCOM2 DS    CL(L'MHGRCOM1)      Group comment line 2                         
MHGRCOM3 DS    CL(L'MHGRCOM1)      Group comment line 3                         
MHVALLNQ EQU   *-MHVALSTR                                                       
*                                                                               
MMVALSTR DS    0X                  Makegood missed buys                         
MMLINNUM DS    CL(L'RMKGMGLI)      Missed line number                           
MMSTRDAT DS    CL(L'RMKGMGD1)      Missed start date                            
MMENDDAT DS    XL(L'RMKGMGD2)      Missed end date                              
MMSPOTWK DS    XL(L'RMKGMGSP)      Missed spots per week                        
MM#WKSMS DS    XL1                 Number of weeks missed (calculated)          
MM#TOTSP DS    XL4                 Missed total spots (calculated)              
MM#BYRAT DS    XL(L'RBUYCOS)       Missed rate (from Buy record)                
MM#DEMOV DS    XL(L'RBUYDMDM)      Missed program rating (from Buy rec)         
MM#TOTCO DS    XL4                 Total missed cost                            
MMVALLNQ EQU   *-MMVALSTR                                                       
*                                                                               
MOVALSTR DS    0X                  Makegood offers                              
MOOFRLN# DS    XL(L'RMKGKRTY)      Offer line number                            
MOOFRTYP DS    CL1                 Offered type                                 
MO#TOTSP DS    XL(L'RMKGTSPT)      Offered total spots                          
MO#RATE_ DS    XL(L'RMKGCOS)       Offered rate                                 
MO#LENG_ DS    XL(L'RMKGDUR)       Offered length                               
MO#LENGI DS    CL1                 Offered length indicator (Min/Sec)           
MO#OPRGM DS    CL34                Offered program (RMKGPGM)                    
MO#LCOM1 DS    CL60                Offered line comment 1 (RMKGDCCM)            
MO#LCOM2 DS    CL60                Offered line comment 2 (RMKGDCCM)            
MO#DEMOV DS    XL(L'RMKGDMDM)      Offered Rep's program rating                 
MO#TOTCO DS    XL(L'RMKGTCOS)      Offered total cost                           
MOVALLNQ EQU   *-MOVALSTR                                                       
*                                                                               
MKGRPYLQ EQU   *-MKGRPYVS          End of makegood reply values                 
*                                                                               
SUVALSTR DS    0X                  Makegood summary reply data                  
SUMISSPT DS    XL4                 Summary - missed spot                        
SUMISCOS DS    XL4                 Summary - missed rate                        
SUMSOFSP DS    XL4                 Summary - offered spot                       
SUMSOFCO DS    XL4                 Summary - offered cost                       
SUVALLNQ EQU   *-SUVALSTR                                                       
*                                                                               
X02VALST DS    0X                  Values from X'02' element in Buy rec         
X02STRDY DS    XL(L'RMKGDYIN)      Start day                                    
X02ENDDY DS    XL(L'RMKGDYIN)      End day                                      
X02DAYS_ DS    CL7                                                              
X02STRTM DS    CL4                 Start time                                   
X02ENDTM DS    CL4                 End time                                     
X02DAYTW DS    XL(L'RMKGDYWT)      Day/Time Weighting Factor                    
X02TCCSW DS    C                   Y=end time is displayed CC                   
X02TMTXT DS    CL4                 Time text (NONE or VARY)                     
X02LENGQ EQU   *-X02VALST                                                       
*                                                                               
X03VALST DS    0X                  Values from X'03' element in Buy rec         
X03STRDT DS    XL(L'RMKGDTST)      Start date                                   
X03ENDDT DS    XL(L'RMKGDTED)      End date                                     
X03#OFWK DS    XL(L'RMKGDTWK)      Number of weeks                              
X03WEKSW DS    C                   Week/Alernate Week switch                    
X03SPTWK DS    XL(L'RMKGDTNW)      Spots per week                               
X03LENGQ EQU   *-X03VALST                                                       
*                                                                               
SAVEL    EQU   *-SAVED                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE RELNKWRK                                                       
*                                                                               
B#MGC    EQU   3                   Makegood comments                            
B#COV    EQU   3                   Coversheet record                            
B#CMT    EQU   3                   Comment record                               
B#AGY    EQU   4                   Agency record                                
B#STA    EQU   5                   Station record                               
B#CON    EQU   5                   Contract record                              
B#MKG    EQU   5                   Makegood record                              
B#BUY    EQU   6                   Buy record                                   
                                                                                
ARMKGDCM EQU   LP_BLKS+((B#MGC-1)*L'LP_BLKS),,C'A'                              
ARCOVREC EQU   LP_BLKS+((B#COV-1)*L'LP_BLKS),,C'A'                              
ARCMTREC EQU   LP_BLKS+((B#CMT-1)*L'LP_BLKS),,C'A'                              
ARAGYREC EQU   LP_BLKS+((B#AGY-1)*L'LP_BLKS),,C'A'                              
ARSTAREC EQU   LP_BLKS+((B#STA-1)*L'LP_BLKS),,C'A'                              
ARCONREC EQU   LP_BLKS+((B#CON-1)*L'LP_BLKS),,C'A'                              
ARMKGREC EQU   LP_BLKS+((B#MKG-1)*L'LP_BLKS),,C'A'                              
ARBUYREC EQU   LP_BLKS+((B#BUY-1)*L'LP_BLKS),,C'A'                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006RELNK13   10/30/14'                                      
         END                                                                    
