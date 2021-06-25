*          DATA SET RELNK12    AT LEVEL 017 AS OF 04/11/14                      
*PHASE T82B12A                                                                  
RELNK12  TITLE '- Rep system download - Contract/Buy'                           
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,REQUEST=*,CODE=CODE,SYSTEM=REPSYSQ,              +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,                    +        
               B#COV,RCOVRECD,B#CMT,RCMTRECD,B#AGY,RAGYRECD,           +        
               B#STA,RSTARECD,B#CON,RCONRECD,B#BUY,RBUYRECD,           +        
               B#EBC,EMBCOMMD,B#DAC,DARSDCMD)                                   
                                                                                
CODE     NMOD1 0,**RL12**,RR=RE                                                 
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
         USING SECD,SECBLOCK       SECRET BLOCK                                 
                                                                                
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
         MVC   LP_BLKS+((B#CMT-1)*L'LP_BLKS),AIO3                               
         MVC   LP_BLKS+((B#AGY-1)*L'LP_BLKS),AIO4                               
         MVC   LP_BLKS+((B#CON-1)*L'LP_BLKS),AIO5                               
         MVC   LP_BLKS+((B#BUY-1)*L'LP_BLKS),AIO6                               
         MVC   LP_BLKS+((B#EBC-1)*L'LP_BLKS),AIO7                               
         MVC   AGY,LP_AGY          Set agency code                              
*                                                                               
         L     RF,LP_ACOM                                                       
         MVC   VRECUP,CRECUP-COMFACSD(RF)                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RREPKEY,RE          Look up master Rep code                      
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGY                                                     
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         CLC   IOKEY(L'RREPKEY),IOKEYSAV                                        
         JE    *+6                                                              
         DC    H'0'                No Rep record                                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO1'                           
         L     R3,AIO1                                                          
         LA    R3,RREPELEM-RREPREC(R3)                                          
         CLI   0(R3),X'01'                                                      
         JE    *+6                                                              
         DC    H'0'                No Rep element                               
         USING RREPELEM,R3                                                      
         MVC   SVREPNAM,RREPNAME   Save rep name                                
         DROP  R3                                                               
*                                                                               
         USING RREPELEM,R3                                                      
         MVC   MASTRCOD,AGY        Init master Rep code                         
         MVC   R_REPPRF,RREPPROF   Rep profile values                           
         CLC   RREPMAST,SPACES     Have master control?                         
         JNH   *+20                                                             
         CLC   RREPMAST,=X'FFFF'   Master control?                              
         JE    *+10                                                             
         MVC   MASTRCOD,RREPMAST   Set master Rep code                          
         DROP  R3                                                               
*                                                                               
         XC    R_SOMPRF,R_SOMPRF                                                
         XC    REPPRGPF(RREPPGML*15),REPPRGPF                                   
         XC    PROFDATA,PROFDATA                                                
         USING RREPPGMP,R3                                                      
         MVI   ELCODE,X'04'        Program profile element                      
         BRAS  RE,NXTELEM                                                       
         JNE   RUNSTR40                                                         
         MVC   REPPRGPF(RREPPGML*15),RREPPGM1                                   
         LA    RE,RREPPGM1                                                      
         LLC   RF,RREPPGM#         # of program units (loop counter)            
RUNSTR22 CLI   0(RE),RREPQCNT      Contract?                                    
         JNE   *+14                                                             
         MVC   PROFDATA,0(RE)      Save contract program profile                
         J     RUNSTR40                                                         
         LA    RE,RREPPGML(RE)                                                  
         JCT   RF,RUNSTR22                                                      
RUNSTR40 GOTOR BITOUT,DMCB,REPPRGPF+SOMPBITQ,R_SOMPRF                           
         DROP  R3                                                               
*                                                                               
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Run a download request                                              *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   Run request mode?                            
         JNE   EXITY                                                            
                                                                                
         XC    SECVALS(SECVALSL),SECVALS                                        
         XC    SECD(256),SECD                                                   
         XC    DMCB(6*4),DMCB                                                   
         GOTOR VSECRET,DMCB,('SECPINIT',SECD),SECLENQ                           
                                                                                
         LA    R2,SECFLDS          R2=A(FIELD SECURITY DISPLACEMENTS)           
         LHI   R0,SECFLDSN         R0=N'SECURITY FIELDS                         
         XC    DMCB(6*4),DMCB                                                   
RUNREQ02 GOTOR VSECRET,DMCB,('SECPFLDP',SECD),1(R2)                             
         BE    RUNREQ04                                                         
         LA    RF,C'Y'             C'Y'=READ ONLY                               
         BH    *+8                                                              
         LA    RF,C'N'             C'N'=NO ACCESS                               
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,SECVALS(RE)                                                   
         STC   RF,0(RE)            YES - DISPLAY AND CHANGE                     
RUNREQ04 AHI   R2,L'SECFLDS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,RUNREQ02         DO FOR NUMBER OF SECURITY FIELDS             
*                                                                               
         LA    R2,SECACTS                                                       
         LHI   R0,SECACTSN         R0=N'SECURITY ACTIONS                        
         XC    DMCB(6*4),DMCB                                                   
*                                                                               
RUNREQ06 GOTOR VSECRET,DMCB,('SECPRACT',SECD),(1(R2),2(R2))                     
         BNE   RUNREQ08                                                         
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,SECVALS(RE)                                                   
         MVI   0(RE),C'Y'          SET ACTION IS VALID                          
RUNREQ08 AHI   R2,L'SECACTS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,RUNREQ06         DO FOR NUMBER OF SECURITY ACTIONS            
         B     RUNREQ18                                                         
                                                                                
RUNREQ18 DS    0H                                                               
         L     R1,ALP                                                           
         GOTOR LP_APUTO                                                         
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NOMORE   L     R1,ALP                                                           
         MVI   LP_RMODE,LP_RLAST   Set no more data                             
         J     EXITY                                                            
*                                                                               
XCOLEN   L     R1,ALP                                                           
         XC    LP_OLEN,LP_OLEN     Clear output field length & exit             
         J     EXITY                                                            
*                                                                               
SETOLENX L     R1,ALP                                                           
         STCM  R0,15,LP_OLEN       Set output field length & exit               
         J     EXITY                                                            
*                                                                               
EXITN    LA    RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
*                                                                               
SECFLDS  DS    0XL2                ** DISPS. TO SECURITY VALUES **              
         DC    AL1(SECFDUM1-SECVALS,01)                                         
         DC    AL1(SECFDUM2-SECVALS,02)                                         
SECFLDSN EQU   (*-SECFLDS)/L'SECFLDS                                            
*                                                                               
SECACTS  DS    0XL3                                                             
         DC    AL1(SECADUM1-SECVALS),AL1(1,1)                                   
         DC    AL1(SECADUM2-SECVALS),AL1(1,2)                                   
SECACTSN EQU   (*-SECACTS)/L'SECACTS                                            
*                                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Station Xpress Initial Download Request                             *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
REQINI   LKREQ *,Q#DLINI,OUTINI,NEXTREQ=REQCON                                  
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Station Xpress Initial Download Reply                               *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
OUTINI   LKOUT H                                                                
                                                                                
STXINI   LKOUT R,R#DLINI                                                        
Array    LKOUT C,R#DLINI,(A,ARYIDL)                                             
         LKOUT E                                                                
*                                                                               
RSOMPF   LKOUT R,R#RSOMP                                                        
         LKOUT C,002,(D,B#SAVED,R_SOMPRF+01),CHAR,LEN=1                         
         LKOUT C,003,(D,B#SAVED,R_SOMPRF+02),CHAR,LEN=1                         
         LKOUT C,004,(D,B#SAVED,R_SOMPRF+03),CHAR,LEN=1                         
         LKOUT C,005,(D,B#SAVED,R_SOMPRF+04),CHAR,LEN=1                         
         LKOUT E                                                                
*                                                                               
RREPPF   LKOUT R,R#RREPP                                                        
         LKOUT C,005,(D,B#SAVED,R_REPPRF+04),CHAR,LEN=1                         
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Array definition for Station record download                        *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
ARYIDL   LKOUT A,(R,NXTSTA),MULTIROW=Y,ROWNAME=RSTARECD                         
CalLet   LKOUT C,001,RSTAKREP,CHAR,ND=Y                                         
RepNam   LKOUT C,002,(D,B#SAVED,SVREPNAM),CHAR                                  
RepCod   LKOUT C,008,RSTAKSTA,(R,EDTRCOD)                                       
Array    LKOUT C,020,(A,ARYSDSP)                                                
         LKOUT E                                                                
*                                                                               
EDTRCOD  LM    R2,R4,LP_AINP                                                    
         MVC   0(L'RSTPKSTA,R4),0(R2)                                           
         CLI   (L'RSTPKSTA-1)(R2),C' '                                          
         JNE   *+8                                                              
         MVI   (L'RSTPKSTA-1)(R4),C'T'                                          
         LHI   R0,L'RSTPKSTA                                                    
         J     SETOLENX                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Rep Contract record download Request                                *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
REQCON   LKREQ H,Q#DLCON,OUTCON,NEXTREQ=REQPCO                                  
Con#s    LKREQ F,001,(I,B#SAVED,QCONIND),SPAK,TEXT=(*,CON#LIT),        +        
               OLEN=L'RCONPCON+1,LIST=F,SORT=N                                  
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Rep Contract record download Reply                                  *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
OUTCON   LKOUT H                                                                
                                                                                
REPCON   LKOUT R,R#DLCON                                                        
Array    LKOUT C,R#DLCON,(A,ARYCON)                                             
         LKOUT E                                                                
*                                                                               
REPNDC   LKOUT R,R#NDISCM                                                       
PRout    LKOUT P,,SETNDCM                                                       
EmbCom1  LKOUT C,032,(D,B#EBC,EMBCOMM1),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom2  LKOUT C,032,(D,B#EBC,EMBCOMM2),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom3  LKOUT C,032,(D,B#EBC,EMBCOMM3),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom4  LKOUT C,032,(D,B#EBC,EMBCOMM4),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom5  LKOUT C,032,(D,B#EBC,EMBCOMM5),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom6  LKOUT C,032,(D,B#EBC,EMBCOMM6),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom7  LKOUT C,032,(D,B#EBC,EMBCOMM7),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom8  LKOUT C,032,(D,B#EBC,EMBCOMM8),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom9  LKOUT C,032,(D,B#EBC,EMBCOMM9),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComA  LKOUT C,032,(D,B#EBC,EMBCOMMA),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComB  LKOUT C,032,(D,B#EBC,EMBCOMMB),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComC  LKOUT C,032,(D,B#EBC,EMBCOMMC),CHAR,ND=Y,FILTROUT=TSTEMB               
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Rep Pending Contract record download Request                        *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
REQPCO   LKREQ H,Q#DLPCO,OUTPCO,NEXTREQ=REQEND                                  
StaCLet  LKREQ F,001,(I,B#SAVED,QSTAIND),CHAR,TEXT=(*,SCALLIT),        +        
               OLEN=L'RCON9DST,LIST=F,SORT=N                                    
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Rep Pending Contract record download Reply                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
OUTPCO   LKOUT H                                                                
                                                                                
REPPCO   LKOUT R,R#DLPCO                                                        
Array    LKOUT C,R#DLPCO,(A,ARYPST)                                             
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Array definition for Contract record download                       *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
ARYCON   LKOUT A,(R,NXTCON),MULTIROW=Y,ROWNAME=RCONRECD                         
PRout    LKOUT P,,SETCONF                                                       
ConSta   LKOUT C,010,RCONKSTA,CHAR,ND=Y                                         
ConSNm   LKOUT C,011,(D,B#SAVED,SVSTANAM),CHAR                                  
ConCon   LKOUT C,012,RCONKCON,(R,EDTCON#)                                       
ConRepC  LKOUT C,013,RCONKREP,CHAR                                              
RepNam   LKOUT C,014,(D,B#SAVED,SVREPNAM),CHAR                                  
ConOff   LKOUT C,056,RCONKOFF,CHAR,ND=Y                                         
Array    LKOUT C,015,(A,ARYCDAO)                                                
Array    LKOUT C,016,(A,ARYCXDE)                                                
Array    LKOUT C,024,(A,ARYCDSP)                                                
CTyNam   LKOUT C,031,(D,B#SAVED,SVCTYNAM),CHAR                                  
AgyCod   LKOUT C,035,RCONKAGY,CHAR,ND=Y                                         
AgyOff   LKOUT C,036,RCONKAOF,CHAR,ND=Y                                         
AgyConn  LKOUT C,043,(D,B#SAVED,SVAGYCON),CHAR                                  
                                                                                
ConAdv   LKOUT C,050,RCONKADV,CHAR,ND=Y                                         
AdvNam   LKOUT C,051,(D,B#SAVED,SVADVNAM),CHAR                                  
PrdNam   LKOUT C,053,(D,B#SAVED,SVPRDNAM),CHAR                                  
SalNam   LKOUT C,055,(D,B#SAVED,SVSALNAM),CHAR                                  
SalTel   LKOUT C,093,(D,B#SAVED,SVSALTEL),CHAR                                  
SalFax   LKOUT C,094,(D,B#SAVED,SVSALFAX),CHAR                                  
SalEml   LKOUT C,095,(D,B#SAVED,SVSALEML),CHAR                                  
OffNam   LKOUT C,057,(D,B#SAVED,SVOFFNAM),CHAR                                  
OffAd1   LKOUT C,096,(D,B#SAVED,SVOFFAD1),CHAR                                  
OffAd2   LKOUT C,097,(D,B#SAVED,SVOFFAD2),CHAR                                  
OffStt   LKOUT C,098,(D,B#SAVED,SVOFFSTT),CHAR                                  
OffZip   LKOUT C,099,(D,B#SAVED,SVOFFZIP),CHAR                                  
CPendS   LKOUT C,065,(D,B#SAVED,SVCPNDSW),CHAR                                  
ConSta   LKOUT C,100,(D,B#SAVED,CONSTATS),CHAR                                  
Array    LKOUT C,058,(A,ARYCXDS)                                                
Array    LKOUT C,060,(A,ARYCEAS)                                                
WIPSta   LKOUT C,066,RCONELEM,(R,EDTWIPS)                                       
Array    LKOUT C,067,(A,ARYCSND)                                                
Array    LKOUT C,074,(A,ARYCRFE)                                                
*                                                                               
PRout    LKOUT P,,PRCCBUC          Process contract bucket                      
Array    LKOUT C,075,(A,ARYCBKE)                                                
Array    LKOUT C,075,(A,ARYCBKT)                                                
*                                                                               
Array    LKOUT C,077,(A,ARYCECE)                                                
Array    LKOUT C,079,(A,ARYCSPL)                                                
Array    LKOUT C,086,(A,ARYCSPC)                                                
Array    LKOUT C,087,(A,ARYCCME)                                                
Array    LKOUT C,088,(A,ARYCROC)                                                
Array    LKOUT C,089,(A,ARYSROC)                                                
Array    LKOUT C,090,(A,ARYCSA1)                                                
Array    LKOUT C,102,(A,ARYCFCM)   Confirmation comment                         
                                                                                
Array    LKOUT C,037,(A,ARYAGYN)   Extracting data from Agency rec              
Array    LKOUT C,046,(A,ARYAGLL)   Extracing data from LIAB Comment rec         
Array    LKOUT C,092,(A,ARYACOV)   Extracing data from Coversheet rec           
                                                                                
Array    LKOUT C,R#DLBUY,(A,ARYBUY)                                             
                                                                                
PRout    LKOUT P,,PRCBBUC                                                       
Array    LKOUT C,R#BUYTOT,(A,ARYBBK),FILTROUT=TSTBBK                            
                                                                                
         LKOUT E                                                                
                                                                                
TSTBBK   CLI   WKBBKSW,YESQ        Have buy bucket to reply?                    
         BR    RE                                                               
                                                                                
TSTCBL   CLI   WKCBLSW,YESQ        Filtering on cancelled buy line?             
         BR    RE                                                               
                                                                                
ARYBBK   LKOUT A,(D,B#SAVED,SAVED),NEWEL=Y,NROWS=1                              
PRout    LKOUT P,,SETBBK1                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+000+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+000+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+000+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBK2                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+014+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+014+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+014+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBK3                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+028+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+028+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+028+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBK4                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+042+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+042+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+042+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBK5                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+056+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+056+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+056+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBK6                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+070+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+070+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+070+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBK7                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+084+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+084+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+084+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBK8                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+098+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+098+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+098+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBK9                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+112+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+112+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+112+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBKA                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+126+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+126+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+126+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBKB                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+140+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+140+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+140+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBKC                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+154+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+154+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+154+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
PRout    LKOUT P,,SETBBKD                                                       
SptMonYr LKOUT C,001,(D,B#SAVED,BYTOTBDA+168+002),BDAT,LEN=3,          +        
               FILTROUT=TSTBMB                                                  
SptMonCn LKOUT C,002,(D,B#SAVED,BYTOTBDA+168+010),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
SptMonAm LKOUT C,006,(D,B#SAVED,BYTOTBDA+168+006),CBIN,LEN=4,          +        
               FILTROUT=TSTBMB                                                  
*                                                                               
BuyGTSpt LKOUT C,003,(D,B#SAVED,BYGTBSPT),UBIN                                  
BuyGTAmt LKOUT C,004,(D,B#SAVED,BYGTBAMT),CBIN                                  
BuyGTGRP LKOUT C,005,(D,B#SAVED,BYGTGRPS),(R,EDTGGRP),ND=Y                      
         LKOUT E                                                                
*                                                                               
TSTBMB   CLI   BUYBUKSW,YESQ       Send monthly buy bucket?                     
         BR    RE                                                               
*                                                                               
ARYCDSP  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONCODE,X'01'),    +        
               ROWWIDTH=(V,RCONELLN)                                            
StrDate  LKOUT C,024,RCONDATE+0,BDAT,LEN=3,ND=Y                                 
EndDate  LKOUT C,025,RCONDATE+3,BDAT,LEN=3,ND=Y                                 
ConType  LKOUT C,026,RCONTYPE,CHAR,ND=Y                                         
ConProd  LKOUT C,052,RCONPRD,CHAR                                               
ConSal   LKOUT C,054,RCONSAL,CHAR,ND=Y                                          
AgyBuyr  LKOUT C,059,RCONBUYR,CHAR,ND=Y                                         
ModVer#  LKOUT C,069,RCONMOD,UBIN                                               
         LKOUT E                                                                
*                                                                               
ARYCCME  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONCMCO,X'02'),    +        
               ROWWIDTH=(V,RCONCMLN)                                            
ConComm  LKOUT C,087,RCONCMCO,(R,EDTCCOM),ND=Y                                  
EmbCom1  LKOUT C,087,(D,B#EBC,EMBCOMM1),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom2  LKOUT C,087,(D,B#EBC,EMBCOMM2),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom3  LKOUT C,087,(D,B#EBC,EMBCOMM3),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom4  LKOUT C,087,(D,B#EBC,EMBCOMM4),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom5  LKOUT C,087,(D,B#EBC,EMBCOMM5),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom6  LKOUT C,087,(D,B#EBC,EMBCOMM6),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom7  LKOUT C,087,(D,B#EBC,EMBCOMM7),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom8  LKOUT C,087,(D,B#EBC,EMBCOMM8),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom9  LKOUT C,087,(D,B#EBC,EMBCOMM9),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComA  LKOUT C,087,(D,B#EBC,EMBCOMMA),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComB  LKOUT C,087,(D,B#EBC,EMBCOMMB),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComC  LKOUT C,087,(D,B#EBC,EMBCOMMC),CHAR,ND=Y,FILTROUT=TSTEMB               
         LKOUT E                                                                
*                                                                               
ARYCFCM  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONCODE,X'01'),    +        
               ROWWIDTH=(V,RCONELLN)                                            
ConComm  LKOUT C,102,RCONCODE,(R,EDTCFCM),ND=Y                                  
EmbCom1  LKOUT C,102,(D,B#EBC,EMBCOMM1),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom2  LKOUT C,102,(D,B#EBC,EMBCOMM2),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom3  LKOUT C,102,(D,B#EBC,EMBCOMM3),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom4  LKOUT C,102,(D,B#EBC,EMBCOMM4),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom5  LKOUT C,102,(D,B#EBC,EMBCOMM5),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom6  LKOUT C,102,(D,B#EBC,EMBCOMM6),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom7  LKOUT C,102,(D,B#EBC,EMBCOMM7),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom8  LKOUT C,102,(D,B#EBC,EMBCOMM8),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom9  LKOUT C,102,(D,B#EBC,EMBCOMM9),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComA  LKOUT C,102,(D,B#EBC,EMBCOMMA),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComB  LKOUT C,102,(D,B#EBC,EMBCOMMB),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComC  LKOUT C,102,(D,B#EBC,EMBCOMMC),CHAR,ND=Y,FILTROUT=TSTEMB               
         LKOUT E                                                                
*                                                                               
TSTEMB   CLI   EMBCOMSW,YESQ       Have embedded comment to process?            
         BR    RE                                                               
*                                                                               
                                                                                
ARYCBKE  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONBKCO,X'03'),    +        
               ROWWIDTH=(V,RCONBKLN)                                            
MonBukD  LKOUT C,075,RCONBKYR,(R,EDTMBDT),ND=Y                                  
MonBukD  LKOUT C,075,(D,B#SAVED,SVBKDATE),BDAT                                  
BukMon$  LKOUT C,076,RCONBKAM,CBIN                                              
         LKOUT E                                                                
*                                                                               
ARYCBKT  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONBKCO,X'63'),    +        
               ROWWIDTH=(V,RCONBKLN)                                            
MonBukD  LKOUT C,075,RCONBKYR,(R,EDTMBDT),ND=Y                                  
MonBukD  LKOUT C,075,(D,B#SAVED,SVBKDATE),BDAT                                  
BukMon$  LKOUT C,076,RCONBKAM,CBIN                                              
         LKOUT E                                                                
*                                                                               
ARYCSPL  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONSPCO,X'06'),    +        
               ROWWIDTH=(V,RCONSPLN)                                            
PRout    LKOUT P,,SETSPLV                                                       
SPLSta   LKOUT C,079,RCONSPST,CHAR                                              
SPLShr   LKOUT C,080,RCONSPAM,(R,EDTSPL$)                                       
SPLPct   LKOUT C,081,RCONSPAM,(R,EDTSPCT)                                       
Array    LKOUT C,082,(A,ARY_SPL)                                                
SPLTot   LKOUT C,085,(D,B#SAVED,SVSPLTOT),(R,EDTSPLT)                           
         LKOUT E                                                                
*                                                                               
ARY_SPL  LKOUT A,(I,B#SAVED,SVSPLSTR),NROWS=(B#SAVED,SVNUMSPL),        +        
               ROWNAME=RCONSPST,ROWWIDTH=L'RCONSPST+L'RCONSPAM                  
StaCLet  LKOUT C,082,RCONSPST,CHAR                                              
SharAmt  LKOUT C,083,RCONSPAM,(R,EDTSPL$)                                       
SharPct  LKOUT C,084,RCONSPAM,(R,EDTSPCT)                                       
         LKOUT E                                                                
*                                                                               
ARYCSPC  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONSMCO,X'07'),    +        
               ROWWIDTH=(V,RCONSMLN)                                            
SPLComm  LKOUT C,086,RCONSMNT,CHAR,LEN=V                                        
         LKOUT E                                                                
*                                                                               
ARYCSA1  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RSARXCO,X'12'),     +        
               ROWWIDTH=(V,RSARXLEN)                                            
SARBook  LKOUT C,090,RSARXBKS,(R,EDTBOOK)                                       
SARDemo  LKOUT C,091,RSARXDEM,(R,EDTDEMO)                                       
         LKOUT E                                                                
*                                                                               
ARYCECE  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONECCO,X'15'),    +        
               ROWWIDTH=(V,RCONECLN)                                            
ECSndDt  LKOUT C,077,RCONECDT,CDAT                                              
ECSndTm  LKOUT C,078,RCONECTM,HEXD                                              
         LKOUT E                                                                
*                                                                               
ARYCDAO  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONDRCD,X'1D'),    +        
               ROWWIDTH=(V,RCONDRLN)                                            
AgDarO#  LKOUT C,015,RCONDRLK,(R,EDTCON#)                                       
         LKOUT E                                                                
*                                                                               
ARYCRFE  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONRFCD,X'1E'),    +        
               ROWWIDTH=(V,RCONRFLN)                                            
TradeFl  LKOUT C,074,RCONRF1,(R,EDTTRFG)                                        
         LKOUT E                                                                
*                                                                               
ARYCXDE  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONXEL,X'1F'),     +        
               ROWWIDTH=(V,RCONXEL+1)                                           
TrfOrd#  LKOUT C,016,RCONTRF,CHAR                                               
OrdrTot  LKOUT C,073,RCONTOT,CBIN                                               
* * * *  LKOUT C,073,RCONXEL,(R,EDTCTOT)                                        
         LKOUT E                                                                
*                                                                               
ARYCSND  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONSNCO,X'20'),    +        
               ROWWIDTH=(V,RCONSNLN)                                            
RepVer#  LKOUT C,067,RCONSRV,UBIN                                               
StaVer#  LKOUT C,068,RCONSSV,UBIN                                               
LstSnBy  LKOUT C,072,RCONSENF,(R,EDTLSBY)                                       
LstSndt  LKOUT C,070,(D,B#SAVED,WKSNDDAT),CDAT                                  
LstSnTm  LKOUT C,071,(D,B#SAVED,WKSNDTIM),CHAR                                  
         LKOUT E                                                                
*                                                                               
ARYCROC  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONROCO,X'82'),    +        
               ROWWIDTH=(V,RCONROCO+1)                                          
RepOrdC  LKOUT C,088,RCONROCO,(R,EDTCCOM),ND=Y                                  
EmbCom1  LKOUT C,088,(D,B#EBC,EMBCOMM1),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom2  LKOUT C,088,(D,B#EBC,EMBCOMM2),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom3  LKOUT C,088,(D,B#EBC,EMBCOMM3),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom4  LKOUT C,088,(D,B#EBC,EMBCOMM4),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom5  LKOUT C,088,(D,B#EBC,EMBCOMM5),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom6  LKOUT C,088,(D,B#EBC,EMBCOMM6),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom7  LKOUT C,088,(D,B#EBC,EMBCOMM7),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom8  LKOUT C,088,(D,B#EBC,EMBCOMM8),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom9  LKOUT C,088,(D,B#EBC,EMBCOMM9),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComA  LKOUT C,088,(D,B#EBC,EMBCOMMA),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComB  LKOUT C,088,(D,B#EBC,EMBCOMMB),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComC  LKOUT C,088,(D,B#EBC,EMBCOMMC),CHAR,ND=Y,FILTROUT=TSTEMB               
         LKOUT E                                                                
*                                                                               
ARYSROC  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONSOCO,X'92'),    +        
               ROWWIDTH=(V,RCONSOCO+1)                                          
StaOrdC  LKOUT C,089,RCONSOCO,(R,EDTCCOM),ND=Y                                  
EmbCom1  LKOUT C,089,(D,B#EBC,EMBCOMM1),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom2  LKOUT C,089,(D,B#EBC,EMBCOMM2),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom3  LKOUT C,089,(D,B#EBC,EMBCOMM3),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom4  LKOUT C,089,(D,B#EBC,EMBCOMM4),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom5  LKOUT C,089,(D,B#EBC,EMBCOMM5),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom6  LKOUT C,089,(D,B#EBC,EMBCOMM6),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom7  LKOUT C,089,(D,B#EBC,EMBCOMM7),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom8  LKOUT C,089,(D,B#EBC,EMBCOMM8),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom9  LKOUT C,089,(D,B#EBC,EMBCOMM9),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComA  LKOUT C,089,(D,B#EBC,EMBCOMMA),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComB  LKOUT C,089,(D,B#EBC,EMBCOMMB),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComC  LKOUT C,089,(D,B#EBC,EMBCOMMC),CHAR,ND=Y,FILTROUT=TSTEMB               
         LKOUT E                                                                
*                                                                               
ARYCXDS  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONXXEL,X'9F'),    +        
               ROWWIDTH=(V,RCONXXEL+1)                                          
SalAsNm  LKOUT C,058,RCONXAST,CHAR                                              
         LKOUT E                                                                
*                                                                               
ARYCEAS  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONICD,X'A2'),     +        
               ROWWIDTH=(V,RCONILN)                                             
AdvCode  LKOUT C,060,RCONIADV,CHAR                                              
AdvPrd1  LKOUT C,061,RCONIPRD,CHAR                                              
AdvPrd2  LKOUT C,062,RCONIPR2,CHAR                                              
AdvEstm  LKOUT C,063,RCONIEST,(R,EDTEST)                                        
         LKOUT E                                                                
*                                                                               
ARYAGYN  LKOUT A,(R,NXTAGY),ROWNAME=RAGYRECD                                    
Array    LKOUT C,037,(A,ARYAGNM)                                                
Array    LKOUT C,038,(A,ARYAGAE)                                                
Array    LKOUT C,044,(A,ARYAGCM)                                                
         LKOUT E                                                                
*                                                                               
ARYAGNM  LKOUT A,(D,B#AGY,RAGY2FXE),EOT=EOR,ROWID=(RAG2CODE,X'1F'),    +        
               ROWWIDTH=(V,RAG2ELLN)                                            
AgyNam   LKOUT C,037,RAG2NAM2,CHAR                                              
AgyCRk   LKOUT C,045,RAG2RISK,(R,EDTRSK)                                        
         LKOUT E                                                                
*                                                                               
ARYAGAE  LKOUT A,(D,B#AGY,RAGY2FXE),EOT=EOR,ROWID=(RAGY2AC1,X'20'),    +        
               ROWWIDTH=(V,RAGY2AX1)                                            
AgyAd1   LKOUT C,038,RAGY2AD1,CHAR                                              
AgyAd2   LKOUT C,039,RAGY2AD2,CHAR                                              
AgyCty   LKOUT C,040,RAGY2CTY,CHAR                                              
AgySta   LKOUT C,041,RAGY2STE,CHAR                                              
AgyZip   LKOUT C,042,RAGY2ZIP,CHAR                                              
         LKOUT E                                                                
*                                                                               
ARYAGCM  LKOUT A,(D,B#AGY,RAGY2FXE),EOT=EOR,ROWID=(RAGY2CCD,X'40'),    +        
               ROWWIDTH=(V,RAGY2CLN)                                            
AgyComm  LKOUT C,044,RAGY2CCD,(R,EDTCCOM),ND=Y                                  
EmbCom1  LKOUT C,044,(D,B#EBC,EMBCOMM1),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom2  LKOUT C,044,(D,B#EBC,EMBCOMM2),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom3  LKOUT C,044,(D,B#EBC,EMBCOMM3),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom4  LKOUT C,044,(D,B#EBC,EMBCOMM4),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom5  LKOUT C,044,(D,B#EBC,EMBCOMM5),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom6  LKOUT C,044,(D,B#EBC,EMBCOMM6),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom7  LKOUT C,044,(D,B#EBC,EMBCOMM7),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom8  LKOUT C,044,(D,B#EBC,EMBCOMM8),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom9  LKOUT C,044,(D,B#EBC,EMBCOMM9),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComA  LKOUT C,044,(D,B#EBC,EMBCOMMA),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComB  LKOUT C,044,(D,B#EBC,EMBCOMMB),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComC  LKOUT C,044,(D,B#EBC,EMBCOMMC),CHAR,ND=Y,FILTROUT=TSTEMB               
         LKOUT E                                                                
*                                                                               
ARYAGLL  LKOUT A,(R,NXTCMT),ROWNAME=RCMTRECD                                    
Array    LKOUT C,046,(A,ARYLIAB)                                                
         LKOUT E                                                                
*                                                                               
ARYLIAB  LKOUT A,(D,B#CMT,RCMTELM1),EOT=EOR,ROWID=(RCMTELM2,X'02'),    +        
               ROWWIDTH=(V,RCMT2LEN)                                            
AgyCTxt  LKOUT C,046,RCMT2TXT,CHAR,LEN=V                                        
         LKOUT E                                                                
*                                                                               
ARYACOV  LKOUT A,(R,NXTCOV),ROWNAME=RCOVRECD                                    
Array    LKOUT C,092,(A,ARYCVCM)                                                
         LKOUT E                                                                
*                                                                               
ARYCVCM  LKOUT A,(D,B#COV,RCOVEL1),EOT=EOR,ROWID=(RCOVTXEL,X'03'),     +        
               ROWWIDTH=(V,RCOVTXLN)                                            
CvCmTxt  LKOUT C,092,RCOVTEXT,CHAR,LEN=V                                        
         LKOUT E                                                                
*                                                                               
                                                                                
EDTWIPS  LM    R2,R4,LP_AINP                                                    
         BRAS  RE,CKWIPS           Check for WIP status                         
         MVC   0(1,R4),BYTE1                                                    
SETOLONE LHI   R0,1                                                             
         J     SETOLENX                                                         
*                                                                               
         USING RCONSENF,R2                                                      
EDTLSBY  LM    R2,R4,LP_AINP                                                    
         XC    WKSNDDAT,WKSNDDAT   Init last send date                          
         MVC   WKSNDTIM,SPACES     Init last send time                          
         MVI   0(R4),C'N'          Default to "Never"                           
         TM    RCONSENF,X'80'                                                   
         JZ    *+20                                                             
         MVI   0(R4),C'R'          Set to "Send by Rep"                         
         MVC   WKSNDDAT,RCONSRDT                                                
         MVC   WKSNDTIM,RCONSRTI                                                
         TM    0(R2),X'40'                                                      
         JZ    *+20                                                             
         MVI   0(R4),C'S'          Set to "Send by Station"                     
         MVC   WKSNDDAT,RCONSSDT                                                
         MVC   WKSNDTIM,RCONSSTI                                                
         CLI   0(R4),C'R'          Already last send by Rep?                    
         JE    SETOLONE                                                         
         CLI   0(R4),C'S'          Already last send by Station?                
         JE    SETOLONE                                                         
         TM    0(R2),X'02'+X'01'                                                
         JZ    *+8                                                              
         MVI   0(R4),C'C'          Set to "Confirmed" (by Rep or Sta)           
         J     SETOLONE                                                         
         DROP  R2                                                               
*                                                                               
EDTTRFG  LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),C'0'          Set to false                                 
         TM    0(R2),X'08'                                                      
         JZ    *+8                                                              
         MVI   0(R4),C'1'          Set to true                                  
         J     SETOLONE                                                         
*                                                                               
         USING RCONXEL,R2                                                       
EDTCTOT  LM    R2,R4,LP_AINP                                                    
         TM    RCONCONF,X'40'+X'20'                                             
         JZ    *+10                                                             
         XC    RCONTOT,RCONTOT                                                  
         EDIT  (B4,RCONTOT),(10,0(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                
         J     SETOLENX                                                         
         DROP  R2                                                               
*                                                                               
EDTMBDT  LM    R2,R4,LP_AINP       Set bucket date                              
         XC    SVBKDATE,SVBKDATE                                                
         MVC   SVBKDATE+0(2),0(R2) Year and month                               
         MVI   SVBKDATE+2,15       Set day to middle of the month               
         J     XCOLEN                                                           
*                                                                               
         USING RCONSPAM,R2                                                      
EDTSPL$  LM    R2,R4,LP_AINP                                                    
         OC    RCONSPAM,RCONSPAM   Have percentage?                             
         JZ    XCOLEN                                                           
         ICM   R5,15,RCONSPAM                                                   
         SR    RE,RE                                                            
         ICM   RF,15,SVSPLTOT      Competitive market total                     
         MR    RE,R5               Multiple by percent                          
         SLDA  RE,1                Double foe rounding                          
         A     RF,=F'10000'        Add for rounding                             
         D     RE,=F'10000'        Divide for decimal scaling                   
         SRA   RF,1                Divide by 2                                  
         LR    R5,RF                                                            
EDTSPL$K EDIT  (R5),(10,0(R4)),2,ALIGN=LEFT                                     
         J     SETOLENX                                                         
         DROP  R2                                                               
*                                                                               
EDTSPLT  LM    R2,R4,LP_AINP                                                    
         OC    SVSPLTOT,SVSPLTOT   Have Competitive Market Total?               
         JZ    XCOLEN                                                           
         ICM   R5,15,SVSPLTOT                                                   
         J     EDTSPL$K                                                         
*                                                                               
         USING RCONSPAM,R2                                                      
EDTSPCT  LM    R2,R4,LP_AINP                                                    
         MVC   0(8,R4),SPACES                                                   
         EDIT  RCONSPAM,(7,0(R4)),2,ALIGN=LEFT                                  
         LA    RE,0(R4)                                                         
         AR    RE,R0               Point to end of output field                 
         MVI   0(RE),C'%'                                                       
         AHI   R0,1                                                             
         J     SETOLENX                                                         
         DROP  R2                                                               
*                                                                               
EDTGGRP  LM    R2,R4,LP_AINP                                                    
         EDIT  BYGTGRPS,(8,0(R4)),1,ALIGN=LEFT,ZERO=NOBLANK                     
         AHI   R0,8                                                             
         J     SETOLENX                                                         
*                                                                               
         USING RCONIEST,R2                                                      
EDTEST   LM    R2,R4,LP_AINP                                                    
         MVC   0(L'RCONXEST,R4),SPACES                                          
         CLC   RCONIEST,SPACES     Have estimate?                               
         JNH   *+14                                                             
         MVC   0(L'RCONIEST,R4),RCONIEST                                        
         J     *+10                                                             
         MVC   0(L'RCONXEST,R4),RCONXEST                                        
         LHI   R0,L'RCONXEST                                                    
         J     SETOLENX                                                         
         DROP  R2                                                               
*                                                                               
EDTRSK   LM    R2,R4,LP_AINP                                                    
         LLC   RF,0(R2)            Risk code                                    
         BCTR  RF,0                                                             
         MHI   RF,L'RISKTAB        Risk text displacement in table              
         LA    RE,RISKTAB          Point to risk text table                     
         AR    RE,RF                                                            
         MVC   0(L'RISKTAB,R4),0(RE)                                            
         LHI   R0,L'RISKTAB                                                     
         J     SETOLENX                                                         
*                                                                               
         USING RCONCMEL,R2                                                      
EDTCCOM  LM    R2,R4,LP_AINP                                                    
         MVI   EMBCOMSW,NOQ                                                     
         MVI   WKCMTYPE,0                                                       
         MVC   SVEMBCMC,SPACES                                                  
         CLI   0(R2),X'04'         Buy comment?                                 
         JE    EDTCCO10            Don't expand for buy comments                
         LA    R0,L'EMBCOMMN                                                    
         MVI   WKCMTYPE,RCMTKIDQ   Retrieve to comment record                   
         CLC   EMBCOMMN,RCONCMNT   Embedded comment?                            
         JE    EDTCCO05                                                         
         LA    R0,L'EMBSCCMN                                                    
         MVI   WKCMTYPE,ROCMKIDQ   Retrieve to office comment record            
         CLC   EMBSCCMN,RCONCMNT   Embedded SC comment?                         
         JNE   EDTCCO10                                                         
EDTCCO05 LA    R1,RCONCMNT                                                      
         AR    R1,R0               point to comment code                        
         LLC   RE,RCONCMLN                                                      
         AHI   RE,-(RCONCMNT-RCONCMEL)                                          
         SR    RE,R0               Adjust comment notation length               
         BCTR  RE,0                                                             
         BASR  RF,0                                                             
         MVC   SVEMBCMC(0),0(R1)                                                
         EX    RE,0(RF)                                                         
         BRAS  RE,GETCMT                                                        
         J     XCOLEN                                                           
*                                                                               
EDTCCO10 CLI   0(R2),X'04'         Buy comment?                                 
         JNE   *+14                                                             
         CLC   SPCMGCMN,RCONCMNT  Supressing MG= comment?                       
         JE    XCOLEN                                                           
         LLC   RE,RCONCMLN                                                      
         AHI   RE,-(RCONCMNT-RCONCMEL)                                          
         BCTR  RE,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R4),RCONCMNT                                                 
         EX    RE,0(RF)                                                         
         AHI   RE,1                                                             
         LR    R0,RE               Length of comment                            
         J     SETOLENX                                                         
*                                                                               
EDTCFCM  LM    R2,R4,LP_AINP                                                    
         MVI   EMBCOMSW,NOQ                                                     
         MVI   WKCMTYPE,RCFCKTYQ   Retrieve confirmation comment                
         BRAS  RE,GETCMT                                                        
         J     XCOLEN                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Array definition for Pending Station Call Letter                    *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
ARYPST   LKOUT A,(R,NXTPST),MULTIROW=Y,ROWNAME=RCONRECD                         
Array    LKOUT C,R#DLPCO,(A,ARYPCO)                                             
         LKOUT E                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Array definition for Pending Contract record download               *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
ARYPCO   LKOUT A,(R,NXTPCO),MULTIROW=Y,ROWNAME=RCONRECD                         
SCalLe   LKOUT C,001,RCONKSTA,CHAR,ND=Y                                         
ConCon   LKOUT C,002,RCONKCON,(R,EDTCON#)                                       
Array    LKOUT C,020,(A,ARYCDS2)                                                
Array    LKOUT C,040,(A,ARYCPCM)                                                
Array    LKOUT C,050,(A,ARYCSAR)                                                
AgyNam   LKOUT C,070,(D,B#SAVED,SVAGYNAM),CHAR                                  
ADVNam   LKOUT C,080,(D,B#SAVED,SVADVNAM),CHAR                                  
OFFNam   LKOUT C,090,(D,B#SAVED,SVOFFNAM),CHAR                                  
PRDNam   LKOUT C,100,(D,B#SAVED,SVPRDNAM),CHAR                                  
SALNam   LKOUT C,110,(D,B#SAVED,SVSALNAM),CHAR                                  
CTYNam   LKOUT C,120,(D,B#SAVED,SVCTYNAM),CHAR                                  
         LKOUT E                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ARYSDSP  LKOUT A,(D,B#STA,RSTAELEM),EOT=EOR,ROWID=(RSTACODE,X'01'),    +        
               ROWWIDTH=(V,RSTAELLN)                                            
StaMNam  LKOUT C,020,RSTAMKT,CHAR,ND=Y                                          
JoinDat  LKOUT C,024,RSTASTRT,BDAT,ND=Y                                         
LeavDat  LKOUT C,025,RSTAEND,BDAT,ND=Y                                          
StaProf  LKOUT C,026,RSTAPROF,CHAR,ND=Y                                         
StaOpt6  LKOUT C,027,RSTAP6,CHAR                                                
StaOpt7  LKOUT C,028,RSTAP7,CHAR                                                
Status   LKOUT C,029,RSTASTAT,(R,EDTSSTA)                                       
         LKOUT E                                                                
*                                                                               
EDTSSTA  LM    R2,R4,LP_AINP                                                    
         XC    0(40,R4),0(R4)      Init STATUS output                           
         LR    RE,R4                                                            
         TM    0(R2),X'80'         BOP=N?                                       
         JZ    *+14                                                             
         MVC   0(06,R4),=C'BOP=N,'                                              
         LA    R4,06(R4)                                                        
         TM    0(R2),X'40'         CONTRACT=N?                                  
         JZ    *+14                                                             
         MVC   0(11,R4),=C'CONTRACT=N,'                                         
         LA    R4,11(R4)                                                        
         TM    0(R2),X'10'         BUDGET=Y?                                    
         JZ    *+14                                                             
         MVC   0(11,R4),=C'BUDGET=YES,'                                         
         LA    R4,11(R4)                                                        
         OC    0(40,RE),0(RE)      Have STATUS output?                          
         JZ    XCOLEN                                                           
         BCTR  R4,0                                                             
         MVI   0(R4),0             Remove last commas                           
         LHI   R0,40                                                            
         J     SETOLENX                                                         
*                                                                               
ARYCDS2  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONCODE,X'01'),    +        
               ROWWIDTH=(V,RCONELLN)                                            
StrDate  LKOUT C,020,RCONDATE+0,BDAT,LEN=3,ND=Y                                 
EndDate  LKOUT C,021,RCONDATE+3,BDAT,LEN=3,ND=Y                                 
HdrCrDt  LKOUT C,022,RCONHDRD,BDAT,ND=Y                                         
AgyBuyr  LKOUT C,023,RCONBUYR,CHAR,ND=Y                                         
         LKOUT E                                                                
*                                                                               
ARYCPCM  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RCONBCCO,X'11'),    +        
               ROWWIDTH=(V,RCONBCLN)                                            
BOPComm  LKOUT C,040,RCONBCOM,CHAR,LEN=V                                        
         LKOUT E                                                                
*                                                                               
ARYCSAR  LKOUT A,(D,B#CON,RCONELEM),EOT=EOR,ROWID=(RSARXCO,X'12'),     +        
               ROWWIDTH=(V,RSARXLEN)                                            
MktBudg  LKOUT C,050,RSARXBGT,CBIN                                              
ShrGoal  LKOUT C,051,RSARXSHG,UBIN                                              
PriDemo  LKOUT C,052,RSARXDEM,(R,EDTDEMO)                                       
         LKOUT E                                                                
*                                                                               
ARYBUY   LKOUT A,(R,NXTBUY),MULTIROW=Y,ROWNAME=RBUYRECD                         
MstNumb  LKOUT C,010,RBUYKMLN,UBIN                                              
LinNumb  LKOUT C,011,RBUYKLIN,UBIN                                              
PRout    LKOUT P,,SETBUYVS                                                      
AbStrDt  LKOUT C,030,(D,B#SAVED,ABSTRDAT),BDAT,ND=Y                             
AbEndDt  LKOUT C,031,(D,B#SAVED,ABENDDAT),BDAT,ND=Y                             
Array    LKOUT C,020,(A,ARYBDSP)                                                
Array    LKOUT C,032,(A,ARYBCMM)                                                
Array    LKOUT C,033,(A,ARYPRGN)                                                
Array    LKOUT C,034,(A,ARYORDC)                                                
*                                                                               
DemoVal  LKOUT C,041,(D,B#SAVED,SVBYDEMV),(R,EDTDEMV),ND=Y                      
BuyLGRP  LKOUT C,044,(D,B#SAVED,BUYLNGRP),CHAR,ND=Y                             
*                                                                               
PRout    LKOUT P,,DARCMNT                                                       
DARCom1  LKOUT C,045,(D,B#DAC,DARSDCM1),CHAR,ND=Y                               
DARCom2  LKOUT C,045,(D,B#DAC,DARSDCM2),CHAR,ND=Y                               
DARCom3  LKOUT C,045,(D,B#DAC,DARSDCM3),CHAR,ND=Y                               
DARCom4  LKOUT C,045,(D,B#DAC,DARSDCM4),CHAR,ND=Y                               
DARCom5  LKOUT C,045,(D,B#DAC,DARSDCM5),CHAR,ND=Y                               
DARCom6  LKOUT C,045,(D,B#DAC,DARSDCM6),CHAR,ND=Y                               
*                                                                               
PRout    LKOUT P,,MGOCMNT                                                       
Array    LKOUT C,000,(A,ARYMGCM)                                                
*                                                                               
Array    LKOUT C,R#BUYX02,(A,ARYDYTM)                                           
Array    LKOUT C,R#BUYX03,(A,ARYEFDT)                                           
Array    LKOUT C,R#BUYXD0,(A,ARYMODC)                                           
         LKOUT E                                                                
*                                                                               
ARYBDSP  LKOUT A,(D,B#BUY,RBUYELEM),EOT=EOR,ROWID=(RBUYCODE,X'01'),    +        
               ROWWIDTH=(V,RBUYELLN)                                            
BuyCost  LKOUT C,020,RBUYCOS,CBIN,ND=Y                                          
TotSpot  LKOUT C,021,RBUYTSPT,CBIN,ND=Y,FILTROUT=TSTCBL                         
TotCost  LKOUT C,022,RBUYTCOS,CBIN,ND=Y,FILTROUT=TSTCBL                         
TotWeek  LKOUT C,023,RBUYTWKS,UBIN,ND=Y,FILTROUT=TSTCBL                         
StrDay_  LKOUT C,024,RBUYSTED,(R,EDTSDAY)                                       
EndDay_  LKOUT C,025,RBUYSTED,(R,EDTEDAY),ND=Y                                  
LenIsM?  LKOUT C,026,RBUYDUR,(R,EDTISMN),ND=Y                                   
Length_  LKOUT C,027,RBUYDUR,(R,EDTLENG),ND=Y                                   
DayPtCd  LKOUT C,028,RBUYDPT,CHAR,ND=Y                                          
BuyIsDy  LKOUT C,029,RBUYFLG2,(R,EDTBISD),ND=Y                                  
BuyChgI  LKOUT C,035,RBUYCHGI,CHAR                                              
FlghtCd  LKOUT C,037,RBUYFLT,CHAR,ND=Y                                          
SpotPWk  LKOUT C,038,RBUYNW,UBIN,ND=Y,FILTROUT=TSTCBL                           
BuyTrad  LKOUT C,039,RBUYFLG2,(R,EDTBTRD),ND=Y                                  
VernNum  LKOUT C,040,RBUYVER,UBIN,ND=Y                                          
         LKOUT E                                                                
*                                                                               
ARYBEDT  LKOUT A,(D,B#BUY,RBUYELEM),EOT=EOR,ROWID=(RBUYDTCD,X'03'),    +        
               ROWWIDTH=(V,RBUYDTLN)                                            
BStrDat  LKOUT C,999,RBUYDTST,BDAT,ND=Y                                         
BEndDat  LKOUT C,999,RBUYDTED,BDAT,ND=Y                                         
         LKOUT E                                                                
*                                                                               
ARYBCMM  LKOUT A,(D,B#BUY,RBUYELEM),EOT=EOR,ROWID=(RBUYCMCD,X'04'),    +        
               ROWWIDTH=(V,RBUYCMLN)                                            
BuyComm  LKOUT C,032,RBUYCMEL,(R,EDTCCOM),ND=Y                                  
EmbCom1  LKOUT C,032,(D,B#EBC,EMBCOMM1),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom2  LKOUT C,032,(D,B#EBC,EMBCOMM2),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom3  LKOUT C,032,(D,B#EBC,EMBCOMM3),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom4  LKOUT C,032,(D,B#EBC,EMBCOMM4),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom5  LKOUT C,032,(D,B#EBC,EMBCOMM5),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom6  LKOUT C,032,(D,B#EBC,EMBCOMM6),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom7  LKOUT C,032,(D,B#EBC,EMBCOMM7),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom8  LKOUT C,032,(D,B#EBC,EMBCOMM8),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbCom9  LKOUT C,032,(D,B#EBC,EMBCOMM9),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComA  LKOUT C,032,(D,B#EBC,EMBCOMMA),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComB  LKOUT C,032,(D,B#EBC,EMBCOMMB),CHAR,ND=Y,FILTROUT=TSTEMB               
EmbComC  LKOUT C,032,(D,B#EBC,EMBCOMMC),CHAR,ND=Y,FILTROUT=TSTEMB               
         LKOUT E                                                                
*                                                                               
ARYPRGN  LKOUT A,(D,B#BUY,RBUYELEM),EOT=EOR,ROWID=(RBUYPGCD,RBUYPGEQ), +        
               ROWWIDTH=(V,RBUYPGLN)                                            
BPrgNam  LKOUT C,033,RBUYPGM,CHAR,LEN=V                                         
         LKOUT E                                                                
*                                                                               
ARYORDC  LKOUT A,(D,B#BUY,RBUYELEM),EOT=EOR,ROWID=(RBUYOCCD,X'84'),    +        
               ROWWIDTH=(V,RBUYOCLN)                                            
BOrdCom  LKOUT C,034,RBUYOCNT,CHAR,LEN=V                                        
         LKOUT E                                                                
*                                                                               
ARYMGCM  LKOUT A,(R,NXTMGCM),MULTIROW=Y,ROWNAME=RMKGDCMD                        
MkGdCom1 LKOUT C,042,(D,B#WORKD,ELEM2),CHAR,LEN=100,ND=Y                        
MkGdCom2 LKOUT C,043,(D,B#WORKD,ELEM),CHAR,LEN=100,ND=Y                         
         LKOUT E                                                                
*                                                                               
ARYDYTM  LKOUT A,(R,NXTDYTM),MULTIROW=Y,ROWNAME=RBUYRECD                        
StrDay   LKOUT C,010,(D,B#SAVED,X02STRDY),UBIN,ND=Y                             
EndDay   LKOUT C,011,(D,B#SAVED,X02ENDDY),UBIN,ND=Y                             
Days     LKOUT C,012,(D,B#SAVED,X02DAYS_),CHAR,ND=Y                             
Strtime  LKOUT C,013,(D,B#SAVED,X02STRTM),CHAR,ND=Y                             
Endtime  LKOUT C,014,(D,B#SAVED,X02ENDTM),CHAR,ND=Y                             
DayTmWF  LKOUT C,015,(D,B#SAVED,X02DAYTW),UBIN,ND=Y                             
EndTmCC  LKOUT C,016,(D,B#SAVED,X02TCCSW),CHAR,ND=Y                             
TimeText LKOUT C,017,(D,B#SAVED,X02TMTXT),CHAR,ND=Y                             
         LKOUT E                                                                
*                                                                               
ARYEFDT  LKOUT A,(R,NXTEFDT),MULTIROW=Y,ROWNAME=RBUYRECD                        
StrDate  LKOUT C,010,(D,B#SAVED,X03STRDT),BDAT,ND=Y                             
EndDate  LKOUT C,011,(D,B#SAVED,X03ENDDT),BDAT,ND=Y                             
NumOfWk  LKOUT C,012,(D,B#SAVED,X03#OFWK),UBIN,ND=Y                             
WeekInd  LKOUT C,013,(D,B#SAVED,X03WEKSW),CHAR,ND=Y                             
SpotWek  LKOUT C,014,(D,B#SAVED,X03SPTWK),UBIN,ND=Y,FILTROUT=TSTCBL             
         LKOUT E                                                                
*                                                                               
ARYMODC  LKOUT A,(R,NXTMODC),MULTIROW=Y,ROWNAME=RBUYRECD                        
BModCod  LKOUT C,035,(D,B#SAVED,XD0MODCD),CHAR,ND=Y                             
BModVer  LKOUT C,040,(D,B#SAVED,XD0MODV#),UBIN,ND=Y                             
         LKOUT E                                                                
*                                                                               
EDTCON#  LM    R2,R4,LP_AINP                                                    
         UNPK  0(9,R4),0(5,R2)                                                  
         LHI   R0,8                                                             
         J     SETOLENX                                                         
*                                                                               
EDTBOOK  LM    R2,R4,LP_AINP                                                    
         BRAS  RE,FMTBOOK                                                       
         CLC   0(BKOLNQ,R4),SPACES Have books?                                  
         JE    XCOLEN                                                           
         LHI   R0,BKOLNQ                                                        
         J     SETOLENX                                                         
*                                                                               
EDTDEMO  LM    R2,R4,LP_AINP                                                    
         LA    RE,ELEM2                                                         
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         DROP  RE                                                               
         LA    RE,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   0(L'RSARXDEM,RE),0(R2)                                           
         LA    RF,7                                                             
EDTDEMO6 CLI   1(RE),C'T'          Set input for DEMOCON                        
         JNE   *+8                                                              
         MVI   1(RE),C'I'                                                       
         LA    RE,3(RE)                                                         
         JCT   RF,EDTDEMO6                                                      
         GOTOR VDEMOCON,DMCB,(7,ELEM),(9,0(R4)),(0,ELEM2)                       
         LHI   R0,66                                                            
         J     SETOLENX                                                         
*                                                                               
EDTSDAY  LM    R2,R4,LP_AINP                                                    
         LLC   RF,0(R2)                                                         
         SRL   RF,4                                                             
EDTSDAY6 EDIT  (RF),(1,0(R4))                                                   
         J     SETOLONE                                                         
*                                                                               
EDTEDAY  LM    R2,R4,LP_AINP                                                    
         MVC   BYTE1,0(R2)                                                      
         NI    BYTE1,X'0F'                                                      
         LLC   RF,BYTE1                                                         
         J     EDTSDAY6                                                         
*                                                                               
EDTISMN  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),X'80'         Length is in minutes?                        
         JZ    XCOLEN                                                           
         MVI   0(R4),C'M'          M=Minutes                                    
         J     SETOLONE                                                         
*                                                                               
EDTLENG  LM    R2,R4,LP_AINP                                                    
         MVC   HALF1,0(R2)                                                      
         NI    HALF1,X'0F'         Reset high order flag bits                   
         EDIT  (B2,HALF1),(4,0(R4)),ALIGN=LEFT                                  
         LHI   R0,4                                                             
         J     SETOLENX                                                         
*                                                                               
EDTBISD  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),X'80'         "Daily" order?                               
         JZ    XCOLEN                                                           
         MVI   0(R4),C'D'          D=Daily                                      
         J     SETOLONE                                                         
*                                                                               
EDTBTRD  LM    R2,R4,LP_AINP                                                    
         TM    0(R2),X'02'         Buy is trade?                                
         JZ    XCOLEN                                                           
         MVI   0(R4),C'T'          T=trade                                      
         J     SETOLONE                                                         
*                                                                               
EDTDEMV  LM    R2,R4,LP_AINP                                                    
         OC    0(L'SVBYDEMV,R2),0(R2)                                           
         JZ    XCOLEN                                                           
         CLC   0(L'SVBYDEMV,R2),=X'FFFFFFFF'                                    
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
SETCONF  MVI   SVCONFSW,0                                                       
         L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         USING RCONXEL,R3                                                       
         MVI   ELCODE,X'1F'        Extended description elem                    
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   SETCNF05                                                         
         MVC   SVCONFSW,RCONCONF   Save confirmation switch                     
         DROP  R3                                                               
*                                                                               
SETCNF05 MVI   SVAGYCON,C'M'       Default to manual                            
         L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'1D'        Look for DARE Agency Order elem              
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   SETCNF10                                                         
         USING RCONDREL,R3                                                      
         TM    RCONDRFG,X'80'                                                   
         JZ    *+8                                                              
         MVI   SVAGYCON,C'D'       Set to Dare                                  
         TM    RCONDRF2,X'01'                                                   
         JZ    *+8                                                              
         MVI   SVAGYCON,C'X'       Set to XML                                   
         DROP  R3                                                               
*                                                                               
SETCNF10 MVI   CONWSIDE,C'R'       Deafult to Rep side                          
         MVI   CONSNDSW,NOQ        Init to no SEND element                      
         MVI   SVVERSN#,0          Init version number                          
         L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'20'        Look for Send info elem                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   SETCNF20            Not found, on Rep side                       
         USING RCONSEND,R3                                                      
         MVI   CONSNDSW,YESQ       Yes, SEND elem found                         
         MVC   SVVERSN#,RCONSRV    Set to use Rep's version number              
         CLC   RCONSRV,RCONSSV                                                  
         JNL   SETCNF20                                                         
         MVC   SVVERSN#,RCONSSV    Set to use Sta's version number              
         MVI   CONWSIDE,C'S'       Station side                                 
         DROP  R3                                                               
*                                                                               
SETCNF20 MVC   CONSTATS,SPACES                                                  
         CLI   SVVERSN#,1          Version at 1?                                
         JNE   SETCNF22                                                         
         TM    SVCONFSW,X'40'      Confirmed?                                   
         JNZ   SETCNF23                                                         
         MVC   CONSTATS(L'NEW___TX),NEW___TX                                    
         J     SETCNF30                                                         
*                                                                               
SETCNF22 TM    SVCONFSW,X'40'      Confirmed?                                   
         JZ    SETCNF26                                                         
SETCNF23 MVC   WKIOSAVE,IOVALS     Save current i/o values                      
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING RCFCKEY,R3                                                       
         L     RE,AIO5                                                          
         USING RCONREC,RE                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,RCONKREP                                                
         MVC   RCFCKCON,RCONKCON                                                
         DROP  RE                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         CLC   RCFCKEY,IOKEYSAV    Have partial confirmation comment?           
         JNE   SETCNF24                                                         
         TM    RCFCCNTL-2,X'80'    Deleted?                                     
         JNZ   SETCNF24                                                         
         MVC   CONSTATS(L'PARTCFTX),PARTCFTX                                    
         J     SETCNF25                                                         
SETCNF24 MVC   CONSTATS(L'CONFIRTX),CONFIRTX                                    
SETCNF25 MVC   IOVALS(IOVALL),WKIOSAVE                                          
         J     SETCNF30                                                         
         DROP  R3                                                               
*                                                                               
SETCNF26 CLI   CONWSIDE,C'R'       Rep side?                                    
         JNE   SETCNF28                                                         
         MVC   CONSTATS(L'REVISDTX),REVISDTX                                    
         J     SETCNF30                                                         
SETCNF28 CLI   CONWSIDE,C'S'       Station side?                                
         JNE   SETCNF30                                                         
         MVC   CONSTATS(L'RETURNTX),RETURNTX                                    
*                                                                               
SETCNF30 MVI   SVCPNDSW,C' '       Init Pending status                          
         L     R3,AIO5             Point to contract record                     
         LA    R3,(FRSTELEM)(R3)                                                
         CLI   0(R3),X'01'         Have main contract element?                  
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RCONELEM,R3                                                      
         TM    RCONMODR,X'10'                                                   
         JNZ   *+8                                                              
         MVI   SVCPNDSW,C'P'       Set to Pending                               
         DROP  R3                                                               
*                                                                               
         MVI   ELCODE,X'12'        Look for Expanded SAR elem                   
         BRAS  RE,NXTELEM                                                       
         JNE   SETCNF40                                                         
         USING RSARXEL,R3                                                       
         TM    RSARXFLG,X'08'      Forecast order?                              
         JZ    *+8                                                              
         MVI   SVCPNDSW,C'F'       Set to Forecast order                        
         DROP  R3                                                               
*                                                                               
SETCNF40 DS    0H                                                               
*                                                                               
SETCNFX  J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETSPLV  XC    SVNUMSPL,SVNUMSPL   INIT VALUES FOR MINI ELEM                    
         XC    SVSPLSTR,SVSPLSTR                                                
         XC    SVSPLTOT,SVSPLTOT                                                
*                                                                               
         L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'03'        Look for Contract bucket elem                
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
SETSPL08 BRAS  RE,NXTELEM                                                       
         JNE   SETSPL10                                                         
         USING RCONBKEL,R3                                                      
         ICM   RE,15,SVSPLTOT                                                   
         ICM   RF,15,RCONBKAM                                                   
         AR    RE,RF                                                            
         STCM  RE,15,SVSPLTOT                                                   
         J     SETSPL08                                                         
         DROP  R3                                                               
                                                                                
SETSPL10 L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'06'        Look for SPL elem                            
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   SETSPL_X                                                         
         USING RCONSPEL,R3                                                      
         TM    RCONSPES,X'04'      Data is percentages in mini elems?           
         JZ    SETSPL_X                                                         
         SR    RE,RE                                                            
         IC    RE,RCONSPNU         Number of mini elem                          
         AHI   RE,-1               First one is competitive SPL                 
         STCM  RE,3,SVNUMSPL                                                    
         LA    RE,RCONSPST                                                      
         LA    RE,(L'RCONSPST+L'RCONSPAM)(RE)                                   
         STCM  RE,15,SVSPLSTR      Start of mini elem (after 1st SPL)           
*                                                                               
         OC    RCONSPAM,RCONSPAM   Have Competitive SPL amount?                 
         JZ    SETSPL40                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,SVSPLTOT                                                   
         M     RE,=F'10000'                                                     
         SLDA  RE,1                Double for rounding                          
         A     RF,RCONSPAM         Percent from 1st mini elem                   
         D     RE,RCONSPAM         Divide by percent                            
         SRA   RF,1                Divide by 2                                  
         STCM  RF,15,SVSPLTOT                                                   
         DROP  R3                                                               
*                                                                               
SETSPL40 DS    0H                                                               
*                                                                               
SETSPL_X J     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETNDCM  MVI   EMBCOMSW,NOQ                                                     
         MVI   WKCMTYPE,ROCMKIDQ   Office comment record                        
         MVC   SVEMBCMC,SPACES                                                  
         MVI   SVEMBCMC,C'*'       To get non-discrimination comment            
         BRAS  RE,GETCMT                                                        
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCCBUC  XC    HALF1,HALF1         Process contract bucket                      
         XC    FULL1,FULL1                                                      
         XC    FULL2,FULL2                                                      
         L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         USING RCONBKEL,R3                                                      
         MVI   ELCODE,X'03'        Look for Contract bucket elem                
         CLC   ELCODE,0(R3)                                                     
         JE    PRCCBK06                                                         
         BRAS  RE,NXTELEM                                                       
         JNE   PRCCBK10                                                         
*                                                                               
PRCCBK06 MVC   HALF1,RCONBKYR      Save year and month                          
         MVC   FULL1,RCONBKAM      Save amount                                  
         ST    R3,FULL2            Save element pointer                         
*                                                                               
PRCCBK08 BRAS  RE,NXTELEM                                                       
         JNE   PRCCBK10                                                         
         CLC   HALF1,RCONBKYR      Same year and month?                         
         JNE   PRCCBK06                                                         
         ICM   RE,15,FULL1                                                      
         ICM   RF,15,RCONBKAM                                                   
         AR    RE,RF                                                            
         STCM  RE,15,RCONBKAM      Total up same year/month buckets             
         ST    RE,FULL1                                                         
         L     RF,FULL2            Point to previous contract bucket el         
         MVI   0(RF),X'FF'         Mark it off (not updating record)            
         ST    R3,FULL2                                                         
         J     PRCCBK08                                                         
*                                                                               
PRCCBK10 L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         USING RCONTEEL,R3                                                      
         MVI   ELCODE,X'63'        Look for Contract trade bucket elem          
         CLC   ELCODE,0(R3)                                                     
         JE    PRCCBK06                                                         
         BRAS  RE,NXTELEM                                                       
         JNE   PRCCBK20                                                         
*                                                                               
PRCCBK16 MVC   HALF1,RCONTEYR      Save year and month                          
         MVC   FULL1,RCONTEAM      Save amount                                  
         ST    R3,FULL2            Save element pointer                         
*                                                                               
PRCCBK18 BRAS  RE,NXTELEM                                                       
         JNE   PRCCBK20                                                         
         CLC   HALF1,RCONTEYR      Same year and month?                         
         JNE   PRCCBK16                                                         
         ICM   RE,15,FULL1                                                      
         ICM   RF,15,RCONTEAM                                                   
         AR    RE,RF                                                            
         STCM  RE,15,RCONTEAM      Total up same year/month buckets             
         ST    RE,FULL1                                                         
         L     RF,FULL2            Point to previous contract bucket el         
         MVI   0(RF),X'FF'         Mark it off (not updating record)            
         ST    R3,FULL2                                                         
         J     PRCCBK18                                                         
*                                                                               
PRCCBK20 DS    0H                                                               
*                                                                               
PRCCBK_X J     EXITY                                                            
         DROP  R3                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETBUYVS XC    ABSTRDAT,ABSTRDAT   Init Absolute Start Date                     
         XC    ABENDDAT,ABENDDAT   Init Absolute End Date                       
         XC    SVBYDEMV,SVBYDEMV   Init Buy demo value                          
         XC    TOTBYSPT,TOTBYSPT   Init Buy total spots                         
         MVI   SVBYDEMI,0          Init Buy demo value override flag            
*                                                                               
         L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'03'        Look for Buy Effective date elements         
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
SETBVS30 BRAS  RE,NXTELEM                                                       
         JNE   SETBVS40                                                         
         USING RBUYDTEL,R3                                                      
         CLC   ABSTRDAT,RBUYDTST   Lower than Absolute Start Date?              
         JNL   *+10                                                             
         MVC   ABSTRDAT,RBUYDTST                                                
         CLC   ABENDDAT,RBUYDTED   Higher than Absolute End Date?               
         JH    *+10                                                             
         MVC   ABENDDAT,RBUYDTED                                                
*                                                                               
         LLC   RF,RBUYDTNW         Number of weeks                              
         LLC   R1,RBUYDTWK         Numer of spots per week                      
         MR    RE,R1               Total buy spots                              
         ICM   RE,15,TOTBYSPT      Accumulate total number of spots             
         AR    RE,RF                                                            
         STCM  RE,15,TOTBYSPT                                                   
*                                                                               
         J     SETBVS30                                                         
*                                                                               
SETBVS40 L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'0D'        Look for Dare demo value elem                
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
SETBVS42 BRAS  RE,NXTELEM                                                       
         JNE   SETBVS44                                                         
         USING RBUYDMEL,R3                                                      
         MVC   SVBYDEMV,RBUYDMDM                                                
         J     SETBVS42                                                         
*                                                                               
SETBVS44 L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'0E'        Look for Rep demo value elem                 
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
SETBVS46 BRAS  RE,NXTELEM                                                       
         JNE   SETBVS50                                                         
         USING RBUYRDEL,R3                                                      
         MVC   SVBYDEMV,RBUYRDDM                                                
         MVI   SVBYDEMI,C'*'       To indicate override                         
         J     SETBVS46                                                         
*                                                                               
SETBVS50 XC    BUYLNGRP,BUYLNGRP   Init buy line GRP                            
         OC    TOTBYSPT,TOTBYSPT   Have total # of buy spots?                   
         JZ    SETBVS52                                                         
         CLC   SVBYDEMV,=X'FFFFFFFF'                                            
         JE    SETBVS52                                                         
         ICM   RF,15,TOTBYSPT                                                   
         CVD   RF,DUB1             Total number of spots                        
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),SVBYDEMV                                                 
         ZAP   DUB2,WORK(5)        Rating - packed                              
         ZAP   WORK(16),DUB2                                                    
         MP    WORK(16),DUB1       Calculate GRPS                               
         ZAP   WORK(8),WORK(16)                                                 
         AP    BYGTGRPS,WORK(8)    Accumulate total GRP                         
         EDIT  (P8,WORK),(8,BUYLNGRP),1,ALIGN=LEFT,ZERO=NOBLANK                 
*                                                                               
SETBVS52 DS    0H                                                               
*                                                                               
SETABS_X J     EXITY                                                            
         DROP  R3                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCBBUC  MVI   WKBBKSW,NOQ         Init buy bucket reply switch                 
*                                                                               
         USING BYTOTBKD,R3                                                      
         LA    R3,BYTOTBDA                                                      
PRCBBK10 CLI   BYTBELCD,0          Have total bucket to reply?                  
         JE    PRCBBK_X                                                         
         CLI   BYTBELCD,BYTBELCQ   Total bucket element?                        
         JE    PRCBBK14                                                         
         CLI   BYTBELCD,X'63'      Total bucket element (trade)?                
         JE    PRCBBK14                                                         
         J     PRCBBK20                                                         
*                                                                               
PRCBBK14 OC    BYTBYRMO,BYTBYRMO   Have year/month?                             
         JZ    *+8                                                              
         MVI   BYTBUNK1,1          Set to 1st day to complete BDAT              
*                                                                               
         ICM   RE,15,BYGTBSPT      To calculate total spots                     
         ICM   RF,15,BYTBSPOT                                                   
         AR    RE,RF                                                            
         STCM  RE,15,BYGTBSPT                                                   
*                                                                               
         ICM   RE,15,BYGTBAMT      To calculate total amount                    
         ICM   RF,15,BYTB$AMT                                                   
         AR    RE,RF                                                            
         STCM  RE,15,BYGTBAMT                                                   
*                                                                               
         MVI   WKBBKSW,YESQ        Set to reply total bucket data               
*                                                                               
PRCBBK20 SR    R0,R0                                                            
         IC    R0,BYTBELLN                                                      
         AR    R3,R0               Point to next total bucket element           
         J     PRCBBK10                                                         
*                                                                               
PRCBBK_X J     EXITY                                                            
*                                                                               
         DROP  R3                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETBBK1  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+000+002(3),BYTOTBDA+000+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBK2  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+014+002(3),BYTOTBDA+014+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBK3  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+028+002(3),BYTOTBDA+028+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBK4  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+042+002(3),BYTOTBDA+042+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBK5  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+056+002(3),BYTOTBDA+056+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBK6  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+070+002(3),BYTOTBDA+070+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBK7  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+084+002(3),BYTOTBDA+084+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBK8  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+098+002(3),BYTOTBDA+098+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBK9  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+112+002(3),BYTOTBDA+112+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBKA  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+126+002(3),BYTOTBDA+126+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBKB  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+140+002(3),BYTOTBDA+140+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBKC  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+154+002(3),BYTOTBDA+154+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
SETBBKD  MVI   BUYBUKSW,NOQ        Init monthly buy bucket switch               
         OC    BYTOTBDA+168+002(3),BYTOTBDA+168+002                             
         JZ    *+8                                                              
         MVI   BUYBUKSW,YESQ       Set to reply monthly bucket                  
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MGOCMNT  L     R0,AIO2             Init Makegood comment lines                  
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   WKBUYRTS,0          Init working Buy flag                        
         L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         ST    R3,WKFRSTEL         Save first elem pointer to Buy rec           
         L     RE,AIO2                                                          
         ST    RE,WKAOUTFD         Save output field pointer                    
         XC    WKTMPELM,WKTMPELM   Used to save makegood line numbers           
*                                                                               
         MVI   ELCODE,X'01'        Buy description elem                         
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   MGO010                                                           
         USING RBUYELEM,R3                                                      
         MVC   WKBUYRTS,RBUYRTS    Save Buy flag                                
         TM    RBUYRTS,X'24'                                                    
         JNO   MGO010                                                           
         XC    ELEM,ELEM                                                        
         MVI   ELEM,RMKGMGOQ       Makegood offer comment                       
         MVC   ELEM+2(14),=CL14'LATE RUN BONUS'                                 
         BRAS  RE,SETMGCOM                                                      
         DROP  R3                                                               
*                                                                               
MGO010   MVI   WKTEMPSW,0                                                       
         L     R3,WKFRSTEL         Point to first elem in Buy record            
         MVI   ELCODE,X'04'        Comment elem                                 
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
MGO011   BRAS  RE,NXTELEM                                                       
         JNE   MGO030                                                           
                                                                                
MGO013   CLC   2(3,R3),=C'CR='                                                  
         JNE   MGO011                                                           
         OI    WKTEMPSW,X'80'      Tag as credit instead of makegood            
                                                                                
         USING RBUYCAEL,R3                                                      
MGO030   L     R3,WKFRSTEL         Point to first elem in Buy record            
         MVI   ELCODE,X'16'        Credit audit elem                            
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
MGO035   BRAS  RE,NXTELEM                                                       
         JNE   MGO100                                                           
                                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM+2(28),=CL28'CREDIT WAS TAKEN FOR SPOT ON'                   
                                                                                
         GOTO1 VDATCON,DMCB,(3,RBUYCASD),(0,ELEM+200)                           
         MVC   ELEM+2+30(2),ELEM+202                                            
         CLI   ELEM+2+30,C'0'                                                   
         JNE   *+8                                                              
         MVI   ELEM+2+30,C' '                                                   
         MVI   ELEM+2+32,C'/'                                                   
         MVC   ELEM+2+33(2),ELEM+204                                            
         LA    R5,ELEM+2+36                                                     
         OC    RBUYCAED,RBUYCAED                                                
         JZ    MGO040                                                           
                                                                                
         GOTO1 VDATCON,DMCB,(3,RBUYCAED),(0,ELEM+200)                           
         MVI   ELEM+2+35,C'-'                                                   
         MVC   ELEM+2+36(2),ELEM+202                                            
         CLI   ELEM+2+36,C'0'                                                   
         JNE   *+8                                                              
         MVI   ELEM+2+36,C' '                                                   
         MVI   ELEM+2+38,C'/'                                                   
         MVC   ELEM+2+39(2),ELEM+204                                            
         LA    R5,ELEM+2+42                                                     
                                                                                
MGO040   XC    DUB,DUB                                                          
         EDIT  (1,RBUYCASP),(3,DUB),ALIGN=LEFT,ZERO=NOBLANK                     
         MVI   0(R5),C'('                                                       
                                                                                
         LA    R0,3                                                             
         LA    R1,DUB+1                                                         
         CLI   0(R1),C' '                                                       
         JE    *+12                                                             
         LA    R1,1(R1)                                                         
         JCT   R0,*-12                                                          
                                                                                
         MVC   0(4,R1),=CL4'/WK)'                                               
         MVC   1(7,R5),DUB                                                      
         LA    R5,8(R5)                                                         
                                                                                
         GOTO1 VSQUASH,DMCB,ELEM+2,100                                          
                                                                                
         MVI   0(R5),C'.'                                                       
         LA    R1,ELEM+2                                                        
MGO050   CLI   0(R1),C'.'                                                       
         JE    MGO080                                                           
         LA    R1,1(R1)                                                         
         J     MGO050                                                           
                                                                                
MGO080   MVI   0(R1),0                                                          
         MVI   ELEM,RMKGMGOQ       Makegood offer comment                       
         BRAS  RE,SETMGCOM                                                      
         J     MGO035              Loop back for more                           
                                                                                
MGO100   MVI   BYTE1,0                                                          
         USING RBYMGSEL,R3                                                      
         XC    ELEM,ELEM                                                        
         L     R3,WKFRSTEL         Point to first elem in Buy record            
         MVI   ELCODE,X'56'        Makegood split buy missed elem               
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   MGO100A                                                          
MGO105   TM    RBYMGSFG,X'80'                                                   
         JZ    MGO110                                                           
         BRAS  RE,NXTELEM                                                       
         JE    MGO105                                                           
         J     MGO100A                                                          
                                                                                
MGO110   MVC   ELEM+2(20),=CL20'MADE-GOOD BY LINE(S)'                           
         LA    R5,ELEM+2+21                                                     
MGO120   TM    RBYMGSFG,X'80'                                                   
         JO    MGO130                                                           
*                                                                               
* Set A(X'56' element coming in)                                                
*                                                                               
         LA    R1,WKTMPELM         Point to makegood line # table               
MGO120H  CLI   0(R1),0             Empty?                                       
         JE    MGO120M             Yes, Makegood not previously seen            
         CLC   RBYMGSLI,0(R1)      Already in table?                            
         JE    MGO130                                                           
         LA    R1,1(R1)            NO  - BUMP TO NEXT SLOT                      
         J     MGO120H             GO BACK FOR NEXT                             
*                                                                               
MGO120M  MVC   0(1,R1),RBYMGSLI    Save makegood line # to table                
*                                                                               
         XC    FULL1,FULL1                                                      
         EDIT  (1,RBYMGSLI),(3,FULL1),ALIGN=LEFT,ZERO=NOBLANK                   
         MVC   0(3,R5),FULL1                                                    
         LA    R5,3(R5)                                                         
MGO130   BRAS  RE,NXTELEM                                                       
         JE    MGO120                                                           
                                                                                
         GOTO1 VSQUASH,DMCB,ELEM+2,100                                          
         MVI   0(R5),C'.'                                                       
         LA    R1,ELEM+2                                                        
MGO140   CLI   0(R1),C'.'                                                       
         JE    MGO150                                                           
         LA    R1,1(R1)                                                         
         J     MGO140                                                           
                                                                                
MGO150   MVI   0(R1),0                                                          
         MVI   ELEM,RMKGMGOQ       Makegood offer comment                       
         BRAS  RE,SETMGCOM                                                      
         DROP  R3                                                               
                                                                                
*  This block handles those orders created by replacement offers                
                                                                                
         USING RBYMGSEL,R3                                                      
MGO100A  MVI   BYTE1,0                                                          
         XC    ELEM,ELEM                                                        
         L     R3,WKFRSTEL         Point to first elem in Buy record            
         MVI   ELCODE,X'56'        Makegood split buy missed elem               
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   MGO160                                                           
MGO105A  TM    RBYMGSFG,X'80'                                                   
         JO    MGO110A                                                          
         BRAS  RE,NXTELEM                                                       
         JE    MGO105A                                                          
         J     MGO160                                                           
                                                                                
MGO110A  MVC   ELEM+2(28),=CL28'NA SPOTS REPLACED BY LINE(S)'                   
         LA    R5,ELEM+2+29                                                     
MGO120A  TM    RBYMGSFG,X'80'                                                   
         JZ    MGO130A                                                          
         CLC   RBYMGSLI,BYTE1                                                   
         JE    MGO130A                                                          
         MVC   BYTE1,RBYMGSLI                                                   
         XC    FULL1,FULL1                                                      
         EDIT  (1,RBYMGSLI),(3,FULL1),ALIGN=LEFT,ZERO=NOBLANK                   
         MVC   0(3,R5),FULL1                                                    
         LA    R5,3(R5)                                                         
MGO130A  BRAS  RE,NXTELEM                                                       
         JE    MGO120A                                                          
                                                                                
         GOTO1 VSQUASH,DMCB,ELEM+2,100                                          
         MVI   0(R5),C'.'                                                       
         LA    R1,ELEM+2                                                        
MGO140A  CLI   0(R1),C'.'                                                       
         JE    MGO150A                                                          
         LA    R1,1(R1)                                                         
         J     MGO140A                                                          
                                                                                
MGO150A  MVI   0(R1),0                                                          
         MVI   ELEM,RMKGMGOQ       Makegood offer comment                       
         BRAS  RE,SETMGCOM                                                      
         DROP  R3                                                               
                                                                                
         USING RBUYMGEL,R3                                                      
MGO160   MVI   BYTE1,0                                                          
         L     R3,WKFRSTEL         Point to first elem in Buy record            
         MVI   ELCODE,X'05'        Buy makegood reference elem                  
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   MGO330                                                           
                                                                                
         MVI   BYTE3,0             Loop counter                                 
                                                                                
         CLI   RBUYMGSP,0                                                       
         JNE   MGO170                                                           
                                                                                
         USING RBUYSLEL,R3                                                      
         L     R3,WKFRSTEL         Point to first elem in Buy record            
         MVI   ELCODE,X'26'        Makegood sibling link elem                   
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   MGO330                                                           
                                                                                
         MVC   ELEM+2(31),=CL31'PART OF A MKGD MADE UP OF LINES'                
         LA    R1,ELEM+2+32                                                     
         SR    R2,R2                                                            
         IC    R2,RBUYSLLN                                                      
         AHI   R2,-2                                                            
                                                                                
MGO165   MVC   BYTE2,RBUYSLLK                                                   
         EDIT  (1,BYTE2),(3,(R1)),ALIGN=LEFT,ZERO=NOBLANK                       
         LA    R3,1(R3)            Bump to next buy link                        
         LA    R1,4(R1)                                                         
         JCT   R2,MGO165                                                        
                                                                                
MGO168   MVI   ELEM,RMKGORGQ       Makegood original comment (target)           
         BRAS  RE,SETMGCOM                                                      
         J     MGO330                                                           
         DROP  R3                                                               
                                                                                
         USING RBUYMGEL,R3                                                      
MGO170   CLI   BYTE3,2             Need to bump to next comment line?           
         JH    MGO172                                                           
         CLC   BYTE1,RBUYMGLI                                                   
         JE    MGO230                                                           
         CLI   BYTE1,0                                                          
         JE    MGO210                                                           
                                                                                
MGO172   MVI   0(R5),C'.'                                                       
                                                                                
         GOTO1 VSQUASH,DMCB,ELEM+2,100                                          
                                                                                
         LA    R1,ELEM+2                                                        
MGO190   CLI   0(R1),C'.'                                                       
         JE    MGO200                                                           
         LA    R1,1(R1)                                                         
         J     MGO190                                                           
                                                                                
MGO200   MVI   0(R1),0                                                          
         MVI   ELEM,RMKGORGQ       Makegood original comment (target)           
         BRAS  RE,SETMGCOM                                                      
         LA    R5,ELEM+2           Build next comment line                      
         MVI   BYTE3,0             Reset loop counter                           
                                                                                
MGO210   MVC   BYTE1,RBUYMGLI                                                   
         XC    ELEM+2(150),ELEM+2                                               
                                                                                
         TM    WKBUYRTS,X'0C'                                                   
         JZ    MGO215                                                           
         MVC   ELEM+2(23),=CL23'SPOT RAN LATE FOR LINE '                        
         LA    R5,ELEM+2+23                                                     
         J     MGO217                                                           
                                                                                
MGO215   TM    WKBUYRTS,X'02'                                                   
         JZ    MGO216                                                           
         MVC   ELEM+2(32),=CL32'REPLACEMENT FOR SPOTS NA ON LN #'               
         LA    R5,ELEM+2+32                                                     
         J     MGO217                                                           
                                                                                
MGO216   MVC   ELEM+2(29),=CL29'MKGD FOR SPOTS MISSED ON LN #'                  
         LA    R5,ELEM+2+29                                                     
         TM    WKTEMPSW,X'80'                                                   
         JZ    *+14                                                             
         MVC   ELEM+2(31),=CL31'CREDIT FOR SPOTS MISSED ON LN #'                
         LA    R5,ELEM+2+31                                                     
                                                                                
MGO217   XC    FULL1,FULL1                                                      
         EDIT  (1,RBUYMGLI),(3,FULL1),ALIGN=LEFT,ZERO=NOBLANK                   
         TM    WKBUYRTS,X'0C'                                                   
         JZ    MGO218                                                           
         MVC   0(4,R5),FULL1                                                    
         MVC   6(3,R5),=CL3'ON '                                                
         LA    R5,8(R5)                                                         
         J     MGO230                                                           
                                                                                
MGO218   CLI   FULL1+1,C' '                                                     
         JNE   *+12                                                             
         MVI   FULL1+1,C':'                                                     
         J     MGO220                                                           
         CLI   FULL1+2,C' '                                                     
         JNE   *+12                                                             
         MVI   FULL1+2,C':'                                                     
         J     MGO220                                                           
         CLI   FULL1+3,C' '                                                     
         JNE   MGO220                                                           
         MVI   FULL1+3,C':'                                                     
                                                                                
MGO220   MVC   0(4,R5),FULL1                                                    
         LA    R5,4(R5)                                                         
                                                                                
MGO230   LA    R5,1(R5)                                                         
         GOTO1 VDATCON,DMCB,(3,RBUYMGD1),(0,ELEM+200)                           
         MVC   0(2,R5),ELEM+202                                                 
         CLI   0(R5),C'0'                                                       
         JNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         MVI   2(R5),C'/'                                                       
         MVC   3(2,R5),ELEM+204                                                 
         LA    R5,5(R5)                                                         
                                                                                
         OC    RBUYMGD2,RBUYMGD2                                                
         JZ    MGO260                                                           
         GOTO1 VDATCON,DMCB,(3,RBUYMGD2),(0,ELEM+200)                           
         MVI   0(R5),C'-'                                                       
         MVC   1(2,R5),ELEM+202                                                 
         CLI   1(R5),C'0'                                                       
         JNE   *+8                                                              
         MVI   1(R5),C' '                                                       
         MVI   3(R5),C'/'                                                       
         MVC   4(2,R5),ELEM+204                                                 
         LA    R5,6(R5)                                                         
                                                                                
MGO260   TM    WKBUYRTS,X'0C'                                                   
         JNZ   MGO270                                                           
                                                                                
         XC    DUB1,DUB1                                                        
         EDIT  (1,RBUYMGSP),(3,DUB1),ALIGN=LEFT,ZERO=NOBLANK                    
         MVI   0(R5),C'('                                                       
                                                                                
         LA    R0,3                                                             
         LA    R1,DUB1+1                                                        
         CLI   0(R1),C' '                                                       
         JE    *+12                                                             
         LA    R1,1(R1)                                                         
         JCT   R0,*-12                                                          
                                                                                
         MVC   0(4,R1),=CL4'/WK)'                                               
         MVC   1(7,R5),DUB1                                                     
         LA    R5,7(R5)                                                         
                                                                                
MGO270   BRAS  RE,NXTELEM                                                       
         JNE   MGO272                                                           
         LLC   RE,BYTE3                                                         
         AHI   RE,1                                                             
         STC   RE,BYTE3                                                         
         J     MGO170                                                           
                                                                                
MGO272   MVI   1(R5),C'.'                                                       
                                                                                
         GOTO1 VSQUASH,DMCB,ELEM+2,100                                          
                                                                                
         LA    R1,ELEM+2                                                        
MGO290   CLI   0(R1),C'.'                                                       
         JE    MGO300                                                           
         LA    R1,1(R1)                                                         
         J     MGO290                                                           
                                                                                
MGO300   MVI   0(R1),0                                                          
         MVI   ELEM,RMKGORGQ       Makegood original comment (target)           
         BRAS  RE,SETMGCOM                                                      
         DROP  R3                                                               
                                                                                
MGO330   DS    0H                                                               
                                                                                
         J     EXITY                                                            
*                                                                               
SETMGCOM L     RF,WKAOUTFD                                                      
         USING RMKGDCMD,RF                                                      
         MVC   RMKGDCTY(RMKGDELQ),ELEM                                          
         MVI   RMKGCLEN,RMKGDELQ                                                
         AHI   RF,RMKGDELQ         Bump to next entry                           
         ST    RF,WKAOUTFD         Save address of next entry                   
         BR    RE                                                               
         DROP  RF                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DARCMNT  L     R0,AIO7             Init DARE spot/date line comments            
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   DUB+0(4),ACOMFACS                                                
         MVC   DUB+4(2),AGY                                                     
         GOTO1 (RFAUTOCM,AREPFACS),DMCB,AIO6,AIO7,AIO5,DUB                      
*                                                                               
         J     EXITY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
GLOBALS  DS    0D                  ** Global literals **                        
*                                                                               
         LTORG                                                                  
CON#LIT  DC    C'Contract #'                                                    
SCALLIT  DC    C'Station Call Letters'                                          
PARTCFTX DC    C'PARTIALLY CONFIRMED'                                           
CONFIRTX DC    C'CONFIRMED'                                                     
RETURNTX DC    C'RETURNED'                                                      
REVISDTX DC    C'REVISED'                                                       
NEW___TX DC    C'NEW'                                                           
*                                                                               
EMBCOMMN DC    C'C='               Embedded comment notation                    
EMBSCCMN DC    C'SC='              Embedded SC comment notation                 
SPCMGCMN DC    C'MG='              Special MG= comment notation                 
*                                                                               
       ++INCLUDE RERISKTAB         Credit rating (risk) text table              
*                                                                               
         DROP  RB,R1                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* Display 64 Y/N bytes                                                          
*        P1    A(Double word data bits)                                         
*        P2    A(output)                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BITOUT   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,64               64 bits to display                           
         L     RE,0(R1)            A(Data bits)                                 
         L     R2,4(R1)            A(Output)                                    
         LM    R4,R5,0(RE)                                                      
         J     BOUT30                                                           
*                                                                               
BOUT20   SLDL  R4,1                                                             
BOUT30   MVI   0(R2),C'Y'                                                       
         LTR   R4,R4               High order bit on?                           
         JM    BOUT40                                                           
         MVI   0(R2),C'N'                                                       
BOUT40   LA    R2,1(R2)            Next entry in output                         
         JCT   R0,BOUT20                                                        
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  Check WIP Status                                                   *         
*        R2    Points first element in Contract record                *         
*        BYTE1 Return status                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKWIPS   NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE1,C'0'          Set WIP=false                                
         LR    R3,R2                                                            
         MVI   ELCODE,X'1F'        Look for Extended description elem           
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   CKWIP10                                                          
         USING RCONXEL,R3                                                       
         TM    RCONCONF,X'40'      Confirmed now?                               
         JO    CKWIPX              Yes - not WIP                                
         DROP  R3                                                               
*                                                                               
CKWIP10  LR    R3,R2                                                            
         MVI   ELCODE,X'20'        Look for Send info elem                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   CKWIPX                                                           
         USING RCONSEND,R3                                                      
         TM    RCONSENF,X'30'      REP/STA version not advanced?                
         JO    CKWIPX                                                           
         MVI   BYTE1,C'1'          Set WIP=true                                 
         DROP  R3                                                               
*                                                                               
CKWIPX   J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  Format Book field                                                  *         
*        R2    Points to input                                        *         
*        R4    Points to output                                       *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTBOOK  NTR1  BASE=*,LABEL=*                                                   
         MVC   0(BKOLNQ,R4),SPACES Init output                                  
         CLC   0(2,R2),=C'DR'                                                   
         JNE   *+14                                                             
         MVC   0(02,R4),=C'DR'                                                  
         J     FMTBK_X                                                          
         CLC   0(2,R2),=C'PP'                                                   
         JNE   *+14                                                             
         MVC   0(02,R4),=C'PP'                                                  
         J     FMTBK_X                                                          
FMTBK10  OC    0(L'RSARXBKS,R2),0(R2)                                           
         JZ    FMTBK_X                                                          
         LA    R5,NUMBKQ           Number of books to process                   
*                                                                               
FMTBK30  OC    0(3,R2),0(R2)       Have book?                                   
         JZ    FMTBK60                                                          
*                                                                               
         XC    ELEM2,ELEM2                                                      
         MVI   ELEM2,BKOLNQ+8      Total length of fake field header            
*                                                                               
         GOTOR VUNBOOK,DMCB,(1,0(R2)),ELEM2,0,(C'+',=CL6' ')                    
*                                                                               
         SR    RE,RE                                                            
         IC    RE,ELEM2                                                         
         LA    RE,ELEM2(RE)        Point to end of output                       
         CLI   0(RE),C')'                                                       
         JNE   FMTBK36                                                          
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         JNE   *+10                                                             
         BCTR  RE,0                                                             
         J     *+12                                                             
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*                                                                               
FMTBK36  LR    R0,RE                                                            
         BRAS  RE,FMTBKLAS         Point R1 to next slot for output             
         LR    RE,R0                                                            
*                                                                               
         LA    RF,ELEM2+8                                                       
         SR    RE,RF                                                            
         BASR  RF,0                                                             
         MVC   0(0,R1),ELEM2+8                                                  
         EX    RE,0(RF)                                                         
         BRAS  RE,FMTBKLAS         Point R1 to next slot for output             
         MVI   0(R1),C','                                                       
         LA    R2,3(R2)            Point to next book to be translated          
         JCT   R5,FMTBK30                                                       
*                                                                               
FMTBK60  BRAS  RE,FMTBKLAS         Point R1 to next slot for output             
         BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         JNE   *+8                                                              
         MVI   0(R1),C' '          Blank out last commas                        
*                                                                               
FMTBK_X  J     EXIT                                                             
*                                                                               
FMTBKLAS LR    R1,R4               Point to beginning of output                 
         LA    R1,(BKOLNQ-1)(R1)   Point to last char in output                 
         LA    RF,BKOLNQ-1         Counter to avoid infinite loop               
FMTBKL10 CLI   0(R1),C' '                                                       
         JH    FMTBKL20                                                         
         BCTR  R1,0                Check previous char                          
         JCT   RF,FMTBKL10                                                      
         J     FMTBKL_X                                                         
FMTBKL20 LA    R1,1(R1)            Next output slot for books                   
FMTBKL_X BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
BKOLNQ   EQU   66                  Length of output for books                   
NUMBKQ   EQU   L'RSARXBKS/3        Number of books                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTSTA   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXSTA20                                                          
* * * *  GOTOR (#GETPID,AGETPID)                                                
* * * *  JNE   NXSTA_ER                                                         
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    RE,IOKEY                                                         
         USING RSTAKEY,RE                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGY                                                     
         DROP  RE                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         JNE   NXSTA_N                                                          
         J     NXSTA30                                                          
                                                                                
NXSTA20  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO5'                            
         JNE   NXSTA_N                                                          
                                                                                
NXSTA30  CLC   IOKEY(RSTAKSTA-RSTAKEY),IOKEYSAV                                 
         JNE   NXSTA_X                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO5'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO5                                                          
         USING RSTAKEY,RE                                                       
         CLI   RSTAKSTA+(L'RSTAKSTA-1),C' '                                     
         JNE   NXSTA20             Skip station if not TV                       
         J     NXSTA_Y                                                          
*                                                                               
NXSTA_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXSTA_Y  MVC   LP_ADATA,ARSTAREC                                                
         J     EXITY                                                            
*                                                                               
NXSTA_ER MVI   LP_RMODE,LP_RLAST   No more - error                              
*                                                                               
NXSTA_N  MVC   LP_ADATA,ARSTAREC                                                
         J     EXITN                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCON   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXCON20                                                          
*                                                                               
         LA    R0,SVNAMSTR         Init names to be returned                    
         LHI   R1,SVNAMLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    BYTOTBUC(BYTOTBLQ),BYTOTBUC                                      
         XC    BYGTBSPT,BYGTBSPT   Buy grand total spots count                  
         XC    BYGTBAMT,BYGTBAMT   Buy grand total amount                       
         ZAP   BYGTGRPS,=P'0'      Buy grand total GRPs                         
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,QACON                                                       
         JZ    NXCON_X                                                          
         MVC   NUMCON,LW_NUMN-LW_D(RE)                                          
         AHI   RE,LW_LN2Q                                                       
         ST    RE,ANXTCON          Set A(next Contract number)                  
                                                                                
NXCON20  SR    R0,R0                                                            
         ICM   R0,3,NUMCON         R0=remaining Contract count                  
         JZ    NXCON_X             Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCON                                                      
         L     R1,ANXTCON                                                       
                                                                                
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),0(L'RCONPCON+1,R1)                                    
         MVO   WORK(5),WORK+10(5)                                               
         MVC   SVCON9CM,WORK                                                    
                                                                                
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    RE,IOKEY                                                         
         USING RCONREC,RE                                                       
         MVI   RCONPTYP,X'8C'      Passive Pointer 1 for Contract rec           
         MVC   RCONPREP,AGY        Rep code                                     
         MVC   RCONPCON,WORK       Contract number in 9's complement            
         DROP  RE                                                               
                                                                                
         AHI   R1,L'RCONPCON+1     Bump to next Contract number                 
         ST    R1,ANXTCON                                                       
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         JNE   NXCON_N                                                          
                                                                                
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         JNE   NXCON_N                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO5'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO5                                                          
         USING RCONREC,R3                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RSTAKEY,RE                                                       
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,RSTAKIDQ                                                
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         DROP  RE                                                               
         LA    R1,SVSTAKEY                                                      
         LA    R2,SVSTANAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RCTYKEY,RE                                                       
         XC    RCTYKEY,RCTYKEY                                                  
         MVI   RCTYKTYP,RCTYKTYQ                                                
         MVC   RCTYKREP,RCONKREP                                                
         MVC   RCTYKCTY,RCONTYPE                                                
         DROP  RE                                                               
         LA    R1,SVCTYKEY                                                      
         LA    R2,SVCTYNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RADVKEY,RE                                                       
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKTYP,RADVKIDQ                                                
         MVC   RADVKADV,RCONKADV                                                
         MVC   RADVKREP,RCONKREP                                                
         DROP  RE                                                               
         LA    R1,SVADVKEY                                                      
         LA    R2,SVADVNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RPRDKEY,RE                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKTYP,RPRDKIDQ                                                
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKREP,RCONKREP                                                
         DROP  RE                                                               
         LA    R1,SVPRDKEY                                                      
         LA    R2,SVPRDNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RSALKEY,RE                                                       
         XC    RSALKEY,RSALKEY                                                  
         MVI   RSALKTYP,RSALKIDQ                                                
         MVC   RSALKREP,RCONKREP                                                
         MVC   RSALKSAL,RCONSAL                                                 
         DROP  RE                                                               
         LA    R1,SVSALKEY                                                      
         LA    R2,SVSALNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RSA2KEY,RE                                                       
         XC    RSA2KEY,RSA2KEY                                                  
         MVI   RSA2KTYP,RSA2KIDQ                                                
         MVC   RSA2KREP,RCONKREP                                                
         MVC   RSA2KSAL,RCONSAL                                                 
         DROP  RE                                                               
         LA    R1,SVSA2KEY                                                      
         LA    R2,SVSALEML                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING ROFFKEY,RE                                                       
         XC    ROFFKEY,ROFFKEY                                                  
         MVI   ROFFKTYP,ROFFKIDQ                                                
         MVC   ROFFKREP,RCONKREP                                                
         MVC   ROFFKOFF,RCONKOFF                                                
         DROP  RE                                                               
         LA    R1,SVOFFKEY                                                      
         LA    R2,SVOFFNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         J     NXCON_Y                                                          
         DROP  R3                                                               
*                                                                               
NXCON_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXCON_Y  MVC   LP_ADATA,ARCONREC                                                
         J     EXITY                                                            
*                                                                               
NXCON_ER MVI   LP_RMODE,LP_RLAST   No more - error                              
*                                                                               
NXCON_N  MVC   LP_ADATA,ARCONREC                                                
         J     EXITN                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTAGY   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXAGY20                                                          
*                                                                               
         L     R3,AIO5             Point to Contract record                     
         USING RCONREC,R3                                                       
         LA    RE,WORK                                                          
         USING RAGY2KEY,RE                                                      
         XC    RAGY2KEY,RAGY2KEY                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         DROP  RE                                                               
         LA    R1,SVAGYKEY                                                      
         LA    R2,SVAGYNAM                                                      
         BRAS  RE,GETNAME                                                       
         L     R0,AIO4             Save Agency record                           
         LHI   R1,IOLENQ                                                        
         L     RE,AIO6                                                          
         LHI   RF,IOLENQ                                                        
         MVCL  R0,RE                                                            
*                                                                               
         J     NXAGY_Y                                                          
         DROP  R3                                                               
*                                                                               
NXAGY20  DS    0H                                                               
*                                                                               
NXAGY_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXAGY_Y  MVC   LP_ADATA,ARAGYREC                                                
         J     EXITY                                                            
*                                                                               
NXAGY_N  MVC   LP_ADATA,ARAGYREC                                                
         J     EXITN                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCMT   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXCMT20                                                          
                                                                                
         L     R3,AIO4             Point to Agency record                       
         CLI   0(R3),RAGYKIDQ                                                   
         JNE   NXCMT_N                                                          
         LA    R3,(RAGY2FXE-RAGY2REC)(R3)                                       
         MVI   ELCODE,X'1F'        Look for name elem from  Agy2 rec            
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   NXCMT_N                                                          
         USING RAG2ELEM,R3                                                      
         CLI   RAG2LIAB,0          Having Liability position?                   
         JE    NXCMT_N                                                          
         EDIT  (B1,RAG2LIAB),(2,HALF1),FILL=0                                   
         DROP  R3                                                               
         L     R3,AIO5             Point to Contract record                     
         USING RCONREC,R3                                                       
         LA    RE,WORK                                                          
         USING RCMTKEY,RE                                                       
         XC    RCMTKEY,RCMTKEY                                                  
         MVI   RCMTKTYP,RCMTKIDQ                                                
         MVC   RCMTKREP,RCONKREP                                                
         MVC   RCMTKOFF,=X'FFFF'                                                
         MVC   RCMTKCDE+0(4),=C'LIAB'                                           
         MVC   RCMTKCDE+4(2),HALF1                                              
         OC    RCMTKCDE,SPACES                                                  
         DROP  RE                                                               
         LA    R1,SVCMTKEY                                                      
         BRAS  RE,GETNAME                                                       
         L     R0,AIO3             Save Comment record                          
         LHI   R1,IOLENQ                                                        
         L     RE,AIO6                                                          
         LHI   RF,IOLENQ                                                        
         MVCL  R0,RE                                                            
*                                                                               
         J     NXCMT_Y                                                          
         DROP  R3                                                               
*                                                                               
NXCMT20  DS    0H                                                               
*                                                                               
NXCMT_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXCMT_Y  MVC   LP_ADATA,ARCMTREC                                                
         J     EXITY                                                            
*                                                                               
NXCMT_N  MVC   LP_ADATA,ARCMTREC                                                
         J     EXITN                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTCOV   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXCOV20                                                          
         L     R3,AIO5             Point to Contract record                     
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'A6'        Look for coversheet elem                     
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   NXCOV_X                                                          
         USING RCONCVEL,R3                                                      
         OC    RCONCVNM,RCONCVNM                                                
         JZ    NXCOV_X                                                          
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING RCOVKEY,RE                                                       
         MVI   RCOVKTYP,RCOVKIDQ                                                
         L     RF,AIO5                                                          
         MVC   RCOVKREP,(RCONKREP-RCONKEY)(RF)                                  
         MVC   RCOVKNAM,RCONCVNM                                                
         DROP  RE,R3                                                            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO3'                            
NXCOV16  CLC   IOKEY(RCOVKSEQ-RCOVKEY),IOKEYSAV                                 
         JNE   NXCOV_X                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO3                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'03'        Look for Coversheet text elem                
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   NXCOV20                                                          
*                                                                               
         J     NXCOV_Y                                                          
*                                                                               
NXCOV20  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO3'                            
         J     NXCOV16                                                          
*                                                                               
NXCOV_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXCOV_Y  MVC   LP_ADATA,ARCOVREC                                                
         J     EXITY                                                            
*                                                                               
NXCOV_N  MVC   LP_ADATA,ARCOVREC                                                
         J     EXITN                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCMT   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO5             Point to Contract record                     
         USING RCONREC,R3                                                       
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
*                                                                               
         CLI   WKCMTYPE,RCMTKIDQ   Comment record?                              
         JE    GTCMT12                                                          
         CLI   WKCMTYPE,ROCMKIDQ   Office comment record?                       
         JE    GTCMT14                                                          
         CLI   WKCMTYPE,RCFCKTYQ   Confirmation comment record?                 
         JE    GTCMT16                                                          
         DC    H'0'                Unsupported comment type                     
*                                                                               
         USING RCMTKEY,RE                                                       
GTCMT12  MVI   RCMTKTYP,RCMTKIDQ                                                
         MVC   RCMTKREP,RCONKREP                                                
         MVC   RCMTKOFF,=X'FFFF'                                                
         MVC   RCMTKCDE,SVEMBCMC                                                
         J     GTCMT20                                                          
         DROP  RE                                                               
*                                                                               
         USING ROCMKEY,RE                                                       
GTCMT14  MVI   ROCMKTYP,ROCMKIDQ                                                
         MVC   ROCMKREP,RCONKREP                                                
         MVC   ROCMKOFC,RCONKOFF                                                
         MVC   ROCMKNUM,SVEMBCMC                                                
         CLI   SVEMBCMC,C'*'       Non-discrimination comment?                  
         JNE   *+16                                                             
         MVC   ROCMKOFC,SVEMBCMC                                                
         MVC   ROCMKNUM,=C'01'                                                  
         MVI   ROCMKPAG,C'0'       Default page number                          
         J     GTCMT20                                                          
         DROP  RE                                                               
*                                                                               
         USING RCFCKEY,RE                                                       
GTCMT16  MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,RCONKREP                                                
         MVC   RCFCKCON,RCONKCON                                                
         J     GTCMT20                                                          
         DROP  RE                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
GTCMT20  L     R0,AIO7             Init comment lines                           
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO1'                            
         CLC   IOKEY(L'RCMTKEY),IOKEYSAV                                        
         JNE   EXIT                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO7             Prepare to copy comment lines                
         L     R3,AIO1             Point to comment record just read            
         LA    R3,FRSTELEM(R3)                                                  
*                                                                               
         CLI   WKCMTYPE,RCFCKTYQ   Confirmation comment record?                 
         JNE   GTCMT50                                                          
         USING RCFCTEL,R3                                                       
         MVI   ELCODE,X'02'        Looking for comment text lines elem          
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
GTCMT24  BRAS  RE,NXTELEM                                                       
         JNE   GTCMT80                                                          
         LLC   RE,RCFCTLEN                                                      
         AHI   RE,-(RCFCTEXT-RCFCTEL)                                           
         BCTR  RE,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R2),RCFCTEXT                                                 
         EX    RE,0(RF)                                                         
         LA    R2,L'RCMT2TXT(R2)                                                
         MVI   EMBCOMSW,YESQ       Need to reply confirmation comments          
         J     GTCMT24                                                          
         DROP  R3                                                               
*                                                                               
* Note: comment text elements have same DSECT                                   
*                                                                               
         USING RCMTELM2,R3                                                      
GTCMT50  MVI   ELCODE,X'02'        Looking for comment text lines elem          
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
GTCMT52  BRAS  RE,NXTELEM                                                       
         JNE   GTCMT80                                                          
         LLC   RE,RCMT2LEN                                                      
         AHI   RE,-(RCMT2TXT-RCMTELM2)                                          
         BCTR  RE,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R2),RCMT2TXT                                                 
         EX    RE,0(RF)                                                         
         LA    R2,L'RCMT2TXT(R2)                                                
         MVI   EMBCOMSW,YESQ       Set embedded comment swtich to yes           
         J     GTCMT52                                                          
         DROP  R3                                                               
*                                                                               
GTCMT80  J     GTCMT_X                                                          
*                                                                               
GTCMT_X  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTPST   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXPST20                                                          
                                                                                
         LA    R0,SVNAMSTR         Init names to be returned                    
         LHI   R1,SVNAMLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR VDATCON,DMCB,(3,TODAYB),(0,DUB1)                                 
         MVC   DUB2,DUB1                                                        
         GOTOR VGETDAY,DMCB,DUB1                                                
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         MVC   DUB1,DUB2           Restore today's date                         
         CHI   RF,1                Monday?                                      
         JNH   NXPST12                                                          
         MHI   RF,-1                                                            
         GOTOR VADDAY,DMCB,DUB2,DUB1,(RF)                                       
NXPST12  LHI   RF,-91              Minus 13 weeks                               
         GOTOR VADDAY,DMCB,DUB1,DUB2,(RF)                                       
         GOTOR VDATCON,DMCB,(0,DUB2),(3,PENDCFSD)                               
                                                                                
         SR    RE,RE                                                            
         ICM   RE,7,QASTA                                                       
         JZ    NXPST_X                                                          
         MVC   NUMSTA,LW_NUMN-LW_D(RE)                                          
         AHI   RE,LW_LN2Q                                                       
         ST    RE,ANXTSTA          Set A(next Station)                          
                                                                                
NXPST20  SR    R0,R0                                                            
         ICM   R0,3,NUMSTA         R0=remaining Station count                   
         JZ    NXPST_X             Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMSTA                                                      
         L     R1,ANXTSTA                                                       
                                                                                
         MVC   SVSTALET,0(R1)                                                   
         OC    SVSTALET,SPACES                                                  
                                                                                
         AHI   R1,L'RCON9DST       Bump to next Station Call Letter             
         ST    R1,ANXTSTA                                                       
*                                                                               
         J     NXPST_Y                                                          
*                                                                               
NXPST_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXPST_Y  MVC   LP_ADATA,ARCONREC                                                
         J     EXITY                                                            
*                                                                               
NXPST_ER MVI   LP_RMODE,LP_RLAST   No more - error                              
*                                                                               
NXPST_N  MVC   LP_ADATA,ARCONREC                                                
         J     EXITN                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTPCO   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXPCO20                                                          
                                                                                
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    RE,IOKEY                                                         
         USING RCON9DK,RE                                                       
         MVI   RCON9DTP,X'9D'                                                   
         MVC   RCON9DRP,AGY                                                     
         MVC   RCON9DST,SVSTALET                                                
         DROP  RE                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         JNE   NXPCO_N                                                          
         J     NXPCO30                                                          
                                                                                
NXPCO20  CLI   IOKEY,X'9D'         Still contract record?                       
         JE    NXPCO26                                                          
         MVC   IOKEY,WKIOKEY       Restore contract key                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO5'                            
         JNE   NXPCO_N                                                          
NXPCO26  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO5'                            
         JNE   NXPCO_N                                                          
                                                                                
NXPCO30  CLC   IOKEY(RCON9DDV-RCON9DK),IOKEYSAV                                 
         JNE   NXPCO_N                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO5'                           
         JNE   NXPCO20                                                          
*                                                                               
         L     R3,AIO5                                                          
         USING RCONREC,R3                                                       
         CLI   RCONELEM,X'01'                                                   
         JNE   NXPCO20                                                          
         TM    RCONMODR,X'10'      Not Pending?                                 
         JNZ   NXPCO20                                                          
         CLC   RCONDATE(3),PENDCFSD                                             
         JNH   NXPCO20                                                          
*                                                                               
         LA    R3,(FRSTELEM)(R3)   Check for zero competitive amount            
         MVI   ELCODE,X'12'        Look for Expanded SAR elem                   
         BRAS  RE,NXTELEM                                                       
         JNE   NXPCO32                                                          
         USING RSARXEL,R3                                                       
         TM    RSARXFLG,X'08'      Forecast order?                              
         JNZ   NXPCO20                                                          
*                                                                               
NXPCO32  L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)   Check for zero competitive amount            
         MVI   ELCODE,X'06'        SPL element code                             
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   NXPCO40                                                          
         USING RCONSPEL,R3                                                      
         LLC   RE,RCONSPNU         Number of mini-elements                      
         CHI   RE,0                                                             
         JNH   NXPCO40                                                          
         LA    RF,RCONSPST         Point to first station call letter           
NXPCO34  CLC   SVSTALET,0(RF)                                                   
         JNE   *+14                                                             
         OC    L'RCONSPST(L'RCONSPAM,RF),L'RCONSPST(RF)                         
         JZ    NXPCO20                                                          
         LA    RF,L'RCONSPST+L'RCONSPAM(RF)                                     
         JCT   RE,NXPCO34                                                       
*                                                                               
NXPCO40  L     R3,AIO5             Reestablish contract record pointer          
         USING RCONREC,R3                                                       
         MVC   WKIOKEY,IOKEY       Save contract key                            
*                                                                               
         LA    RE,WORK                                                          
         USING RAGY2KEY,RE                                                      
         XC    RAGY2KEY,RAGY2KEY                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         DROP  RE                                                               
         LA    R1,SVAGYKEY                                                      
         LA    R2,SVAGYNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RADVKEY,RE                                                       
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKTYP,RADVKIDQ                                                
         MVC   RADVKADV,RCONKADV                                                
         MVC   RADVKREP,RCONKREP                                                
         DROP  RE                                                               
         LA    R1,SVADVKEY                                                      
         LA    R2,SVADVNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING ROFFKEY,RE                                                       
         XC    ROFFKEY,ROFFKEY                                                  
         MVI   ROFFKTYP,ROFFKIDQ                                                
         MVC   ROFFKREP,RCONKREP                                                
         MVC   ROFFKOFF,RCONKOFF                                                
         DROP  RE                                                               
         LA    R1,SVOFFKEY                                                      
         LA    R2,SVOFFNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RPRDKEY,RE                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKTYP,RPRDKIDQ                                                
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKREP,RCONKREP                                                
         DROP  RE                                                               
         LA    R1,SVPRDKEY                                                      
         LA    R2,SVPRDNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RSALKEY,RE                                                       
         XC    RSALKEY,RSALKEY                                                  
         MVI   RSALKTYP,RSALKIDQ                                                
         MVC   RSALKREP,RCONKREP                                                
         MVC   RSALKSAL,RCONSAL                                                 
         DROP  RE                                                               
         LA    R1,SVSALKEY                                                      
         LA    R2,SVSALNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         LA    RE,WORK                                                          
         USING RCTYKEY,RE                                                       
         XC    RCTYKEY,RCTYKEY                                                  
         MVI   RCTYKTYP,RCTYKTYQ                                                
         MVC   RCTYKREP,RCONKREP                                                
         MVC   RCTYKCTY,RCONTYPE                                                
         DROP  RE                                                               
         LA    R1,SVCTYKEY                                                      
         LA    R2,SVCTYNAM                                                      
         BRAS  RE,GETNAME                                                       
*                                                                               
         J     NXPCO_Y                                                          
         DROP  R3                                                               
*                                                                               
NXPCO_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXPCO_Y  MVC   LP_ADATA,ARCONREC                                                
         J     EXITY                                                            
*                                                                               
NXPCO_ER MVI   LP_RMODE,LP_RLAST   No more - error                              
*                                                                               
NXPCO_N  MVC   LP_ADATA,ARCONREC                                                
         J     EXITN                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Routine to look up names in various records in X'01' element        *         
* Note: Name field is defined right after element length              *         
*       Except in expanded Agency record (Agy2)                       *         
*                                                                     *         
* Parameters:                                                         *         
*        WORK  Contains key of record to be looked up                 *         
*        R1    Points to saved key to avoid reading same record       *         
*        R2    Points to returned name                                *         
*        AIO5  Contains Contract record                               *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETNAME  NTR1  BASE=*,LABEL=*                                                   
         CLC   0(L'RCONKEY,R1),WORK                                             
         JE    GETNAM_X                                                         
         CLI   WORK,RPRDKIDQ       Product record?                              
         JNE   GETNAM40                                                         
         CLC   WORK+(RPRDKPRD-RPRDKEY)(L'RPRDKPRD),SPACES                       
         JNE   GETNAM40                                                         
         L     R3,AIO5                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'05'        Contract expansion elem                      
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   GETNAM_X                                                         
         USING RCONEXEL,R3                                                      
         MVC   0(L'RCONEXPR,R2),RCONEXPR                                        
         J     GETNAM_X                                                         
         DROP  R3                                                               
                                                                                
GETNAM40 MVC   0(L'RCONKEY,R1),WORK                                             
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'RCONKEY),WORK                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO6'                            
         CLC   IOKEY(L'RCONKEY),IOKEYSAV                                        
         JNE   GETNAM_X                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO6'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO6                                                          
         CLI   0(R3),RAGK2TYQ      Expanded agey record (agy2)?                 
         JE    GETNAM60                                                         
         CLI   0(R3),RSA2KIDQ      Secondary salesperson record (sal2)?         
         JE    GETNAM62                                                         
         CLI   0(R3),RCMTKIDQ      Comment record?                              
         JE    GETNAM_X                                                         
         CLI   (FRSTELEM)(R3),X'01'                                             
         JNE   GETNAM_X                                                         
         MVC   0(L'RCONBUYR,R2),(RCONBUYR-RCONREC)(R3)                          
*                                                                               
         LA    R3,FRSTELEM(R3)     Point to first element                       
*                                                                               
         CLI   WORK,RSALKIDQ       Salesperson record?                          
         JNE   GETNAM52                                                         
         USING RSALELEM,R3                                                      
         MVC   SVSALTEL,RSALTEL    Save telephone number                        
         MVC   SVSALFAX,RSALFAX    Save fax number                              
         DROP  R3                                                               
*                                                                               
GETNAM52 CLI   WORK,ROFFKIDQ       Office record?                               
         JNE   GETNAM54                                                         
         USING ROFFELEM,R3                                                      
         MVC   SVOFFAD1,ROFFADD1   Office address line 1                        
         MVC   SVOFFAD2,ROFFADD2   Office address line 2                        
         MVC   SVOFFSTT,ROFFSTT    Office state                                 
         MVC   SVOFFZIP,ROFFZIP    Office zip code                              
         DROP  R3                                                               
*                                                                               
GETNAM54 DS    0H                                                               
*                                                                               
         J     GETNAM_X                                                         
*                                                                               
GETNAM60 LA    R3,FRSTELEM(R3)                                                  
         MVI   ELCODE,X'1F'                                                     
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   GETNAM_X                                                         
         USING RAG2ELEM,R3                                                      
         MVC   0(L'RAG2NAM2,R2),RAG2NAM2                                        
         J     GETNAM_X                                                         
         DROP  R3                                                               
*                                                                               
GETNAM62 LA    R3,FRSTELEM(R3)                                                  
         MVI   ELCODE,X'20'                                                     
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   GETNAM_X                                                         
         USING RSALEMEM,R3                                                      
         SR    RE,RE                                                            
         IC    RE,RSALEMLN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),RSALEMAL                                                 
         J     GETNAM_X                                                         
         DROP  R3                                                               
*                                                                               
GETNAM_X J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTBUY   NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXBUY20                                                          
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING RBUYKEY,R2                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGY                                                     
         GOTOX (RFCONNUM,AREPFACS),DMCB,(2,SVCON9CM),(3,RBUYKCON)               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO6'                            
         JNE   NXBUY_N                                                          
         J     NXBUY30                                                          
                                                                                
NXBUY20  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO6'                            
         JNE   NXBUY_N                                                          
         J     NXBUY30                                                          
                                                                                
NXBUY30  LA    R2,IOKEY                                                         
         CLC   IOKEY(RBUYKPLN-RBUYKEY),IOKEYSAV                                 
         JNE   NXBUY_N                                                          
         CLC   RBUYKPLN,=X'FFFFFF' Plan buy?                                    
         JNE   NXBUY20                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO6'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         L     R3,AIO6             Point to Buy record                          
         USING RBUYREC,R3                                                       
         TM    RBUYCNTL,X'80'      Deleted?                                     
         JNZ   NXBUY20                                                          
*                                                                               
         MVI   WKCBLSW,YESQ        Init cancelled buy line switch               
         CLI   RBUYCHGI,C'C'       Cancelled?                                   
         JNE   *+8                                                              
         MVI   WKCBLSW,NOQ         Do not reply data for cancelled buy          
*                                                                               
* Deactivated, listing of cancelled buys controlled by CON profile              
*                                                                               
* * * *  CLI   RBUYCHGI,C'C'       Cancelled?                                   
* * * *  JE    NXBUY20                                                          
*                                                                               
         BRAS  RE,SETBBUC          Set buy bucket values                        
         J     NXBUY_Y                                                          
*                                                                               
NXBUY_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXBUY_Y  MVC   LP_ADATA,ARBUYREC                                                
         J     EXITY                                                            
*                                                                               
NXBUY_ER MVI   LP_RMODE,LP_RLAST   No more - error                              
*                                                                               
NXBUY_N  MVC   LP_ADATA,ARBUYREC                                                
         J     EXITN                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4,R3                                                         
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
         L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'02'        Look for Day/Time elements                   
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
NXDYT14  BRAS  RE,NXTELEM                                                       
         JNE   NXDYT_X                                                          
         USING RBUYDYEL,R3                                                      
         LLC   RF,RBUYDYIN                                                      
         SRL   RF,4                                                             
         STC   RF,X02STRDY         Start day                                    
         MVC   X02ENDDY,RBUYDYIN                                                
         NI    X02ENDDY,X'0F'      End day                                      
*                                                                               
         TM    RBUYDAYS,X'80'      End time CC override?                        
         JNZ   NXDYT16                                                          
         MVC   X02DAYS_,=C'NNNNNNN'                                             
         TM    RBUYDAYS,X'40'      Monday?                                      
         JZ    *+8                                                              
         MVI   X02DAYS_+0,C'Y'                                                  
         TM    RBUYDAYS,X'20'      Tuesday?                                     
         JZ    *+8                                                              
         MVI   X02DAYS_+1,C'Y'                                                  
         TM    RBUYDAYS,X'10'      Wednesday?                                   
         JZ    *+8                                                              
         MVI   X02DAYS_+2,C'Y'                                                  
         TM    RBUYDAYS,X'08'      Thursday?                                    
         JZ    *+8                                                              
         MVI   X02DAYS_+3,C'Y'                                                  
         TM    RBUYDAYS,X'04'      Friday?                                      
         JZ    *+8                                                              
         MVI   X02DAYS_+4,C'Y'                                                  
         TM    RBUYDAYS,X'02'      Saturday?                                    
         JZ    *+8                                                              
         MVI   X02DAYS_+5,C'Y'                                                  
         TM    RBUYDAYS,X'01'      Sunday?                                      
         JZ    *+8                                                              
         MVI   X02DAYS_+6,C'Y'                                                  
*                                                                               
NXDYT16  CLC   RBUYDYT1(04),=C'NONE'                                            
         JE    *+14                                                             
         CLC   RBUYDYT1(04),=C'VARY'                                            
         JNE   *+10                                                             
         MVC   X02TMTXT,RBUYDYT1   Save Time Text                               
*                                                                               
         CLC   RBUYDYT1,=C'CC'     Start time CC override?                      
         JE    NXDYT17                                                          
         CLI   RBUYDYT1,X'FF'                                                   
         JE    NXDYT16A                                                         
*        MVC   X02STRTM,RBUYDYT1   SAVE START TIME                              
         EDIT  RBUYDYT1,X02STRTM,FILL=0                                         
*                                                                               
NXDYT16A CLC   RBUYDYT2,=C'CC'     END TIME CC OVERRIDE?                        
         JE    NXDYT17                                                          
         CLI   RBUYDYT2,X'FF'                                                   
         JE    NXDYT18                                                          
*        MVC   X02ENDTM,RBUYDYT2   SAVE END TIME                                
         OC    RBUYDYT2,RBUYDYT2   IF SINGLE TIME, SKIP END TIME                
         JZ    NXDYT18                                                          
         EDIT  RBUYDYT2,X02ENDTM,FILL=0                                         
         J     NXDYT18                                                          
*                                                                               
NXDYT17  MVI   X02TCCSW,C'Y'       Y=end time CC override                       
*                                                                               
NXDYT18  MVC   X02DAYTW,RBUYDYWT   Save Day/Time Weighting Factor               
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
NXDYT_Y  MVC   LP_ADATA,ARBUYREC                                                
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
         L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'03'        Look for Buy Effective date elements         
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
NXEFD14  BRAS  RE,NXTELEM                                                       
         JNE   NXEFD_X                                                          
         USING RBUYDTEL,R3                                                      
         MVC   X03STRDT,RBUYDTST                                                
         MVC   X03ENDDT,RBUYDTED                                                
         MVC   X03#OFWK,RBUYDTWK                                                
         TM    RBUYDTIN,X'80'      Runs every week?                             
         JZ    *+8                                                              
         MVI   X03WEKSW,C'E'       Set to Every week                            
         TM    RBUYDTIN,X'40'      Runs every other week?                       
         JZ    *+8                                                              
         MVI   X03WEKSW,C'A'       Set to Alternating week                      
         MVC   X03SPTWK,RBUYDTNW                                                
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
NXEFD_Y  MVC   LP_ADATA,ARBUYREC                                                
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTMODC  NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         XC    XD0VALST(XD0LENGQ),XD0VALST             Init output              
*                                                                               
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXMDC20                                                          
         XC    FULL1,FULL1         Init elem pointer                            
         L     R3,AIO6             Point to Buy record                          
         LA    R3,(FRSTELEM)(R3)                                                
         MVI   ELCODE,X'D0'        Look for Mod code trap elements              
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
NXMDC14  BRAS  RE,NXTELEM                                                       
         JNE   NXMDC_X                                                          
         USING RBUYMCEL,R3                                                      
         MVC   XD0MODV#,RBUYMCVR                                                
         MVC   XD0MODCD,RBUYMCMO                                                
         ST    R3,FULL1            Save current elem pointer                    
         J     NXMDC30                                                          
         DROP  R3                                                               
*                                                                               
NXMDC20  L     R3,FULL1            Point to previous elem                       
         J     NXMDC14                                                          
*                                                                               
NXMDC30  DS    0H                                                               
         J     NXMDC_Y                                                          
*                                                                               
NXMDC_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXMDC_Y  MVC   LP_ADATA,ARBUYREC                                                
         J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTMGCM  NTR1  BASE=*,LABEL=*                                                   
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         XC    ELEM,ELEM           Init output                                  
         XC    ELEM2,ELEM2                                                      
*                                                                               
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXMGC20                                                          
         XC    FULL1,FULL1         Init makegood comment pointer                
         L     R3,AIO2             Point to makegood comments                   
         ST    R3,FULL1                                                         
         USING RMKGDCMD,R3                                                      
NXMGC14  CLI   RMKGDCTY,0          End of record?                               
         JE    NXMGC_X                                                          
         CLI   RMKGDCTY,RMKGORGQ                                                
         JNE   *+10                                                             
         MVC   ELEM2(L'RMKGCOMM),RMKGCOMM                                       
         CLI   RMKGDCTY,RMKGMGOQ                                                
         JNE   *+10                                                             
         MVC   ELEM(L'RMKGCOMM),RMKGCOMM                                        
*                                                                               
         AHI   R3,RMKGDELQ                                                      
         ST    R3,FULL1            Point to next comment                        
*                                                                               
         J     NXMGC_Y                                                          
         DROP  R3                                                               
*                                                                               
NXMGC20  L     R3,FULL1            Point to current comment elem                
         J     NXMGC14                                                          
*                                                                               
NXMGC30  DS    0H                                                               
         J     NXMGC_Y                                                          
*                                                                               
NXMGC_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXMGC_Y  MVC   LP_ADATA,ARCMTREC                                                
         J     EXITY                                                            
*                                                                               
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETBBUC  NTR1  BASE=*,LABEL=*      Set buy bucket values                        
*                                                                               
         L     R3,AIO6             Point to Buy record                          
         USING RBUYREC,R3                                                       
         TM    RBUYCNTL,X'80'      Deleted?                                     
         JNZ   SETBBK_X                                                         
         CLI   RBUYCHGI,C'C'       Cancelled?                                   
         JE    SETBBK_X                                                         
*                                                                               
         MVC   WORK+00(4),VGTBROAD Required parameters for GENBUC               
         MVC   WORK+04(4),VGETDAY                                               
         MVC   WORK+08(4),VADDAY                                                
         MVC   WORK+12(4),VDATCON                                               
*                                                                               
         GOTOR (RFGENBUC,AREPFACS),DMCB,RBUYREC,ELEM,WORK                       
*                                                                               
         LA    RF,ELEM                                                          
         CLC   0(2,RF),=H'2'       Have data?                                   
         JE    SETBBK_X                                                         
         SR    R0,R0               For bumping elements                         
         LA    R3,ELEM             Prepare to total up buy buckets              
         LA    R3,2(R3)            Point to first buy bucket element            
SETBBK32 LA    R4,BYTOTBDA         Point to buy total bucket elements           
*                                                                               
SETBBK34 CLI   0(R4),0             End of total bucket elements?                
         JE    SETBBK38                                                         
         CLC   2(2,R3),2(R4)       Total bucket date is earlier?                
         JL    SETBBK38                                                         
         JE    SETBBK48            Same date, need to total                     
         IC    R0,1(R4)            LEN                                          
         AR    R4,R0               Bump to next bucket element                  
         J     SETBBK34                                                         
*                                                                               
SETBBK38 GOTOR VRECUP,DMCB,(X'FF',BYTOTBUC),(R3),(R4)                           
*                                                                               
SETBBK36 IC    R0,1(R3)            Bump to next buy bucket                      
         AR    R3,R0                                                            
         CLI   0(R3),0             More buy bucket elem to process?             
         JNE   SETBBK32                                                         
         J     SETBBK60                                                         
*                                                                               
SETBBK48 MVC   DUB(8),6(R3)        Add to total bucket                          
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R4)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R4),DUB                                                      
         J     SETBBK36                                                         
*                                                                               
SETBBK60 DS    0H                                                               
*                                                                               
SETBBK_X J     EXITY                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NOQ      EQU   C'N'                                                             
YESQ     EQU   C'Y'                                                             
EOR      EQU   0                   End of record element code                   
EFF      EQU   X'FF'               End of table marker                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SAVED    DSECT                                                                  
*                                                                               
VRECUP   DS    A                   A(RECUP)                                     
ANXTSTA  DS    A                   A(next Station in work map pool)             
ANXTCON  DS    A                   A(next Contract # in work map pool)          
SVSPLSTR DS    AL4                 Start of mini SPL elements                   
SVMGCPTR DS    F                   Save makegood comment pointer                
WKFRSTEL DS    F                   General first element pointer                
WKAOUTFD DS    F                   General output field pointer                 
*                                                                               
QCONIND  DS    X                   Contract # request array                     
QACON    DS    AL3                                                              
NUMCON   DS    XL(L'LW_NUMN)       N'Contract numbers to process                
*                                                                               
QSTAIND  DS    X                   Station Call Letter array                    
QASTA    DS    AL3                                                              
NUMSTA   DS    XL(L'LW_NUMN)       N'Stations to process                        
*                                                                               
AGY      DS    CL2                 Agency code                                  
MASTRCOD DS    CL2                 Master Rep Code                              
*                                                                               
WKIOSAVE DS    XL(IOVALL)          Saved i/o values                             
*                                                                               
SVREPNAM DS    CL(L'RREPNAME)      Rep name                                     
REPPRGPF DS    (RREPPGML*15)X      Set of 15 rep program profiles               
SOMPBITQ EQU   ((RREPQSOM-1)*RREPPGML)+2     Displacement to prof bits          
R_SOMPRF DS    CL64                Bit translated SOM program profile           
R_REPPRF DS    CL(L'RREPPROF)      Rep profile values                           
PENDCFSD DS    XL3                 Pending Contract Flight start date           
EMBCOMSW DS    CL1                 Embedded comment processing switch           
SVEMBCMC DS    CL(L'RCMTKCDE)      Embedded comment code                        
BUYBUKSW DS    CL1                 Buy bucket reply switch                      
*                                                                               
PROFDATA DS    0CL10               Contract profile data from REP rec           
PROFEQU  DS    X                   1=Profiles loaded/0=Not                      
         DS    X                   Unused at this time                          
PROFILES DS    XL8                 64 Profile bits                              
*                                                                               
WKBUYRTS DS    XL(L'RBUYRTS)       Working flag - Buy Status/Type               
WKTEMPSW DS    X                   Temp working switch                          
WKTMPELM DS    XL256               Temp working elem                            
WKBBKSW  DS    CL1                 Buy bucket data reply switch                 
WKCBLSW  DS    CL1                 Cancelled buy line filtering switch          
WKSNDDAT DS    XL2                 Compressed send date (Rep or Sta)            
WKSNDTIM DS    CL6                 Send time (Rep or Station)                   
WKCMTYPE DS    XL1                 Comment rec type to expand comm code         
*                                                                               
SVSTALET DS    CL(L'RCON9DST)      Station Call Letter                          
SVCON9CM DS    XL(L'RCONPCON)      Contract # in 9's complement                 
SVAGYCON DS    CL(L'RCONDRFG)      Agency Connect flag                          
SVNUMSPL DS    XL2                 Number of mini SPL elements                  
SVBKDATE DS    XL3                 Bucket date (RCONBKYR+RCONBKMN)              
SVSPLTOT DS    XL4                 Competitive Market Total                     
SVCONFSW DS    XL(L'RCONCONF)      Save confirmation switch                     
SVCPNDSW DS    CL1                 Save contract pending flag                   
CONWSIDE DS    CL1                 Contract side, R=Rep and S=Station           
CONSNDSW DS    CL1                 Contract has SEND elem switch - Y/N          
SVVERSN# DS    XL1                 Latest version number                        
CONSTATS DS    CL19                Contract status                              
*                                                                               
ABSTRDAT DS    XL(L'RBUYDTST)      Absolute Start Date                          
ABENDDAT DS    XL(L'RBUYDTED)      Absoulte End Date                            
SVBYDEMV DS    XL(L'RBUYRDDM)      Saved Buy Demo value                         
SVBYDEMI DS    C                   * means override (Rep demo value)            
TOTBYSPT DS    XL4                 Calculated buy total spots                   
BUYLNGRP DS    CL8                 Buy line GRP (char format, 1 dec)            
*                                                                               
SVNAMSTR DS    0X                                                               
SVAGYKEY DS    XL(L'RAGY2KEY)      Agency key                                   
SVAGYNAM DS    CL(L'RAG2NAM2)      Agency name (contract name)                  
SVADVKEY DS    XL(L'RADVKEY)       Advertiser key                               
SVADVNAM DS    CL(L'RADVNAME)      Advertiser name                              
SVOFFKEY DS    XL(L'ROFFKEY)       Office key                                   
SVOFFNAM DS    CL(L'ROFFNAME)      Office name                                  
SVOFFAD1 DS    CL(L'ROFFADD1)      Office address line 1                        
SVOFFAD2 DS    CL(L'ROFFADD2)      Office address line 2                        
SVOFFSTT DS    CL(L'ROFFSTT)       Office state                                 
SVOFFZIP DS    CL(L'ROFFZIP)       Office zip code                              
SVPRDKEY DS    XL(L'RPRDKEY)       Product key                                  
SVPRDNAM DS    CL(L'RPRDNAME)      Product name                                 
SVSALKEY DS    XL(L'RSALKEY)       Salesperson key                              
SVSALNAM DS    CL(L'RSALNAME)      Salesperson name                             
SVSALTEL DS    CL(L'RSALTEL)       Salesperson telephone number                 
SVSALFAX DS    CL(L'RSALFAX)       Salesperson fax number                       
SVSA2KEY DS    XL(L'RSA2KEY)       Secondary salesperson key                    
SVSALEML DS    CL48                Salesperson e-mail                           
SVCTYKEY DS    XL(L'RCTYKEY)       Contract Type key                            
SVCTYNAM DS    CL(L'RCTYDESC)      Contract Type name                           
SVSTAKEY DS    XL(L'RSTAKEY)       Station key                                  
SVSTANAM DS    CL(L'RSTAMKT)       Station Market name                          
SVCMTKEY DS    XL(L'RCMTKEY)       Comment key                                  
SVCOVKEY DS    XL(L'RCOVKEY)       Coversheet key                               
SVNAMLNQ EQU   *-SVNAMSTR                                                       
*                                                                               
X02VALST DS    0X                  Values from X'02' element in Buy rec         
X02STRDY DS    XL(L'RBUYDYIN)      Start day                                    
X02ENDDY DS    XL(L'RBUYDYIN)      End day                                      
X02DAYS_ DS    CL7                                                              
X02STRTM DS    CL4                 START TIME                                   
X02ENDTM DS    CL4                 END TIME                                     
X02DAYTW DS    XL(L'RBUYDYWT)      Day/Time Weighting Factor                    
X02TCCSW DS    C                   Y=end time is displayed CC                   
X02TMTXT DS    CL4                 Time text (NONE or VARY)                     
X02LENGQ EQU   *-X02VALST                                                       
*                                                                               
X03VALST DS    0X                  Values from X'03' element in Buy rec         
X03STRDT DS    XL(L'RBUYDTST)      Start date                                   
X03ENDDT DS    XL(L'RBUYDTED)      End date                                     
X03#OFWK DS    XL(L'RBUYDTWK)      Number of weeks                              
X03WEKSW DS    C                   Week/Alernate Week switch                    
X03SPTWK DS    XL(L'RBUYDTNW)      Spots per week                               
X03LENGQ EQU   *-X03VALST                                                       
*                                                                               
XD0VALST DS    0X                  Values from X'D0' element in Buy rec         
XD0MODV# DS    XL(L'RBUYMCVR)      Mod version                                  
XD0MODCD DS    XL(L'RBUYMCMO)      Mod codes                                    
XD0LENGQ EQU   *-XD0VALST                                                       
*                                                                               
WKIOKEY  DS    XL(L'IOKEY)                                                      
*                                                                               
BYTOTBUC DS    0X                  Buy total bucket storage area                
BYTOTBOV DS    XL2                 Bucket overhead (length)                     
BYTOTBDA DS    XL(BYTBDALQ)        Bucket elements                              
BYTOTBXX DS    X                   End of bucket marker                         
BYTOTBLQ EQU   *-BYTOTBUC                                                       
BYTBDALQ EQU   BYTBMAXQ*BYTBLENQ                                                
*                                                                               
BYGTBSPT DS    XL4                 Buy grand total spots                        
BYGTBAMT DS    XL4                 Buy grand total amount                       
BYGTGRPS DS    PL8                 Total GRP                                    
*                                                                               
SECVALS  DS    0X                                                               
*                                                                               
* FIELD SECURITY VALUES                                                         
*                                                                               
SECFDUM1 DS    C                                                                
SECFDUM2 DS    C                                                                
*                                                                               
* ACTION SECURITY VALUES                                                        
*                                                                               
SECADUM1 DS    C                                                                
SECADUM2 DS    C                                                                
SECVALSL EQU   *-SECVALS                                                        
                                                                                
SECBLOCK DS    (SECLENQ)X          SECRET BLOCK                                 
                                                                                
SAVEL    EQU   *-SAVED                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
RMKGDCMD DSECT                     Makegood buy comment lines                   
RMKGDCTY DS    X                   Type of comment                              
RMKGORGQ EQU   001                 Makegood original comment (target)           
RMKGMGOQ EQU   002                 Makegood offer comment                       
RMKGCLEN DS    X                   Length                                       
RMKGCOMM DS    CL100               Comment                                      
RMKGDELQ EQU   *-RMKGDCTY                                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BYTOTBKD DSECT                     Buy total bucket elements                    
BYTBELCD DS    X                   Element code                                 
BYTBELCQ EQU   X'03'                                                            
BYTBELLN DS    X                   Element length                               
BYTBYRMO DS    0XL2                Year/Month (lable)                           
BYTBYEAR DS    X                   Year                                         
BYTBMON_ DS    X                   Month                                        
BYTBUNK1 DS    X                   Unknown byte 1                               
BYTBUNK2 DS    X                   Unknown byte 2                               
BYTB$AMT DS    XL4                 Monthly amount                               
BYTBSPOT DS    XL4                 Monthly spots                                
BYTBLENQ EQU   *-BYTBELCD                                                       
BYTBMAXQ EQU   12+1                Max of 13 bucket elements                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EMBCOMMD DSECT                     Miscellaneous uses                           
EMBCOMM1 DS    CL(L'RCMT2TXT)      Embedded comment line 1                      
EMBCOMM2 DS    CL(L'RCMT2TXT)                                                   
EMBCOMM3 DS    CL(L'RCMT2TXT)                                                   
EMBCOMM4 DS    CL(L'RCMT2TXT)                                                   
EMBCOMM5 DS    CL(L'RCMT2TXT)                                                   
EMBCOMM6 DS    CL(L'RCMT2TXT)                                                   
EMBCOMM7 DS    CL(L'RCMT2TXT)                                                   
EMBCOMM8 DS    CL(L'RCMT2TXT)                                                   
EMBCOMM9 DS    CL(L'RCMT2TXT)                                                   
EMBCOMMA DS    CL(L'RCMT2TXT)                                                   
EMBCOMMB DS    CL(L'RCMT2TXT)                                                   
EMBCOMMC DS    CL(L'RCMT2TXT)      Max of 12 comment lines                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DARSDCMD DSECT                     Dare spot/date line comment                  
DARSDCM1 DS    CL60                                                             
DARSDCM2 DS    CL(L'DARSDCM1)                                                   
DARSDCM3 DS    CL(L'DARSDCM1)                                                   
DARSDCM4 DS    CL(L'DARSDCM1)                                                   
DARSDCM5 DS    CL(L'DARSDCM1)                                                   
DARSDCM6 DS    CL(L'DARSDCM1)      Max of 6 comment lines                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE RELNKWRK                                                       
B#COV    EQU   3                   Coversheet record                            
B#CMT    EQU   3                   Comment record                               
B#AGY    EQU   4                   Agency record                                
B#STA    EQU   5                   Station record                               
B#CON    EQU   5                   Contract record                              
B#BUY    EQU   6                   Buy record                                   
*                                                                               
B#EBC    EQU   7                   For embedded comments                        
B#DAC    EQU   7                   For DARE spot/date line comments             
*                                                                               
ARCOVREC EQU   LP_BLKS+((B#COV-1)*L'LP_BLKS),,C'A'                              
ARCMTREC EQU   LP_BLKS+((B#CMT-1)*L'LP_BLKS),,C'A'                              
AREBCREC EQU   LP_BLKS+((B#EBC-1)*L'LP_BLKS),,C'A'                              
ARAGYREC EQU   LP_BLKS+((B#AGY-1)*L'LP_BLKS),,C'A'                              
ARSTAREC EQU   LP_BLKS+((B#STA-1)*L'LP_BLKS),,C'A'                              
ARCONREC EQU   LP_BLKS+((B#CON-1)*L'LP_BLKS),,C'A'                              
ARBUYREC EQU   LP_BLKS+((B#BUY-1)*L'LP_BLKS),,C'A'                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017RELNK12   04/11/14'                                      
         END                                                                    
