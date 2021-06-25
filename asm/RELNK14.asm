*          DATA SET RELNK14    AT LEVEL 001 AS OF 10/17/12                      
*PHASE T82B14A                                                                  
RELNK14  TITLE '- Rep system download - Audit'                                  
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,REQUEST=*,CODE=CODE,SYSTEM=REPSYSQ,              +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,                    +        
               B#COV,RCOVRECD,B#CMT,RCMTRECD,B#AGY,RAGYRECD,           +        
               B#STA,RSTARECD,B#CON,RCONRECD,B#BUY,RBUYRECD,           +        
               B#MGC,RMKGDCMD,B#MKG,RMKGRECD,B#AUD,RAUDRECD)                    
                                                                                
CODE     NMOD1 0,**RL14**,RR=RE                                                 
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
         MVC   LP_BLKS+((B#CMT-1)*L'LP_BLKS),AIO3                               
         MVC   LP_BLKS+((B#AGY-1)*L'LP_BLKS),AIO4                               
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
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Refresh Audit data request                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
REQAUD   LKREQ H,Q#AUDFR,OUTAUD,NEXTREQ=REQEND                                  
OrdNumb  LKREQ F,001,(D,B#SAVED,QORDNUMB),SPAK,TEXT=(*,ORD#LIT),COL=*           
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Refresh Audit data reply                                            *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
OUTAUD   LKOUT H                                                                
                                                                                
AUDITR   LKOUT R,R#AUDFR                                                        
Array    LKOUT C,R#AUDFR,(A,ARYAUD)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
* Array definition for Audit record download                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
ARYAUD   LKOUT A,(R,NXTAUD),MULTIROW=Y,ROWNAME=RAUDRECD                         
Versn#   LKOUT C,001,RAUDKVER,(R,EDTVER#)                                       
ModNum   LKOUT C,002,RAUDKMOD,(R,EDTMOD#),ND=Y                                  
Array    LKOUT C,003,(A,ARY_X10)                                                
Array    LKOUT C,007,(A,ARY_X15)                                                
Array    LKOUT C,010,(A,ARY_X30)                                                
         LKOUT E                                                                
*                                                                               
ARY_X10  LKOUT A,(D,B#AUD,RAUDEL1),EOT=EOR,ROWID=(RAUDTHEL,RAUDTHQ),   +        
               ROWWIDTH=(V,RAUDTHLN)                                            
SendDat  LKOUT C,003,RAUDTHDT,CDAT,ND=Y                                         
SendTim  LKOUT C,004,RAUDTHTM,CHAR,ND=Y                                         
Send_By  LKOUT C,005,RAUDTHTP,CHAR,ND=Y                                         
OrderTo  LKOUT C,006,RAUDTH$,SPAK,ND=Y                                          
PID____  LKOUT C,008,RAUDTHPD,(R,EDTBPID),ND=Y                                  
UserNam  LKOUT C,009,RAUDTHAS,CHAR,ND=Y                                         
         LKOUT E                                                                
*                                                                               
ARY_X15  LKOUT A,(D,B#AUD,RAUDEL1),EOT=EOR,ROWID=(RAUDTXEL,RAUDTXQ),   +        
               ROWWIDTH=(V,RAUDTXLN)                                            
AudComm  LKOUT C,007,RAUDTEXT,CHAR,LEN=V                                        
         LKOUT E                                                                
*                                                                               
ARY_X30  LKOUT A,(D,B#AUD,RAUDEL1),EOT=EOR,ROWID=(RAUDSUEL,RAUDSUQ),   +        
               ROWWIDTH=(V,RAUDSULN)                                            
AudComm  LKOUT C,010,RAUDSUNM,CHAR,LEN=V                                        
         LKOUT E                                                                
*                                                                               
EDTVER#  LM    R2,R4,LP_AINP                                                    
         SR    RF,RF                                                            
         AHI   RF,EFF              X'FF'                                        
         LLC   RE,0(R2)                                                         
         SR    RF,RE                                                            
         EDIT  (RF),(3,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                           
         LHI   R0,3                                                             
         J     SETOLENX                                                         
*                                                                               
EDTMOD#  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),EFF           No mod number?                               
         JE    XCOLEN                                                           
         SR    RF,RF                                                            
         AHI   RF,EFF              X'FF'                                        
         LLC   RE,0(R2)                                                         
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EDIT  (RF),(3,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                           
         LHI   R0,3                                                             
         J     SETOLENX                                                         
*                                                                               
         USING EPWORKD,RC                                                       
         USING SA0REC,EPIO                                                      
         USING RAUDTHPD,R2                                                      
EDTBPID  LM    R2,R4,LP_AINP                                                    
         MVC   EPIOSAVE,IOVALS     Save current i/o values                      
         LHI   R0,L'SAPALPID                                                    
         ST    R0,LP_OLEN                                                       
         OC    RAUDTHPD,RAUDTHPD   Have PID?                                    
         JZ    EDTPIDN                                                          
*                                                                               
         XC    SA0KEY,SA0KEY       Read person password record                  
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KNUM,RAUDTHPD                                                 
         MVC   SA0KAGY,RAUDTHAG                                                 
         MVC   IOKEY,SA0KEY                                                     
         LA    R1,EPIO                                                          
         ST    R1,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE'                                
         JNE   EDTPIDN                                                          
                                                                                
         LA    R1,SA0DATA                                                       
         USING SAPALD,R1                                                        
EDTPID02 CLI   SAPALEL,EOR         Test end of record                           
         JE    EDTPIDN                                                          
         CLI   SAPALEL,SAPALELQ    Test new security person element             
         JE    EDTPID04                                                         
         LLC   R0,SAPALLN          Bump to next element                         
         AR    R1,R0                                                            
         J     EDTPID02                                                         
                                                                                
EDTPID04 MVC   0(L'SAPALPID,R4),SAPALPID                                        
         J     EDTPIDY                                                          
         DROP  R1                                                               
                                                                                
EDTPIDN  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     XCOLEN                                                           
                                                                                
EDTPIDY  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         J     EXITY                                                            
         DROP  RC,R2                                                            
                                                                                
EPWORKD  DSECT ,                   ** EDTBPID local working storage **          
EPIOSAVE DS    XL(IOVALL)          Saved i/o values                             
EPIO     DS    XL1000              I/O area                                     
SVRDEF   CSECT ,                                                                
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
ORD#LIT  DC    C'Order Number'                                                  
                                                                                
         DROP  RB,R1                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTAUD   NTR1  BASE=*,LABEL=*      Get Audit records                            
*                                                                               
         L     R4,ALP                                                           
         USING LP_D,R4                                                          
         CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXAUD20                                                          
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
*                                                                               
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),QORDNUMB                                              
         MVO   WORK(5),WORK+10(5)                                               
         MVC   SVCON9CM,WORK                                                    
         GOTOR (RFCONNUM,AREPFACS),DMCB,(2,SVCON9CM),(1,SVCONXL4)               
         GOTOR (RFCONNUM,AREPFACS),DMCB,(2,SVCON9CM),(3,SVCON9RV)               
*                                                                               
         LA    RE,IOKEY                                                         
         USING RAUDKEY,RE                                                       
         MVI   RAUDKTYP,RAUDKIDQ   Audit record                                 
         MVC   RAUDKREP,AGY                                                     
         MVC   RAUDKCON,SVCONXL4                                                
         DROP  RE                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOREPDIR+IO3'                            
         JNE   NXAUD_N                                                          
         J     NXAUD30                                                          
*                                                                               
NXAUD20  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOREPDIR+IO3'                            
         JNE   NXAUD_N                                                          
*                                                                               
NXAUD30  CLC   IOKEY(RAUDKVER-RAUDKEY),IOKEYSAV                                 
         JNE   NXAUD_X                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOREPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO3                                                          
         LA    R3,(FRSTELEM)(R3)                                                
         USING RAUDTHEL,R3                                                      
         MVI   ELCODE,X'10'        Audit comment header elem code               
         CLC   ELCODE,0(R3)                                                     
         JE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         JNE   NXAUD20                                                          
*                                                                               
         CLI   RAUDTHTP,C'S'       Station?                                     
         JNE   NXAUD90X                                                         
         XC    RAUDTHPD,RAUDTHPD   Send PID is not station                      
         XC    RAUDTHAS,RAUDTHAS   Send user name is not station                
*                                                                               
NXAUD90X J     NXAUD_Y                                                          
*                                                                               
NXAUD_X  MVI   LP_RMODE,LP_RLAST   No more                                      
*                                                                               
NXAUD_Y  MVC   LP_ADATA,ARAUDREC                                                
         J     EXITY                                                            
*                                                                               
NXAUD_ER MVI   LP_RMODE,LP_RLAST   No more - error                              
*                                                                               
NXAUD_N  MVC   LP_ADATA,ARAUDREC                                                
         J     EXITN                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R4                                                            
         J     EXIT                                                             
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
AGY      DS    CL2                 Agency code                                  
MASTRCOD DS    CL2                 Master Rep Code                              
*                                                                               
QORDNUMB DS    CL(L'RAUDKCON+1)    Order number                                 
*                                                                               
SVCONXL4 DS    XL(L'RAUDKCON)      Contract # in PWOS format                    
SVCON9CM DS    XL(L'RCONPCON)      Contract # in 9's complement                 
SVCON9RV DS    XL(L'RMKGKCON)      Con# in 9's complement/Reversed              
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
B#AUD    EQU   3                   Audit record                                 
B#AGY    EQU   4                   Agency record                                
B#STA    EQU   5                   Station record                               
B#CON    EQU   5                   Contract record                              
B#MKG    EQU   5                   Makegood record                              
B#BUY    EQU   6                   Buy record                                   
                                                                                
ARMKGDCM EQU   LP_BLKS+((B#MGC-1)*L'LP_BLKS),,C'A'                              
ARCOVREC EQU   LP_BLKS+((B#COV-1)*L'LP_BLKS),,C'A'                              
ARCMTREC EQU   LP_BLKS+((B#CMT-1)*L'LP_BLKS),,C'A'                              
ARAUDREC EQU   LP_BLKS+((B#AUD-1)*L'LP_BLKS),,C'A'                              
ARAGYREC EQU   LP_BLKS+((B#AGY-1)*L'LP_BLKS),,C'A'                              
ARSTAREC EQU   LP_BLKS+((B#STA-1)*L'LP_BLKS),,C'A'                              
ARCONREC EQU   LP_BLKS+((B#CON-1)*L'LP_BLKS),,C'A'                              
ARMKGREC EQU   LP_BLKS+((B#MKG-1)*L'LP_BLKS),,C'A'                              
ARBUYREC EQU   LP_BLKS+((B#BUY-1)*L'LP_BLKS),,C'A'                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001RELNK14   10/17/12'                                      
         END                                                                    
