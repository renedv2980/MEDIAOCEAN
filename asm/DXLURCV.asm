*          DATA SET DXLURCV    AT LEVEL 046 AS OF 03/09/03                      
*PHASE DXLURCV                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDICTEXT -- EDICT EXTRACT TRANSFER SYSTEM SUB TASK   *         
*                            CONTROLLING APPC LU 6.2 COMMUNICATIONS   *         
*                            BETWEEN MVS AND A REMOTE SERVER LUID.    *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- WORK                                           *         
*                R8 -- CPRINT                                         *         
*                R9 -- PARAMETERS FROM EDICT (VIA R1)                 *         
*                RA -- 2ND BASE                                       *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- LOCAL WORKING STORAGE                          *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDICTEXT -- EXTRACT TRANSFER APPC LU 6.2 CONTROL'             
         MACRO                                                                  
&NAME    PRNT  &A                                                               
&NAME    MVC   P(18),=CL18'&A'                                                  
         TIME  DEC                                                              
         ST    R0,DUB                                                           
         UNPK  WORK(7),DUB(4)                                                   
         MVC   P+20(2),WORK                                                     
         MVI   P+22,C':'                                                        
         MVC   P+23(2),WORK+2                                                   
         MVI   P+25,C':'                                                        
         MVC   P+26(2),WORK+4                                                   
         GOTO1 =V(PRINTER)                                                      
         MEND                                                                   
         EJECT                                                                  
DXLUDRV  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DXLUDRV*,=A(R13CHAIN),RA                                       
*                                                                               
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=CL8'DXRCVPRT'                             
*                                                                               
         L     RF,=A(APPCLOGR)     A(APPC LOG FILE)                             
         MVC   DCBDDNAM-IHADCB(8,RF),=CL8'DXRCVLOG'                             
*                                                                               
         XC    TESTECB,TESTECB                                                  
         LA    R1,TESTECB                                                       
         STCM  R1,7,APPCPARM+21                                                 
         OI    APPCPARM+20,X'80'                                                
*                                                                               
         MVI   OPERSTOP,C'N'                                                    
         OPEN  (APPCLOGR,OUTPUT)                                                
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     EQU   *                                                                
*                                                                               
MOPEN    BAL   RE,APPCOPEN         OPEN APPC LINK                               
*                                                                               
MTEST    BAL   RE,APPCTEST         TEST APPC LINK                               
         BNE   MSHUT                                                            
         B     MTEST                                                            
*                                                                               
MTEST1   BAL   RE,APPCTST1         TEST APPC LINK                               
         BNE   MSHUT                                                            
         B     MTEST1                                                           
*                                                                               
MCLOS    BAL   RE,APPCCLOS         APPC CLOSE                                   
         BNE   MSHUT                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    MSHUT                                                            
         B     MOPEN                                                            
*                                                                               
MSHUT    BAL   RE,SHUTDOWN         SHUT DOWN SUB TASK                           
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE APPC LU6.2 LINK TO SERVER LU                             *         
* WAIT FOR OPEN REQUEST FROM REMOTE SERVER LU                         *         
* OPEN IN 'OPEN TO SEND' MODE                                         *         
***********************************************************************         
         SPACE 1                                                                
APPCOPEN NTR1                                                                   
         MVC   APPCACTN,APPCACOR                                                
         MVC   APPCDATA(8),=CL8'LUXRUK1 '                                       
         MVC   APPCDATA+8(8),=CL8'LUXSUK1 '                                     
         MVC   APPCDATA+16(8),=CL8'DDSLU62 '                                    
         MVC   APPCDATA+24(8),=CL8'EDICT   '                                    
*                                                                               
         OI    APPCPARM+20,X'80'                                                
         LA    R1,APPCPARM         PARAMETERS TO LU6.2 LINK                     
         LINK  EP=DDSAPPC                                                       
         MVC   P+30(9),=C'REG 15 = '                                            
         EDIT  (RF),(5,P+39),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  OPENEXTRACTSENDER                                                
         B     AOPEOK                                                           
AOPEOK   SR    RC,RC                                                            
AOPENO   LTR   RC,RC                                                            
AOPEX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SEND TEST RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
APPCTEST NTR1                                                                   
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         MVC   APPCDTA(10),=C'          '                                       
         MVI   APPCDTA+10,C' '                                                  
         LA    R1,11                                                            
         MVC   HALF,APPCACR                                                     
         BAL   RE,APPCCOM                                                       
         BE    ATSTERR                                                          
         CLI   APPCHDR,X'80'                                                    
         BNE   ATSTERR                                                          
         PRNT  SENTEXTRACTREC1                                                  
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         MVC   APPCDTA(10),=C'SEND FILE '                                       
         MVI   APPCDTA+10,C' '                                                  
         LA    R1,11                                                            
         MVC   HALF,APPCACSD       ISSUE SEND CONFIRM                           
         BAL   RE,APPCCOM                                                       
         BNE   ATSTERR                                                          
         CLI   OPERSTOP,C'Y'                                                    
         BE    ATSTSTOP                                                         
         PRNT  SENTEXTRACTREC3                                                  
*                                                                               
ATSTLOOP LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         MVC   APPCDTA(10),=C'RECEIVEFIL'                                       
         MVI   APPCDTA+10,C' '                                                  
         LA    R1,11                                                            
         MVC   HALF,APPCACR        ISSUE SEND CONFIRM                           
         BAL   RE,APPCCOM                                                       
         BNE   ATSTNEXT                                                         
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         B     ATSTLOOP                                                         
ATSTNEXT CLI   APPCHDR,X'40'                                                    
         BNE   ATSTERR                                                          
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         MVC   APPCDTA(10),=C'RCV CONFIR'                                       
         MVI   APPCDTA+10,C' '                                                  
         LA    R1,11                                                            
         MVC   HALF,APPCACRC       ISSUE SEND CONFIRM                           
         BAL   RE,APPCCOM                                                       
         BNE   ATSTERR                                                          
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         PRNT  SENTEXTRACTCONFIRM                                               
         B     ATSTOK                                                           
*                                  DEBUG MESSAGE                                
*                                                                               
ATSTWAIT BAL   RE,WAITABIT                                                      
         BNE   ATSTNO                                                           
         B     ATSTOK                                                           
*                                                                               
ATSTERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'ATESTERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     ATSTNO                                                           
*                                                                               
ATSTSTOP EQU   *                                                                
         PRNT  APPCTESTOPERSTOP                                                 
         B     ATSTOK                                                           
*                                                                               
ATSTOK   SR    RC,RC                                                            
ATSTNO   LTR   RC,RC                                                            
ATSTX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SEND TEST RECORDS 1                                                 *         
***********************************************************************         
         SPACE 1                                                                
APPCTST1 NTR1                                                                   
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         MVC   APPCDTA(10),=C'RECEIVE   '                                       
         MVI   APPCDTA+10,C' '                                                  
         LA    R1,11                                                            
         MVC   HALF,APPCACR        ISSUE SEND CONFIRM                           
         BAL   RE,APPCCOM                                                       
*        BE    *+6                                                              
*        DC    H'00'                                                            
         PRNT  SENTEXTRACTREC3                                                  
*                                                                               
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         MVC   APPCDTA(10),=C'SEND FILE '                                       
         MVI   APPCDTA+10,C' '                                                  
         LA    R1,11                                                            
         MVC   HALF,APPCACSR       ISSUE SEND CONFIRM                           
         BAL   RE,APPCCOM                                                       
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   OPERSTOP,C'Y'                                                    
         BE    A1TSSTOP                                                         
         PRNT  SENTEXTRACTREC3                                                  
*                                                                               
A1TSLOOP LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         MVC   APPCDTA(10),=C'RECEIVEFIL'                                       
         MVI   APPCDTA+10,C' '                                                  
         LA    R1,11                                                            
         MVC   HALF,APPCACR        ISSUE SEND CONFIRM                           
         BAL   RE,APPCCOM                                                       
         BNE   A1TSNEXT                                                         
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         B     A1TSLOOP                                                         
A1TSNEXT CLI   APPCHDR,X'40'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    RE,APPCDTA                                                       
         SR    RF,RF                                                            
         ICM   RF,3,=Y(APPCDLQ)                                                 
         XCEF                                                                   
         MVC   APPCDTA(10),=C'RCV CONFIR'                                       
         MVI   APPCDTA+10,C' '                                                  
         LA    R1,11                                                            
         MVC   HALF,APPCACRC       ISSUE SEND CONFIRM                           
         BAL   RE,APPCCOM                                                       
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
         LA    R2,80                                                            
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
         GOTO1 =V(PRINTER)                                                      
         PRNT  SENTEXTRACTCONFIRM                                               
         B     A1TSOK                                                           
*                                  DEBUG MESSAGE                                
*                                                                               
A1TSWAIT BAL   RE,WAITABIT                                                      
         BNE   A1TSNO                                                           
         B     A1TSOK                                                           
*                                                                               
A1TSERR  MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'ATESTERR     '                                       
         GOTO1 =V(PRINTER)                                                      
         B     A1TSNO                                                           
*                                                                               
A1TSSTOP EQU   *                                                                
         PRNT  APPCTESTOPERSTOP                                                 
         B     A1TSOK                                                           
*                                                                               
A1TSOK   SR    RC,RC                                                            
A1TSNO   LTR   RC,RC                                                            
A1TSX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CLOSE APPC LU6.2 LINK TO SERVER LU                                  *         
***********************************************************************         
         SPACE 1                                                                
APPCCLOS NTR1                                                                   
         PRNT  XTCLOSEPREPARE                                                   
         MVC   APPCACTN,APPCACCP   ISSUE CLOSE/PREPARE                          
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOGR),   +        
               VL=1                                                             
         PRNT  XTRSENDERPCLOSE                                                  
         B     ACLOOK                                                           
*                                                                               
ACLO010  DS    0H                                                               
         PRNT  EZCLOSE                                                          
         MVC   APPCACTN,APPCACC    ISSUE CLOSE                                  
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOGR),   +        
               VL=1                                                             
         DS    0H                                                               
         PRNT  XTRSENDERCLOSED                                                  
         B     ACLOOK                                                           
ACLOOK   SR    RC,RC                                                            
ACLONO   LTR   RC,RC                                                            
ACLOX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SHUT DOWN SUB TASK                                                  *         
***********************************************************************         
         SPACE 1                                                                
SHUTDOWN NTR1                                                                   
         PRNT  SHUTDOWNEDICTEXT                                                 
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* WAIT A BIT                                                          *         
***********************************************************************         
         SPACE 1                                                                
WAITABIT NTR1                                                                   
         L     R2,WAITSECS         R2 = THE WAIT INTERVAL                       
         ST    R2,FULL                                                          
         STIMERM SET,ID=STIMERID,BINTVL=FULL,WAIT=YES                           
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TRANSMIT DATA VIA APPC LU6.2 COMMAND                     *         
*                                                                     *         
* ON ENTRY, R1 = LENGTH OF DATA                                       *         
*           FIELD 'HALF' CONTAINS APPC COMMAND                        *         
***********************************************************************         
         SPACE 1                                                                
APPCCOM  NTR1                                                                   
         LR    RF,R1                                                            
         LA    RF,4(RF)            L'DATA + LENGTH                              
         STCM  RF,3,APPCLEN                                                     
         LA    R2,8(RF)            L'DATA + OVERHEAD                            
         LA    RF,APPCDTA                                                       
         AR    RF,R1                                                            
         MVC   0(2,RF),=X'0D25'    TERMINATE RECORD WITH CRLF                   
         MVC   APPCMXLN,=Y(APPCMXLQ)                                            
         MVC   APPCACTN,HALF       APPC COMMAND                                 
         XC    APPCHDR,APPCHDR                                                  
         GOTO1 =V(PRNTBL),DMCB,0,APPCDATA,C'DUMP',(R2),=C'1D'                   
*                                  DEBUG MESSAGE                                
         MVC   P(16),=C'APPCCOM MESSAGE:'                                       
         MVC   P+19(2),APPCACTN                                                 
         MVC   P+23(50),APPCDATA                                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OI    APPCPARM+20,X'80'                                                
         LA    R1,APPCPARM                                                      
         LINK  EPLOC=APPCEPLC                                                   
*                                                                               
ACOM010  LTR   RF,RF                                                            
         BZ    ACOMOK                                                           
         MVC   P+50(9),=C'REG 15 = '                                            
         EDIT  (RF),(5,P+59),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   P+66(8),=C'FLAGS = '                                             
         GOTO1 =V(HEXOUT),DMCB,APPCHDR,P+74,2,=C'TOG'                           
         B     ACOMNO                                                           
*                                                                               
ACOMOK   SR    RC,RC                                                            
ACOMNO   LTR   RC,RC                                                            
ACOMX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PROCESS INFO IN EXTRACT FILE HEADER RECORD                          *         
*                                                                     *         
* ON ENTRY, R5 = A(HEADER RECORD)                                     *         
***********************************************************************         
         SPACE 1                                                                
EXTFHDR  NTR1                                                                   
         USING MXHDRD,R5                                                        
         SR    RF,RF                                                            
         ICM   RF,3,MXHDRLEN                                                    
         SH    RF,=H'4'                                                         
         STCM  RF,3,LENMAX                                                      
         B     EHDROK                                                           
*                                                                               
EHDROK   SR    RC,RC                                                            
EHDRNO   LTR   RC,RC                                                            
EHDRX    XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
SSB      DC    F'0'                FOR DATAMGR (OFFLINE)                        
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         SPACE 2                                                                
CRLF     DS    0XL2                                                             
CR       DC    X'0D'                                                            
LF       DC    X'25'                                                            
ETB      EQU   X'26'                                                            
ETX      EQU   X'03'                                                            
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
COUNT    DS    F                                                                
TESTECB  DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
STIMERID DS    XL4                 FOR TIMERPOPS                                
LENMAX   DS    XL4                                                              
WORK     DS    CL256                                                            
         SPACE 2                                                                
WAITSECS DC    F'3000'                                                          
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
RQSTCONF DS    C                   'Y' = WE GOT REQUEST FOR CONFIRM             
         DS    0F                                                               
EXECBLST DC    X'00',AL3(0)        A(XMAINECB)                                  
         DC    X'80',AL3(0)        A(XSTOPECB)                                  
         SPACE 2                                                                
         DS    0D                                                               
APPCPARM DS    0D                                                               
         DC    A(APPCACTN)                                                      
         DC    A(APPCTOKN)                                                      
         DC    A(APPCDATA)                                                      
         DC    A(APPCEPLC)                                                      
         DC    A(APPCLOGR)                                                      
         DC    X'80000000'         A(ECB) GETS STORED HERE                      
         SPACE 2                                                                
APPCACTN DS    CL4                 ACTION CODE                                  
APPCACOA DC    C'OA  '             ACTION OPEN SEND TO AT&T EASYLINK            
APPCACOS DC    C'OS  '             ACTION OPEN SEND                             
APPCACOR DC    C'OR  '             ACTION OPEN RECEIVE                          
APPCACOI DC    C'OI  '             ACTION OPEN FOR INPUT                        
APPCACSD DC    C'SD  '             ACTION SEND DATA                             
APPCACSF DC    C'SF  '             ACTION SEND FLUSH                            
APPCACSC DC    C'SC  '             ACTION SEND CONFIRMATION                     
APPCACSR DC    C'SR  '             ACTION SEND/RECEIVE                          
APPCACR  DC    C'R   '             ACTION RECEIVE                               
APPCACRC DC    C'RC  '             ACTION RECEIVE/CONFIRM                       
APPCACRS DC    C'RS  '             ACTION RECEIVE/SEND                          
APPCACCP DC    C'CP  '             ACTION CLOSE/PREPARE                         
APPCACC  DC    C'C   '             ACTION CLOSE                                 
APPCTOKN DC    F'0'                TOKEN FOR EASYLINK                           
APPCEPLC DS    CL8                 PROVIDED BY APPC OPEN CALL                   
         DS    0D                                                               
         DC    CL8'APPCDATA'                                                    
APPCDATA DS    0X                  DATA AREA                                    
APPCHDR  DC    XL6'00'             HEADER INFO                                  
APPCMXLN DC    XL2'00'             MAXIMUM LENGTH OF DATA                       
APPCLEN  DC    XL2'00'             LENGTH OF DATA                               
APPCDTA  DC    1000X'00'           ACTUAL DATA                                  
APPCDLQ  EQU   *-APPCDTA           MAXIMUM DATA LENGTH                          
APPCMAXL EQU   *-APPCLEN           MAXIMUM LENGTH OF DATA                       
APPCMXLQ EQU   *-APPCMXLN          MAXIMUM LENGTH OF DATA FOR RECEIVE           
         SPACE 2                                                                
APPCLOGR DCB   DDNAME=EAPPCLGR,RECFM=FBA,LRECL=133,MACRF=PM,DSORG=PS            
         SPACE 2                                                                
APPCM1   DC    C'*DDS*HERE COMES A REPORT'                                      
APPCM1LQ EQU   (*-APPCM1)+2                                                     
*                                                                               
APPCM2   DC    C'SEND FILE'                                                     
APPCM2LQ EQU   (*-APPCM2)+2                                                     
*                                                                               
APPCM3   DC    C'*DDS*REPORT IS OPENED XXXXX'                                   
APPCM3LQ EQU   (*-APPCM3)+2                                                     
*                                                                               
APPCM4   DC    C'*DDS*END OF REPORT'                                            
APPCM4LQ EQU   (*-APPCM4)+2                                                     
*                                                                               
APPCM5   DC    C'*DDS*REPORT CLOSED'                                            
APPCM5LQ EQU   (*-APPCM5)+2                                                     
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE C/I BUFFER                       
*                                                                               
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
XIO      DC    10000X'00'          EXFILE I/O AREA                              
*                                                                               
         DVGAPL                    ??                                           
*                                                                               
         DS    0D                                                               
         DC    C'EDICTBLK'                                                      
EDICTBLK DC    14336X'00'          EDICT FILE BLOCK                             
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 3                                                                
       ++INCLUDE DDEDICTWUK                                                     
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
* DMGREQUS                                                                      
* DMPRTQL                                                                       
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
