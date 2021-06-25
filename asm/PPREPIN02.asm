*          DATA SET PPREPIN02  AT LEVEL 010 AS OF 01/07/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE PPIN02A                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE BINSRCH2        LATEST VERSION                                         
*INCLUDE PPFMTINO                                                               
*INCLUDE OFFOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE DDUCOM                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE MQRPT                                                                  
*                                                                               
         TITLE 'PPIN02 - NISSAN INTERFACE'                                      
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-28806  01/07/19 CHANGE SORT ORDER SO CREDITS COMES FIRST  *         
***********************************************************************         
*                                                                               
* BPLA  06/15  DISPLAY $ AMOUNTS LESS THAN $1.00 WITH A ZERO                    
*              BEFORE THE DECIMAL POINT                                         
*                                                                               
* BPLA  09/14  CHGS FOR NEW AGENCY OO - OMDUSEC                                 
*              JOB NAME           (ZFI02100?)                                   
*              JOB DESCRIPTION  ?                                               
*              COMPANY CODE       (2177?)                                       
*              VENDOR NUMBER      (GC00001?)                                    
*              TAX CODE           (A1?)                                         
*              TAX JURISDICTION CODE  ?                                         
*              NEW VALUES CONTROLLED BY OMDUSASW (=Y)                           
*                                                                               
*                                                                               
* QOPT1        'F' = PRINT FORMULAS                                             
* QOPT2        'R' = PRINT REVERSAL INFO                                        
* QOPT3        'N' = NO PRINTING                                                
* QOPT4        ' ' = NO TAPE, Y = NORMAL TAPE, ELSE SPECIAL TAPE CODE           
* QOPT5         S= SOON BILLS ONLY                                              
* QOPT6         AOR OPTION                                                      
*               A=AOR ONLY                                                      
*               B=AOR ONLY AND AOR/CLIENT                                       
*               X=EXCLUDE AOR                                                   
*               N=EXCLUDE COMMISSION ONLY                                       
*               BLANK=AOR AND NON-AOR                                           
*              'C' = COMMISION ONLY BILLS                                       
* QOPT7    Y =  PRINT 50 INPUT AND OUTPUT RECORDS                               
*               BUT DON'T ACTUALLY PRODUCE A TAPE                               
*                                                                               
* QINVNO1      COL 53(4) START INVOICE NUMBER                                   
* QINVNO2      COL 57(4) END INVOICE NUMBER                                     
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* SMUR SPEC-7743/8563 03/02/17  REMOVE ZERO DOLLAR INVOICES           *         
* AKAT ITMF-4581   02/11/16 HANDLE DUPLICATE BILL RECORDS             *         
***********************************************************************         
PPIN02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PPIN02                                                       
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
*                                                                               
         LA    R8,SPACEND                                                       
         USING PPINWRKD,R8                                                      
*                                                                               
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         MVC   ADMASTC,VMASTC          SAVE VMASTC                              
         DROP  R9                                                               
*                                                                               
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,PPIN02+4095                                                   
         LA    R9,1(R9)                                                         
         USING PPIN02+4096,R9           ** NOTE USE OF R9 AS                    
*                                          SECOND BASE REGISTER                 
         LA    R7,P                                                             
         USING BILLINED,R7                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,PROCBIL                                                     
         BE    PRBIL                                                            
         CLI   MODE,FBUYREQ                                                     
         BE    FBLR                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    LBLR                                                             
***OFF                                                                          
         CLI   MODE,OFCFRST                                                     
         BE    FBOFF                                                            
         CLI   MODE,OFCLAST                                                     
         BE    LBOFF                                                            
***OFF                                                                          
         CLI   MODE,FBUYCLI                                                     
         BE    FBC                 FIRST BILL FOR CLIENT                        
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
RUNF     DS    0H                  RUN FIRST                                    
         L     RF,=A(FMTAMTS)                                                   
         ST    RF,AFMTAMTS                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,CTODAY) TODAY - YYYYMMDD                   
         MVC   MCTODAY(2),CTODAY+4        MTH                                   
         MVI   MCTODAY+2,C'/'                                                   
         MVC   MCTODAY+3(2),CTODAY+6      DAY                                   
         MVI   MCTODAY+5,C'/'                                                   
         MVC   MCTODAY+6(4),CTODAY        YYYY                                  
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,MTODAYB) TODAY BINARY                       
*                                                                               
*        GET TIME OF DAY                                                        
         TIME                                                                   
*                                                                               
*        R0 NOW HAS TIME HHMMSSHS  (PWOS)                                       
*                                                                               
         ST    R0,FULL                                                          
         SRL   R0,4                                                             
         ST    R0,MYFULL                                                        
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),MYFULL                                                  
         OI    DUB+7,X'0F'                                                      
         CVB   R6,DUB                                                           
         EDIT  (R6),(5,TIMEOFD),2,FILL=0                                        
         MVI   TIMEOFD+5,C'.'                                                   
         UNPK  WORK(3),FULL+2(2)                                                
         MVC   TIMEOFD+6(2),WORK     HH.MM.SS                                   
*                                                                               
*        TIMEOFDF (FOR THE FILE) IS HHMMSS                                      
*                                                                               
         MVC   TIMEOFDF(2),TIMEOFD   HH                                         
         MVC   TIMEOFDF+2(2),TIMEOFD+3  MM                                      
         MVC   TIMEOFDF+4(2),TIMEOFD+6  SS                                      
*                                                                               
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
         MVI   DYNSW,C'N'          SET TAPE NOT ALLOCATED                       
*                                                                               
*        GET A(OFFICER)                                                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVC   DUB,SPACES          GET OFFICER                                  
         MVC   DUB(6),=C'T00A38'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         MVC   VOFFICER,4(R1)      SAVE ADDRESS                                 
         B     EXIT                                                             
         EJECT                                                                  
REQF     DS    0H                                                               
         SPACE 3                                                                
FBLR     DS    0H                  FIRST FOR REQUEST                            
*                                                                               
         MVI   AGYSW,0                                                          
*                                                                               
         XC    LOWINV,LOWINV     CLEAR LOW INVOICE NUMBER FILTER                
         MVC   HIINV,=X'FFFF'    SET HIGH INVOICE NUMBER FILTER TO MAX          
         CLC   QINVNO1(4),SPACES                                                
         BE    REQF5                                                            
         PACK  DUB,QINVNO1(4)                                                   
         CVB   R0,DUB                                                           
         STH   R0,LOWINV                                                        
*                                                                               
REQF5    DS    0H                                                               
         CLC   QINVNO2(4),SPACES                                                
         BE    REQF10                                                           
         PACK  DUB,QINVNO2(4)                                                   
         CVB   R0,DUB                                                           
         STH   R0,HIINV                                                         
*                                                                               
REQF10   DS    0H                                                               
         MVC   QINVNO1(4),SPACES      CLEAR FOR PPG                             
         MVC   QINVNO2(4),SPACES      CLEAR FOR PPG                             
*                                                                               
         B     REQF13M                                                          
*************************************************************                   
***** CODE BELOW FOR SFTP STYLE OUTPUT                                          
***** CHANGED TO PRTTPAE STYLE                                                  
*************************************************************                   
         MVI   TESTMQ,C'P'         SET TO PROD MQ NOTIFICATION                  
         CLI   QOPT7,C'N'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT7,C'Y'          TRACING OUTPUT RECORDS                       
         BNE   *+8                 ALSO SUPPRESS MQ                             
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT7,C'T'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'T'         TEST MQ                                      
*                                                                               
REQF13B  DS    0H                                                               
*                                                                               
         MVI   INSFTP,C'Y'                                                      
*                                                                               
REQF13B6 CLI   BKOPENSW,C'Y'    IS BK FILE ALREADY OPEN?                        
         BE    REQF13M          IF SO, SKIP CODE BELOW                          
*                                                                               
         MVC   DSNAME,SPACES                                                    
         MVC   DSNAME+0(4),=C'BIL.'                                             
         MVC   DSNAME+4(3),=C'PRT'                                              
         MVI   DSNAME+7,C'.'                                                    
*                                                                               
         L     RF,ADMASTC             USE MASTC'S AGYID                         
         USING MASTD,RF                                                         
         L     R1,MCAEXTRA                                                      
         MVC   DSNAME+8(4),MCAGYCOD-MCEXTRA(R1)                                 
         DROP  RF                                                               
*                                                                               
REQF13BA MVC   DSNAME+12(2),=C'.D'                                              
         MVC   DSNAME+14(6),CTODAY+2    YYMMDD                                  
         MVC   DSNAME+20(2),=C'.T'                                              
         MVC   DSNAME+22(2),TIMEOFD         WITHOUT .'S                         
         MVC   DSNAME+24(2),TIMEOFD+3                                           
         MVC   DSNAME+26(2),TIMEOFD+6                                           
         MVC   MQMAPNM,=C'SFTPDISK.PROD.'                                       
         CLI   QOPT7,C'Y'                                                       
         BE    REQF13H                                                          
         CLI   QOPT7,C'N'   NO MQ NOTIFICATION                                  
         BE    REQF13H      ALSO PUT 'TEST' IN MQMAPNM                          
         CLI   TESTMQ,C'T'  PUTTING TO TEST BROKER?                             
         BNE   *+10                                                             
REQF13H  MVC   MQMAPNM+9(4),=C'TEST'                                            
*                                                                               
         MVI   BYTE,X'45'         X'04' = BIG NAMES                             
         MVC   DUB,=X'000005000001'                                             
         GOTO1 DYNALLOC,DMCB,(X'80',=C'INPBIT1 '),(BYTE,DUB),          X        
               (X'80',MQMAPNM)                                                  
*                                                                               
REQF13X  DS    0H                                                               
         OPEN  (INPBIT1,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   BKOPENSW,C'Y'     SET IN FILE OPEN                               
         B     REQF13M                                                          
**************************************************************                  
****  CODE ABOVE FOR SPTF STYLE OUTPUT                                          
****  FOR PRTTAPE STYLE DYNALLOC DONE EARLIER                                   
**************************************************************                  
*                                                                               
*                                                                               
REQF13M  DS    0H                                                               
*                                                                               
         SR    R0,R0            SET INVOICE LIST BINSRCH PARS                   
         L     R1,=A(INVTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,4                                                             
         LA    R4,4                                                             
         L     R5,=A(INVMAX)    INVMAX IS 10,000 INVS PER CLT                   
         STM   R0,R5,INVPARS                                                    
*                                                                               
         CLI   FIRSTSW,0           FIRST TIME TEST                              
         BNE   FBLR1                                                            
         MVI   FIRSTSW,1                                                        
*                                                                               
         LA    R3,RUNTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         LA    R3,TRUNTOTS         TOTAL FOR TAPE                               
         BAS   RE,CLRTOTS                                                       
*                                                                               
         XC    RUNINVS,RUNINVS      RUN INVOICE TOTALS                          
         XC    TRUNINVS,TRUNINVS    TAPE INVOICE TOTALS                         
*                                                                               
         XC    START(12),START                                                  
         XC    AOUTP,AOUTP                                                      
         XC    LSTBLKY,LSTBLKY                                                  
         XC    EATOTS,EATOTS       CLEAR ESTIMATE AMTS                          
         MVC   SAVMAX,MAXLINES                                                  
*                                                                               
FBLR1    DS    0H                                                               
*                                                                               
         MVI   RACTSW,0              ZERO REQUEST ACTIVITY                      
         MVI   OACTSW,0              ZERO REQUEST ACTIVITY                      
*                                                                               
         MVC   DYNDDN,=CL8'PINTAPE'                                             
         MVC   DYNDSN,=CL44'PRTTAPE.PP0INXX1'                                   
         MVC   DYNDSN+13(2),QAGENCY                                             
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         ICM   RE,15,MCSSB                                                      
         JZ    *+2                 MUST HAVE OFFLINE SSB!                       
         USING SSBD,RE                                                          
         CLI   SSODSPAC,C'C'                                                    
         BNE   *+14                                                             
         MVC   DYNDSN(7),=C'CSCTAPE'                                            
         B     FBLR1E                                                           
         CLI   SSODSPAC,C'T'                                                    
         BNE   *+14                                                             
         MVC   DYNDSN(7),=C'TSTTAPE'                                            
         B     FBLR1E                                                           
         CLI   SSODSPAC,C'Q'                                                    
         BNE   *+14                                                             
         MVC   DYNDSN(7),=C'FQATAPE'                                            
         B     FBLR1E                                                           
         DROP  RE                                                               
*                                                                               
FBLR1E   DS    0H                                                               
         OC    MCREMPQK,MCREMPQK   SOON RUN?                                    
         BZ    FBLR1H                                                           
         DROP  RF                                                               
*                                                                               
         LA    RE,DYNDSN           YES: PREVENT DYNALLOC FAILURES               
         ENQ   (MAJORNAM,(RE),E,DSNLENQ,SYSTEM)                                 
**                                                                              
FBLR1H   DS    0H                                                               
*                                                                               
*              NO-OP MIX FOR TAPE SPECS CHK                                     
**                                                                              
         CLI   QOPT4,C'N'                                                       
         BE    FBLR2                                                            
*                                                                               
         MVC   BYTE,QOPT4          TAPE SPEC                                    
         CLI   BYTE,C'Y'           Y = BLANK                                    
         BNE   *+8                                                              
         MVI   BYTE,C' '                                                        
         CLI   TAPTYP,0            TEST ALREADY SET                             
         BNE   *+14                                                             
         MVC   TAPTYP,BYTE                                                      
         B     FBLR2                                                            
         CLC   TAPTYP,BYTE         MAY NOT MIX TAPE SPECS                       
         BE    FBLR3                                                            
*                                                                               
         MVC   P(80),QPROG                                                      
         MVC   PSECOND(55),=C'**MIXED TAPE SPECS IN ONE RUN - THIS REQUX        
               EST BYPASSED**'                                                  
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
FBLR2    DS    0H                                                               
         CLI   QOPT4,C'N'          NO TAPE                                      
         BE    FBLR3                                                            
         LM    R3,R5,AGYLST                                                     
         MVC   WORK(4),SPACES                                                   
         MVC   WORK(2),RCAGYFLT                                                 
         MVC   WORK+2(1),TAPTYP    SPECIAL FORMAT CODE                          
*                                                                               
         CLC   WORK(4),0(R3)                                                    
         BE    *+8                                                              
         BXLE  R3,R4,*-10                                                       
*                                                                               
         L     RF,4(R3)                                                         
         LTR   RF,RF               TEST HAVE TAPE PROC                          
         BNZ   FBLR2D                                                           
         CLI   QOPT4,C' '         OK IF NO TAPE SPEC                            
         BNH   FBLR3                                                            
*                                                                               
         MVC   P(80),QPROG                                                      
         MVC   PSECOND(35),=C'**INVALID TAPE SPEC- REQ BYPASSED**'              
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
FBLR2D   DS    0H                                                               
         ST    RF,AOUTP                                                         
*                                                                               
FBLR3    DS    0H                                                               
         LA    R3,PRDTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         LA    R3,CLTTOTS                                                       
         BAS   RE,CLRTOTS                                                       
***OFF                                                                          
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
***OFF                                                                          
         LA    R3,REQTOTS                                                       
         BAS   RE,CLRTOTS                                                       
*                                                                               
         XC    CLTINVS,CLTINVS   CLEAR CLIENT AND REQ INVOICE TOTALS            
         XC    CLTZINV,CLTZINV   CLIENT ZERO INVOICE TOTALS                     
         XC    REQINVS,REQINVS                                                  
*                                                                               
         XC    LASTKEY,LASTKEY                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   TOTSW,0                                                          
         OC    START(12),START                                                  
         BNZ   *+10                                                             
         MVC   START(12),QSTART    SET START-END FROM FRST REQ                  
         CLC   START(12),SPACES                                                 
         BNE   FBLR4                                                            
         MVC   START+00(2),RCDATE+06   YY                                       
         MVC   START+02(2),RCDATE+00   MM                                       
         MVC   START+04(2),RCDATE+03   DD       ***   CONVERT START             
         GOTO1 DATCON,DMCB,(0,START),(0,START)  *** TO NEW DATE FORMAT          
         MVC   END,START                                                        
FBLR4    DS    0H                                                               
         CLC   QSTART(12),SPACES                                                
         BNE   *+10                                                             
         MVC   QSTART(12),START                                                 
         B     EXIT                                                             
         SPACE 3                                                                
AGYLST   DS    0F                AGENCY TABLE                                   
         DC    A(AGYLST+12)                                                     
         DC    A(8)                                                             
         DC    A(AGYLSTX-1)                                                     
         DC    CL4'OU  ',A(INPROC)      OMDTOA - NISSAN FORMAT                  
         DC    CL4'OO  ',A(INPROC)      OMDUSA - NISSAN FORMAT                  
         DC    CL4'SJM ',A(INPROC)      SJR TEST  - NISSAN FORMAT               
*                                    *AGENCY CODE UNKNOWN AT THIS TIME          
*                                                                               
AGYLSTX  EQU   *                                                                
         DC    CL4'XX',A(0)                                                     
*                                                                               
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL(DSNLENQ)                                                      
DSNLENQ  EQU   44                                                               
MAJORNAM DC    C'PINTAPE '         MAJOR RESOURCE NAME FOR ENQ                  
*                                                                               
         EJECT                                                                  
FBC      DS    0H                  FIRST FOR CLIENT                             
         CLC   QAGENCY,=C'OO'      OMDUSA?                                      
         BNE   *+12                                                             
         OI    AGYSW,AGYOO                                                      
         B     *+18                                                             
         CLC   QAGENCY,=C'OU'      CANADA?                                      
         BNE   *+8                                                              
         OI    AGYSW,AGYOU                                                      
*                                                                               
         MVI   OMDUSASW,C'N'                                                    
         CLC   QAGENCY,=C'OO'      OMDUSA?                                      
         BNE   *+8                                                              
         MVI   OMDUSASW,C'Y'                                                    
*                                                                               
*        SET SAVCOFF HERE FOR ALL REQUESTS                                      
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         MVC   SAVCOFF(1),PCLTOFF    SAVE OFFICE FOR HEADLINES                  
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'P'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,QAGENCY                                              
         MVC   OFFD.OFCPMED,QMEDIA                                              
         MVC   OFFD.OFCOFC,PCLTOFF                                              
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK),(0,VCOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   TBCLTF0                                                          
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
TBCLTF0  DS    0H                                                               
         CLC   SAVCOFF,=X'0000'   IF STILL ZEROS, MAKE SPACES                   
         BNE   *+10                                                             
         OC    SAVCOFF,SPACES    NEEDED FOR CLIENTS WITHOUT AN OFFICE           
*                                                                               
         CLI   QOPT4,C'N'           SEE IF A TAPE REQUEST                       
         BE    FBC0X                NO - PROCESS ALL CLTS                       
*                                                                               
*                                                                               
FBC0D    DS    0H                                                               
         B     FBC0X                                                            
FBC0X    DS    0H                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   FBC1                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
*                                                                               
         CLI   QCLIENT,C'*'        ONE OFFICE REQUEST ?                         
         BE    FBC1B               YES                                          
         CLI   QCLIENT,C'$'        ALL OFFICE REQUEST ?                         
         BE    FBC1B               YES                                          
         CLC   QCLIENT,=C'ALL'     ALL CLIENTS REQUEST ?                        
         BNE   FBC1D               NO - ONE CLIENT ONLY REQUEST                 
*                                                                               
FBC1B    MVI   PCLTREC,0           TO CLEAR MEDIA NAME OVERRIDE                 
*                                                                               
FBC1D    MVI   NEWCLT,C'Y'                                                      
         MVC   SVMID,SPACES                                                     
         MVC   SVMID2,SPACES                                                    
         MVC   SVMID(7),=C'CLIENT='                                             
         MVC   SVMID+8(3),PCLTKCLT                                              
         MVC   SVMID+12(L'PCLTNAME),PCLTNAME                                    
         MVC   WORK(8),SPACES                                                   
         CLI   PCLTNUM,X'FF'                                                    
         BNE   FBC2                                                             
         MVI   WORK,C'('                                                        
         UNPK  WORK+1(5),PCLTNUM+1(3)                                           
         MVI   WORK+5,C')'                                                      
         B     FBC4                                                             
*                                                                               
FBC2     MVC   BYTE,PCLTNUM                                                     
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'80'                                                       
         BNE   FBC3                                                             
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PCLTNUM                                                
         NI    FULL+1,X'7F'                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   WORK,C'('                                                        
         UNPK  WORK+1(5),DUB                                                    
         MVI   WORK+6,C')'                                                      
         B     FBC4                                                             
*                                                                               
FBC3     DS    0H                                                               
         MVC   WORK+1(3),PCLTNUM                                                
         OC    WORK+1(3),SPACES                                                 
         CLC   WORK+1(3),SPACES                                                 
         BE    FBC4X                                                            
         MVI   WORK,C'('                                                        
         LA    R1,WORK+3                                                        
FBC3C    CLI   0(R1),C' '                                                       
         BH    FBC3D                                                            
         BCT   R1,FBC3C                                                         
*                                                                               
FBC3D    MVI   1(R1),C')'                                                       
*                                                                               
FBC4     DS    0H                                                               
         LA    R1,SVMID+12+L'PCLTNAME                                           
FBC4C    CLI   0(R1),C' '                                                       
         BH    FBC4E                                                            
         BCT   R1,FBC4C                                                         
FBC4E    MVC   2(8,R1),WORK                                                     
*                                                                               
FBC4X    DS    0H                                                               
         LA    R1,SVMID+12+L'PCLTNAME+10                                        
FBC5     CLI   0(R1),C' '                                                       
         BH    FBC10                                                            
         BCT   R1,FBC5                                                          
*                                                                               
FBC10    LA    R2,SVMID2                                                        
         LA    R1,132(R1)           SAME DISPLACEMENT INTO SVMID2               
         SR    R1,R2                                                            
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'               SOMETING VERY WRONG                           
*                                                                               
         EX    R1,*+8                                                           
         B     FBCX                                                             
         MVC   SVMID2(0),=40C'-'                                                
*                                                                               
FBCX     B     EXIT                                                             
         EJECT                                                                  
         SPACE 3                                                                
LBLR     DS    0H                  LAST FOR REQUEST                             
         CLI   QCLIENT,C'$'                                                     
         BE    LBLR2                                                            
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         L     R0,CLTINVS                                                       
         L     RE,CLTZINV          CLIENT ZERO INVOICE COUNT                    
         SR    R0,RE               ADJUST CLIENT INVOICE COUNT                  
         ST    R0,CLTINVS                                                       
*                                                                               
         CLI   RACTSW,0            CHK FOR ACTIVITY                             
         BNE   PRB2B               IF YES - GO FINISH LAST PRD/CLT              
LBLR2    DS    0H                                                               
         CLI   RACTSW,0                                                         
         BE    EXIT                                                             
         LA    R3,REQTOTS                                                       
         LA    R4,RUNTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         L     R0,RUNINVS                                                       
         A     R0,REQINVS                                                       
         ST    R0,RUNINVS                                                       
         CLI   QOPT4,C'N'                                                       
         BE    LBLR4                                                            
         LA    R3,REQTOTS                                                       
         LA    R4,TRUNTOTS         ALSO POST TO TAPE TOTALS                     
         BAS   RE,ROLTOTS                                                       
         L     R0,TRUNINVS                                                      
         A     R0,REQINVS                                                       
         ST    R0,TRUNINVS                                                      
*                                                                               
LBLR4    BAS   RE,PRNT                                                          
         MVC   BLINE(18),=C'**REQUEST TOTALS**'                                 
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,REQINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,REQTOTS                                                       
         MVI   TOTSW,2                                                          
         GOTO1 AFMTAMTS                                                         
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
RUNL     DS    0H                  RUN LAST                                     
         BAS   RE,PRNT                                                          
         MVC   BLINE(14),=C'**RUN TOTALS**'                                     
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,RUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
         LA    R3,RUNTOTS                                                       
         MVI   TOTSW,2                                                          
         GOTO1 AFMTAMTS                                                         
         MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
         MVC   BLINE(15),=C'**TAPE TOTALS**'                                    
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,TRUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT               
         LA    R3,TRUNTOTS                                                      
         MVI   TOTSW,2                                                          
         GOTO1 AFMTAMTS                                                         
         BAS   RE,PRNT                                                          
         L     RF,AOUTP                                                         
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         BASR  RE,RF                                                            
*                                                                               
         CLOSE (INPBIT1)          NISSAN FILE                                   
*                                                                               
         L     RE,ADMASTC                                                       
         OC    MCREMPQK-MASTD(,RE),MCREMPQK-MASTD(RE)   SOON RUN?               
         BZ    EXIT                                                             
         LARL  RE,DYNDSN           YES                                          
         DEQ   (MAJORNAM,(RE),DSNLENQ,SYSTEM),RET=HAVE                          
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*                                  PROCESS BILL                                 
         SPACE 2                                                                
PRBIL    DS    0H                                                               
*                                                                               
         CLC   PBILKBNO,LOWINV       BE SURE BILL PASSES INV # RANGE            
         BL    EXIT                                                             
         CLC   PBILKBNO,HIINV                                                   
         BH    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'S'            SOON BILLS ONLY?                           
         BNE   PRBIL5                                                           
         TM    PBILSTAT,PSTSOONQ    UPDATIVE SOON BILL                          
         BZ    EXIT                                                             
*                                                                               
PRBIL5   CLC   QAGENCY,=C'DF'       SEE IF SAATCHI                              
         BNE   PRB05                NO                                          
         CLI   QOPT4,C'T'           AND IF TOYOTA TAPE                          
         BNE   PRB05                NO                                          
         CLC   PBILKCLT,=C'TX '     SAATCHI CLIENT TX ?                         
         BNE   PRB05                NO                                          
         CLC   PBILKPRD,=C'TMS'     PRODUCT TMS FOR CLIENT TX ?                 
         BNE   EXIT                 NO - SKIP                                   
*                                                                               
PRB05    DS    0H                                                               
         TM    PBILCMSW,X'02'      IS IT A COMMISION ONLY BILL                  
         BNZ   PRB09               YES, OK                                      
         CLI   QOPT6,C'C'          NO, ARE WE SKIPPING OTHERS                   
         BE    EXIT                                                             
         B     PRB09A                                                           
*                                                                               
PRB09    DS    0H                                                               
         CLI   QOPT6,C'N'          EXCLUDING COMMISSION ONLY?                   
         BE    EXIT                                                             
PRB09A   DS    0H                                                               
*                                                                               
         CLI   QOPT6,C' '            CHECK AOR OPTION                           
         BE    PRB09D                                                           
         CLI   QOPT6,C'A'            ONLY AOR                                   
         BNE   PRB09B                                                           
         TM    PBILCMSW,X'20'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09B   CLI   QOPT6,C'B'            AOR AND AOR/CLIENT                         
         BNE   PRB09C                                                           
         TM    PBILCMSW,X'30'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09C   CLI   QOPT6,C'X'            SEE IF EXCLUDING AOR                       
         BNE   PRB09D                                                           
         TM    PBILCMSW,X'20'                                                   
         BO    EXIT                                                             
*                                                                               
PRB09D   DS    0H                                                               
*                                                                               
*        SPECIAL HANDLING OF GSTX (G7) BILLS FOR CLIENTS LLB AND MCI            
*        FOR THESE COST 2 CLIENTS SET BNETP TO BNET2P (COST2 NET)               
*        SO THAT NO COMMISSION WILL BE REPORTED                                 
*                                                                               
         CLC   QAGENCY,=C'G7' GSTX                                              
         BNE   PRB09DX                                                          
         CLC   PBILKCLT,=C'LLB'     MUST BE EITHER CLIENT LLB OR MCI            
         BE    PRB09D5                                                          
         CLC   PBILKCLT,=C'MCI'                                                 
         BNE   PRB09DX                                                          
*                                                                               
PRB09D5  CLI   PBILOTH,X'09'       THIS ELEMENT MUST BE PRESENT                 
         BNE   PRB09DX                                                          
         TM    PBILLIND,X'80'      MUST BE COST 2 FACTOR BILL                   
         BZ    PRB09DX                                                          
         MVC   PBILLNET,PBILLOPN   SET PBILLNET TO COST 2 NET                   
*                                                                               
PRB09DX  GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*****                                                                           
*****    SET EFFECTIVE VALUES INTO PBILLREC                                     
*****                                                                           
         MVC   PBILLGRS,PPBVEBG                                                 
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
*                                 SET MYGST AND MYPST AND MYHST                 
*****    ZAP   MYGST,=P'0'                                                      
*****    ZAP   MYPST,=P'0'                                                      
         L     R0,PPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYGST,DUB                                                        
         L     R0,PPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
         L     R0,PPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYHST,DUB                                                        
*                                                                               
*****    CLC   QAGENCY,=C'DA'    DONER AGENCY ?                                 
*****    BNE   *+10              NO                                             
*****    SP    MYPST,MYHST       YES - REMOVE HST INCLUDED IN PST TOTAL         
************                                                                    
******   NOTE USE PPBVEBC FOR CASH DISC                                         
******   DO NOT CALCULATE FROM PBILLGRS AND PBILLBIL (G-CD)                     
************                                                                    
*                                                                               
         CLC   PBILLKEY(14),LSTBLKY  IF FIRST FOR EST/MOS                       
         BE    PRB1                                                             
         MVI   REVSW,C' '          SET NOT A REVISION                           
         MVC   LSTBLKY,PBILLKEY                                                 
         XC    EATOTS,EATOTS                                                    
         B     PRB1D                                                            
*                                                                               
PRB1     DS    0H                                                               
         MVI   REVSW,C'R'          SET IS A REVISION                            
*                                                                               
PRB1D    DS    0H                  ADD TO ESTIMATE AMTS                         
         LA    R2,PBILLGRS                                                      
         LA    R3,EATOTS                                                        
         LA    R4,4                4 FIELDS STARTING AT GROSS                   
*                                                                               
PRB1F    DS    0H                                                               
         ZAP   DUB,0(BPLEQ,R2)                                                  
         CVB   R0,DUB                                                           
         A     R0,0(R3)                                                         
         ST    R0,0(R3)                                                         
*                                                                               
         LA    R2,BPLEQ(R2)                                                     
         LA    R3,4(R3)                                                         
         BCT   R4,PRB1F                                                         
*                                                                               
         CLC   PBILLDAT,QSTART     DATES FILTER                                 
         BL    EXIT                                                             
         CLI   QEND,C' '                                                        
         BE    PRB2                                                             
         CLC   PBILLDAT,QEND                                                    
         BH    EXIT                                                             
*                                                                               
PRB2     DS    0H                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   PBRETAIL,0                                                       
         BE    PRB2C                                                            
*                                  RETAIL BILL                                  
         CLI   PBRETAIL,X'81'      IGNORE CORP BILLS                            
         BE    EXIT                                                             
         MVI   RETAIL,C'Y'                                                      
*                                                                               
*                                                                               
PRB2C    CLI   PBILLMOD,C'P'       BYPASS PRD MODE BILLS                        
         BE    EXIT                                                             
         CLI   PBILLMOD,C'S'       AND SERIES MODE                              
         BE    EXIT                                                             
*                                                                               
         CLC   PBILKCLT,LASTKCLT    SEE IF SAME CLIENT                          
         BE    PRB2C2                                                           
*                                                                               
         MVC   CLTINVS,INVPARS+8    SAVE LAST CLIENT'S TOTAL                    
*                                                                               
         L     R0,CLTINVS                                                       
         L     RE,CLTZINV          CLIENT ZERO INVOICE COUNT                    
         SR    R0,RE               ADJUST CLIENT INVOICE COUNT                  
         ST    R0,CLTINVS                                                       
*                                                                               
         SR    R0,R0            RESET INVOICE LIST BINSRCH PARS                 
         L     R1,=A(INVTAB)    FOR NEW CLIENT I REALLY PROCESS                 
         SR    R2,R2                                                            
         LA    R3,4                                                             
         LA    R4,4                                                             
         L     R5,=A(INVMAX)     INVMAX IS 10,000 INVOICE PER CLT               
         STM   R0,R5,INVPARS                                                    
*                                                                               
*                                  USE BINSRCH TO ADD TO INVTAB                 
PRB2C2   DS    0H                                                               
         MVC   WORK(2),PBILKBMN                                                 
         MVC   WORK+2(2),PBILKBNO                                               
         GOTO1 BINSRCH,INVPARS,(1,WORK)                                         
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   QOPT3,C'N'          NO PRINTING                                  
         BE    PRB26                                                            
*                                                                               
PRB2C5   CLC   PBILKCLT(6),LASTKCLT CHK FOR SAME      CLT + PRD                 
         BE    PRB3                YES                                          
         CLC   PBILKCLT,LASTKCLT   CHK FOR CHANGE IN CLT                        
         BNE   PRB2A               FORCE CHG IN PRD ALSO                        
         CLC   PBILKPRD,LASTKPRD                                                
         BE    PRB2D                                                            
PRB2A    CLI   LASTKPRD,0                                                       
         BE    PRB2D                                                            
PRB2B    DS    0H                                                               
         MVC   BLPRD,LASTKPRD                                                   
         MVC   BLINE+4(19),=C' **PRODUCT TOTALS**'                              
         LA    R3,PRDTOTS                                                       
         MVI   TOTSW,1                                                          
         GOTO1 AFMTAMTS                                                         
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
         LA    R3,PRDTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         CLI   MODE,LBUYREQ        REQUEST LAST                                 
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
         CLI   MODE,OFCLAST        OR LAST FOR OFFICE                           
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
*                                                                               
         CLC   PBILKCLT,LASTKCLT   SEE IF NEW CLIENT ALSO                       
         BNE   PRB2D               IF YES THEN DON'T CHK PROFILE                
*                                                                               
         CLI   PROGPROF+1,C'Y'     CHK PAGE FOR PRODUCT                         
         BNE   PRB2D                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   NEWCLT,C'Y'         SO CLIENT WILL REPRINT                       
*                                  FOR EACH PRODUCT                             
*                                                                               
*                                                                               
PRB2D    CLC   PBILKCLT,LASTKCLT                                                
         BE    PRB3                                                             
*                                                                               
         CLI   LASTKCLT,0          SEE IF FIRST TIME                            
         BNE   PRB2F                                                            
         MVC   LASTPROF,PROGPROF   MUST SAVE PROFILE                            
         B     PRB3                                                             
*                                                                               
PRB2F    MVC   BLPRD,LASTKCLT                                                   
         XC    LASTKPRD(25),LASTKPRD                                            
         MVC   BLINE+5(17),=C'**CLIENT TOTALS**'                                
*                                                                               
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,CLTINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,CLTTOTS                                                       
         MVI   TOTSW,1                                                          
         GOTO1 AFMTAMTS                                                         
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
*                                                                               
*                                  FIRST CHECK PROFILE OF LAST CLIENT           
         CLI   LASTPROF+0,C'Y'     CHK NEW PAGE FOR CLIENT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   LASTPROF+1,C'Y'     OR NEW PAGE FOR PRODUCT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROGPROF+0,C'Y'     CHK NEW PAGE FOR CLIENT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROGPROF+1,C'Y'     OR NEW PAGE FOR PRODUCT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   LASTPROF,PROGPROF                                                
*                                                                               
***OFF                                                                          
         CLI   QCLIENT,C'$'          SEE IF DOING OFFILCE LIST                  
         BNE   PRB2M                 ROLL CLTTOTS TO OFFTOTS                    
         LA    R3,CLTTOTS                                                       
         LA    R4,OFFTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         L     R0,OFFINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,OFFINVS                                                       
         B     PRB2P                                                            
***OFF                                                                          
PRB2M    LA    R3,CLTTOTS                                                       
         LA    R4,REQTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         L     R0,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,REQINVS                                                       
PRB2P    LA    R3,CLTTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         XC    CLTINVS,CLTINVS                                                  
         XC    CLTZINV,CLTZINV                                                  
         CLI   MODE,LBUYREQ            RETURN TO REQTOTALS                      
         BE    LBLR2                                                            
         CLI   MODE,OFCLAST            RETURN TO OFFICE TOTALS                  
         BE    LBOFF5                                                           
PRB3     DS    0H                                                               
         CLI   NEWCLT,C'Y'         SEE IF NEW CLIENT                            
         BNE   PRB4                                                             
         MVC   P,SVMID                                                          
         MVC   PSECOND,SVMID2                                                   
         MVI   ALLOWLIN,5                                                       
         MVI   NEWCLT,C'N'                                                      
         MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
*                                                                               
PRB4     DS    0H                                                               
         MVC   LASTKEY,PBILLKEY                                                 
         SPACE 2                                                                
*                                  CREATE PRINT LINE(S)                         
         MVC   BLPRD,PBILKPRD           PRD                                     
         OC    PBILKEST,PBILKEST                                                
         BZ    PRB7                                                             
         LA    R3,PBILKEST                                                      
         BRAS  RE,CVD                                                           
         MVC   BLEST(3),WORK+2        ONE EST                                   
*                                                                               
*                                                                               
PRB4E    MVI   WORK+3,7                                                         
         CLC   PESTREC(12),WORK        TEST HAVE EST HDR                        
         BE    PRB5                YES                                          
         MVC   WORK(64),KEY        SAVE KEY FOR SEQ                             
         XC    KEY,KEY                                                          
         MVC   KEY(12),PBILLREC                                                 
         MVI   KEY+3,7                                                          
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PRB5     DS    0H                                                               
         B     PRB8                                                             
*                                                                               
PRB6     DC    H'0'                                                             
*                                                                               
PRB7     DS    0H                                                               
         MVC   BLEST+1(2),=C'NO'                                                
*                                                                               
         B     PRB8                                                             
*                                                                               
*        PBILESTS AND PBILESTE WERE USED BY "OLD" BILLING                       
*        FOR ESTIMATE RANGES                                                    
*                                                                               
*        NOW USED AS PBILLIMO (INVOICE MONTH)                                   
*                                                                               
**       OC    PBILESTS(4),PBILESTS                                             
**       BZ    PRB8                                                             
**       LA    R3,PBILESTS                                                      
**       BAS   RE,CVD                                                           
**       MVC   BLEST(3),WORK+2                                                  
**       MVI   PSECOND+BLESTD-1,C'-'                                            
**       LA    R3,PBILESTE                                                      
**       BAS   RE,CVD                                                           
**       MVC   PSECOND+BLESTD(3),WORK+2                                         
*                                                                               
PRB8     DS    0H                                                               
         CLI   PBILLPER,C' '       NOT USED FOR NEW BILLING                     
         BNH   PRB9                                                             
         CLI   PBILLPER,C'M'       TEST MONTH BILL                              
         BNE   PRB10                                                            
*PRB9     GOTO1 DTCNV,DMCB,(1,PBILKMOS),(5,BLPER)                               
PRB9     GOTO1 DATCON,DMCB,(3,PBILKMOS),(9,BLPER)                               
*                                                                               
         B     PRB12                                                            
*                                                                               
PRB10    DS    0H                  SPECIAL BILL PERIOD                          
         MVC   FULL(2),PBILKMOS                                                 
         MVC   FULL+2(1),PBILLSTA                                               
*        GOTO1 DTCNV,DMCB,(1,FULL),(4,BLPER)                                    
         GOTO1 DATCON,DMCB,(3,FULL),(7,BLPER)                                   
*                                                                               
         MVI   PSECOND+BLPERD-1,C'-'                                            
         MVC   FULL+2(1),PBILLEND                                               
*        GOTO1 (RF),(R1),,(3,WORK)                                              
         GOTO1 DATCON,(R1),,(5,WORK)                                            
         MVC   PSECOND+BLPERD(5),WORK+3                                         
*                                                                               
PRB12    DS    0H                                                               
*                                                                               
         GOTO1 =V(PPFMTINO),DMCB,PBILLDAT,(2,PBILKBNO),                X        
               (PBILKMED,B1PROF),B1XPROF                                        
*                                                                               
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)   SAVE FULL INVOICE NUMBERR                       
*                                                                               
         L     RF,DMCB+4        ADDRESS OF "SHORT" FORMAT                       
         MVC   DSINVNO(7),0(RF)     MAY INCLUDE DASH                            
******   MVC   BLINO(2),0(RF)                                                   
         MVC   BLINO+2(4),3(RF)     DON'T MOVE '-'                              
         L     RF,DMCB+8        ADDRESS OF Y/M                                  
         MVC   BLINO(2),0(RF)                                                   
         MVC   DINVNO(6),BLINO                                                  
         L     RF,DMCB+12                                                       
         MVC   DINVMED,0(RF)       MEDIA PART                                   
*                                                                               
PRB13X   DS    0H                                                               
*            SEE IF RETAIL CORP OR DISTRIBUTOR BILL                             
         CLI   PBRETAIL,X'02'                                                   
         BE    PRB13X2                                                          
         CLI   PBRETAIL,X'01'                                                   
         BNE   PRB13X5                                                          
*                                                                               
PRB13X2  MVC   PSECOND+BLRDATD(12),=C'RETAIL ACCT='                             
         MVC   PSECOND+BLRDATD+12(12),PBRACCT                                   
*                                                                               
PRB13X5  DS    0H                                                               
*                                                                               
*        GOTO1 DTCNV,DMCB,PBILLDAT,(3,BLRDAT)          RUN DATE                 
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(5,BLRDAT)         RUN DATE             
*                                                                               
*        GOTO1 (RF),(R1),(1,PBILINVD),(4,BLBDAT)       BILL DATE                
         GOTO1 DATCON,(R1),(3,PBILINVD),(7,BLBDAT)      BILL DATE               
*                                                      TYPE OF BILL             
         MVC   BLTYP(2),PBILLTYP                                                
         TM    PBILSTAT,X'20'      SEE IF GRP M MIDAS BILL                      
         BZ    PRB13X7                                                          
         MVI   BLTYP+2,C'T'                                                     
         LA    R3,BLTYP+2                                                       
         B     PRB14B              MUST BE NEW TYPE                             
*                                  NO ROOM FOR A DASH                           
*                                                                               
PRB13X7  LA    R3,BLTYP+2                                                       
         CLI   PBILLTYP,C'0'       SEE IF NEW BILL                              
         BNH   PRB14               YES                                          
*                                                                               
         MVC   BLTYP(3),=C'ORI'                                                 
         LA    R3,BLTYP+3                                                       
         CLI   PBILLTYP,C'3'                                                    
         BE    PRB14                                                            
         MVC   BLTYP(3),=C'DET'                                                 
         CLI   PBILLTYP,C'4'                                                    
         BE    PRB14                                                            
         MVC   BLTYP(3),=C'MAN'                                                 
PRB14    DS    0H                                                               
         MVI   0(R3),C'-'                                                       
*                                                                               
PRB14B   TM    PBILCMSW,X'02'          FOR COMMISSION ONLY BILL                 
         BZ    PRB15                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    PBILCMSW,X'20'        SEE IF ALSO AOR                            
         BO    PRB14D                                                           
         MVC   1(3,R3),=C'COM'                                                  
*                                                                               
PRB14D   CLC   QAGENCY,=C'WI'      WESTERN INTERNATIONAL                        
*SMY*    BNE   PRB14E                                                           
         BE    PRB14D5                                                          
         CLC   QAGENCY,=C'M1'      MEDIA FIRST INTERNATIONAL USES               
         BNE   PRB14E                "WI" PROC'S                                
PRB14D5  CLI   QOPT4,C'A'                                                       
         BE    PRB16                                                            
*                                                                               
PRB14E   CLI   QOPT6,C'C'          TEST TO LEAVE NET                            
         BE    *+10                                                             
         ZAP   PBILLNET,=P'0'      NO, SET NET = 0 (AC = RCVBL)                 
         B     PRB16                                                            
*                                                                               
PRB15    DS    0H                                                               
         MVC   1(3,R3),=C'ADJ'                                                  
         CLI   PBILSEP,C'A'                                                     
         BE    PRB16                                                            
         MVC   1(3,R3),=C'CD '                                                  
         CLI   PBILSEP,C'C'                                                     
         BE    PRB16                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    PBILCMSW,X'20'        AOR BILL                                   
         BO    PRB16                                                            
         MVC   1(3,R3),=C'UFC'                                                  
         TM    PBILCMSW,X'01'         UP FRONT COMMISSION                       
         BO    PRB16                                                            
         MVC   1(3,R3),=C'NET'                                                  
         TM    PBILCMSW,X'08'         UP FRONT COMMISSION - NET                 
         BO    PRB16                                                            
*                                                                               
         MVC   1(3,R3),=C'REG'                                                  
*                                                                               
PRB16    DS    0H                                                               
*                                                                               
         ZAP   BILTOTS+30(6),MYGST                                              
         ZAP   BILTOTS+36(6),MYPST                                              
*                                                                               
**NEW 3/17/89                                                                   
         CLI   QOPT6,C'A'          SEE IF AOR ONLY                              
         BE    PRB16A              YES - THEN INCLUDE IN TOTALS                 
         TM    PBILCMSW,X'20'      SEE IF AOR BILL                              
         BZ    PRB16B                                                           
PRB16A   ZAP   BILTOTS(6),=P'0'                                                 
         ZAP   BILTOTS+6(6),=P'0'                                               
         ZAP   BILTOTS+12(6),=P'0'                                              
         ZAP   BILTOTS+18(6),PBILLRCV    SET AC TO RCVBL                        
         ZAP   BILTOTS+24(6),PBILLRCV    ACTUAL                                 
         AP    BILTOTS+24(6),MYGST                                              
         AP    BILTOTS+24(6),MYPST                                              
         ZAP   BILTOTS+30(6),MYGST                                              
         ZAP   BILTOTS+36(6),MYPST                                              
         B     PRB16F                                                           
*                                                                               
*        SET MATS IN 5 FIELDS FOR ACCUMES                                       
*        (MATCH 5 PRINT COLUMNS)                                                
*                                                                               
PRB16B   DS    0H                                                               
         CLC   QAGENCY,=C'H9'                                                   
         BE    PRB16E                                                           
*                                                                               
         ZAP   BILTOTS+0*BPLEQ(BPLEQ),PBILLGRS     GROSS                        
         ZAP   BILTOTS+1*BPLEQ(BPLEQ),PBILLNET     NET                          
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'                                     
         SP    BILTOTS+2*BPLEQ(BPLEQ),PBILLRCV     CD IS -(RCVBL)               
*                                                                               
         CLI   PBILSEP,C'C'        FOR SEP CD BILL                              
         BE    PRB16C                                                           
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'        CD IS 0                      
*                                                                               
         CLI   PBILCDSW,C'S'       IF SEP (ON OTHER BILL) BILL                  
         BE    PRB16C                                                           
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),PPBVEBC      EFFECTIVE CD                 
*                                                                               
PRB16C   ZAP   BILTOTS+4*BPLEQ(BPLEQ),PBILLRCV     RCVBL                        
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYGST        ADD GST TO BILL AMT          
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYPST        ADD PST TO BILL AMT          
         ZAP   BILTOTS+5*BPLEQ(BPLEQ),MYGST        AND GST TOTALS               
         ZAP   BILTOTS+6*BPLEQ(BPLEQ),MYPST        AND PST TOTALS               
*                                                                               
         ZAP   DUB,PBILLRCV                                                     
         TM    PBILCMSW,X'02'      UNLESS COMMISSION ONLY BILL                  
         BNZ   PRB16D                                                           
         TM    PBILBASA,X'04'                      IS CD ALREADY OUT?           
         BNZ   PRB16D                              YES                          
         SP    DUB,BILTOTS+2*BPLEQ(BPLEQ)          NO, TAKE IT OUT NOW          
*                                                                               
PRB16D   DS    0H                                                               
         ZAP   BILTOTS+3*BPLEQ(BPLEQ),DUB          FOR AC CALC                  
         SP    BILTOTS+3*BPLEQ(BPLEQ),PBILLNET     -NET = TRUE AC               
         B     PRB16F                                                           
*                                                                               
PRB16E   DS    0H                                                               
*                                                                               
         ZAP   BILTOTS+0*BPLEQ(BPLEQ),PBILLGRS     GROSS                        
         SP    BILTOTS+0*BPLEQ(BPLEQ),PBILLBIL                                  
         AP    BILTOTS+0*BPLEQ(BPLEQ),PBILLRCV                                  
*                                                                               
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),PBILLGRS                                  
         SP    BILTOTS+2*BPLEQ(BPLEQ),PBILLBIL     CD IS -(RCVBL)               
*                                                                               
         ZAP   BILTOTS+1*BPLEQ(BPLEQ),PBILLNET     NET                          
         AP    BILTOTS+1*BPLEQ(BPLEQ),BILTOTS+2*BPLEQ(BPLEQ)                    
*                                                                               
         ZAP   BILTOTS+3*BPLEQ(BPLEQ),BILTOTS+0*BPLEQ(BPLEQ)                    
         SP    BILTOTS+3*BPLEQ(BPLEQ),BILTOTS+1*BPLEQ(BPLEQ)                    
         SP    BILTOTS+3*BPLEQ(BPLEQ),BILTOTS+2*BPLEQ(BPLEQ)                    
*                                                                               
         ZAP   BILTOTS+4*BPLEQ(BPLEQ),PBILLRCV     RCVBL                        
*                                                                               
PRB16F   DS    0H                                                               
         CLC   QAGENCY,=C'OO'      OMDUSEC (USA)                                
         BE    *+14                                                             
         CLC   QAGENCY,=C'OU'      OMDTOA                                       
         BNE   PRB16J                                                           
         CP    BILTOTS+4*BPLEQ(BPLEQ),=P'0'  ZERO DOLLAR INVOICE?               
         BNE   PRB16J                                                           
         L     R0,CLTZINV                                                       
         A     R0,=F'1'            ADD ONE TO ZERO INVOICE TOTALS               
         ST    R0,CLTZINV                                                       
         MVC   BLINE(132),SPACES                                                
         B     EXIT                                                             
*                                                                               
PRB16J   LA    R3,BILTOTS                                                       
         GOTO1 AFMTAMTS                                                         
*                                                                               
         BAS   RE,PRNT                                                          
*                                                                               
         CLI   QOPT1,C'F'               FORMULA OPTION                          
         BNE   PRB22                    NO                                      
         OC    PBILBASA(5),PBILBASA                                             
         BZ    PRB22                    NO FORMULA                              
         CLI   PBILBASA,5          DONT PRINT IF G-CD +0 PCT                    
         BNE   *+14                                                             
         OC    PBILADJ,PBILADJ                                                  
         BZ    PRB22                                                            
         LA    R3,BLINO                                                         
         MVC   0(8,R3),=C'FORMULA='                                             
         SR    RF,RF                                                            
         IC    RF,PBILBASA                                                      
         SLL   RF,2                                                             
         LA    RF,ADJWRDS-4(RF)                                                 
         MVC   9(4,R3),0(RF)                                                    
         LA    R3,11(R3)                                                        
         CLI   1(RF),C' '                                                       
         BE    *+8                                                              
         LA    R3,3(R3)                                                         
         MVI   1(R3),C'0'                                                       
         LA    R0,1                                                             
         OC    PBILADJ,PBILADJ                                                  
         BZ    PRB18                                                            
         EDIT  (B3,PBILADJ),(8,1(R3)),5,ALIGN=LEFT                              
         LR    R4,R3                                                            
         AR    R4,R0               POINT TO END                                 
PRB17    DS    0H                                                               
         LR    R5,R4                                                            
         CLI   0(R4),C'0'                                                       
         BE    PRB17D                                                           
         CLI   0(R4),C'.'                                                       
         BE    PRB17B                                                           
         B     PRB18                                                            
PRB17B   LA    R4,1                END SCAN ON .                                
PRB17D   BCTR  R0,R0               SHORTEN LENGTH                               
         MVI   0(R5),C' '                                                       
PRB17F   BCT   R4,PRB17                                                         
*                                                                               
PRB18    DS    0H                                                               
         MVI   0(R3),C'+'                                                       
         TM    PBILADJ,X'80'                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
*                                                                               
         AR    R3,R0                                                            
         MVC   2(6,R3),=C'PCT OF'                                               
         SR    RF,RF                                                            
         IC    RF,PBILBASB                                                      
         SLL   RF,2                                                             
         LA    RF,ADJWRDS-4(RF)                                                 
         MVC   9(4,R3),0(RF)                                                    
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
*                                                                               
PRB22    DS    0H                                                               
         CLI   QOPT2,C'R'               REVERSALS OPTION                        
         BNE   PRB26                    NO                                      
         CLI   PBILLCDT,C'0'                                                    
         BE    PRB26                    NOT REVERSED                            
         LA    R3,BLINO                                                         
         MVC   0(11,R3),=C'REVERSED BY'                                         
         MVC   12(4,R3),PBILLCAN+2                                              
         MVC   17(2,R3),=C'ON'                                                  
*        GOTO1 DTCNV,DMCB,PBILLCDT,(3,20(R3))                                   
         GOTO1 DATCON,DMCB,(0,PBILLCDT),(5,20(R3))                              
*                                                                               
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
*                                                                               
PRB26    DS    0H                                                               
         LA    R3,BILTOTS                                                       
         LA    R4,PRDTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         LA    R3,BILTOTS                                                       
         LA    R4,CLTTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         MVI   RACTSW,1            SET REQUEST ACTIVITY                         
         MVI   OACTSW,1            SET REQUEST ACTIVITY                         
*                                                                               
PRB28    DS    0H                                                               
         L     RF,AOUTP            SPECIAL INTERFACE MODULE                     
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         CLI   QOPT4,C'N'           SEE IF NOT PUTTING ON TAPE                  
         BE    EXIT                                                             
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
***OFF                                                                          
LBOFF    DS    0H                                                               
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         L     R0,CLTINVS                                                       
         L     RE,CLTZINV          CLIENT ZERO INVOICE COUNT                    
         SR    R0,RE               ADJUST CLIENT INVOICE COUNT                  
         ST    R0,CLTINVS                                                       
*                                                                               
         CLI   OACTSW,0                                                         
         BNE   PRB2B      MUST DO PRD/CLT TOTALS FIRST                          
*                         WILL RETURN TO LBOFF5                                 
LBOFF5   MVI   BLPRD,C'*'                                                       
*****    MVC   BLPRD+1(1),RCSVOFC                                               
*****    MVI   BLPRD+2,C' '                                                     
*****    GOTO1 =V(OFFOUT),DMCB,RCSVOFC,HEXOUT,BLPRD+1                           
         MVC   BLPRD+1(2),SAVCOFF  (FROM OFFICER CALL)                          
         MVC   BLINE+5(17),=C'**OFFICE TOTALS**'                                
*                                                                               
         MVI   OACTSW,0                                                         
*                                                                               
         CLC   OFFTOTS(24),=4PL6'0'                                             
         BNE   LBOFF8                                                           
         OC    OFFINVS,OFFINVS     ANY INVOICES?                                
         BNZ   LBOFF8                                                           
*                                                                               
         MVC   BLINE+25(11),=C'NO ACTIVITY'                                     
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
*                                                                               
LBOFF8   DS    0H                                                               
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,OFFINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,OFFTOTS                                                       
         MVI   TOTSW,2                                                          
         GOTO1 AFMTAMTS                                                         
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
         LA    R3,OFFTOTS                                                       
         LA    R4,REQTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
*                                                                               
         L     R0,REQINVS        ROLL OFFICE INVOICE TOTAL                      
         A     R0,OFFINVS        TO REQUEST                                     
         ST    R0,REQINVS                                                       
         XC    OFFINVS,OFFINVS                                                  
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
FBOFF    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTKEY,LASTKEY                                                  
         MVI   OACTSW,0                                                         
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         XC    OFFINVS,OFFINVS                                                  
         B     EXIT                                                             
***OFF                                                                          
*                                  FORMAT $ AMTS                                
         SPACE 2                                                                
*                                                                               
NEXTEL1  DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JE    NEXTEL1X                                                         
         CLC   0(1,R2),MELCODE                                                  
         BER   RE                                                               
         J     NEXTEL1                                                          
*                                                                               
NEXTEL1X LTR   RE,RE       NOT FOUND -                                          
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
PRNT     NTR1                                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,0                                                       
         CLI   PAGYNAT,C'C'           SEE IF CANADIAN                           
         BNE   *+8                                                              
         MVI   RCSUBPRG,50                                                      
*                                                                               
         CLI   FORCEHED,C'Y'                                                    
         BE    PRNT2                                                            
         ZIC   R1,LINE                                                          
         ZIC   R0,ALLOWLIN                                                      
         AR    R1,R0                                                            
         STC   R1,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    PRNT90                                                           
PRNT2    DS    0H                                                               
         CLI   MODE,LBUYREQ          REQUEST TOTALS                             
         BNL   PRNT90                                                           
         CLI   QCLIENT,C'$'          SEE IF DOING OFFICE LIST REQ               
         BNE   PRNT3                                                            
         MVC   HEAD4(8),=C'OFFICE X'                                            
*****    MVC   HEAD4+7(1),RCSVOFC                                               
*****    GOTO1 =V(OFFOUT),DMCB,RCSVOFC,HEXOUT,HEAD4+7                           
         MVC   HEAD4+7(2),SAVCOFF                                               
         B     PRNT4                                                            
PRNT3    CLI   QCLIENT,C'*'                                                     
         BNE   PRNT4                                                            
         CLI   QCLIENT+1,C'-'          ALL BUT OFFICE REQUEST                   
         BE    PRNT3B                                                           
         MVC   HEAD4(8),=C'OFFICE X'                                            
*****    MVC   HEAD4+7(1),QCLIENT+1                                             
*****    GOTO1 =V(OFFOUT),DMCB,QCLIENT+1,HEXOUT,HEAD4+7                         
         MVC   HEAD4+7(2),SAVCOFF                                               
         B     PRNT4                                                            
*                                                                               
PRNT3B   MVC   HEAD4(16),=C'* NOT OFFICE X *'                                   
         MVC   HEAD4+13(1),QCLIENT+2                                            
         B     PRNT4                                                            
*                                                                               
PRNT4    DS    0H                                                               
         CLI   QOPT6,C'A'         TEST AOR ONLY                                 
         BNE   PRNT5                                                            
         MVC   HEAD5(18),=C'**AOR BILLS ONLY**'                                 
*                                                                               
PRNT5    DS    0H                                                               
         CLI   QOPT6,C'B'         AOR AND AOR/CLIENT                            
         BNE   PRNT5B                                                           
         MVC   HEAD5(24),=C'**AOR AND CLIENT BILLS**'                           
*                                                                               
PRNT5B   DS    0H                                                               
         CLI   QOPT6,C'X'         NON=AOR BILLS ONLY                            
         BNE   PRNT6                                                            
         MVC   HEAD5(22),=C'**NON-AOR BILLS ONLY**'                             
*                                                                               
PRNT6    DS    0H                                                               
         CLI   QOPT6,C'C'         COMMISSION ONLY BILLS                         
         BNE   PRNT7                                                            
         MVC   HEAD5(25),=C'**COMMISSION ONLY BILLS**'                          
*                                                                               
PRNT7    DS    0H                                                               
         CLI   QOPT6,C'N'     NON-COMMISSION ONLY BILLS                         
         BNE   PRNT8                                                            
         MVC   HEAD5(34),=C'**COMMISSION ONLY BILLS EXCLUDED**'                 
PRNT8    DS    0H                                                               
         CLI   QOPT5,C'S'     SOON BILLS ONLY                                   
         BNE   PRNT90                                                           
         LA    R1,HEAD5                                                         
         CLC   HEAD5(5),SPACES  SEE IF HEAD5 USED ALREADY                       
         BE    *+8                                                              
         LA    R1,HEAD6         IF SO, USE HEAD6                                
         MVC   0(19,R1),=C'**SOON BILLS ONLY**'                                 
*                                                                               
PRNT90   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   MAXLINES,SAVMAX                                                  
         B     EXIT                                                             
         SPACE 3                                                                
ROLTOTS  DS    0H                                                               
         LA    R0,7           FOR BCT                                           
ROLTOTS2 DS    0H                                                               
         AP    0(6,R4),0(6,R3)                                                  
         LA    R4,6(R4)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,ROLTOTS2                                                      
         BR    RE                                                               
         SPACE 3                                                                
CLRTOTS  DS    0H                                                               
         LA    R0,7                                                             
         ZAP   0(6,R3),=PL6'0'                                                  
         LA    R3,6(R3)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         SPACE 3                                                                
CVD      DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),0(R3)                                                  
*                                                                               
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         BR    RE                                                               
         SPACE 3                                                                
ADJWRDS  DS    0C                                                               
         DC    C'G   '                                                          
         DC    C'N   '                                                          
         DC    C'    '                                                          
         DC    C'    '                                                          
         DC    C'G-CD'                                                          
         DC    C'N-CD'                                                          
         SPACE 2                                                                
         DROP  RB,R9                                                            
         SPACE 2                                                                
         EJECT                                                                  
MQOPEN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
*                                                                               
* IF WE'RE RUNNING A TEST, SEND TO TEST MQ BROKER                               
         CLI   TESTMQ,C'T'         IS THIS A MQ TEST RUN                        
         BNE   *+8                  NO                                          
         OI    DMCB+8,X'01'         YES -PUT TO TEST MQ BROKER                  
*                                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'OPEN'),(0,=C'MEDIACOMSFTP****'),,0          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
FMTAMTS  NMOD1 0,FMTAMTS                                                        
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         MVI   STAGSW,0                                                         
         CP    0(6,R3),=P'9999999999'   COMPARE GROSS TO 100 MILLION            
         BL    *+8                                                              
         MVI   STAGSW,1            SET TO STAGGER TOTALS                        
*                                                                               
         LA    R5,0(R3)            GROSS                                        
         LA    R4,BLGRS                                                         
         LH    R6,=H'-1'          USED WHEN STAGGERING                          
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,6(R3)           NET                                           
         LA    R4,BLNET                                                         
         LA    R6,131              NEXT LINE -1                                 
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,12(R3)           CD                                           
         LA    R4,BLCD                                                          
         LH    R6,=H'-1'                                                        
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,18(R3)           AC                                           
         LA    R4,BLAC                                                          
         LA    R6,131              NEXT LINE-1                                  
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,24(R3)           ACTUAL                                       
         LA    R4,BLBIL                                                         
         LH    R6,=H'-1'                                                        
         BAS   RE,FMTEDT                                                        
*                                                                               
         CLI   PAGYNAT,C'C'        SEE IF CANADIAN                              
         BNE   FMTAX                                                            
*                                                                               
         LA    R5,30(R3)           GST (PAST IS AT 36(R3))                      
         LA    R4,BLGST                                                         
         BAS   RE,FMTEDT8       PRINT PST UNDER GST                             
*                                                                               
FMTAX    MVI   TOTSW,0                                                          
         XIT1                                                                   
         SPACE 2                                                                
FMTEDT   DS    0H                                                               
         CLI   STAGSW,1       SEE IF STAGGERING                                 
         BNE   FMTEDT5                                                          
         AR    R4,R6          ADJUST R4 FOR STAGGERING                          
         EDIT  (P6,0(R5)),(16,0(R4)),2,COMMAS=YES,CR=YES                        
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   14(R4),C'C'                                                      
         BER   RE                                                               
         MVI   14(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   15(R4),C'*'                                                      
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
FMTEDT5  EDIT  (P6,0(R5)),(15,0(R4)),2,COMMAS=YES,CR=YES                        
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   13(R4),C'C'                                                      
         BER   RE                                                               
         MVI   13(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   14(R4),C'*'                                                      
         BR    RE                                                               
         SPACE 3                                                                
FMTEDT8  DS    0H               SPECIAL EDT FOR GST AND PST                     
         EDIT  (P6,0(R5)),(14,0(R4)),2,COMMAS=YES,CR=YES                        
         EDIT  (P6,6(R5)),(14,132(R4)),2,COMMAS=YES,CR=YES                      
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   12(R4),C'C'                                                      
         BE    FMTEDT8C                                                         
         MVI   12(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BE    FMTEDT8C                                                         
         MVI   13(R4),C'*'                                                      
*                                                                               
FMTEDT8C DS    0H                                                               
         CLI   12+132(R4),C'C'                                                  
         BER   RE                                                               
         MVI   12+132(R4),C'*'                                                  
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   13+132(R4),C'*'                                                  
*                                                                               
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
*        NISSAN INTERFACE (H7 + SJ)                                             
*                                                                               
INPROC   NMOD1 0,INPROC                                                         
         L     RC,PPFILEC                                                       
*                                                                               
INPR1    DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BE    INPRL                                                            
*                                                                               
         MVI   SVQOPT7,C'N'                                                     
         CLI   QOPT7,C'Y'                                                       
         BNE   *+8                                                              
         MVI   SVQOPT7,C'Y'                                                     
*                                                                               
         MVI   PBIREC,C' '                                                      
         MVC   PBIREC+1(250),PBIREC                                             
         MVC   PBIREC+250(250),PBIREC                                           
*                                                                               
         LA    R2,PPRDREC+33                                                    
*                                                                               
         MVI   MELCODE,X'08'        GET PRD USER FIELDS                         
         BRAS  RE,NEXTEL1                                                       
         BNE   INPR1A               NONE - GET ESTIMATE                         
         MVC   PBIREC+159(32),PUSER1      GL ACCOUNT                            
         MVC   PBIREC+181(16),PUSER2      COST CENTER                           
         OC    PBIREC+159(32),SPACES                                            
         OC    PBIREC+181(16),SPACES                                            
*                                                                               
INPR1A   MVC   WORK(12),PBILLREC                                                
         MVI   WORK+3,7                                                         
         CLC   PESTREC(12),WORK        TEST HAVE EST HDR                        
         BE    INPR1C              YES                                          
         MVC   WORK(64),KEY        SAVE KEY FOR SEQ                             
         XC    KEY,KEY                                                          
         MVC   KEY(12),PBILLREC                                                 
         MVI   KEY+3,7                                                          
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
INPR1C   DS    0H                                                               
         LA    R2,PESTREC+33                                                    
PEU      USING PESTUDEF,R2                                                      
         MVI   MELCODE,X'08'     LOOK FOR USER ELEMENT                          
         BRAS  RE,NEXTELM                                                       
         BNE   INPR1E                                                           
         MVC   PBIREC+197(32),PEU.PEUSER1                                       
         MVC   PBIREC+229(16),PEU.PEUSER2                                       
         OC    PBIREC+197(32),SPACES                                            
         OC    PBIREC+229(16),SPACES                                            
*                                                                               
         DROP  PEU                                                              
*                                                                               
*                                                                               
INPR1E   MVI   PBIREC,C'L'             DETAIL RECORD                            
*                                                                               
*        COST CENTER - EST USER1 FIELD?  (PBIREC+159)                           
*        INTERNAL ORDER - EST USER1 FIELD?  (PBIREC+169)                        
*                                                                               
*        OLD CODE - SET DEFAULT G/L CODE - NOW IN EST USER 2                    
*******  MVC   PBIREC+7(6),=C'631000'   DEFAULT G/L ACCOUNT                     
*                                                                               
******   MVC   PBIREC+211(3),PBILKPRD                                           
         MVC   PBIREC+364(L'DINVFULL),DINVFULL                                  
*                                                                               
******   MVC   WORK(2),PBILKMOS            YYMM FROM HEADER                     
******   MVI   WORK+2,X'01'                DAY TO 01                            
******                                                                          
******   GOTO1 DATCON,DMCB,(3,WORK),(6,WORK+6)                                  
******   MVC   PBIREC+273(11),=C'TRADE MEDIA'                                   
******   LA    RE,PBIREC+285                                                    
******   CLI   PBILKMED,C'T'                                                    
******   BE    INP1A                                                            
******   MVC   PBIREC+273(12),=C'SEARCH MEDIA'                                  
******   LA    RE,PBIREC+286                                                    
******   CLI   PBILKMED,C'S'                                                    
******   BE    INP1A                                                            
******   MVC   PBIREC+273(13),=C'OUTDOOR MEDIA'                                 
******   LA    RE,PBIREC+287                                                    
******   CLI   PBILKMED,C'O'                                                    
******   BE    INP1A                                                            
******   MVC   PBIREC+273(14),=C'MAGAZINE MEDIA'                                
******   LA    RE,PBIREC+288                                                    
******   CLI   PBILKMED,C'M'                                                    
******   BE    INP1A                                                            
******   MVC   PBIREC+273(15),=C'NEWSPAPER MEDIA'                               
******   LA    RE,PBIREC+289                                                    
******   CLI   PBILKMED,C'N'                                                    
******   BE    INP1A                                                            
******   MVC   PBIREC+273(17),=C'INTERACTIVE MEDIA'                             
******   LA    RE,PBIREC+291                                                    
******   CLI   PBILKMED,C'I'                                                    
******   BE    INP1A                                                            
******   DC    H'0'     UNKNOWN MEDIA CODE                                      
******                                                                          
**P1A    DS    0H                                                               
******   MVC   0(6,RE),WORK+6     FLOAT MOS AFTER MEDIA                         
*****                                                                           
******   OLD CODE - SET DEFAULT G/L CODE - NOW IN EST USER 2                    
*******  MVC   PBIREC+7(6),=C'631000'    BUDGET CODE/GL ACCOUNT                 
*                                                                               
         LA    R3,PBILKEST                                                      
         BRAS  RE,CVD                                                           
*                                                                               
         MVC   PBIREC+273(3),WORK+2                                             
         MVC   PBIREC+277(L'PESTNAME),PESTNAME                                  
         OC    PBIREC+277(L'PESTNAME),SPACES                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILINVD),(20,PBIREC+347)  INVOICE DATE           
         GOTO1 DATCON,DMCB,(3,PBILDUED),(X'20',WORK)       DUE DATE             
*                                                                               
         MVC   PBIREC+339(2),WORK+2    MM                                       
         MVI   PBIREC+341,C'/'                                                  
         MVC   PBIREC+342(2),WORK+4    DD    DUE DATE = MM/DD/YY                
         MVI   PBIREC+344,C'/'                                                  
         MVC   PBIREC+345(2),WORK      YY                                       
*                                                                               
         MVC   PBIREC+355(8),CTODAY    CCYYMMDD                                 
*                                                                               
INPR4    DS    0H                                                               
*                                                                               
         ZAP   PBIREC+28(8),PBILLRCV    ACTUAL                                  
         ZAP   PBIREC+36(8),MYGST                                               
         AP    PBIREC+36(8),MYPST       NOTE-INCLUDES HST                       
*                                                                               
*        REST OF RECORD IS SPACES                                               
*                                                                               
         CLI   SVQOPT7,C'Y'   TEST RUN                                          
         BNE   INLAR1                                                           
*                                                                               
         B     INLAR1         NORMALLY SKIP THIS DISPLAY                        
*                             PATCH IF YOU WANT TO SEE IT                       
         CP    INCOUNT,=P'50'                                                   
         BH    INLAR1                                                           
         MVC   P(23),=C'***BILLING RECORD - HEX'                                
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBILLKEY,P,65,0                                      
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBILLKEY+65,P,65,0                                   
         GOTO1 REPORT                                                           
         MVC   P(14),=C'***TEMP RECORD'                                         
         GOTO1 REPORT                                                           
         MVC   P(124),PBIREC                                                    
         MVC   PSECOND(124),PBIREC+124                                          
         GOTO1 REPORT                                                           
         MVC   P(127),PBIREC+248                                                
         GOTO1 REPORT                                                           
         MVC   P(20),=C'***TEMP RECORD - HEX'                                   
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC,P,65,0                                        
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC+65,P,65,0                                     
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC+130,P,65,0                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC+195,P,65,0                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC+260,P,65,0                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC+325,P,50,0                                    
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
INLAR1   DS    0H                                                               
*                                                                               
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         MVI    INTYPE,C'1'                                                     
         XC     INMOS,INMOS                                                     
*                                                                               
INLAR2   DS     0H                                                              
         MVI    INCRDT,C'2'         INIT TO DEBIT                               
         CP     PBIREC+28(8),=P'0'  IS THIS A CREDIT?                           
         BNL    *+8                 NO                                          
         MVI    INCRDT,C'1'         YES - CREDITS MUST COME FIRST               
         MVC    INMED,PBILKMED                                                  
         MVC    INCLI,PBILKCLT                                                  
         MVC    INPRO,PBILKPRD                                                  
         MVC    INEST,PBILKEST                                                  
         MVC    ININVMO,PBILKBMN+1     BILLING MONTH                            
         MVC    ININVN,PBILKBNO                                                 
*                                                                               
         MVC    INREC(250),PBIREC                                               
         MVC    INREC+250(125),PBIREC+250                                       
         MVI    INREC,C'1'        CHANGES THE 'L'                               
         CLI    INTYPE,C'1'                                                     
         BNE    INLAR2A                                                         
***                                                                             
* WE MAY HAVE A JOB THAT PROCESSES THE SAME INVOICE MORE THAN ONCE              
* SUCH AS AN ALL CLIENT REQUEST FOLLOWED BY A CLIENT SPECIFIC REQUEST           
* THE FOLLOWING CODE WAS ADDED TO IGNORE DUP INVOICES RATHER THAN ABEND         
***                                                                             
***      MVC   ININVALS,=A(INMED) RESET KEY                                     
         MVC   ININVALS,=A(INKEY) RESET KEY                                     
         MVI   ININVALS,0         SET TO SEARCH FOR EXACT MATCH                 
         GOTO1 =V(BINSRCH),ININVALS                                             
         CLI   ININVALS,1         RECORD FOUND?                                 
         BNE   INTOXITX           YES - AVOID DUP REC - RETURN CC NEQ           
         B     INLAR3             NO - GO ADD RECORD                            
*                                                                               
INLAR2A  MVI    INREC,C'2'        INTYPE MUST BE 2                              
         MVC   INMOS,PBILKMOS                                                   
*                                                                               
INLAR3   L      R2,AOFINT         ADDRESS OF INTAB                              
***      MVC   ININVALS,=A(INMED)                                               
         MVC   ININVALS,=A(INKEY)                                               
         MVI   ININVALS,1                                                       
         GOTO1 =V(BINSRCH),ININVALS                                             
*                                                                               
         CLI    ININVALS,1         RECORD INSERTED                              
         BE     INTOXIT                                                         
         OC     ININVALS+1(3),ININVALS+1 IF ZERO TABLE IS FULL                  
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         CLI    INTYPE,C'1'                                                     
         BE     *+6                                                             
         DC     H'0'                     SOMETHING WRONG                        
*                                                                               
         L      RF,ININVALS              ADDRESS OF FOUND RECORD                
         LA     RF,L'INKEY(RF)           PAST KEY                               
*                                                                               
         AP     28(8,RF),PBIREC+28(8)  PRE-TAX AMOUNT                           
         AP     36(8,RF),PBIREC+36(8)  TOTAL CANADIAN TAXES                     
*                                                                               
INTOXIT  DS    0H                                                               
         CLI   INTYPE,C'1'         DID I JUST ADD A TYPE 1 REC?                 
         BNE   INTOXITX            IF  NOT THEN DONE                            
         MVI   INTYPE,C'2'                                                      
         B     INLAR2              NOW ADD TYPE 2 RECORD                        
*                                                                               
INTOXITX DS    0H                                                               
***      MVC   ININVALS,=A(INMED)                                               
         MVC   ININVALS,=A(INKEY)                                               
         MVI   ININVALS,1                                                       
         XIT1                                                                   
         EJECT                                                                  
*                      FINALLY PRODUCE RECORDS                                  
INPRL    DS    0H                                                               
*                                                                               
         CLI   SVQOPT7,C'Y'           TEST RUN - NO TAPE                        
         BE    INPRL5                                                           
***************************************************************                 
***** NO ALLOCATION HERE FOR SFTP STYLE OUTPUT                                  
***************************************************************                 
         BC    0,INPRL5                                                         
         OI    *-3,X'F0'                                                        
*                                                                               
         L     RF,ADMASTC                                                       
         MVI   MCTAPETY-MASTD(RF),C'V'  UNIT=VTS                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+0(4),=A(DYNDDN)                                             
         MVC   DMCB+4(4),=A(DYNDSN)                                             
         MVI   DMCB+4,X'FE'                                                     
         MVI   BYTE,1              GENERATION +1                                
         GOTO1 DYNALLOC,DMCB,,,(X'80',BYTE)                                     
         OPEN  (INPBIT1,(OUTPUT))                                               
*                                                                               
INPRL5   DS    0H                                                               
         ZAP   INHCNT,=P'0'        INVOICE HEADERS                              
         ZAP   INLCNT,=P'0'        ALL RECORDS                                  
         ZAP   INCOUNT,=P'0'       RECORD COUNT                                 
         MVI   INRECSW,0                                                        
         L     R2,INRECNT          FOR BCT RECORD COUNT                         
         CH    R2,=H'0'            NO RECORDS                                   
         BE    INZERO                                                           
*                                                                               
         LA    RE,PBIREC           CLEAR PBIREC                                 
         LH    R1,=H'600'                                                       
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
*        FIRST PUT OUT HEADER RECORD                                            
*                                                                               
         MVC   PBIREC(3),=C'HDR'                                                
         MVC   PBIREC+3(8),=C'ZFI04010'       JOB NAME                          
         CLI   OMDUSASW,C'Y'      OMDUSA?                                       
         BNE   *+10                                                             
         MVC   PBIREC+3(8),=C'ZFI02125'       JOB NAME                          
*                                                                               
         MVC   PBIREC+11(10),MCTODAY       TODAY  MM/DD/YYYY                    
         MVC   PBIREC+21(6),TIMEOFDF     HHMMSS                                 
         MVC   PBIREC+27(16),=C'IT306OMD INVOICE'                               
         CLI   OMDUSASW,C'Y'      OMDUSA?                                       
         BNE   *+10                                                             
         MVC   PBIREC+27(23),=C'IT306 MARKETING INVOICE'                        
*                                                                               
         MVI   INRECSW,C'A'        FILE HEADER                                  
         AP    INLCNT,=P'1'                                                     
         B     INAGN5                                                           
*                                                                               
INPRL8   L     R3,AOFINT                                                        
         LA    R3,L'INKEY(R3)      BUMP PAST PSUEDO KEY                         
*                                                                               
INAGN    DS    0H                                                               
         TM    AGYSW,AGYOO+AGYOU   AGENCY OU/OO NISSAN                          
         BZ    INAGN00             NO                                           
         CLI   0(R3),C'1'          INVOICE HEADER?                              
         BNE   INAGN00                                                          
         CP    28(8,R3),=P'0'      ZERO DOLLAR INVOICE?                         
         BNE   INAGN00                                                          
         LA    R3,INRECLEN(R3)     AND INVOICE DETAIL                           
         BCTR  R2,0                                                             
         CLI   0(R3),C'2'          INVOICE DETAIL?                              
         BE    INAGNX              SKIP                                         
                                                                                
INAGN00  DS    0H                                                               
         CLI   0(R3),C'1'          FIRST INVOICE DETAIL                         
         BNE   INAGN2                                                           
*                                                                               
*        DO INVOICE HEADER (1) RECORD FIRST                                     
*                                                                               
*                                                                               
         LA    RE,PBIREC           CLEAR PBIREC                                 
         LH    R1,=H'600'                                                       
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
*                                                                               
*                                                                               
         MVI   PBIREC,C'1'           HEADER INDICATOR                           
*                                                                               
*   BILL RUN DATE COMES TO ME AS YYYYMMDD                                       
*                                                                               
         MVC   PBIREC+1(2),351(R3)   MM                                         
         MVI   PBIREC+3,C'/'                                                    
         MVC   PBIREC+4(2),353(R3)   DD                                         
         MVI   PBIREC+6,C'/'                                                    
         MVC   PBIREC+7(4),347(R3)   YYYY                                       
*                                                                               
         MVC   PBIREC+11(4),=C'2135' COMPANY CODE                               
         CLI   OMDUSASW,C'Y'         OMDUSEC                                    
         BNE   *+10                                                             
         MVC   PBIREC+11(4),=C'2177' COMPANY CODE                               
*                                                                               
*        DUE DATE COMES TO ME AS MM/DD/YY                                       
*                                                                               
         MVC   PBIREC+15(6),339(R3) DUE DATE MM/DD/YYYY - FIX                   
* ASSUME 20TH CENTURY (Y2K!)                                                    
         MVC   PBIREC+21(2),=C'20'   CENTURY                                    
         MVC   PBIREC+23(2),345(R3)  YY                                         
         MVC   PBIREC+25(3),=C'CAD'  CURRENCY                                   
         CLI   OMDUSASW,C'Y'      OMDUSEC                                       
         BNE   *+10                                                             
         MVC   PBIREC+25(3),=C'USD'  CURRENCY                                   
         MVC   PBIREC+28(10),364(R3) FULL INV # WITH DASHES                     
         MVC   PBIREC+44(25),273(R3)       JOB DESCRIPTION                      
         MVC   PBIREC+69(9),=C'NC1003512'  VENDOR CODE                          
         CLI   OMDUSASW,C'Y'     OMDUSA?                                        
         BNE   *+10                                                             
         MVC   PBIREC+69(9),=C'AO0025401'  VENDOR CODE                          
*                                                                               
         ZAP   MYDUB,28(8,R3)        NON-FIRST DETAIL RECORD                    
         EDIT  (P8,MYDUB),(16,PBIREC+79),2                                      
*                                                                               
         CLC   PBIREC+91(2),=C' .'  IF LESS THAN $1.00 ADD A ZERO               
         BNE   *+8                                                              
         MVI   PBIREC+91,C'0'                                                   
*                                                                               
*                                                                               
         CLI   OMDUSASW,C'Y'        OMDUSA - NO CANADIAN TAXES                  
         BE    INAGN0                                                           
*        ALL CANADIAN TAXES HERE  - LENGTH 16  LIKE ABOVE                       
*                                                                               
         ZAP   MYDUB,36(8,R3)        NON-FIRST DETAIL RECORD                    
         EDIT  (P8,MYDUB),(16,PBIREC+95),2                                      
         CLC   PBIREC+107(2),=C' .'  IF LESS THAN $1.00 ADD A ZERO              
         BNE   *+8                                                              
         MVI   PBIREC+107,C'0'                                                  
*                                                                               
*                                                                               
INAGN0   AP    INHCNT,=P'1'                                                     
         AP    INLCNT,=P'1'                                                     
         B     INAGN5                                                           
*                                                                               
*                                                                               
INAGN1   ZAP   MYDUB,28(8,R3)        FIRST DETAIL RECORD                        
*                                                                               
         MVI   PBIREC,C'2'                                                      
         MVC   PBIREC+1(2),=C'40'                                               
         CP    28(8,R3),=P'0'                                                   
         BNL   *+10                                                             
         MVC   PBIREC+1(2),=C'50'    CREDIT MEMO                                
         MVC   PBIREC+3(10),159(R3)       GL ACCOUNT  PRD USER 1                
         EDIT  (P8,MYDUB),(16,PBIREC+13),2                                      
*                                                                               
         CLC   PBIREC+25(2),=C' .'  IF LESS THAN $1.00 ADD A ZERO               
         BNE   *+8                                                              
         MVI   PBIREC+25,C'0'                                                   
*                                                                               
         MVC   PBIREC+29(2),=C'I7'      TAX CODE?                               
         CLI   OMDUSASW,C'Y'       OMDUSA?                                      
         BNE   *+10                                                             
         MVC   PBIREC+29(2),SPACES      TAX CODE?                               
*                                                                               
         MVC   PBIREC+46(12),197(R3)      JOB NUMBER EST USER 1                 
         MVC   PBIREC+58(10),181(R3)      COST CENTER  PRD USER 2               
*                                                                               
         AP    INLCNT,=P'1'                                                     
         B     INAGN5                                                           
*                                                                               
*                                                                               
INAGN2   ZAP   MYDUB,28(8,R3)        NON-FIRST DETAIL RECORD                    
*                                                                               
         MVI   PBIREC,C'2'                                                      
         MVC   PBIREC+1(2),=C'40'                                               
         MVC   PBIREC+3(10),159(R3)       GL ACCOUNT  PRD USER 1                
         CP    28(8,R3),=P'0'                                                   
         BNL   *+10                                                             
         MVC   PBIREC+1(2),=C'50'    CREDIT MEMO                                
         EDIT  (P8,MYDUB),(16,PBIREC+13),2                                      
*                                                                               
         CLC   PBIREC+25(2),=C' .'  IF LESS THAN $1.00 ADD A ZERO               
         BNE   *+8                                                              
         MVI   PBIREC+25,C'0'                                                   
*                                                                               
         MVC   PBIREC+29(2),=C'I7'      TAX CODE?                               
         CLI   OMDUSASW,C'Y'       OMDUSA?                                      
         BNE   *+10                                                             
         MVC   PBIREC+29(2),SPACES                                              
*                                                                               
         MVC   PBIREC+46(12),197(R3)      JOB NUMBER EST USER 1                 
         MVC   PBIREC+58(10),181(R3)      COST CENTER  PRD USER 2               
*                                                                               
         AP    INLCNT,=P'1'                                                     
         B     INAGN5                                                           
*                                                                               
*                                                                               
INAGN5   DS    0H                                                               
         LA    R4,PBIREC                                                        
         CLI   SVQOPT7,C'Y'          TEST RUN - NO RECORDS TO FILE              
         BE    INAGN7                                                           
*                                                                               
         L     R1,=A(INPBIT1)                                                   
         PUT   (1),(4)                                                          
*                                                                               
INAGN7   DS    0H                                                               
         AP    INCOUNT,=P'1'                                                    
         CLI   SVQOPT7,C'Y'          DISPLAY FIRST 100 RECORDS                  
         BNE   INAGNX                                                           
         CLI   PBIREC,C'T'           ALWAYS SHOW TRAILER                        
         BE    INAGN8                                                           
         CP    INCOUNT,=P'75'                                                   
         BH    INAGNX                                                           
*                                                                               
INAGN8   MVC   P(13),=C'***OUTPUT HEX'                                          
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,0(R4),P,60,0                                         
         GOTO1 REPORT                                                           
         LA    R4,60(R4)                                                        
         GOTO1 HEXOUT,DMCB,0(R4),P,52,0                                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
INAGN9   MVC   P(14),=C'***OUTPUT CHAR'                                         
         GOTO1 REPORT                                                           
         LA    R4,PBIREC        RESET TO START OF REC                           
         MVC   P(112),0(R4)                                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
INAGNX   DS    0H                                                               
         CLI   INRECSW,C'A'  DID I JUST DO THE FILE HEADER                      
         BNE   INAGNX2       GO DO THE DETAILS                                  
         MVI   INRECSW,0     CLEAR                                              
         MVC   PBIREC(112),SPACES  CLEAR PBIREC                                 
         B     INPRL8        GO DO THE H RECORD                                 
*                                                                               
INAGNX2  DS    0H                                                               
         CLI   INRECSW,C'H'  DID I JUST DO THE INVOICE HEADER                   
         BNE   INAGNX3       GO DO THE DETAILS                                  
         MVI   INRECSW,0     CLEAR                                              
         MVC   PBIREC(112),SPACES  CLEAR PBIREC                                 
         B     INAGN1        GO DO FIRST DETAIL RECORD                          
*                            SAME BINSEARCH RECORD AS INV. HEADER               
*                                                                               
INAGNX3  CLI   INRECSW,C'F'          DID I JUST DO THE TRAILER                  
         BE    INZERO        DONE                                               
*                                                                               
INAGNXX  LA    R3,INRECLEN(R3)     TO NEXT RECORD IN TABLE                      
*                                                                               
         LA    RE,PBIREC           CLEAR PBIREC                                 
         LH    R1,=H'600'                                                       
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         BCT   R2,INAGN                                                         
*                                                                               
*        LAST  PUT OUT TRAILER RECORD                                           
*                                                                               
         LA    RE,PBIREC           CLEAR PBIREC                                 
         LH    R1,=H'600'                                                       
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         AP    INLCNT,=P'1'        ADD ONE FOR TRAILER                          
         MVC   PBIREC(4),=C'TRLR'                                               
         MVC   PBIREC+4(8),=C'ZFI04010'   JOB NAME?                             
         CLI   OMDUSASW,C'Y'      OMDUSA?                                       
         BNE   *+10                                                             
         MVC   PBIREC+4(8),=C'ZFI02125'       JOB NAME                          
         MVC   PBIREC+12(10),MCTODAY      MM/DD/YYYY                            
         MVC   PBIREC+22(4),TIMEOFDF    HHMM  (NO SECONDS HERE)                 
         EDIT  (P3,INLCNT),(8,PBIREC+26),0,FILL=0                               
         LA    R3,PBIREC                                                        
         MVI   INRECSW,C'F'                                                     
         B     INAGN5                                                           
*                                                                               
INZERO   DS    0H                                                               
*                                                                               
***      MVC   ININVALS,=A(INMED)                                               
         MVC   ININVALS,=A(INKEY)                                               
         MVI   ININVALS,1                                                       
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
INRECSW  DS    CL1                                                              
INHCNT   DS    PL3                INVOICE HEADER COUNT                          
INLCNT   DS    PL3                INVOICE DETAIL COUNT                          
INCOUNT  DS    PL3                RECORD COUNT                                  
*                                                                               
         EJECT                                                                  
*                                                                               
INPBIT1  DCB   DDNAME=PINTAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00112,                                            X        
               BLKSIZE=00112,                                          X        
               MACRF=PM                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
ININVALS DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
***      DC    AL3(INMED)         RECORD TO BE ADDED                            
         DC    AL3(INKEY)         RECORD TO BE ADDED                            
         DC    A(INTABLE)         ADDRESS OF TABLE WHERE REC IS TO BE           
INRECNT  DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    AL4(INRECLEN)      LEN OF RECORD                                 
         DC    AL4(INKEYLEN)      KEY SIZE                                      
         DC    F'1200'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFINT   DC    A(INTABLE)                                                       
*                                                                               
INKEY    DS    0XL16                                                            
INCRDT   DS    XL1           CREDIT = 2 DEBIT = 1                               
INMED    DS    CL1                                                              
INCLI    DS    CL3                                                              
INPRO    DS    CL3                                                              
INEST    DS    XL2                                                              
ININUMB  DS    0XL3                                                             
ININVMO  DS    XL1           INVOICE MONTH                                      
ININVN   DS    XL2           INVOICE NUMBER                                     
INTYPE   DS    XL1           1=HEADER,2=DETAIL                                  
INMOS    DS    XL2           ONLY PRESENT FOR TYPE 2                            
*            ______                                                             
*              16                                                               
INKEYLEN EQU   *-INKEY                                                          
*            ______                                                             
INREC    DS    CL375                                                            
INRECLEN EQU   *-INKEY                                                          
ENDINR   DS    0C                                                               
*                                                                               
***********************                                                         
*                                                                               
NEXTELM  NTR1  BASE=*,LABEL=*                                                   
NEXTEL   ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELMX                                                         
         CLC   0(1,R2),MELCODE                                                  
         BE    *+10                                                             
         B     NEXTEL                                                           
NEXTELMX LTR   RE,RE       NOT FOUND -                                          
*                                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
         DROP  R7                                                               
         SPACE 2                                                                
         EJECT                                                                  
         TITLE 'DSECTS AND WORK AREAS'                                          
PPINWRKD DSECT                                                                  
PPINWRK  DS    0C                                                               
AOUTP    DS    A                                                                
VOFFICER DS    A                                                                
AFMTAMTS DS    A                                                                
ADMASTC  DS    A           VMASTC                                               
*                                                                               
LOWINV   DS    H           START INVOICE NUMBER                                 
HIINV    DS    H           END INVOICE NUMBER                                   
*                                                                               
START    DS    CL6                                                              
END      DS    CL6                                                              
ZEROS    DS    CL30                                                             
DYNSW    DS    CL1                                                              
OMDUSASW DS    CL1          Y= OMDUSA, N=OMD CANADA                             
         DS    0D                                                               
DGCOUNT  DS    PL8                                                              
DGTOTAL  DS    PL8                                                              
*                                                                               
MYGST    DS    PL8                                                              
MYPST    DS    PL8                                                              
MYHST    DS    PL8                                                              
MYDUB    DS    PL8                                                              
BIGDUB   DS    PL12                                                             
*                                                                               
FIRSTSW  DS    X                                                                
SAVCOFF  DS    CL2         OFFICE CODE (FROM OFFICER CALL)                      
REVSW    DS    CL1                 REVISION STATUS                              
RETAIL   DS    CL1                 'Y' IF RETAIL BILL                           
OACTSW   DS    CL1         SET TO 1 IF BILL FOR OFFICE PROCESSED                
RACTSW   DS    CL1         SET TO 1 IF BILL FOR REQUEST PROCESSED               
LSTBLKY  DS    XL25                                                             
         DS    0F                                                               
EATOTS   DS    XL16                                                             
TAPTYP   DS    C                                                                
LASTKEY  DS    0CL32                                                            
         DS    CL4                                                              
LASTKCLT DS    CL3                                                              
LASTKPRD DS    CL3                                                              
         DS    CL22                NOT SPARE - REST OF LASTKEY                  
*                                                                               
TESTMQ   DS    C                                                                
INSFTP   DS    C                   NISSAN SFTP                                  
*                                  FOR MINDSHARE BURGER KING MQ MSG             
BKOPENSW DS    C                   Y=BURGER KING FILE OPEN                      
*                                  NOTE - ALSO SET FOR SC JOHNSON               
*                                  AND MAZDA SFTP FILES                         
*                                                                               
BKFISCAL DS    CL4                 FISCAL YEAR                                  
BKBYR    DS    CL4                 BILLING MOS - YEAR                           
BKBMTH   DS    CL2                 BILLING MOS - MONTH                          
*                                                                               
CTODAY   DS    CL8                 YYYYMMDD                                     
MCTODAY  DS    CL10                MM/DD/YYYY                                   
TIMEOFD  DS    CL8                 HH.MM.SS                                     
TIMEOFDF DS    CL6                 HHMMSS                                       
*                                                                               
MTODAYB  DS    XL3                 YMD - TODAY BINARY                           
ELEM     DS    CL200                                                            
*                                                                               
MQMAPNM  DS    CL14                SFTPDISK.PROD.                               
*                                                                               
DSNAME   DS    CL35  DSN -  BIL.SYS.AGID.DYYYMMDD.THHMMSS                       
*                    FOR MINDSHARE BURGER KING AGID = H7PK                      
*                                                                               
AGYSW    DS    XL1                                                              
AGYOO    EQU   X'80'                                                            
AGYOU    EQU   X'40'                                                            
*                                                                               
         DS    CL21                                                             
*                                                                               
LASTPROF DS    XL16                                                             
*                                                                               
B1PROF   DS    XL16                                                             
B1XPROF  DS    XL16                                                             
DINVMED  DS    CL2                  INVOICE MEDIA PART                          
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                 FULL INVOICE NUMBER                         
*                                   ME-MN-NNNN                                  
*                               OR  MN-ME-NNNN                                  
DSINVNO  DS    CL7                  MN-NNNN (OR YMMNNNN)                        
*                                                                               
MCMRECS  DS    PL2'0'                                                           
MCCRECS  DS    PL2'0'                                                           
OORECS   DS    PL2'0'                                                           
*                                                                               
BKCOM    DS    PL6                                                              
BKCOMACC DS    CL10                BK COMMISSION ACCOUNT                        
BKACCT   DS    CL10                BURGER KING ACCOUNT                          
BKCOSTC  DS    CL10                BURGER KING COST CENTER                      
BKMAR#   DS    CL23                BURGER KING MAR NUMBER                       
*                                                                               
FISCAL   DS    CL4                 BK FISCAL YEAR                               
*                                                                               
BILTOTS  DS    7PL6                                                             
PRDTOTS  DS    7PL6                                                             
CLTTOTS  DS    7PL6                                                             
OFFTOTS  DS    7PL6                                                             
REQTOTS  DS    7PL6                                                             
RUNTOTS  DS    7PL6                                                             
TRUNTOTS DS    7PL6                FOR TAPE REQS                                
*                                                                               
*                                                                               
INVPARS  DS    6F               FOR INVTAB BINSRCH                              
*                                                                               
INVMAX   EQU   10000            ALLOW 10000 INVOICES PER CLIENT                 
MYFULL   DS    F                                                                
SAVER1   DS    F                                                                
*                                                                               
CLTZINV  DS    F                ZERO INVOICE TOTALS                             
CLTINVS  DS    F                INVOICE TOTALS                                  
OFFINVS  DS    F                                                                
REQINVS  DS    F                                                                
RUNINVS  DS    F                                                                
TRUNINVS DS    F                                                                
*                                                                               
STAGSW   DS    X                                                                
TOTSW    DS    X                                                                
SAVMAX   DS    X                                                                
SVCD     DS    PL8                                                              
*                                                                               
SVQOPT7  DS    CL1                                                              
NEWCLT   DS    CL1                                                              
MELCODE  DS    CL1                                                              
SH9PRD   DS    CL3                                                              
SVMID    DS    CL132                                                            
SVMID2   DS    CL132                                                            
*                                                                               
SVEDWARD DS    CL60     SAVED $ AMTS FOR EDWARD JONES TRAILER RECORD            
*                       (OOPROCE)                                               
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
*                                                                               
       ++INCLUDE PPBVALD                                                        
*                                                                               
PBIREC   DS    CL256                                                            
         DS    CL234            TO ALLOW FOR UP TO 500 BYTES                    
*                               FOR BIG RECORDS                                 
         DS    CL1000           FOR EVEN BIGGER RECORDS                         
*                                                                               
WID      DSECT                                                                  
WIOFFICE DS    CL1           1                                                  
WISYSTEM DS    CL1           2                                                  
WIMEDIA  DS    CL1           3                                                  
WICLICOD DS    CL3           4                                                  
WICLINAM DS    CL20          7                                                  
         DS    CL10                                                             
WIPRDCOD DS    CL3           37                                                 
WIESTNUM DS    CL3           40                                                 
         DS    CL3                                                              
WIESTNAM DS    CL20          46                                                 
         DS    CL15                                                             
WIINVNUM DS    CL6           81                                                 
         DS    CL4                                                              
WIINVDAT DS    CL6           91                                                 
WIDUEDAT DS    CL6           97                                                 
WIMONSER DS    CL4                                                              
WIACTBAM DS    CL11                                                             
WICASH   DS    CL11                                                             
WITAX    DS    CL11                                                             
WIGST    DS    CL11                                                             
WIPST    DS    CL11                                                             
WIAGENCY DS    CL2                                                              
WINET    DS    CL11                                                             
WISUBMED DS    CL1                                                              
WIPW#ACT DS    CL11                                                             
WIPW#NET DS    CL11                                                             
WIPW#TAX DS    CL11                                                             
WIPWBILL DS    CL1                                                              
WIAORNAM DS    CL30                                                             
WIPRDGCD DS    CL4                                                              
WIPRDGNM DS    CL24                                                             
WIUDEFP1 DS    CL32                                                             
WIUDEFP2 DS    CL16                                                             
WIUDEFE1 DS    CL32                                                             
WIUDEFE2 DS    CL16                                                             
WIBILLTP DS    CL3                                                              
         DS    CL1                                                              
WICLTNUM DS    CL6                                                              
         DS    CL2                                                              
WIARDAT  DS    CL4                                                              
WIGROSS  DS    CL11                                                             
WICLIPRD DS    CL5                                                              
WIPRDDES DS    CL20                                                             
WIAGYCOM DS    CL11                                                             
WIRETAIL DS    CL12                                                             
*                                                                               
BILLINED DSECT                                                                  
BLINE    DS    0C                                                               
BLPRD    DS    CL3                                                              
         DS    CL1                                                              
BLEST    DS    CL3                                                              
         DS    CL1                                                              
BLPER    DS    CL6                                                              
         DS    CL1                                                              
BLINO    DS    CL6                                                              
         DS    CL1                                                              
BLRDAT   DS    CL8                                                              
         DS    CL1                                                              
BLBDAT   DS    CL5                                                              
         DS    CL1                                                              
BLTYP    DS    CL6                                                              
BLGRS    DS    CL15                                                             
BLNET    DS    CL15                                                             
BLCD     DS    CL15                                                             
BLAC     DS    CL15                                                             
BLBIL    DS    CL15                                                             
BLGST    DS    CL13                                                             
         SPACE 3                                                                
BLESTD   EQU   BLEST-BLINE                                                      
BLPERD   EQU   BLPER-BLINE                                                      
BLRDATD  EQU   BLRDAT-BLINE                                                     
*                                                                               
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM                                       
MQAGYID  DS    CL4                 AGENCY 1D 4-CHAR                             
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 YYMMDD OF DSN                                
MQTIME   DS    CL6                 HHMMSS OF DSN                                
MQDATA1  DS    CL32                NOT USED                                     
MQDATA2  DS    CL32                NOT USED                                     
MQFILE   DS    CL64                DSN  (MINUS SFTPDISK.PROD.)                  
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPREPWORK                                                      
*                                                                               
*                                                                               
* START AND END INVOIVE NUMBERS COL 53(4), AND COL 57 (4)                       
*                                                                               
QINVNO1  EQU   QRECORD+52                                                       
QINVNO2  EQU   QRECORD+56                                                       
       ++INCLUDE PPREPWORK2                                                     
*                                                                               
       ++INCLUDE PPNEWFILE         HAVE NEW PBILLREC DSECT                      
*                                                                               
BPLEQ    EQU   6                   LENGTH OF PBILLREC PACKS (PL6)               
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PAORREC                                                        
*                                                                               
       ++INCLUDE DDUCOMD                                                        
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
*                                                                               
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
*                                                                               
INVTAB   CSECT                                                                  
         ORG   *+(INVMAX*4)                                                     
         DC    X'00'                                                            
*                                                                               
INTABLE  CSECT                                                                  
         DS    1200CL(INRECLEN)                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREPIN02 01/07/19'                                      
         END                                                                    
