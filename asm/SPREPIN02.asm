*          DATA SET SPREPIN02  AT LEVEL 013 AS OF 01/07/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE SPIN02A                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE DDUCOM                                                                 
*INCLUDE NETCOM                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE MQRPT                                                                  
         TITLE 'SPIN02 - SPOTPAK BILLING INTERFACE TAPES'                       
         SPACE 1                                                                
*                                                                               
***********************************************************************         
*  QOPT1 -   N = NO TAPE                                                        
*  QOPT2 -   TAPE SPEC CODE                                                     
*  QOPT3 -   Y = PRINT TAPE RECORD TRACE (ALSO NO MQ NOTIFICATION)              
*            T = MQ NOTIFICATION TO TEST  (DDS TESTING ONLY)                    
*            N = NO MQ NOTIFICATION (DDS TESTING ONLY)                          
*                (ALSO PUT TEST IN FILE NAME)                                   
*  QOPT4 -   M = START-END DATES ARE MOS                                        
*  QOPT5 -   C = DO ONLY COMMISSION ONLY BILLS, AND PRESERVE NET                
*            A = AOR ONLY, B=AOR AND AOR/CLIENT, X = NON-AOR ONLY               
*            2 = USE COS2 BILLS, N = NON-COMMISSION-ONLY BILLS                  
*            S = SOON BILLS ONLY                                                
*  QOPT5+1 - * = SHOW NETPAK SUB-MED INSTEAD OF CLIENT NUMBER                   
*            X = FILTER ON NETPAK SUB-MED=X                                     
*                                                                               
*                                                                               
*  BELOW FIELDS IN QAREA2 - Q2USER                                              
*                                                                               
* QINVNO1      COL 21(4) START INVOICE NUMBER                                   
* QINVNO2      COL 28(4) END INVOICE NUMBER                                     
*                                                                               
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-28806  01/07/19 CHANGE SORT ORDER SO CREDITS COMES FIRST  *         
* AKAT ITMF-19301  09/13/17 INCREASE INPAR6 FROM 1000 TO 1500         *         
* SMUR SPEC-7743/8563 2/28/17 REMOVE $0 INVOICES FROM EDI AND T/A REP *         
* AKAT ITMF-4581   02/10/16 HANDLE DUPLICATE BILL RECORDS             *         
***********************************************************************         
         PRINT NOGEN                                                            
SPIN02   CSECT                                                                  
         NMOD1 0,SPIN02,R6,R8                                                   
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SPINWRKD,RC                                                      
*                                                                               
APLNP    EQU   7846                APL/WESTERN IDS- WESTERN MODE                
APLND    EQU   7847                               - WESTERN MODE                
APLNP1   EQU   7991                               - APL MODE                    
APLND1   EQU   7990                               - APL MODE                    
WIAPLP1  EQU   7996                               - QA MODE                     
WIAPLD2  EQU   7997                               - QA MODE                     
WAPLBT   EQU   8060                               - ?SUPPLANTS QA?              
*                                                                               
         SPACE 2                                                                
         CLI   MODE,PROCBILL                                                    
         BE    PRBL                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,OFCFRST                                                     
         BE    FBILO                                                            
         CLI   MODE,CLTFRST                                                     
         BE    FBILC                                                            
         CLI   MODE,PRDFRST                                                     
         BE    FBILP                                                            
         CLI   MODE,PRDLAST                                                     
         BE    LBILP                                                            
         CLI   MODE,CLTLAST                                                     
         BE    LBILC                                                            
         CLI   MODE,OFCLAST                                                     
         BE    LBILO                                                            
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        RUN FIRST                                                              
         SPACE 2                                                                
RUNF     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,CTODAY) TODAY - YYYYMMDD                   
*                                                                               
         MVC   MCTODAY(2),CTODAY+4        MTH                                   
         MVI   MCTODAY+2,C'/'                                                   
         MVC   MCTODAY+3(2),CTODAY+6      DAY                                   
         MVI   MCTODAY+5,C'/'                                                   
         MVC   MCTODAY+6(4),CTODAY        YYYY                                  
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
         CVB   R4,DUB                                                           
         EDIT  (R4),(5,TIMEOFD),2,FILL=0                                        
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
         XC    RUNINVS,RUNINVS     CLEAR RUN INVOICE TOTALS                     
         MVI   BKSFTP,C'N'         SET OFF DOING BURGER SFTP                    
*                                                                               
         MVI   G7SW,0              WILL BE SET TO 1 FOR GSTX REQ                
         MVI   SCSW,0              WILL BE SET TO 1 FOR H7 TYPE S               
         MVI   INSW,0            WILL BE SET TO 1 FOR H7 TYPE N NISSAN          
         XC    SCLMOS,SCLMOS       CLEAR MOS DATEES FOR HEADER                  
         XC    SCHMOS,SCHMOS                                                    
         MVI   MCCSW,0             WILL BE SET TO 1 FOR MC TYPE C               
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   VSPFMTIN,VSPFMINO   A(SPFMTINO)                                  
         MVC   MVOFFICE,VOFFICER  A(OFFICER)                                    
         DROP  RF                                                               
*                                                                               
         MVI   OPENSW,C'N'                                                      
         LA    R3,RAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
*        SET ININVALS (FOR NISSAN)                                              
*                                                                               
         L     R1,=A(INTABLE)     STORE SOME ADDRESSES FIRST                    
         ST    R1,AOFINT                                                        
         LA    R1,INKEY                                                         
         ST    R1,AINKEY                                                        
         MVC   INPAR1,AINKEY                                                    
         MVI   INPAR1,X'01'      SET TO ADD RECORD                              
         MVC   INPAR2,AOFINT                                                    
         XC    INRECNT,INRECNT   ZERO RECORD COUNTER                            
***      MVC   INPAR4,=F'390'    LENGTH OF RECORD                               
         MVC   INPAR4,=AL4(INRECLEN)    LENGTH OF RECORD                        
***      MVC   INPAR5,=F'15'     LENGTH OF KEY                                  
         MVC   INPAR5,=AL4(INKEYLEN)    LENGTH OF KEY                           
         MVC   INPAR6,=F'1500'   MAX NUMBER OF RECORDS                          
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        REQUEST FIRST                                                          
         SPACE 2                                                                
REQF     DS    0H                                                               
*                                                                               
         XC    LOWINV,LOWINV     CLEAR LOW INVOICE NUMBER FILTER                
         MVC   HIINV,=X'FFFF'    SET HIGH INVOICE NUMBER FILTER TO MAX          
         CLC   QINVNO1,SPACES                                                   
         BE    REQF0A                                                           
         PACK  DUB,QINVNO1                                                      
         CVB   R0,DUB                                                           
         STH   R0,LOWINV                                                        
*                                                                               
REQF0A   DS    0H                                                               
         CLC   QINVNO2,SPACES                                                   
         BE    REQF0B                                                           
         PACK  DUB,QINVNO2                                                      
         CVB   R0,DUB                                                           
         STH   R0,HIINV                                                         
*                                                                               
REQF0B   DS    0H                                                               
         B     REQF1J                                                           
*************************************************************                   
***** CODE BELOW FOR SFTP STYLE OUTPUT                                          
***** CHANGED TO PRTTPAE STYLE                                                  
*************************************************************                   
         MVI   TESTMQ,C'P'         SET TO PROD MQ NOTIFICATION                  
         CLI   QOPT3,C'N'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT3,C'Y'          TRACING OUTPUT RECORDS                       
         BNE   *+8                 ALSO SUPPRESS MQ                             
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT3,C'T'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'T'         TEST MQ                                      
*                                                                               
         MVI   WIQASW,C'N'         QA MODE                                      
         MVI   WIAPLSW,C'N'        SET WESTERN/APL WESTERN MODE                 
         MVI   WAPLBTSW,C'N'       WA MODE                                      
*                                                                               
         CLC   RCORIGID,=Y(WAPLBT) SPECIAL WA ID                                
         BNE   *+8                                                              
         MVI   WAPLBTSW,C'Y'                                                    
*                                                                               
         CLI   NETPAKSW,C'Y'       ON ONLY FOR NETPAK                           
         BNE   REQF1H              BUT QA AND WA STUFF ONLY FOR SPOT            
*                                                                               
         MVI   WIAPLSW,C'W'                                                     
         CLC   RCORIGID,=Y(APLNP)                                               
         BE    REQF1J                                                           
         CLC   RCORIGID,=Y(APLND)                                               
         BE    REQF1J                                                           
         MVI   WIAPLSW,C'A'        SET WESTERN/APL APL MODE                     
         CLC   RCORIGID,=Y(APLNP1)                                              
         BE    REQF1J                                                           
         CLC   RCORIGID,=Y(APLND1)                                              
         BE    REQF1J                                                           
         MVI   WIAPLSW,C'N'                                                     
         B     REQF1J                                                           
*                                  SPOT, NOT NET                                
REQF1H   DS    0H                                                               
         MVI   WIQASW,C'Y'                                                      
         CLC   RCORIGID,=Y(WIAPLP1)   2 SPECIAL QA ID'S                         
         BE    REQF1J                                                           
         CLC   RCORIGID,=Y(WIAPLD2)                                             
         BE    REQF1J                                                           
         MVI   WIQASW,C'N'                                                      
*                                                                               
**************************************************************                  
****  CODE ABOVE FOR SPTF STYLE OUTPUT                                          
****  FOR PRTTAPE STYLE DYNALLOC DONE EARLIER                                   
**************************************************************                  
REQF1J   DS    0H                                                               
*                                                                               
         MVI   WIQASW,C'N'         QA MODE                                      
         MVI   WIAPLSW,C'N'        SET WESTERN/APL WESTERN MODE                 
         MVI   WAPLBTSW,C'N'       WA MODE                                      
*                                                                               
         CLC   QAGY,=C'OU'      OMDTOA- TYPE N - NISSAN                         
         BNE   *+22                                                             
         CLI   QOPT2,C'N'                                                       
         BNE   *+14                                                             
         MVI   INSW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'OO'      OMDUSA- TYPE N - NISSAN                         
         BNE   *+26                                                             
         CLI   QOPT2,C'N'                                                       
         BNE   *+18                                                             
         MVI   INSW,1                                                           
         MVI   OMDUSASW,C'Y'                                                    
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+8                                                              
         MVI   NETPAKSW,C'N'                                                    
         DROP  RF                                                               
*                                  SET OFF VARIOUS WESTERN/APL SWITCHES         
*                                                                               
         XC    REQINVS,REQINVS   CLEAR REQUEST                                  
         XC    OFFINVS,OFFINVS   OFFICE                                         
         XC    CLTINVS,CLTINVS   AND CLIENT INVOICE TOTALS                      
         XC    CLTZINV,CLTZINV   CLIENT ZERO INVOICE TOTALS                     
*                                                                               
         L     RF,ADAGY                                                         
         MVC   CNTRY,AGYPROF+7-AGYHDR(RF)   SET COUNTRY                         
         MVI   RCSUBPRG,0                                                       
         CLI   CNTRY,C'C'          CANADA GETS DIFFERENT SPROG                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,50         FOR GST                                      
         L     RF,=A(HHROUT)                                                    
         ST    RF,HEADHOOK                                                      
         L     RF,=A(TPFMT)                                                     
         ST    RF,ATPFMT                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVQST,QSTART        SAVE QSTART AND END                          
         CLC   QEND,SPACES         ANY END DATE PROVIDED?                       
         BNE   *+10                                                             
         MVC   QEND,QSTART         NO -- USE QSTART (SINGLE DATE RQST)          
         MVC   SVQEND,QEND                                                      
*                                                                               
*Y2K*                                                                           
         MVC   QSTART,=C'700101'   SET LONG RANGE START                         
         MVI   QEND,X'FF'            AND END FOR SPONSOR                        
         MVC   QEND+1(5),=C'91231'                                              
         MVC   DUB,SVQST                                                        
         OC    DUB(6),=6C'0'                                                    
         GOTO1 DATCON,DMCB,DUB,(3,BQSTART)                                      
         MVC   DUB,SVQEND                                                       
         OC    DUB(6),=6C'0'                                                    
         GOTO1 DATCON,DMCB,DUB,(3,BQEND)                                        
*                                 SET VALUES FOR RUN-DATE FILTERING             
         GOTO1 DATCON,DMCB,TODAY,(2,TODAYC)      COMPRESSED                     
*                                                                               
         GOTO1 DATCON,DMCB,TODAY,(3,TODAYB)                                     
         ZIC   R1,TODAYB                                                        
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         STC   R1,DECADE           1980,1990,2000,2010, ETC.                    
*                                  HEX 50,5A,64,6E...                           
         MVC   YEARDIG,TODAY+1     GET YEAR WITHIN DECADE                       
         NI    YEARDIG,X'FF'-X'F0' ISOLATE YEAR DIGIT                           
*                                                                               
         XC    STARTMOS,STARTMOS   SET START MOS TO LOWEST POSSIBLE             
         MVC   ENDMOS,=X'FFFF'     SET END MOS TO HIGHEST POSSIBLE              
         CLC   QMOSSTRT,SPACES                                                  
         BE    REQF1N              NO START MOS FILTER                          
         MVC   DUB(4),QMOSSTRT                                                  
         MVC   DUB+4(2),=C'01'     FOR DATCON (COMPLETE DATE REQUIRED)          
         GOTO1 DATCON,DMCB,DUB,(3,THREE)                                        
         MVC   STARTMOS,THREE                                                   
         CLC   QMOSEND,SPACES                                                   
         BE    REQF1N              NO END MOS FILTER                            
         MVC   DUB(4),QMOSEND                                                   
         MVC   DUB+4(2),=C'01'     FOR DATCON (COMPLETE DATE REQUIRED)          
         GOTO1 DATCON,DMCB,DUB,(3,THREE)                                        
         MVC   ENDMOS,THREE                                                     
*                                                                               
REQF1N   DS    0H                                                               
         XC    ASPECS,ASPECS                                                    
         CLI   QOPT1,C'N'          SKIP TAPE                                    
         BE    REQF16                                                           
*                                                                               
*                                                                               
         L     R4,=A(AGYTAB)       ELSE GET TAPE SPEC FROM AGYTAB               
         USING AGYTABD,R4                                                       
*                                                                               
REQF2    DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    REQF10                                                           
         CLC   QAGY,AGYTAGY        AGY (2)                                      
         BNE   REQF4                                                            
         CLC   QOPT2,AGYTAPE       MUST MATCH REQ TYPE                          
         BNE   REQF4               NOTE-BLANK IS NORMAL                         
         CLI   AGYTMED,C'Z'        ALL MEDIA                                    
         BE    *+14                                                             
         CLC   QMED,AGYTMED        ONE MEDIA                                    
         BNE   REQF4                                                            
         CLC   AGYTCLT,SPACES      ALL CLIENTS?                                 
         BE    REQF12                                                           
         CLC   QCLT,AGYTCLT        ONE CLIENT                                   
         BE    REQF12                                                           
*                                                                               
REQF4    DS    0H                                                               
         LA    R4,AGYTABL(R4)                                                   
         B     REQF2                                                            
*                                                                               
REQF10   DS    0H                                                               
         MVC   P(38),=C'**NO TAPE SPECS FOUND - REQ BYPASSED**'                 
         BRAS  RE,PRNT                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
REQF12   DS    0H                                                               
         B     REQ13A                                                           
************************************************                                
********* SKIP CODE BELOW                                                       
         MVC   RECLEN,AGYTLRCL     SAVE RECORD LENGTH                           
         MVC   CONTROLS,AGYTCTRL   SAVE CONTROLS                                
         L     R5,=A(INSBIT1)                                                   
         CLI   OPENSW,C'N'                                                      
         BNE   REQF13                                                           
         USING IHADCB,R5                                                        
         OI    DCBRECFM,DCBRECBR   BLOCKED RECORDS                              
         CLI   AGYRECFM,C'F'       'F' = FIXED, 'V' = VARIABLE                  
         BNE   *+12                                                             
         OI    DCBRECFM,DCBRECF    FIXED BLOCK                                  
         B     *+8                                                              
         OI    DCBRECFM,DCBRECV    VARIABLE BLOCK                               
         MVC   DCBBLKSI,AGYTBLKS   BLOCK SIZE                                   
         MVC   DCBLRECL,AGYTLRCL   RECORD SIZE                                  
*                                                                               
REQF13   DS    0H                                                               
         CLC   DCBBLKSI,AGYTBLKS   BLOCK SIZE                                   
         BNE   REQF13X                                                          
         CLC   DCBLRECL,AGYTLRCL   RECORD SIZE                                  
         BNE   REQF13X                                                          
         DROP  R5                                                               
************************************                                            
REQ13A   CLI   OPENSW,C'N'                                                      
         BNE   REQF14                                                           
         MVI   OPENSW,C'Y'                                                      
         CLI   QOPT1,C'N'          TEST DOING TAPE                              
         BE    REQF14                                                           
         MVI   OPENSW,C'T'         SET TAPE REALLY OPEN                         
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         LA    R3,NEDYNDSN         NETPAK                                       
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+12                                                             
         MVI   NETPAKSW,C'N'                                                    
         LA    R3,SPDYNDSN         OR SPOT                                      
         DROP  RF                                                               
*                                                                               
         MVC   13(2,R3),QAGY                                                    
*                                                                               
         CLC   QAGY,=C'SJ'      SJR TESTING FOR SC JOHNSON                      
         BE    REQF13B6                                                         
         CLC   QAGY,=C'OO'     OMDUSEC  (USA)                                   
         BE    REQF13B6                                                         
         CLC   QAGY,=C'OU'     OMDTOA                                           
         BE    *+6                                                              
         DC    H'0'            INVALID AGENCY                                   
*                                                                               
REQF13B6 CLI   QOPT2,C'N'      FILE TYPE N - NISSAN                             
         BE    REQF13M                                                          
         DC    H'0'             MUST BE  N                                      
************************************************************                    
******  CODE BELOW FOR SFTP STYLE OUTPUT                                        
************************************************************                    
         MVI   INSFTP,C'Y'      DOING NISSAN SFTP                               
*                                                                               
REQF13B7 MVC   DSNAME,SPACES                                                    
         MVC   DSNAME+0(4),=C'BIL.'                                             
         MVC   DSNAME+4(3),0(R3)      SPT OR NET                                
         MVI   DSNAME+7,C'.'                                                    
*                                                                               
REQF13B8 L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         L     R1,MCAEXTRA                                                      
         MVC   DSNAME+8(4),MCAGYCOD-MCEXTRA(R1)                                 
         DROP  RF                                                               
*                                                                               
REQF13B9 MVC   DSNAME+12(2),=C'.D'                                              
         MVC   DSNAME+14(6),CTODAY+2    YYMMDD                                  
         MVC   DSNAME+20(2),=C'.T'                                              
         MVC   DSNAME+22(2),TIMEOFD         WITHOUT .'S                         
         MVC   DSNAME+24(2),TIMEOFD+3                                           
         MVC   DSNAME+26(2),TIMEOFD+6                                           
         MVC   MQMAPNM,=C'SFTPDISK.PROD.'                                       
         CLI   QOPT3,C'Y'   PDUMPING OUTPUT - SET TO TEST                       
         BE    REQF13D                                                          
         CLI   QOPT3,C'N'   NO MQ NOTIFICATION                                  
         BE    REQF13D      ALSO PUT 'TEST' IN MQMAPNM                          
         CLI   TESTMQ,C'T'  PUTTING TO TEST BROKER?                             
         BNE   *+10                                                             
REQF13D  MVC   MQMAPNM+9(4),=C'TEST'                                            
*                                                                               
         MVI   BYTE,X'45'         X'04' = BIG NAMES                             
         MVC   DUB,=X'000005000001'                                             
         GOTO1 DYNALLOC,DMCB,(X'80',=C'INSBIT1 '),(BYTE,DUB),          X        
               (X'80',MQMAPNM)                                                  
         OPEN  (INSBIT1,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     REQF14                                                           
**************************************************                              
****  CODE ABOVE FOR SFTP STYLE OUTPUT                                          
**************************************************                              
*                                                                               
REQF13M  DS    0H                                                               
********************                                                            
*                                                                               
         MVC   DYNDDN,=CL8'SINTAPE'                                             
         MVC   DYNDSN,=CL44'SPTTAPE.SP0INXX1'                                   
         MVC   DYNDSN+13(2),QAGY                                                
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         ICM   RE,15,MCSSB                                                      
         JZ    *+2                 MUST HAVE OFFLINE SSB!                       
         USING SSBD,RE                                                          
         CLI   SSODSPAC,C'C'                                                    
         BNE   *+14                                                             
         MVC   DYNDSN(7),=C'CSCTAPE'                                            
         B     REQF13P                                                          
         CLI   SSODSPAC,C'T'                                                    
         BNE   *+14                                                             
         MVC   DYNDSN(7),=C'TSTTAPE'                                            
         B     REQF13P                                                          
         CLI   SSODSPAC,C'Q'                                                    
         BNE   REQF13P                                                          
         MVC   DYNDSN(7),=C'FQATAPE'                                            
         DROP  RE                                                               
*                                                                               
REQF13P  DS    0H                                                               
         OC    MCREMPQK,MCREMPQK   SOON RUN?                                    
         BZ    REQF14                                                           
         DROP  RF                                                               
*                                                                               
         LA    RE,DYNDSN           YES: PREVENT DYNALLOC FAILURES               
         ENQ   (MAJORNAM,(RE),E,DSNLENQ,SYSTEM)                                 
         B     REQF14                                                           
*                                                                               
SPDYNDSN DC    CL(DSNLENQ)'SPTTAPE.SP0INAG1'                                    
NEDYNDSN DC    CL(DSNLENQ)'NETTAPE.NE0INAG1'                                    
DSNLENQ  EQU   44                                                               
MAJORNAM DC    C'SINTAPE '         MAJOR RESOURCE NAME FOR ENQ                  
*                                                                               
         DS    0H                                                               
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL(DSNLENQ)                                                      
*                                                                               
REQF13X  DS    0H                                                               
         L     RE,=A(MULTMSG)                                                   
         MVC   P(52),0(RE)                                                      
         BRAS  RE,PRNT                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
REQF14   DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,7,AGYTSPCS       A(SPECS)                                     
         ST    RF,ASPECS                                                        
         DROP  R4                                                               
*                                                                               
REQF16   DS    0H                                                               
         LA    R3,RQAMTS                                                        
         BAS   RE,CLRTOTS                                                       
         XC    LSTBLKY,LSTBLKY                                                  
*                                                                               
         ZAP   EATOTGRS,=P'0'                                                   
         ZAP   EATOTAC,=P'0'                                                    
         ZAP   EATOTACT,=P'0'                                                   
         ZAP   EATOTNET,=P'0'                                                   
*                                                                               
         MVI   OFFICE,0            SET NO OFFICE                                
*                                  CLEAR SAVED TAPE REC AREA                    
*                                                                               
*        NOTE - SVTPREC CAN'T BE USED FOR FILES WHOSE                           
*               LRECL IS OVER 600                                               
*                                                                               
         LA    RE,SVTPREC                                                       
         LHI   R1,L'SVTPREC-1                                                   
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         XC    SVBILL,SVBILL       AND SAVED BILL AREA                          
         LA    R3,SVBAMTS          AND SAVED BILL VALUES                        
         BAS   RE,CLRTOTS                                                       
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        FIRST FOR OFFICE                                                       
         SPACE 2                                                                
FBILO    DS    0H                                                               
*                                                                               
         XC    CLTINVS,CLTINVS     CLEAR CLIENT                                 
         XC    CLTZINV,CLTZINV     CLIENT ZERO                                  
         XC    OFFINVS,OFFINVS     AND OFFICE INVOICE TOTALS                    
*                                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,CLRTOTS                                                       
         L     RF,ADCLT            SAVE OFFICE CODE                             
         MVC   OFFICE,COFFICE-CLTHDR(RF)                                        
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 3                                                                
*        FIRST FOR CLI                                                          
         SPACE 2                                                                
FBILC    DS    0H                                                               
*        SET SAVCOFF HERE FOR ALL REQUESTS                                      
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         MVC   SAVCOFF(1),0(RF)      SAVE OFFICE FOR HEADLINES                  
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'N'       NETPAK                                    
         CLI   NETPAKSW,C'Y'                                                    
         BE    *+8                                                              
         MVI   OFFD.OFCSYS,C'S'       ELSE SPOT                                 
*                                                                               
         MVC   OFFD.OFCAGY,QAGY                                                 
         MVC   OFFD.OFCPMED,QMED                                                
         MVC   OFFD.OFCOFC,0(RF)     FROM CLIENT HEADER                         
*                                                                               
         GOTO1 MVOFFICE,DMCB,(C'2',WORK),(0,ACOMFACS)                           
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
*                                                                               
         CLC   =C'*BILLING',QUESTOR  IF GENERATED BY B1/BU                      
         BNE   FBILC2                                                           
         CLI   PROGPROF+2,C'Y'     DOES THIS CLIENT WANT BT?                    
         BNE   FBILC3              NO, SKIP IT                                  
*                                                                               
FBILC2   CLC   AGY,=C'PU'          FOR AMMARATTI/PURIS                          
         BNE   FBILC3D                                                          
         CLI   QOPT1,C'N'          IF DOING TAPE                                
         BE    FBILC3D                                                          
         CLC   CLT,=C'COM'         SKIP THESE CLIENTS                           
         BE    FBILC3                                                           
         CLC   CLT,=C'CDI'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'CMI'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'MIN'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'MLO'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'CDM'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'LCO'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'CCW'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'BAC'                                                      
         BE    FBILC3                                                           
         BNE   FBILC3D                                                          
*                                                                               
FBILC3   DS    0H                                                               
         MVI   MODE,CLTLAST        SKIP CLIENT                                  
         B     EXIT                                                             
*                                                                               
FBILC3D  DS    0H                                                               
         CLI   QOPT2,C'T'          IF IT IS THE TOYOTA SPECIAL                  
         BNE   FBILC3G                                                          
         CLC   AGY,=C'DF'          FOR SAATCHI                                  
         BE    FBILC3F                                                          
         CLC   AGY,=C'TH'          OR ZENITH                                    
         BNE   FBILC3G                                                          
*                                                                               
FBILC3F  DS    0H                                                               
         CLC   CLT,=C'TM '         DO ONLY THESE CLIENTS                        
         BE    FBILC3G                                                          
         CLC   CLT,=C'TMS'                                                      
         BE    FBILC3G                                                          
         CLC   CLT,=C'LEX'                                                      
         BE    FBILC3G                                                          
         CLC   CLT,=C'LDA'                                                      
         BE    FBILC3G                                                          
         CLC   CLT,=C'LDL'                                                      
         BE    FBILC3G                                                          
         CLC   CLT,=C'TOC'                                                      
         BE    FBILC3G                                                          
         B     FBILC3                                                           
*                                                                               
FBILC3G  DS    0H                                                               
         CLI   WIAPLSW,C'N'        FOR WESTERN/APL EITHER MODE                  
         BE    FBILC7                                                           
*                                                                               
         BAS   RE,WIAPLCT          DO ONLY WI/APL CLIENTS                       
         BE    FBILC7                                                           
         MVI   MODE,CLTLAST        SKIP THIS CLENT                              
         B     EXIT                                                             
*                                                                               
FBILC7   DS    0H                                                               
         CLI   WAPLBTSW,C'N'       FOR WESTERN/APL WA ID                        
         BE    FBILC7B                                                          
*                                                                               
         BAS   RE,WAPLBTCT         DO ONLY WI/APL CLIENTS                       
         BE    FBILC7B                                                          
         MVI   MODE,CLTLAST        SKIP THIS CLENT                              
         B     EXIT                                                             
*                                                                               
FBILC7B  DS    0H                                                               
         SR    R0,R0               SET INVOICE LIST BINSRCH PARS                
         L     R1,=A(INVTAB)                                                    
         SR    R2,R2                                                            
         LHI   R3,3                                                             
         LHI   R4,3                                                             
         L     R5,=A(INVMAX)                                                    
         STM   R0,R5,INVPARS                                                    
         XC    CLTINVS,CLTINVS                                                  
         XC    CLTZINV,CLTZINV                                                  
*                                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         XC    SV00APRF,SV00APRF   READ 00A PROFILE                             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'BF'          MAKE 'S' LOWERCASE                           
         MVC   WORK+4(2),AGY                                                    
         GOTO1 GETPROF,DMCB,WORK,SV00APRF,DATAMGR                               
*                                                                               
         XC    B1PROF,B1PROF       READ B1 PROFILE                              
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING PROFKD,RE                                                        
         MVI   PROFKSYS,C'S'                                                    
         MVC   PROFKPGM,=C'0B1'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLIENT                                                  
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C' '                                                       
         BNH   *+14                                                             
         MVI   PROFKOI2,C'*'                                                    
         MVC   PROFKOCD,0(RF)                                                   
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
*                                                                               
         XC    B1XPROF,B1XPROF     READ B1X PROFILE                             
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         MVI   PROFKSYS,C'S'-X'40' MAKE SYSTEM LOWER CASE                       
         MVC   PROFKPGM,=C'B1X'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLIENT                                                  
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C' '                                                       
         BNH   *+14                                                             
         MVI   PROFKOI2,C'*'                                                    
         MVC   PROFKOCD,0(RF)                                                   
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
         DROP  RE                                                               
*                                                                               
         CLI   PROGPROF+0,C'Y'     NEW PAGE PER CLIENT?                         
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                  SET CLIENT DATA IN SVMID                     
         MVI   NEWCLT,C'Y'                                                      
         MVC   SVMID,SPACES                                                     
         MVC   SVMID(7),=C'CLIENT='                                             
         MVC   SVMID+8(3),CLT                                                   
         MVC   SVMID+12(24),CLTNM                                               
*                                                                               
         LA    RF,SVMID+36                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         L     RE,ADCLT                                                         
         CLI   CCLTIFC-CLTHDR(RE),C' '                                          
         BNH   FBILC9                                                           
         MVI   2(RF),C'('                                                       
         MVC   3(8,RF),CCLTIFC-CLTHDR(RE)                                       
         LA    RF,11(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
*                                                                               
FBILC9   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
*        FIRST FOR PRODUCT                                                      
         SPACE 2                                                                
FBILP    DS    0H                                                               
         LA    R3,PAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         CLI   PROGPROF+1,C'Y'     NEW PAGE PER PRODUCT?                        
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        LAST FOR PRODUCT                                                       
         SPACE 2                                                                
LBILP    DS    0H                                                               
         LA    R3,PAMTS                                                         
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,PAMTS                                                         
         LA    R4,CAMTS                                                         
         BAS   RE,PBROLL                                                        
*                                                                               
         BRAS  RE,PRNT                                                          
         MVC   P+10(20),=C'** PRODUCT TOTALS **'                                
         LA    R3,PAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST  FOR CLIENT                                                       
         SPACE 2                                                                
LBILC    DS    0H                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,CAMTS                                                         
         LA    R4,OAMTS            ROLL TO OFFICE TOTS                          
         CLI   QCLT,C'$'           IF IN OFFICE LIST MODE                       
         BE    *+8                                                              
         LA    R4,RQAMTS           ELSE TO REQUEST TOTS                         
         BAS   RE,PBROLL                                                        
*                                                                               
         MVC   CLTINVS,INVPARS+8   INVOICE COUNT                                
*                                                                               
         L     R0,CLTINVS                                                       
         L     RE,CLTZINV          CLIENT ZERO INVOICE COUNT                    
         SR    R0,RE               ADJUST CLIENT INVOICE COUNT                  
         ST    R0,CLTINVS                                                       
*                                                                               
         L     R0,OFFINVS                                                       
         LA    RE,OFFINVS                                                       
         CLI   QCLT,C'$'           IF IN OFFICE LIST MODE                       
         BE    *+12                                                             
         L     R0,REQINVS                                                       
         LA    RE,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,0(RE)                                                         
*                                                                               
         MVI   ALLOWLIN,2                                                       
         MVC   P+10(19),=C'** CLIENT TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  CLTINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         XC    CLTINVS,CLTINVS                                                  
         XC    CLTZINV,CLTZINV                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST  FOR OFFICE                                                       
         SPACE 2                                                                
LBILO    DS    0H                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,OAMTS                                                         
         LA    R4,RQAMTS                                                        
         BAS   RE,PBROLL                                                        
*                                                                               
         L     R0,REQINVS                                                       
         A     R0,OFFINVS                                                       
         ST    R0,REQINVS                                                       
*                                                                               
         BRAS  RE,PRNT                                                          
         MVC   P+7(2),SAVCOFF                                                   
         MVC   P+10(19),=C'** OFFICE TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  OFFINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         XC    OFFINVS,OFFINVS                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST FOR REQ                                                           
REQL     DS    0H                                                               
         L     RF,ADBILL           CLEAR BILL RECORD                            
         XC    0(L'BILLREC,RF),0(RF)                                            
*                                                                               
         LA    R3,RQAMTS                                                        
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,RQAMTS                                                        
         LA    R4,RAMTS                                                         
         BAS   RE,PBROLL                                                        
*                                                                               
         L     R0,RUNINVS                                                       
         A     R0,REQINVS                                                       
         ST    R0,RUNINVS                                                       
*                                                                               
         MVC   P+10(20),=C'** REQUEST TOTALS **'                                
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  REQINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,RQAMTS                                                        
         BAS   RE,TOTPRNT                                                       
         XC    REQINVS,REQINVS                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST FOR RUN                                                           
         SPACE 2                                                                
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+10(19),=C'** REPORT TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  RUNINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,RAMTS                                                         
         BAS   RE,TOTPRNT                                                       
*                                                                               
         CLI   OPENSW,C'T'                                                      
         BNE   EXIT                                                             
*                                                                               
*                                                                               
         CLI   BKSFTP,C'Y'       AM I DOING A BURGER KING SFTP?                 
         BE    RUNLBK                                                           
*                                                                               
*                                                                               
RUNL2    CLI   INSW,1              NISSAN INTERFACE?                            
         BNE   RUNL5                                                            
         BRAS  RE,INOUT            FINALLY PRODUCE RECORDS                      
         B     RUNLX                                                            
*                                                                               
RUNL5    DS    0H                                                               
         CLOSE (INSBIT1)                                                        
*                                                                               
RUNLX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
***** CODE BELOW FRO SFTP STYLE OUTPUT                                          
****************************************************************                
RUNLBK   DS    0H      HERE IF DOING BURGER KING SFTP (MINDSHARE)               
*                                                                               
         CLOSE (INSBIT1)                                                        
*                                                                               
         CLI   TESTMQ,C'N'       SEE IF SUPRESSING MQ NOTIFICATION              
         BE    RUNLBK10                                                         
* SEND MQ MESSAGE WITH FILE NAME                                                
         LA    R5,ELEM                                                          
         USING MQMSGD,R5                                                        
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
         LA    R1,MQMAPNM                                                       
         MVC   MQFILE(34),14(R1)   BIL.SYS.AGID.DYYMMDD.THHMMSS                 
*                                  SYS=SYSTEM,AGID= 4 CHARACTER AGY ID          
*                          14(R1) TO GET PAST SPTPDISK.PROD (OR TEST)           
         MVC   MQDATE(6),28(R1)    YYMMDD OF FILE NAME                          
         MVC   MQTIME(6),36(R1)    HHMMSS OF FILE NAME                          
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVC   MQHID,=CL6'DANOT1'                                               
         MVC   MQSYS,MQFILE+4  SYSTEM (+4 PAST BIL.)                            
*                                                                               
RUNLBK5  DS    0H                                                               
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         L     R1,MCAEXTRA                                                      
         MVC   MQAGYID,MCAGYCOD-MCEXTRA(R1)                                     
         DROP  RF                                                               
*                                                                               
RUNLBK5M CLI   INSFTP,C'Y'          NISSAN?                                     
         BNE   RUNLBK5X                                                         
         MVC   MQQUAL(14),=C'NISSAN BILLING'                                    
         B     RUNLBK8                                                          
*                                                                               
RUNLBK5X DC    H'0'          UNKNOW H7 SFTP                                     
*                                                                               
RUNLBK7  MVC   MQQUAL(7),=C'BILLING'                                            
*                                                                               
RUNLBK8  GOTO1 =V(MQRPT),DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'CLOSE'),0,0,0                               
         CLI   DMCB+8,0                                                         
         BE    RUNLBK10                                                         
         DCHO                                                                   
*                                                                               
RUNLBK10 MVC   P+1(49),MQMAPNM       WHOLE FILE NAME                            
         BRAS  RE,PRNT                                                          
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
*********************************************************                       
****  CODE ABOVE FOR SFTP STYLE OUTPUT                                          
*********************************************************                       
         EJECT                                                                  
*        PROCESS BILL  **NOTE- BILL RECORD NOT READ YET**                       
         SPACE 2                                                                
PRBL     DS    0H                                                               
*                                                                               
         CLC   KEY+BKEYINV-BKEY(2),LOWINV                                       
         BL    EXIT                                                             
         CLC   KEY+BKEYINV-BKEY(2),HIINV                                        
         BH    EXIT                                                             
*                                                                               
         MVI   SKIPBILL,0                                                       
         CLI   KEY+BKEYEST-BKEY,0  SKIP EST 0 BILLS (BILLING BUG)               
         BE    EXIT                                                             
         CLC   KEY(10),LSTBLKY     IF FIRST FOR EST/MOS                         
         BE    PRB1                                                             
         MVI   REVSW,C' '          SET NOT A REVISION                           
*                                                                               
         ZAP   EATOTGRS,=P'0'                                                   
         ZAP   EATOTAC,=P'0'                                                    
         ZAP   EATOTACT,=P'0'                                                   
         ZAP   EATOTNET,=P'0'                                                   
         B     PRB1D                                                            
*                                                                               
PRB1     DS    0H                                                               
         MVI   REVSW,C'R'          SET IS A REVISION                            
*                                                                               
PRB1D    DS    0H                                                               
*                                                                               
PRB1H    DS    0H                                                               
         MVC   LSTBLKY,KEY                                                      
         TM    CONTROL1,X'80'      TEST NEED 'ESTIMATE' AMOUNTS                 
         BNZ   PRB3                YES- MUST READ ALL BILLS                     
*                                                                               
         CLC   KEY+BKEYYSRV-BKEY(2),STARTMOS  MONTH-OF-SERVICE FILTERS          
         BL    EXIT                                                             
         CLC   KEY+BKEYYSRV-BKEY(2),ENDMOS                                      
         BH    EXIT                                                             
*                                                                               
         CLI   QOPT4,C'M'          TEST MOS FILTERING                           
         BNE   PRB2D                                                            
         CLC   KEY+BKEYYSRV-BKEY(2),BQSTART                                     
         BL    EXIT                                                             
         CLC   KEY+BKEYYSRV-BKEY(2),BQEND                                       
         BH    EXIT                                                             
         B     PRB3                                                             
*                                                                               
PRB2D    DS    0H                  RUN DATE FILTERING                           
         ZIC   R3,KEY+BKEYMBIL-BKEY                                             
         SRL   R3,4                YEAR DIGIT OF BILL                           
         ZIC   RE,DECADE                                                        
         CLM   R3,1,YEARDIG        COMPARE TO YEAR OF TODAY                     
         BNH   *+8                 IF NOT HIGH, OK                              
         SH    RE,=H'10'           ELSE BACK UP TO PREV DECADE                  
         AR    RE,R3                                                            
         STC   RE,FULL             CALCULATED YEAR OF BILL                      
*                                                                               
         MVC   FULL+1(1),KEY+BKEYMBIL-BKEY                                      
         NI    FULL+1,X'FF'-X'F0'  ISOLATE MONTH                                
*                                                                               
         CLC   FULL(2),BQSTART                                                  
         BL    EXIT                                                             
         CLC   FULL(2),BQEND                                                    
         BH    EXIT                                                             
*                                                                               
PRB3     DS    0H                                                               
         GOTO1 GETBILL                                                          
         L     R2,ADBILL                                                        
         USING BILLREC,R2                                                       
*                                                                               
         CLI   QOPT5+1,C'*'        TEST NETPAK SUB MED FILT                     
         BE    PRB4                NO                                           
         CLI   QOPT5+1,C' '                                                     
         BNH   PRB4                NO                                           
         MVC   BYTE,BLMED                                                       
         CLI   BYTE,C' '           IF NO SUB MEDIA                              
         BH    *+8                                                              
         MVI   BYTE,C'N'           DEFAULT TO N                                 
         CLC   BYTE,QOPT5+1        TEST RIGHT SUB-MED                           
         BNE   EXIT                                                             
*                                                                               
PRB4     DS    0H                                                               
         TM    BILSTAT,BSTCMONQ    TEST COMMISSION ONLY BILL                    
         BO    *+16                YES                                          
         CLI   QOPT5,C'C'          NO, TEST TO SKIP OTHERS                      
         BE    EXIT                                                             
         B     *+12                                                             
         CLI   QOPT5,C'N'          EXCLUDE COMMISSION-ONLY BILLS?               
         BE    EXIT                YES                                          
*                                                                               
         CLI   QOPT5,C'A'          AOR BILLS ONLY?                              
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BZ    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'B'          AOR AND AOR/CLIENT BILLS                     
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ+BSTCAORQ                                        
         BZ    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'X'          NON-AOR BILLS ONLY?                          
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BNZ   EXIT                                                             
*                                                                               
         CLI   QOPT5,C'S'          SOON BILLS ONLY?                             
         BNE   *+12                                                             
         TM    BILSTAT3,BSTSOONQ   WAS REC GENERATED BY SOON ?                  
         BNO   EXIT                                                             
*                                                                               
         CLC   =C'*SOON',QUESTOR   IF WAS AUTOREQUESTED BY SOON                 
         BNE   PB6                                                              
         TM    BILSTAT3,BSTSOONQ   WAS REC GENERATED BY SOON ?                  
         BZ    EXIT                                                             
         CLC   BILLUID,RCORIGID    PROCESS ONLY FOR REQUESTING USER ID          
         BNE   EXIT                                                             
*                                  NB- POST TO 'ESTIMATE' AMTS                  
*                                  WHETHER PASSES DATE FILTERS OR NOT           
PB6      DS    0H                  SET BILL AMOUNTS                             
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PB6AX                                                            
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
*                                                                               
PB6AX    GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
*                                                                               
         LA    R3,BAMTS            CLEAR BILL AMOUNTS                           
         BAS   RE,CLRTOTS                                                       
*                                                                               
         USING AMOUNTSD,R3                                                      
         ZAP   AMTGRS,SPBVGRSP     EFFECTIVE GROSS                              
         ZAP   AMTACT,SPBVACTP     ACTUAL                                       
*                                                                               
         TM    BILSTAT3,X'02'      SEE IF EXCHANGE BILL                         
         BNO   PB6AX4              EQU WILL BE BSTTRCNQ                         
         L     R0,BINVSEQ+1        WILL BE CALLED BCLDNET                       
         CVD   R0,DUB                                                           
         ZAP   AMTNET,DUB                                                       
         B     PB6AX5                                                           
*                                                                               
PB6AX4   ZAP   AMTNET,SPBVNETP     EFFECTIVE NET                                
*                                                                               
PB6AX5   L     R0,SPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTGST,DUB          GST                                          
         ZAP   MYGST,AMTGST        SAVE TO USE IN SPEC                          
         L     R0,SPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTPST,DUB          PST'S (INCL HSTS)                            
         ZAP   MYPST,AMTPST                                                     
         L     R0,SPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTHST,DUB          HST'S ALONE                                  
         ZAP   MYHST,AMTHST                                                     
*                                                                               
         CLI   QOPT5,C'2'          IF COS2 REQUEST                              
         BE    *+12                                                             
         CLI   QOPT5,C'P'          (PW = COS2)                                  
         BNE   PB6C4                                                            
*                                                                               
         TM    BILSTAT2,BSTC2Q     AND COS2 BILL, OK                            
         BZ    EXIT                ELSE, SKIP                                   
*                                                                               
         ZAP   AMTGRS,BGRS2P       USE COS2 GROSS                               
         ZAP   AMTNET,BNET2P       NET                                          
         ZAP   AMTACT,BACT2P       AND ACTUAL                                   
*                                                                               
PB6C4    DS    0H                                                               
         ZAP   DUB,AMTACT                                                       
         TM    BILSTAT,BSTCMONQ    IF COMM ONLY BILL                            
         BZ    PB7                                                              
         CLI   QOPT5,C'C'          TEST LEAVE NET                               
         BE    PB7                 YES                                          
         ZAP   AMTNET,=P'0'        ELSE CLEAR NET (AC = RCVBL)                  
*                                                                               
PB7      DS    0H                                                               
         TM    BILSTAT,BSTTAORQ    FOR AOR BILLS                                
         BZ    *+10                'NET' IS ORIGINAL BILLS COMMISION            
         ZAP   AMTNET,=P'0'        SO SIT MUST BE CLEARED HERE                  
*                                                                               
         SP    DUB,AMTNET                                                       
         ZAP   AMTAC,DUB           AC                                           
*                                                                               
         AP    EATOTGRS,AMTGRS     ADD TO EATOTS                                
         AP    EATOTAC,AMTAC                                                    
         AP    EATOTACT,AMTACT                                                  
         AP    EATOTNET,AMTNET                                                  
*                                                                               
         CLI   QOPT4,C'M'          UNLESS FILTERING ON MOS                      
         BE    PB8                                                              
         CLC   BDATE,SVQST         TEST BILL WITHIN REQ PERIOD                  
         BL    EXIT                                                             
         CLC   BDATE,SVQEND                                                     
         BH    EXIT                                                             
*                                                                               
PB8      DS    0H                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   BRETAIL,0                                                        
         BE    PB30                                                             
*                                  RETAIL BILLS                                 
         CLI   BRETAIL,X'81'       SKIP ALL CORP CONTROL                        
         BE    EXIT                                                             
*                                                                               
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                SKIP ZERO BILLS                              
*                                                                               
         MVI   RETAIL,C'Y'                                                      
*                                  BUILD PRINT LINE                             
PB30     DS    0H                                                               
*        FINALLY - PROCESS THIS BILL                                            
*                                  USE BINSRCH TO ADD TO INVTAB                 
         MVC   WORK(1),KEY+BKEYMBIL-BKEY     BILL MONTH                         
         MVC   WORK+1(2),KEY+BKEYINV-BKEY    AND INVOICE NUMBER                 
         GOTO1 BINSRCH,INVPARS,(1,WORK)                                         
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   NEWCLT,C'Y'         IF NEW CLIENT                                
         BNE   PB30B                                                            
         MVC   P,SVMID             PRINT CLIENT DATA                            
         MVI   ALLOWLIN,7                                                       
         MVI   NEWCLT,C'N'                                                      
*                                  MVI   SPACING,2 REMOVED                      
         BRAS  RE,PRNT                                                          
*                                                                               
PB30B    DS    0H                                                               
         LHI   RF,1                SET BILL COUNT                               
         ST    RF,AMTCNT                                                        
         LA    R7,P                                                             
         USING BLINED,R7                                                        
*                                                                               
         CLI   QOPT5+1,C' '        TEST TO SHOW NETPAK SUB-MED                  
         BNH   PB31                                                             
         CLI   BLMED,C' '          TEST ANY                                     
         BNH   PB31                                                             
         MVI   P,C'*'                                                           
         MVC   P+1(1),BLMED                                                     
*                                                                               
PB31     DS    0H                                                               
         MVC   BLPRD,BKEYPRD       PRD                                          
*                                                                               
         L     RF,ADPRD            PRD NUMBER                                   
         LA    RF,PACCT-PRDHDR(RF)                                              
         MVC   BLPNUM+1(4),0(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *+20                                                             
         ZAP   DUB,1(3,RF)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  BLPNUM,DUB                                                       
*                                                                               
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BLEST,DUB                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,BKEYYSRV),(6,BLPER)                               
         CLI   BKEYYSRV+1,12       SPECIAL PERIOD                               
         BNH   PB33                                                             
         ZIC   R1,BKEYYSRV+1                                                    
         EDIT  (R1),(2,BLPER+1)                                                 
         MVI   BLPER,C' '                                                       
*                                                                               
PB33     DS    0H                                                               
         GOTO1 DATCON,DMCB,BDATE,(5,BLRUND)                                     
         GOTO1 DATCON,DMCB,BQDATE,(5,BLINVD)                                    
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(5,BLDUED)                              
*                                                                               
         MVC   BLTYPE(2),BTYPE     BILLING TYPE                                 
         MVI   BLTYPE+2,C' '                                                    
*                                                                               
         TM    BILSTAT3,X'02'      SEE IF EXCHANGE BILL                         
         BNO   *+8                 EQU WILL BE BSTTRCNQ                         
         MVI   BLTYPE+2,C'X'                                                    
*                                                                               
         TM    BILSTAT3,X'01'      SEE IF MIDAS BARTER                          
         BNO   *+8                 EQU WILL BE BSTMBARQ                         
         MVI   BLTYPE+2,C'T'                                                    
*                                                                               
         TM    BILSTAT,BSTSCOMQ    IF UPFRONT COMM                              
         BZ    *+8                                                              
         MVI   BLTYPE,C'U'         U4-U7 = UPFRONT                              
         TM    BILSTAT,BSTSNETQ    IF NET BILL (AFTER UPFRONT)                  
         BZ    *+8                                                              
         MVI   BLTYPE,C'N'         N4-N7 = UPFRONT                              
         TM    BILSTAT,BSTMANQ     MANUAL?                                      
         BZ    *+10                                                             
         MVC   BLTYPE,=C'MAN'                                                   
         TM    BILSTAT,BSTTAORQ    AOR?                                         
         BZ    *+10                                                             
         MVC   BLTYPE,=C'AOR'                                                   
*                                                                               
*                                                                               
PB33D    DS    0H                                                               
         MVC   DINVNO,SPACES                                                    
         LA    R5,BILLREC                                                       
         GOTO1 VSPFMTIN,DMCB,(C'B',BDATE),(6,BINVNO),(QMED,B1PROF),B1XPX        
               ROF,ADBILL                                                       
**OLD**  GOTO1 VSPFMTIN,DMCB,BDATE,(6,BINVNO),(QMED,B1PROF),B1XPROF             
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)      FULL FORMAT INVOICE NUMBER                   
*                                                                               
         L     RF,DMCB+12                                                       
         MVC   DINVMED,0(RF)       MEDIA PART OF INVOICE                        
*                                                                               
         L     RF,DMCB+4           FORMAT MN-NNNN (SHORT INVOICE NO.)           
         LA    RE,DINVNO                                                        
         LHI   R0,7                                                             
*                                                                               
PB34     DS    0H                                                               
         CLI   0(RF),C'-'                                                       
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PB34                                                          
*                                                                               
         MVC   BLINVNO(2),DINVNO                                                
         MVI   BLINVNO+2,C'-'                                                   
         MVC   BLINVNO+3(4),DINVNO+2                                            
*                                                                               
         CLC   QAGY,=C'OO'         OMDUSEC (USA)                                
         BE    *+14                                                             
         CLC   QAGY,=C'OU'         OMDTOA                                       
         BNE   PB34A                                                            
         CP    AMTACT,=P'0'        ZERO DOLLAR INVOICE?                         
         BNE   PB34A                                                            
*                                                                               
         L     R0,CLTZINV                                                       
         A     R0,=F'1'                                                         
         ST    R0,CLTZINV                                                       
         MVC   BLINE,SPACES                                                     
         B     PB34X                                                            
*                                                                               
PB34A    CLI   CNTRY,C'C'          IF CANADA                                    
         BNE   PB34B                                                            
*                                                                               
         ZAP   DOUBLE,AMTACT       ACTUAL                                       
         AP    DOUBLE,AMTGST       PLUS GST                                     
         AP    DOUBLE,AMTPST       PLUS PST (INCL HST)                          
         EDIT  (P8,DOUBLE),BLACTUAL,2,COMMAS=YES,MINUS=YES                      
*                                                                               
         EDIT  AMTGRS,BLGROSS,2,COMMAS=YES,MINUS=YES                            
         EDIT  AMTNET,BLNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  AMTAC,BLAC,2,COMMAS=YES,MINUS=YES                                
         EDIT  AMTGST,BLGST,2,COMMAS=YES,MINUS=YES                              
*                                                                               
         CP    AMTPST,=P'0'        PST ((INCL HST)                              
         BE    PB34D                                                            
         EDIT  AMTPST,BLPST,2,COMMAS=YES,MINUS=YES                              
         B     PB34D                                                            
*                                                                               
PB34B    DS    0H                  NON-CANADIAN GETS WIDER COLUMNS              
         EDIT  AMTGRS,BLGRSWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTACT,BLACTWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTNET,BLNETWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTAC,BLACWIDE,2,COMMAS=YES,MINUS=YES                            
*                                                                               
PB34D    DS    0H                                                               
         BRAS  RE,PRNT                                                          
         LA    R4,PAMTS            ROLL TO PRODUCT TOTALS                       
         BAS   RE,PBROLL                                                        
         DROP  R3                                                               
*                                                                               
*                                  CLEAR TAPE RECORD                            
PB34X    LA    RE,TPREC                                                         
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         CLI   QOPT1,C'N'                                                       
         BE    PSX                 SKIP TAPE                                    
*                                                                               
         ICM   R2,15,ASPECS                                                     
         BZ    PS40                NO SPECS                                     
*                                                                               
         USING DSELEM,R2                                                        
         MVI   ELCODE,X'05'                                                     
         CLC   ELCODE,0(R2)                                                     
         BE    PS4B                                                             
*                                                                               
PS4      DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   PS40                                                             
*                                                                               
PS4B     DS    0H                                                               
         CLI   DSEMEDF,C' '        IF MEDIA FILTER PRESENT                      
         BNH   *+14                                                             
         CLC   DSEMEDF,QMED        TEST IF OK                                   
         BNE   PS4                                                              
*                                                                               
         CLI   CLFILT,C' '         TEST CLIENT FILTER ACTIVE                    
         BNH   *+14                                                             
         CLC   CLT,CLFILT          YES DO ONLY FOR THIS CLIENT                  
         BNE   PS4                                                              
*                                                                               
         CLI   MEFILT,C' '         TEST MEDIA FILTER ACTIVE                     
         BNH   *+14                                                             
         CLC   MED,MEFILT          YES-DO ONLY FOR THIS MEDIA                   
         BNE   PS4                                                              
*                                                                               
         ZIC   RE,DSECODE                                                       
         L     RF,=A(PSTRT)                                                     
         AR    RF,RE                                                            
         ZIC   R3,0(RF)                                                         
         MHI   R3,4                                                             
         LARL  RF,PSBRTAB                                                       
         AR    RF,R3                                                            
         BR    RF                                                               
PSRETURN DS    0H                  *** RETURN ADDRESS ***                       
         B     PS4                                                              
*                                                                               
PS40     DS    0H                                                               
         CLC   QAGY,=C'OO'         OMDUSEC (USA)                                
         BE    PS40S                                                            
         CLC   QAGY,=C'OU'         OMDTOA                                       
         BNE   PS45                                                             
*                                                                               
PS40S    CLI   QOPT2,C'N'          NISSAN FILE?                                 
         BNE   PS45                                                             
         MVI   INTYPE,C'1'                                                      
         MVI   TPREC,C'1'          OVERRIDES 'L'                                
         XC    INMOS,INMOS         CLEAR MOS                                    
         BRAS  RE,INPROC           YES: DO SPECIAL                              
         BNE   PS900               CC NEQ = DUP INVOICE = IGNORE                
         MVI   INTYPE,C'2'                                                      
         MVI   TPREC,C'2'          OVERRIDES 'L'                                
         BRAS  RE,INPROC           YES: DO SPECIAL                              
         B     PS900                                                            
*                                                                               
PS45     DS    0H                                                               
PS50     DS    0H                                                               
         TM    SKIPBILL,X'80'      TEST THIS BILL TO TAPE                       
         BNZ   PSX                                                              
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   PS052                                                            
*                                                                               
         LH    R0,RECLEN                                                        
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
PS052    DS    0H                                                               
         L     R1,=A(INSBIT1)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
*                                                                               
PS060    B     PS900                                                            
                                                                                
*                                                                               
PS900    DS    0H                                                               
         L     RF,ADBILL           SAVE THIS BILL RECORD                        
         MVC   SVBILL,0(RF)                                                     
*                                                                               
         LA    RE,SVBAMTS          AND BILL VALUES                              
         LA    RF,BAMTS                                                         
         LHI   R0,NAMTS                                                         
         ZAP   0(6,RE),0(6,RF)                                                  
         LA    RE,6(RE)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         LA    RE,TPREC            AND THIS TAPE RECORD                         
         LHI   R1,L'TPREC                                                       
         LA    RF,SVTPREC                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
PSX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*    INDIVIDUAL FIELD ROUTINES                                                  
***********************************************************************         
         SPACE 1                                                                
PSBR01   DS    0H                  AGENCY CODE                                  
         LA    R4,QAGY                                                          
         B     PSSET                                                            
*                                                                               
PSBR02   DS    0H                  MEDIA CODE                                   
         LA    R4,MED                                                           
         B     PSSET                                                            
*                                                                               
PSBR03   DS    0H                  OFFICE CODE                                  
         L     R7,ADCLT                                                         
         LA    R4,COFFICE-CLTHDR(R7)                                            
         B     PSSET                                                            
*                                                                               
PSBR04   DS    0H                  MKT GROUP                                    
         L     R7,ADBILL                                                        
         LA    R4,BLMGR-BILLREC(R7)                                             
         B     PSSET                                                            
*                                                                               
PSBR05   DS    0H                  RETAIL ACCOUNT                               
         LA    R4,SPACES                                                        
         CLI   RETAIL,C'Y'         ONLY FOR RETAIL                              
         BNE   PSSET                                                            
         L     R7,ADBILL                                                        
         LA    R4,BRETACCT-BILLREC(R7)                                          
         B     PSSET                                                            
*                                                                               
PSBR06   DS    0H                  NETPAK SUB-MEDIA (BLMED)                     
         LA    R4,=C' '            ONLY IF NETPAK                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   PSSET                                                            
         L     R7,ADBILL                                                        
         LA    R4,BLMED-BILLREC(R7)                                             
         CLI   0(R4),C' '          DEFAULT IS NET                               
         BH    *+8                                                              
         LA    R4,=C'N'                                                         
         B     PSSET                                                            
*                                                                               
PSBR07   DS    0H                  NETPAK - COST TYPE                           
         L     R7,ADBILL                                                        
         LA    R4,BILCTYP-BILLREC(R7)                                           
         B     PSSET                                                            
*                                                                               
PSBR11   DS    0H                  CLT CODE                                     
         LA    R4,CLT                                                           
         B     PSSET                                                            
*                                                                               
PSBR12   DS    0H                  CLT NAME (UPPERCASE)                         
         LA    R4,CLTNM                                                         
         OC    CLTNM,SPACES                                                     
         B     PSSET                                                            
*                                                                               
PSBR13   DS    0H                  CLT NUMBER                                   
         L     R7,ADCLT                                                         
         LA    R4,CCLTIFC-CLTHDR(R7)                                            
         CLI   0(R4),C' '          TEST PRESENT                                 
         BH    PSSET                                                            
         LA    R4,=8C'0'                                                        
         B     PSSET                                                            
*                                                                               
PSBR21   DS    0H                  PRD CODE                                     
         LA    R4,PRD                                                           
         B     PSSET                                                            
*                                                                               
PSBR22   DS    0H                  PRD NAME (UPPERCASE)                         
         LA    R4,PRDNM                                                         
         OC    PRDNM,SPACES                                                     
         B     PSSET                                                            
*                                                                               
PSBR23   DS    0H                  PRD NUMBER                                   
         L     R7,ADPRD                                                         
         LA    R4,PACCT-PRDHDR(R7)                                              
         CLI   0(R4),X'FF'                                                      
         BNE   PSSET                                                            
         ZAP   DUB,1(3,R4)                                                      
         B     PSNUM                                                            
*                                                                               
PSBR24   DS    0H                  DIVISION (FROM PRD HDR)                      
         L     R7,ADPRD                                                         
         LA    R4,PDIV-PRDHDR(R7)                                               
         OC    0(3,R4),=3C'0'                                                   
         B     PSSET                                                            
*                                                                               
PSBR25   DS    0H                  PUSER1 (UPPERCASE)                           
         L     R7,ADPRD                                                         
         LA    R4,PUSER1-PRDHDR(R7)                                             
         OC    0(L'PUSER1,R4),SPACES                                            
         B     PSSET                                                            
*                                                                               
PSBR26   DS    0H                  PUSER2 (UPPERCASE)                           
         L     R7,ADPRD                                                         
         LA    R4,PUSER2-PRDHDR(R7)                                             
         OC    0(L'PUSER2,R4),SPACES                                            
         B     PSSET                                                            
*                                                                               
PSBR31   DS    0H                  EST CODE                                     
         L     R7,ADBILL                                                        
         ZIC   R0,BKEYEST-BILLREC(R7)                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         LA    R4,WORK                                                          
         B     PSSET                                                            
*                                                                               
PSBR32   DS    0H                  EST NAME (UPPERCASE)                         
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBR32B                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBR32B  DS    0H                                                               
         LA    R4,EDESC-ESTHDR(R7)                                              
         OC    0(L'EDESC,R4),SPACES                                             
         B     PSSET                                                            
*                                                                               
PSBR33   DS    0H                  ESTIMATE FILERS                              
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBR33B                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBR33B  DS    0H                                                               
         LA    R4,EPROF-ESTHDR(R7)                                              
         B     PSSET                                                            
*                                                                               
PSBR35   DS    0H                  EUSER1 (UPPERCASE)                           
         L     R7,ADEST                                                         
         LA    R4,EUSER1-ESTHDR(R7)                                             
         OC    0(L'EUSER1,R4),SPACES                                            
         B     PSSET                                                            
*                                                                               
PSBR36   DS    0H                  EUSER2 (UPPERCASE)                           
         L     R7,ADEST                                                         
         LA    R4,EUSER2-ESTHDR(R7)                                             
         OC    0(L'EUSER2,R4),SPACES                                            
         B     PSSET                                                            
*                                                                               
PSBR41   DS    0H                  BILL MONTH (2)                               
         L     R7,ADBILL                                                        
         LA    R4,DINVNO                                                        
         B     PSSET                                                            
*                                                                               
PSBR42   DS    0H                  INVOICE NUMBER (4)                           
         L     R7,ADBILL                                                        
         LA    R4,DINVNO+2                                                      
         B     PSSET                                                            
*                                                                               
PSBR43   DS    0H                  INVOICE NUMBER (6)                           
         L     R7,ADBILL                                                        
         LA    R4,DINVNO                                                        
         B     PSSET                                                            
*                                                                               
PSBR44   DS    0H                  BILL RUN DATE                                
         L     R7,ADBILL                                                        
         LA    R4,BDATE-BILLREC(R7)                                             
         MVI   BYTE,0                                                           
         B     PSDTE                                                            
*                                                                               
PSBR45   DS    0H                  INVOICE DATE                                 
         L     R7,ADBILL                                                        
         LA    R4,BQDATE-BILLREC(R7)                                            
         MVI   BYTE,0                                                           
         B     PSDTE                                                            
*                                                                               
PSBR46   DS    0H                  DUE DATE                                     
         L     R7,ADBILL                                                        
         LA    R4,BDUEDATE-BILLREC(R7)                                          
         MVI   BYTE,3                                                           
         B     PSDTE                                                            
*                                                                               
PSBR47   DS    0H                  MONTH OF SERVICE                             
         L     R7,ADBILL                                                        
         LA    R4,BKEYYSRV-BILLREC(R7)                                          
         MVI   BYTE,3                                                           
         B     PSDTE                                                            
*                                                                               
PSBR48   DS    0H                  TODAY'S DATE                                 
         LA    R4,TODAY                                                         
         MVI   BYTE,0                                                           
         B     PSDTE                                                            
*                                                                               
BILL     USING AMOUNTSD,BAMTS                                                   
PSBR51   DS    0H                  GROSS                                        
         ZAP   DUB,BILL.AMTGRS                                                  
         B     PSNUM                                                            
*                                                                               
PSBR52   DS    0H                  NET                                          
         ZAP   DUB,BILL.AMTNET                                                  
         B     PSNUM                                                            
*                                                                               
PSBR53   DS    0H                  ACTUAL                                       
         ZAP   DUB,BILL.AMTACT                                                  
         B     PSNUM                                                            
*                                                                               
PSBR54   DS    0H                  AGYCOM - (GROSS - NET)                       
         ZAP   DUB,BILL.AMTGRS                                                  
         SP    DUB,BILL.AMTNET                                                  
         B     PSNUM                                                            
*                                                                               
PSBR55   DS    0H                  AGYCOM (ACTUAL - NET)                        
         ZAP   DUB,BILL.AMTACT                                                  
         SP    DUB,BILL.AMTNET                                                  
         B     PSNUM                                                            
*                                                                               
PSBR56   DS    0H                  TAX                                          
         L     R7,ADBILL                                                        
         ICM   R0,15,BTAXAMT-BILLREC(R7)                                        
         CVD   R0,DUB                                                           
         B     PSNUM                                                            
*                                                                               
PSBR57   DS    0H                  GROSS LESS TAX                               
         L     R7,ADBILL                                                        
         ZAP   DUB,BILL.AMTGRS                                                  
         ICM   R0,15,BTAXAMT-BILLREC(R7)                                        
         CVD   R0,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
         B     PSNUM                                                            
*                                                                               
PSBR58   DS    0H                  FEE AMOUNT                                   
         ZAP   DUB,=P'0'           0 IF NOT AOR BILL                            
         L     R7,ADBILL                                                        
         TM    BILSTAT-BILLREC(R7),BSTTAORQ                                     
         BZ    PSNUM                                                            
         ZAP   DUB,BILL.AMTACT     ELSE COMPLEMENT OF ACTUAL                    
         MP    DUB,=P'-1'                                                       
         B     PSNUM                                                            
*                                                                               
PSBR59   DS    0H                  MEDIA ACTUAL                                 
         ZAP   DUB,=P'0'           0 IF AOR BILL                                
         L     R7,ADBILL                                                        
         TM    BILSTAT-BILLREC(R7),BSTTAORQ                                     
         BNZ   PSNUM                                                            
         ZAP   DUB,BILL.AMTACT     ELSE ACTUAL                                  
         B     PSNUM                                                            
*                                                                               
PSBR5A   DS    0H                  GST                                          
         ZAP   DUB,BILL.AMTGST                                                  
         B     PSNUM                                                            
*                                                                               
PSBR5B   DS    0H                  PST (INCL HST)                               
         ZAP   DUB,BILL.AMTPST                                                  
         B     PSNUM                                                            
*                                                                               
PSBR5C   DS    0H                  GST + PST (INCL HST)                         
         ZAP   DUB,BILL.AMTGST                                                  
         AP    DUB,BILL.AMTPST                                                  
         B     PSNUM                                                            
*                                                                               
PSBR5E   DS    0H                  PST ALONE                                    
         ZAP   DUB,BILL.AMTPST     IS PST                                       
         SP    DUB,BILL.AMTHST     LESS HST                                     
         B     PSNUM                                                            
*                                                                               
PSBR5F   DS    0H                  HST ALONE                                    
         ZAP   DUB,BILL.AMTHST                                                  
         B     PSNUM                                                            
         DROP  BILL                                                             
*                                                                               
PSBR5D   DS    0H                  MARKET                                       
         L     R7,ADBILL                                                        
         LA    R4,BLMKT-BILLREC(R7)                                             
         B     PSSET                                                            
*                                                                               
PSBR61   DS    0H                  GROSS   (ESTIMATE AMOUNTS)                   
         ZAP   DUB,EATOTGRS                                                     
         B     PSNUM                                                            
*                                                                               
PSBR62   DS    0H                  NET                                          
         ZAP   DUB,EATOTNET                                                     
         B     PSNUM                                                            
*                                                                               
PSBR63   DS    0H                  ACTUAL                                       
         ZAP   DUB,EATOTACT                                                     
         B     PSNUM                                                            
*                                                                               
PSBR64   DS    0H                  AGYCOM - (GROSS - NET)                       
         ZAP   DUB,EATOTGRS                                                     
         SP    DUB,EATOTNET                                                     
         B     PSNUM                                                            
*                                                                               
PSBR65   DS    0H                  AGYCOM (ACTUAL - NET)                        
         ZAP   DUB,EATOTACT                                                     
         SP    DUB,EATOTNET                                                     
         B     PSNUM                                                            
*                                                                               
PSBR71   DS    0H                  REVISION STATUS                              
         LA    R4,REVSW                                                         
         B     PSSET                                                            
*                                                                               
PSBR72   DS    0H                  AOR STATUS                                   
         L     R7,ADBILL                                                        
         LA    R4,=C'A'            AOR                                          
         TM    BILSTAT-BILLREC(R7),BSTTAORQ                                     
         BNZ   PSSET                                                            
         LA    R4,=C'C'            CLIENT BILL IN AOR SITUATION                 
         TM    BILSTAT-BILLREC(R7),BSTCAORQ                                     
         BNZ   PSSET                                                            
         LA    R4,SPACES           REGULAR                                      
         B     PSSET                                                            
*                                                                               
PSBR73   DS    0H                  BILL TYPE - B4...B7                          
         L     R7,ADBILL                                                        
         USING BILLRECD,R7                                                      
         MVC   FULL,=C'    '       NEW STYLE BILLS                              
         MVC   FULL(2),BTYPE                                                    
*                                                                               
         TM    BILSTAT,X'40'       TEST MANUAL BILL                             
         BZ    *+16                                                             
         MVC   FULL(3),=C'MAN'                                                  
         MVC   FULL+3(1),BTYPE+1                                                
*                                                                               
         TM    BILSTAT,X'20'       TEST AOR BILL                                
         BZ    *+16                                                             
         MVC   FULL(3),=C'AOR'                                                  
         MVC   FULL+3(1),BTYPE+1                                                
*                                                                               
         TM    BILSTAT,BSTSCOMQ    TEST SEP COMM BILL                           
         BZ    *+16                                                             
         MVC   FULL(3),=C'UFC'                                                  
         MVC   FULL+3(1),BTYPE+1                                                
*                                                                               
         TM    BILSTAT,BSTSNETQ    TEST SEP NET BILL                            
         BZ    *+16                                                             
         MVC   FULL(3),=C'NET'                                                  
         MVC   FULL+3(1),BTYPE+1                                                
*                                                                               
         LA    R4,FULL                                                          
         B     PSSET                                                            
*                                                                               
PSBR74   DS    0H                  KETCHUM BILL TYPE (DEBIT/CREDIT)             
         L     R7,ADBILL                                                        
         LA    R4,=C'SI'           STANDARD INVOICE                             
         CP    BACTP,=P'0'                                                      
         BNL   *+8                                                              
         LA    R4,=C'CI'           CREDIT INVOICE                               
         B     PSSET                                                            
         DROP  R7                                                               
*                                                                               
PSBRF0   DS    0H                  ZEROS                                        
         ZAP   DUB,=P'0'                                                        
         B     PSNUM                                                            
*                                                                               
PSBRF1   DS    0H                  LITERAL                                      
         LA    R4,DSEDATA                                                       
         B     PSSET                                                            
*                                                                               
PSBRF2   DS    0H                  SPACES                                       
         LA    R4,SPACES                                                        
         B     PSSET                                                            
*                                                                               
PSBRFE   DS    0H                  AGENCY SPECIAL                               
         SR    RF,RF                                                            
         ICM   RF,7,DSEPROC                                                     
         BASR  RE,RF                                                            
*                                                                               
         CLI   MODE,CLTLAST        ROUNTINE MIGHT HAVE SET THIS                 
         BE    EXIT                TO SKIP THIS CLIENT                          
*                                                                               
         B     PSRETURN                                                         
*                                                                               
PSBRB1   DS    0H                  FILTERS                                      
         CLI   DSEFTYP,C'C'        CLIENT FILTER                                
         BNE   *+14                                                             
         MVC   CLFILT,DSEFILT                                                   
         B     PSRETURN                                                         
*                                                                               
         CLI   DSEFTYP,C'M'        MEDIA FILTER                                 
         BNE   *+10                                                             
         MVC   MEFILT,DSEFILT                                                   
         B     PSRETURN                                                         
*                                                                               
PSBRC0   DS    0H                  SPECIAL (JW - LAST 5 PRD CHARS)              
         L     R7,ADPRD                                                         
         LA    R4,PNAME+19-PRDHDR(R7)                                           
*                                                                               
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
*                                                                               
         SH    R4,=H'4'            LAST 5 CHARS OF PRD NAME                     
         B     PSSET                                                            
*                                                                               
PSBRC2   DS    0H                  SPECIAL (COMPTON - NETWORK VS CABLE)         
         LA    R4,=C'05'           NETWORK                                      
         L     R7,ADBILL                                                        
         CLI   BKEYEST-BILLREC(R7),200                                          
         BL    *+8                                                              
         LA    R4,=C'09'           CABLE                                        
         B     PSSET                                                            
*                                                                               
PSBRC3   DS    0H                  SPECIAL MEDIA CODES                          
*                                                                               
PSBRC3A  DS    0H                                                               
*                                                                               
PSBRC3B  DS    0H                                                               
*                                                                               
PSBRC3C  DS    0H                                                               
         B     PSBRC3X                                                          
*                                  ELSE NETWORK TYPE FROM ESTIMATE              
PSBRC3N  DS    0H                                                               
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBRC3P                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBRC3P  DS    0H                                                               
         LA    RF,EPROF-ESTHDR(R7)                                              
*                                                                               
*                                                                               
PSBRC3Q  DS    0H                                                               
*                                                                               
PSBRC3R  DS    0H                                                               
PSBRC3X  DS    0H                  EXIT WITHOUT SETTING ANY VALUE               
         B     PSRETURN                                                         
*                                                                               
PSBRC4   DS    0H                  DDB- SPECIAL AGENCY CODES                    
         LA    R4,=C'RET'          DNRFO = RET                                  
         CLC   =C'NR',QAGY                                                      
         BE    PSBRC4D                                                          
*                                                                               
         L     R7,ADCLT            ELSE BASED ON OFFICE                         
         LA    R7,COFFICE-CLTHDR(R7)                                            
*                                                                               
         LA    R4,=C'DDB'                                                       
         CLI   0(R7),C'Y'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'2'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'1'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'C'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'W'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'4'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'5'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'T'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'I'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'F'                                                       
         BE    PSBRC4D                                                          
*                                                                               
         LA    R4,=C'DDW'                                                       
         CLI   0(R7),C'L'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'S'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'Z'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'V'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'7'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'8'                                                       
         BE    PSBRC4D                                                          
*                                                                               
         LA    R4,=C'XXX'          UNKNOWN AGENCY                               
*                                                                               
PSBRC4D  DS    0H                                                               
         B     PSSET                                                            
*                                                                               
PSBRC5   DS    0H                  DDB- SPECIAL CLIENT/ESTIMATE NUMBERS         
         L     R7,ADBILL               JUST USE ESTIMATE NUMBER                 
         ZIC   R0,BKEYEST-BILLREC(R7)  UNLESS SPECIAL CLIENT CODE               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         LA    R4,WORK                                                          
*                                                                               
         CLC   CLT(2),=C'GT'                                                    
         BE    PSBRC5D                                                          
         CLC   CLT(2),=C'VX'                                                    
         BE    PSBRC5D                                                          
         B     PSSET                                                            
*                                                                               
PSBRC5D  DS    0H                  EST NAME EXTRACT                             
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBRC5G                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBRC5G  DS    0H                                                               
         LA    R4,EDESC+2-ESTHDR(R7)    ESTNAME+2(4)                            
         B     PSSET                                                            
*                                                                               
PSBRC6   DS    0H                  GROUP W - AGENT                              
*                                  (UP TO 6 NUMERICS AT START OF ESTN)          
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBRC6G                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBRC6G  DS    0H                                                               
         LA    R4,WORK                                                          
         MVC   WORK(6),=C'000000'                                               
         LA    R5,EDESC-ESTHDR(R7)                                              
         LHI   R3,6                                                             
*                                                                               
PSBRC6J  DS    0H                                                               
         CLI   0(R5),C'0'          CHECK FOR NUMERICS                           
         BL    *+12                                                             
         LA    R5,1(R5)            NEXT POS                                     
         BCT   R3,PSBRC6J                                                       
*                                                                               
         LCR   R3,R3                                                            
         AHI   R3,5                R3 HAS LENGTH - 1                            
         BM    PSSET                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,EDESC-ESTHDR(0,R7)                                           
         UNPK  WORK(6),DUB                                                      
         B     PSSET                                                            
*                                                                               
PSSET    DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,3,DSEPOS         POSITION IN OUTPUT                           
         LA    RF,TPREC-1(RF)                                                   
         ZIC   R5,DSELEN           LENGTH                                       
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)   *EXECUTED*                                       
*                                                                               
         CLI   DSEFMT,4            TEST EBCDIC OUTPUT (UPPERCASE)               
         BNE   PSRETURN                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),SPACES  *EXECUTED*                                       
         B     PSRETURN                                                         
*                                                                               
PSNUM    DS    0H                                                               
         CLI   DSEFMT,1                                                         
         BNE   PSNUM2                                                           
*                                  PACKED                                       
         LA    R4,DUB+8                                                         
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         B     PSSET                                                            
*                                                                               
PSNUM2   DS    0H                                                               
         CLI   DSEFMT,2                                                         
         BNE   PSNUM3                                                           
*                                  BINARY                                       
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
         LA    R4,FULL+4                                                        
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         B     PSSET                                                            
*                                                                               
PSNUM3   DS    0H                                                               
         CLI   DSEFMT,3                                                         
         BNE   PSNUM4                                                           
*                             EBCDIC - SIGN OVERPUNCH IN LOW POSITION           
PSNUM3B  DS    0H                                                               
         UNPK  WORK(15),DUB                                                     
         LA    R4,WORK+15                                                       
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         B     PSSET                                                            
*                                                                               
PSNUM4   DS    0H                                                               
         CLI   DSEFMT,4                                                         
         BNE   PSNUM5                                                           
*                                  EBCDIC - NO SIGN OVERPUNCH                   
         OI    DUB+7,X'0F'                                                      
         B     PSNUM3B                                                          
*                                                                               
PSNUM5   DS    0H                  LEADING MINUS SIGN - FOR NEGATIVES           
         CLI   DSEFMT,5                                                         
         BNE   PSNUM6                                                           
*                                                                               
         UNPK  WORK(15),DUB                                                     
         LA    R4,WORK+15                                                       
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         TM    WORK+14,X'10'       IF AMOUNT NEGATIVE                           
         BZ    *+8                                                              
         MVI   0(R4),C'-'          ADD MINUS SIGN                               
         OI    WORK+14,X'F0'       REMOVE OVERPUNCH                             
         B     PSSET                                                            
*                                                                               
PSNUM6   DS    0H                  TRAILING MINUS SIGN - FOR NEGATIVES          
         CLI   DSEFMT,6                                                         
         BNE   PSNUM7                                                           
*                                                                               
         UNPK  WORK(15),DUB                                                     
         LA    R4,WORK+15                                                       
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         TM    WORK+14,X'10'       IF AMOUNT NEGATIVE                           
         BZ    *+12                                                             
         LA    R4,1(R4)                                                         
         MVI   WORK+15,C'-'        ADD MINUS SIGN AT END                        
         OI    WORK+14,X'F0'       REMOVE OVERPUNCH                             
         B     PSSET                                                            
*                                                                               
PSNUM7   DS    0H                                                               
         DC    H'0'                UNKNOWN NUMERIC FORMAT                       
*                                                                               
PSDTE    DS    0H                  DATES                                        
         MVC   BYTE2,DSEFMT        DATE FORMAT FROM FORMAT= PARAMETER           
         CLI   DSEFMT,32           PREDEFINED DATCON OUTPUT TYPE?               
         BNL   PSDTE2              NO                                           
         OI    BYTE2,X'20'         DON'T RETURN FUNNY YEARS                     
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(BYTE2,WORK)                            
         B     PSDTEX                                                           
*                                                                               
PSDTE2   DS    0H                                                               
         CLI   DSEFMT,32           MMDDYY                                       
         BNE   PSDTE3                                                           
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(X'20',WORK)                            
         MVC   WORK+6(6),WORK                                                   
         MVC   WORK(4),WORK+6+2    MMDD                                         
         MVC   WORK+4(2),WORK+6    YY                                           
         B     PSDTEX                                                           
*                                                                               
PSDTE3   DS    0H                                                               
         CLI   DSEFMT,33           MMYY                                         
         BNE   PSDTE4                                                           
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(X'20',WORK)                            
         MVC   WORK+6(6),WORK                                                   
         MVC   WORK(2),WORK+6+2    MM                                           
         MVC   WORK+2(2),WORK+6    YY                                           
         B     PSDTEX                                                           
*                                                                               
PSDTE4   DS    0H                                                               
         CLI   DSEFMT,34           CYYMMDD                                      
         BNE   PSDTE5                                                           
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(15,WORK)                               
         UNPK  DUB,WORK(4)         X'0CYYDDDF' --> C'00CYYDDD'                  
         MVC   WORK(1),DUB+2       CENTURY                                      
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(X'20',WORK+1) YYMMDD                   
*                                                                               
PSDTE5   DS    0H                                                               
         CLI   DSEFMT,35           CMMYY                                        
         BNE   PSDTEX                                                           
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(15,WORK)                               
         UNPK  DUB,WORK(4)         X'0CYYDDDF' --> C'00CYYDDD'                  
         MVC   WORK(1),DUB+2       CENTURY                                      
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(X'20',DUB) YYMMDD                      
         MVC   WORK+1(2),DUB+2     MM                                           
         MVC   WORK+3(2),DUB       YY                                           
*                                                                               
PSDTEX   DS    0H                                                               
         LA    R4,WORK                                                          
         B     PSSET                                                            
         SPACE 2                                                                
NXTEL    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NXTEL+2                                                          
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
PBROLL   NTR1                                                                   
         LHI   R0,NAMTS                                                         
PBROLL2  DS    0H                                                               
         AP    0(6,R4),0(6,R3)                                                  
         LA    R3,6(R3)                                                         
         LA    R4,6(R4)                                                         
         BCT   R0,PBROLL2                                                       
         XIT1                                                                   
         SPACE 3                                                                
CLRTOTS  NTR1                                                                   
         LHI   R0,NAMTS                                                         
CLRT2    DS    0H                                                               
         ZAP   0(6,R3),=P'0'                                                    
         LA    R3,6(R3)                                                         
         BCT   R0,CLRT2                                                         
         XIT1                                                                   
         SPACE 3                                                                
CHKTOTS  NTR1                                                                   
         LHI   R0,NAMTS                                                         
CHKT2    DS    0H                                                               
         CP    0(6,R3),=P'0'                                                    
         BNE   NO                  EXIT WITH CC NOT =                           
         LA    R3,6(R3)                                                         
         BCT   R0,CHKT2                                                         
         B     YES                 EXIT WITH CC =                               
         ANSR                                                                   
         EJECT                                                                  
TOTPRNT  NTR1                                                                   
         SPACE 2                                                                
         ST    R3,ATOTS                                                         
         L     R4,ATOTS                                                         
         GOTO1 ATPFMT                                                           
         BRAS  RE,PRNT                                                          
         XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
WIAPLCT  DS    0H                  FOR WESTERN/APL - TEST CLIENT                
*                                  (CALLED FROM CLTFRST)                        
         LA    RF,WIAPLLST                                                      
*                                                                               
WIAPLC4  DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    WIAPNO                                                           
*                                                                               
         CLC   CLT,0(RF)                                                        
         BER   RE                                                               
*                                                                               
         LA    RF,5(RF)                                                         
         B     WIAPLC4                                                          
*                                                                               
*                                                                               
         SPACE 2                                                                
WAPLBTCT DS    0H                  FOR WESTERN/APL - TEST WA CLT                
*                                  (CALLED FROM CLTFRST)                        
         LA    RF,WAPLLSTS                                                      
         CLI   NETPAKSW,C'Y'                                                    
         BNE   WAPLC4                                                           
         LA    RF,WAPLLSTN         USE NET CLIENT LIST, NOT SPOT LIST           
*                                                                               
WAPLC4   DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    WAPLNO                                                           
*                                                                               
         CLC   CLT,0(RF)                                                        
         BER   RE                                                               
*                                                                               
         LA    RF,3(RF)                                                         
         B     WAPLC4                                                           
*                                                                               
WAPLNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
WAPLOK   DS    0H                                                               
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
         SPACE 2                                                                
WIAPLCMT DS    0H                  FOR WESTERN/APL - TEST CLIENT/MONTH          
*                                  (CALLED FROM PROC BILL)                      
         LA    RF,WIAPLLST                                                      
*                                                                               
WIAPLCM4 DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    WIAPNO                                                           
*                                                                               
         CLC   CLT,0(RF)                                                        
         BNE   *+14                                                             
         CLC   KEY+8(2),3(RF)                                                   
         BNL   WIAPOK                                                           
*                                                                               
         LA    RF,5(RF)                                                         
         B     WIAPLCM4                                                         
*                                                                               
WIAPNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
WIAPOK   DS    0H                                                               
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
WIAPLLST DS    0C                                                               
         DC    C'BKK',X'6304'                                                   
         DC    C'LAB',X'6304'                                                   
         DC    C'LGO',X'6304'                                                   
         DC    C'RCA',X'6304'                                                   
         DC    C'UPS',X'6304'                                                   
         DC    X'FF'                                                            
*                                                                               
*        CLIENT LIST FOR WAPLBT                                                 
*                                                                               
WAPLLSTS DS    0C                  FOR SPOT                                     
         DC    C'LAB'                                                           
         DC    C'LEV'                                                           
         DC    C'LIP'                                                           
         DC    C'RCA'                                                           
         DC    C'HYG'                                                           
         DC    C'JMD'                                                           
         DC    C'MSK'                                                           
         DC    C'JJO'                                                           
         DC    C'GOR'                                                           
         DC    C'CHB'                                                           
         DC    C'AMI'                                                           
         DC    C'UPS'                                                           
         DC    C'ATK'                                                           
         DC    X'FF'                                                            
*                                                                               
WAPLLSTN DS    0C                  FOR NET                                      
         DC    C'AMI'                                                           
         DC    C'BKK'                                                           
         DC    C'LAB'                                                           
         DC    C'LGO'                                                           
         DC    C'UPS'                                                           
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DC    (3*4096-(*-SPIN02))X'00'  ROUND UP TO 12K BOUNDARY               
*                                                                               
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
*                                                                               
PSBRTAB  DS    F'0'                (TABLE IS ZERO-BASED)                        
         B     PSBR01       #01    AGENCY                                       
         B     PSBR02       #02    MEDIA                                        
         B     PSBR03       #03    OFFICE                                       
         B     PSBR04       #04    MKT GROUP                                    
         B     PSBR11       #05    CLT CODE                                     
         B     PSBR12       #06    CLT NAME                                     
         B     PSBR13       #07    CLT NUMBER                                   
         B     PSBR21       #08    PRD CODE                                     
         B     PSBR22       #09    PRD NAME                                     
         B     PSBR23       #10    PRD NUMBER                                   
         B     PSBR31       #11    EST CODE                                     
         B     PSBR32       #12    EST NAME                                     
         B     PSBR41       #13    BILL MONTH (2)                               
         B     PSBR42       #14    INV. NO (4)                                  
         B     PSBR43       #15    INV. NO (6)                                  
         B     PSBR44       #16    BILL RUN DATE                                
         B     PSBR45       #17    INVOICE DATE                                 
         B     PSBR46       #18    DUE DATE                                     
         B     PSBR47       #19    MONTH OF SERVICE                             
         B     PSBR51       #20    GROSS                                        
         B     PSBR52       #21    NET                                          
         B     PSBR53       #22    ACTUAL                                       
         B     PSBR54       #23    AGYCOM (GROSS-NET)                           
         B     PSBR55       #24    AGYCOM (ACTUAL - NET)                        
         B     PSBRF0       #25    ZEROS                                        
         B     PSBRF1       #26    LITERAL                                      
         B     PSBRF2       #27    SPACES                                       
         B     PSBRB1       #28    FILTER                                       
         B     PSBRC0       #29    SPECIAL (JW)                                 
         DC    F'0'         #30                                                 
         B     PSBR24       #31    DIVISION (FROM PRD HEADER)                   
         B     PSBR71       #32    REVISION STATUS                              
         B     PSBR33       #33    ESTIMATE FILTERS                             
         B     PSBR61       #34    GROSS   (ESTIMATE AMOUNTS)                   
         B     PSBR62       #35    NET                                          
         B     PSBR63       #36    ACTUAL                                       
         B     PSBR64       #37    AGYCOM (GROSS-NET)                           
         B     PSBR65       #38    AGYCOM (ACTUAL - NET)                        
         B     PSBRC2       #39    SPECIAL (COMPTON)                            
         B     PSBRC3       #40    SPECIAL (Y&R NETWORK MEDIA)                  
         B     PSBRC4       #41    SPECIAL (DDB AGENCY CODES)                   
         B     PSBR05       #42    RETAIL ACCOUNT                               
         B     PSBR48       #43    TODAY'S DATE                                 
         B     PSBRC5       #44    SPECIAL (DDB CLIENT ESTIMATES)               
         B     PSBR56       #45    TAX                                          
         B     PSBR57       #46    GROSS LESS TAX                               
         B     PSBRC6       #47    SPECIAL - GROUP W AGENT                      
         B     PSBR58       #48    FEE AMOUNT   (0 FOR NON-AOR BILLS)           
         B     PSBR59       #49    MEDIA ACTUAL (0 FOR AOR BILLS)               
         B     PSBR72       #50    AOR STATUS                                   
         B     PSBR73       #51    BILL TYPE                                    
         B     PSBR74       #52    KETCHAM BILL TYPE (DEBIT/CREDIT)             
         B     PSBR06       #53    NETPAK SUB-MEDIA (BLMED)                     
         B     PSBRFE       #54    AGENCY SPECIAL ROUTINE                       
         B     PSBR5A       #55    GST AMOUNT                                   
         B     PSBR07       #56    COST TYPE (NETPAK)                           
         B     PSBR25       #57    PUSER1                                       
         B     PSBR26       #58    PUSER2                                       
         B     PSBR35       #59    EUSER1                                       
         B     PSBR36       #60    EUSER2                                       
         B     PSBR5B       #61    PST AMOUNT (INCL HST)                        
         B     PSBR5C       #62    GST+PST (INCL HST)                           
         B     PSBR5D       #63    MARKET                                       
         B     PSBR5E       #64    PST ALONE                                    
         B     PSBR5F       #65    HST ALONE                                    
         SPACE  3                                                               
*        NOTE X'41' (65) IS HIGHEST USED SO FAR (AT 5F)                         
         SPACE 2                                                                
INSBIT1  DCB   DDNAME=SINTAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00112,                                            X        
               BLKSIZE=00112,                                          X        
               MACRF=PM                                                         
*                                                                               
XXSBIT1  DCB   DDNAME=SINTAPE,DSORG=PS,MACRF=PM                                 
*                                                                               
MULTMSG  DC    C'**MULTIPLE TAPE DESCRIPTIONS IN ONE JOB - BYPASSED**'          
*                                                                               
         EJECT                                                                  
TPFMT    NMOD1 0,TPFMT                                                          
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         LA    R7,P                                                             
         USING BLINED,R7                                                        
         USING AMOUNTSD,R4                                                      
*                                                                               
         CLI   CNTRY,C'C'          IF CANADA                                    
         BNE   TPF02                                                            
*                                                                               
         ZAP   DUB(6),AMTACT       ACTUAL                                       
         AP    DUB(6),AMTGST       PLUS GST                                     
         AP    DUB(6),AMTPST       PLUS PST                                     
         LA    R1,DUB                                                           
         LA    R5,BLACTUAL                                                      
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTNET           NET                                          
         LA    R5,BLNET                                                         
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTGRS           GROSS                                        
         LA    R5,BLGROSS                                                       
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTAC            ACTUAL COMMISSION                            
         LA    R5,BLAC                                                          
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTGST           DO GST                                       
         LA    R5,BLGST                                                         
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTPST           AND PST                                      
         ZAP   DUB(6),AMTPST                                                    
         BZ    TPF04                                                            
         LA    R1,DUB                                                           
         LA    R5,BLPST                                                         
         BAS   RE,TPEDT                                                         
         B     TPF04                                                            
*                                                                               
TPF02    DS    0H                  NON-CANADIAN GETS WIDER COLUMNS              
         EDIT  AMTGRS,BLGRSWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTACT,BLACTWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTNET,BLNETWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTAC,BLACWIDE,2,COMMAS=YES,MINUS=YES                            
         DROP  R4                                                               
*                                                                               
TPF04    DS    0H                                                               
         BRAS  RE,PRNT                                                          
         XIT1                                                                   
         SPACE 3                                                                
TPEDT    DS    0H                                                               
         EDIT  (P6,0(R1)),(15,WORK),2,COMMAS=YES,MINUS=YES                      
         LR    RF,R5                                                            
         SH    RF,=H'2'                                                         
         CLI   WORK,C' '                                                        
         BNE   TPEDT6                                                           
         CLI   WORK+1,C' '                                                      
         BE    *+12                                                             
         CLI   1(RF),C' '                                                       
         BNE   TPEDT6                                                           
*                                                                               
         OC    1(15,RF),WORK                                                    
         BR    RE                                                               
*                                                                               
TPEDT6   DS    0H                                                               
         MVC   133(15,RF),WORK                                                  
         BR    RE                                                               
         LTORG                                                                  
         DROP  R7                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
INPROC   NMOD1 0,INPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         L      R7,ADBILL                                                       
         USING  BILLREC,R7                                                      
         MVI    INCRDT,C'2'         INIT TO DEBIT                               
         CP     TPREC+28(8),=P'0'   IS THIS A CREDIT?                           
         BNL    *+8                 NO                                          
         MVI    INCRDT,C'1'         YES - CREDITS MUST COME FIRST               
         CLI    INTYPE,C'2'         SECOND DETAIL?                              
         BNE    *+10                                                            
         MVC    INMOS,BKEYYSRV                                                  
*                                                                               
INLAR3   MVC    INMED,MEDIA         GO HERE TO TYPE 2 REC                       
         MVC    INCLI,CLT                                                       
         MVC    INPRO,BKEYPRD                                                   
         MVC    INEST(1),BKEYEST                                                
         MVC    ININVMO,BKEYMBIL    BILLING Y/M BYTE                            
         MVC    ININVN,BKEYINV                                                  
*                                                                               
         MVC    INREC(250),TPREC                                                
         MVC    INREC+250(125),TPREC+250                                        
***                                                                             
* WE MAY HAVE A JOB THAT PROCESSES THE SAME INVOICE MORE THAN ONCE              
* SUCH AS AN ALL CLIENT REQUEST FOLLOWED BY A CLIENT SPECIFIC REQUEST           
* THE FOLLOWING CODE WAS ADDED TO IGNORE DUP INVOICES RATHER THAN ABEND         
***                                                                             
         CLI    INTYPE,C'1'      INVOICE HEADER?                                
         BNE    INLAR5           NO                                             
         MVC    ININVALS,AINKEY  RESET KEY                                      
         MVI    ININVALS,0       SET TO SEARCH FOR EXACT MATCH                  
         GOTO1  =V(BINSRCH),ININVALS                                            
         CLI    ININVALS,1       RECORD FOUND?                                  
         BNE    INTONEQ          YES - AVOID DUP REC - RETURN CC NEQ            
*                                                                               
INLAR5   L      R2,AOFINT         ADDRESS OF INTABLE                            
         MVC    ININVALS,AINKEY  RESET KEY                                      
         MVI    ININVALS,1       SET TO ADD RECORD                              
         GOTO1 =V(BINSRCH),ININVALS                                             
*                                                                               
         CLI    ININVALS,1         RECORD INSERTED                              
         BE     INTOXIT                                                         
         OC     ININVALS+1(3),ININVALS+1 IF ZERO TABLE IS FULL                  
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
         CLI     INTYPE,C'1'              SHOULD ONLY HAPPEN FOR                
         BE      *+6                      INVOICE HEADER                        
         DC      H'0'                     SOMETHING'S WRONG                     
*                                                                               
         L       RF,ININVALS              ADDRESS OF FOUND RECORD               
         LA      RF,L'INKEY(RF)           PAST KEY                              
*                                                                               
         AP      28(8,RF),TPREC+28(8)   ACTUAL (AMOUNT DUE)                     
         AP      36(8,RF),TPREC+36(8)   TAXES                                   
*                                                                               
INTOXIT  DS      0H                                                             
         MVC     ININVALS,AINKEY                                                
         MVI     ININVALS,1                                                     
*                                                                               
INTOEQU  CR    RB,RB                                                            
         B     *+6                                                              
INTONEQ  LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   IN NISSAN SPECIAL OUTPUT FROM A SPECIAL BINSRCH TABLE                       
***********************************************************************         
*                                                                               
INOUT    NMOD1 0,INOUT                                                          
         LA    RC,SPACEND                                                       
*                                                                               
*                      FINALLY PRODUCE RECORDS                                  
*                                                                               
         BC    0,INPRL5                                                         
         OI    *-3,X'F0'                                                        
*                                                                               
         L     RF,VMASTC                                                        
         MVI   MCTAPETY-MASTD(RF),C'V'  UNIT=VTS                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+0(4),=A(DYNDDN)                                             
         MVC   DMCB+4(4),=A(DYNDSN)                                             
         MVI   DMCB+4,X'FE'                                                     
         MVI   BYTE,1              GENERATION +1                                
         GOTO1 DYNALLOC,DMCB,,,(X'80',BYTE)                                     
         OPEN  (INSBIT1,(OUTPUT))                                               
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
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
*        FIRST PUT OUT HEADER RECORD                                            
*                                                                               
         MVC   TPREC(3),=C'HDR'                                                 
         MVC   TPREC+3(8),=C'ZFI04010'        JOB NAME                          
         CLI   OMDUSASW,C'Y'     OMDUSA?                                        
         BNE   *+10                                                             
         MVC   TPREC+3(8),=C'ZFI02125'        JOB NAME                          
         MVC   TPREC+11(10),MCTODAY       TODAY                                 
         MVC   TPREC+21(6),TIMEOFDF      HHMMSS                                 
         MVC   TPREC+27(16),=C'IT306OMD INVOICE'                                
         CLI   OMDUSASW,C'Y'     OMDUSA?                                        
         BNE   *+10                                                             
         MVC   TPREC+27(23),=C'IT306 MARKETING INVOICE'                         
*                                                                               
         MVI   INRECSW,C'A'        FILE HEADER                                  
         AP    INLCNT,=P'1'                                                     
         B     INAGN5                                                           
*                                                                               
INPRL8   L     R3,AOFINT                                                        
         LA    R3,L'INKEY(R3)      BUMP PAST PSUEDO KEY                         
*                                                                               
INAGN    DS    0H                                                               
         CLI   INSW,1              AGENCY OU/OO NISSAN                          
         BNE   INAGN00             NO                                           
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
*        DO INVOICE HEADER (H) RECORD FIRST                                     
*                                                                               
*                                                                               
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
*                                                                               
         MVI   TPREC,C'1'            HEADER INDICATOR                           
*                                                                               
*   BILL RUN DATE COMES TO ME AS YYYYMMDD                                       
*                                                                               
         MVC   TPREC+1(2),351(R3)    MM                                         
         MVI   TPREC+3,C'/'                                                     
         MVC   TPREC+4(2),353(R3)    DD                                         
         MVI   TPREC+6,C'/'                                                     
         MVC   TPREC+7(4),347(R3)    YYYY                                       
*                                                                               
         MVC   TPREC+11(4),=C'2135' COMPANY CODE                                
         CLI   OMDUSASW,C'Y'     OMDUSA?                                        
         BNE   *+10                                                             
         MVC   TPREC+11(4),=C'2177' COMPANY CODE                                
*                                                                               
*        DUE DATE COMES TO ME AS MM/DD/YY                                       
*                                                                               
         MVC   TPREC+15(2),339(R3) DUE DATE MM                                  
         MVI   TPREC+17,C'/'                                                    
         MVC   TPREC+18(2),341(R3)     DD                                       
         MVI   TPREC+20,C'/'                                                    
         MVC   TPREC+21(2),=C'20'    CENTURY  (Y2K!)                            
         MVC   TPREC+23(2),343(R3)   YY                                         
         MVC   TPREC+25(3),=C'CAD'   CURRENCY                                   
         CLI   OMDUSASW,C'Y'     OMDUSA?                                        
         BNE   *+10                                                             
         MVC   TPREC+25(3),=C'USD'   CURRENCY                                   
         MVC   TPREC+28(10),364(R3) FULL INV # WITH DASHES                      
         MVC   TPREC+44(25),273(R3)       JOB DESCRIPTION                       
*                                                                               
         MVC   TPREC+69(9),=C'NC1003512'     VENDOR CODE                        
         CLI   OMDUSASW,C'Y'     OMDUSA?                                        
         BNE   *+10                                                             
         MVC   TPREC+69(9),=C'AO0025401'     VENDOR CODE                        
*                                                                               
         ZAP   MYDUB,28(8,R3)        NON-FIRST DETAIL RECORD                    
         EDIT  (P8,MYDUB),(16,TPREC+79),2                                       
*                                                                               
         CLC   TPREC+91(2),=C' .'   IF LESS THAN $1.00 ADD A ZERO               
         BNE   *+8                                                              
         MVI   TPREC+91,C'0'                                                    
*                                                                               
*                                                                               
         CLI   OMDUSASW,C'Y'     OMDUSA?                                        
         BE    INAGN0                                                           
*                                                                               
         ZAP   MYDUB,36(8,R3)        NON-FIRST DETAIL RECORD                    
         EDIT  (P8,MYDUB),(16,TPREC+95),2                                       
*                                                                               
         CLC   TPREC+107(2),=C' .'   IF LESS THAN $1.00 ADD A ZERO              
         BNE   *+8                                                              
         MVI   TPREC+107,C'0'                                                   
*                                                                               
*                                                                               
INAGN0   AP    INHCNT,=P'1'                                                     
         AP    INLCNT,=P'1'                                                     
         B     INAGN5                                                           
***                                                                             
*                                                                               
INAGN1   ZAP   MYDUB,28(8,R3)        FIRST DETAIL RECORD                        
*                                                                               
         MVI   TPREC,C'2'                                                       
         MVC   TPREC+1(2),=C'40'                                                
         CP    28(8,R3),=P'0'                                                   
         BNL   *+10                                                             
         MVC   TPREC+1(2),=C'50'     CREDIT MEMO                                
*                                                                               
         MVC   TPREC+3(10),159(R3)        GL ACCOUNT  PRD USER 1                
         EDIT  (P8,MYDUB),(16,TPREC+13),2                                       
*                                                                               
         CLC   TPREC+25(2),=C' .'   IF LESS THAN $1.00 ADD A ZERO               
         BNE   *+8                                                              
         MVI   TPREC+25,C'0'                                                    
*                                                                               
         MVC   TPREC+29(2),=C'I7'       TAX CODE?                               
         CLI   OMDUSASW,C'Y'        OMDUSA?                                     
         BNE   *+10                                                             
         MVC   TPREC+29(2),SPACES                                               
*                                                                               
         MVC   TPREC+46(12),197(R3)       JOB NUMBER EST USER 1                 
         MVC   TPREC+58(10),181(R3)       COST CENTER  PRD USER 2               
*                                                                               
         AP    INLCNT,=P'1'                                                     
         B     INAGN5                                                           
*                                                                               
INAGN2   ZAP   MYDUB,28(8,R3)        NON-FIRST DETAIL RECORD                    
*                                                                               
         MVI   TPREC,C'2'                                                       
         MVC   TPREC+1(2),=C'40'                                                
         CP    28(8,R3),=P'0'                                                   
         BNL   *+10                                                             
         MVC   TPREC+1(2),=C'50'     CREDIT MEMO                                
         MVC   TPREC+3(10),159(R3)   GL ACCOUNT PRD USER1                       
         EDIT  (P8,MYDUB),(16,TPREC+13),2                                       
*                                                                               
         CLC   TPREC+25(2),=C' .'   IF LESS THAN $1.00 ADD A ZERO               
         BNE   *+8                                                              
         MVI   TPREC+25,C'0'                                                    
*                                                                               
         MVC   TPREC+29(2),=C'I7'     TAX CODE                                  
         CLI   OMDUSASW,C'Y'        OMDUSA?                                     
         BNE   *+10                                                             
         MVC   TPREC+29(2),SPACES                                               
*                                                                               
         MVC   TPREC+46(12),197(R3)       JOB NUMBER EST USER 1                 
         MVC   TPREC+58(10),181(R3)       COST CENTER  PRD USER 2               
*                                                                               
         AP    INLCNT,=P'1'                                                     
         B     INAGN5                                                           
*                                                                               
*                                                                               
INAGN5   DS    0H                                                               
         LA    R4,TPREC                                                         
         CLI   SVQOPT3,C'Y'          TEST RUN - NO RECORDS TO FILE              
         BE    INAGN7                                                           
*                                                                               
         L     R1,=A(INSBIT1)                                                   
         PUT   (1),(4)                                                          
*                                                                               
INAGN7   DS    0H                                                               
         AP    INCOUNT,=P'1'                                                    
         CLI   SVQOPT3,C'Y'          DISPLAY FIRST 100 RECORDS                  
         BNE   INAGNX                                                           
         CP    INCOUNT,=P'50'                                                   
         BH    INAGNX                                                           
*                                                                               
         MVC   P(13),=C'***OUTPUT HEX'                                          
         BRAS  RE,PRNT                                                          
         LR    R5,R4                                                            
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,60(R4)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P,52,0                                          
         BRAS  RE,PRNT                                                          
*                                                                               
INAGN9   MVC   P(13),=C'***OUTPUT CHAR'                                         
         BRAS  RE,PRNT                                                          
         MVC   P(112),0(R4)                                                     
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
*                                                                               
INAGNX   DS    0H                                                               
         CLI   INRECSW,C'A'  DID I JUST DO THE FILE HEADER                      
         BNE   INAGNX2       GO DO THE DETAILS                                  
         MVI   INRECSW,0     CLEAR                                              
         MVC   TPREC(112),SPACES   CLEAR TPREC                                  
         B     INPRL8        GO DO THE H RECORD                                 
*                                                                               
INAGNX2  DS    0H                                                               
         CLI   INRECSW,C'H'  DID I JUST DO THE INVOICE HEADER                   
         BNE   INAGNX3       GO DO THE DETAILS                                  
         MVI   INRECSW,0     CLEAR                                              
         MVC   TPREC(112),SPACES   CLEAR TPREC                                  
         B     INAGN1        GO DO FIRST DETAIL RECORD                          
*                            SAME BINSEARCH RECORD AS INV. HEADER               
*                                                                               
INAGNX3  CLI   INRECSW,C'F'          DID I JUST DO THE TRAILER                  
         BE    INZERO        DONE                                               
*                                                                               
INAGNXX  LA    R3,INRECLEN(R3)     TO NEXT RECORD IN TABLE                      
*                                                                               
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         BCT   R2,INAGN                                                         
*                                                                               
*        LAST  PUT OUT TRAILER RECORD                                           
*                                                                               
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         AP    INLCNT,=P'1'        ADD ONE FOR TRAILER                          
         MVC   TPREC(4),=C'TRLR'   TRAILER RECORD                               
         MVC   TPREC+4(08),=C'ZFI04010'                                         
         CLI   OMDUSASW,C'Y'                                                    
         BNE   *+10                                                             
         MVC   TPREC+4(8),=C'ZFI02125'                                          
         MVC   TPREC+12(10),MCTODAY       MM/DD/YYYY                            
         MVC   TPREC+22(4),TIMEOFDF     HHMM  (NO SECONDS HERE)                 
         EDIT  (P3,INLCNT),(8,TPREC+26),0,FILL=0                                
         LA    R3,TPREC                                                         
         MVI   INRECSW,C'F'                                                     
         B     INAGN5                                                           
*                                                                               
INZERO   DS    0H                                                               
*                                                                               
         MVC   ININVALS,AINKEY                                                  
         MVI   ININVALS,1                                                       
*                                                                               
         CLOSE (INSBIT1)                                                        
*                                                                               
         L     RE,VMASTC                                                        
         OC    MCREMPQK-MASTD(,RE),MCREMPQK-MASTD(RE)   SOON RUN?               
         BZ    INOUTX                                                           
*                                                                               
         LARL  RE,DYNDSN           YES                                          
         DEQ   (MAJORNAM,(RE),DSNLENQ,SYSTEM),RET=HAVE                          
*                                                                               
INOUTX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
INRECSW  DS    CL1                                                              
INHCNT   DS    PL3                INVOICE HEADER COUNT                          
INLCNT   DS    PL3                INVOICE DETAIL COUNT                          
INCOUNT  DS    PL3                RECORD COUNT                                  
*                                                                               
         EJECT                                                                  
INIPROC  NMOD1 0,**INIP**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         AP    TPREC+36(8),TPREC+44(8)     ADD PST TO GST                       
         MVC   TPREC+44(8),SPACES          NOTE - INCLUDES HST                  
         MVC   TPREC+364(L'DINVFULL),DINVFULL                                   
*                                                                               
*****    MVC   WORK(4),TPREC+273      YYMM  FROM INSPECS                        
*****    MVC   WORK+4(2),=C'01'       SET DAY                                   
*****    GOTO1 DATCON,DMCB,(0,WORK),(6,WORK+6)                                  
*****                                 AND FLOAT IT PAST MEDIA                   
*****                                                                           
*****    MVC   TPREC+273(13),=C'SPOT TV MEDIA'                                  
*****    LA    RE,TPREC+288                                                     
*****    CLI   QMED,C'T'                                                        
*****    BE    INIP5                                                            
*****    MVC   TPREC+273(16),=C'SPOT RADIO MEDIA'                               
*****    LA    RE,TPREC+291                                                     
*****    CLI   QMED,C'R'                                                        
*****    BE    INIP5                                                            
*****    CLI   NETPAKSW,C'Y'                                                    
*****    BNE   INIP5                                                            
*****    MVC   TPREC+273(16),=C'NETWORK TV MEDIA'                               
*****    LA    RE,TPREC+291                                                     
*****    CLI   BLMED,C'N'                                                       
*****    BE    INIP5                                                            
*****    CLI   BLMED,C' '              NOT PRESENT                              
*****    BNH   INIP5                                                            
*****    MVC   TPREC+273(25),SPACES                                             
*****    MVC   TPREC+273(14),=C'CABLE TV MEDIA'                                 
*****    LA    RE,TPREC+289                                                     
*****    CLI   BLMED,C'C'                                                       
*****    BE    INIP5                                                            
*****    MVC   TPREC+273(25),SPACES                                             
*****    MVC   TPREC+273(11),=C'SYNDICATION'                                    
*****    LA    RE,TPREC+286                                                     
*****    CLI   BLMED,C'S'                                                       
*****    BE    INIP5                                                            
*****    MVC   TPREC+273(13),=C'NETWORK RADIO'                                  
*****    LA    RE,TPREC+288                                                     
*****    MVC   TPREC+288(6),WORK+6                                              
*****    CLI   BLMED,C'D'                                                       
*****    BE    INIP5                                                            
*****    MVC   TPREC+273(13),=C'NETWORK OTHER'                                  
*****    LA    RE,TPREC+288                                                     
*****    MVC   TPREC+288(6),WORK+6                                              
*****    CLI   BLMED,C'O'                                                       
*****    BE    INIP5                                                            
*****                                 FOR OTHER NETWORK MEDIA                   
**IP5    DS    0H                                                               
*****    MVC   0(6,RE),WORK+6      FLOAT MOS AFTER MEDIA                        
*                                                                               
         L     R7,ADEST                                                         
         USING ESTHDRD,R7                                                       
*                                                                               
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(3),DUB                                                    
*                                                                               
         MVC   TPREC+273(3),WORK+2                                              
         MVC   TPREC+277(L'EDESC),EDESC-ESTHDR(R7)                              
         OC    TPREC+277(L'EDESC),SPACES                                        
         DROP  R7                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
FMTAMT   NTR1                      DUB HAS AMOUNT, R3 POSIT IN TPREC            
         UNPK  WORK(11),DUB                                                     
         LA    R4,WORK                                                          
         TM    WORK+10,X'10'       IF AMOUNT NEGATIVE                           
         BZ    *+12                                                             
         LA    R4,1(R4)                                                         
         MVI   WORK+11,C'-'        ADD MINUS SIGN AT END                        
*                                                                               
         OI    WORK+10,X'F0'       REMOVE OVERPUNCH                             
         LA    R3,TPREC-1(R3)                                                   
         MVC   0(11,R3),0(R4)                                                   
         XIT1                                                                   
         EJECT                                                                  
CKNEWYR  NTR1                                                                   
*                                                                               
         MVC   DUB(4),0(R3)                                                     
         NI    DUB,X'FF'-X'FE'     STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'FF'-X'FE'                                                
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'FF'-X'E0'   ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LHI   R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'FF'-X'E0'   ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   LTR   RE,RE                                                            
         B     *+6                                                              
*                                                                               
CKNYYES  CR    RE,RE                                                            
         XIT1                                                                   
         SPACE 2                                                                
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
*   GETAOR                                                                      
***********************************************************************         
         SPACE 2                                                                
GETAOR   NMOD1 0,GETAOR                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         MVC   KEY2,KEY            PRESERVE THIS KEY                            
         LA    R5,KEY                                                           
         USING AORKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D45'     AOR RECORD CODE                              
         L     RF,ADCLT                                                         
         MVC   AORKAGMD(3),1(RF)   AGY/MED/CLT                                  
*                                                                               
         L     RE,ADBILL                                                        
         USING BILLREC,RE                                                       
         MVC   AORKPRD,BKEYPRD                                                  
         MVC   AORKEST+1(1),BKEYEST                                             
         DROP  RE                                                               
*                                                                               
         MVI   AORKDPT,X'FF'       DEFAULT DAYPART       (NOT USED IN           
         MVI   AORKSTYP,X'FF'      DEFAULT STATION TYPE   SPOT, ONLY            
         MVI   AORKEXT+2,X'FF'     3RD EXTRA NOT USED     IN NET)               
         DROP  R5                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      TEST THRU PRD                                
         BNE   GAKS21              NOTHING FOR PRODUCT                          
         CLC   KEY(10),KEYSAVE     TEST THRU EST                                
         BE    GAKS08                                                           
         CLC   KEY+8(2),=X'FFFF'   DID WE FIND DEFAULT                          
         BE    GAKS08              YES, USE IT                                  
*                                                                               
GAKS07   DS    0H                                                               
         MVC   KEYSAVE+8(2),=X'FFFF'  TRY DEFAULT EST                           
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GAKS21              NOTHING THERE EITHER                         
*                                                                               
GAKS08   DS    0H                                                               
         CLC   KEY+10(1),KEYSAVE+10   DAYPART                                   
         BE    GAKS10                                                           
         CLI   KEY+10,X'FF'        DID WE FIND DEFAULT                          
         BE    GAKS10                                                           
         CLI   KEYSAVE+10,X'FF'    DID WE TRY FOR IT                            
         BE    GAKS19              YES, TRY UNDER DEFAULT EST                   
*                                                                               
GAKS09   DS    0H                                                               
         MVI   KEYSAVE+10,X'FF'    TRY DEFAULT DAYPART                          
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   GAKS19              NOTHING, DONE                                
*                                                                               
GAKS10   DS    0H                                                               
         CLC   KEY+11(1),KEYSAVE+11    STATION TYPE                             
         BE    GAKS20                                                           
         CLI   KEY+11,X'FF'        DID WE FIND DEFAULLT                         
         BE    GAKS20                                                           
         CLI   KEYSAVE+11,X'FF'    DID WE TRY FOR IT                            
         BE    GAKS18                                                           
         MVI   KEYSAVE+11,X'FF'    TRY DEFAULT                                  
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    GAKS20                                                           
*                                                                               
GAKS18   DS    0H                  NOTHING FOUND                                
         CLI   KEYSAVE+10,X'FF'    WERE WE LOOKING UNDER ALL DPTS               
         BE    GAKS19              YES                                          
*                                  NO, DO IT NOW                                
         MVI   KEYSAVE+11,X'FF'    RESET STATION TYPE                           
         B     GAKS09                                                           
*                                                                               
GAKS19   DS    0H                                                               
         CLC   KEYSAVE+8(2),=X'FFFF'   WERE WE LOOKING UNDER ALL ESTS           
         BE    GAKS21              YES, NOTHING MORE TO DO                      
*                                  NO, DO IT NOW                                
         MVI   KEYSAVE+10,X'FF'    RESET DAYPART                                
         MVI   KEYSAVE+11,X'FF'    RESET STATION TYPE                           
         B     GAKS07                                                           
*                                                                               
GAKS20   DS    0H                  HAVE USABLE AOR KEY                          
         B     GAO4                                                             
*                                                                               
GAKS21   DS    0H                  NO USABLE AOR KEY                            
         B     GAO9                                                             
*                                                                               
GAO4     DS    0H                                                               
         L     R7,ADBUY            USE STATION BUCKET AREA                      
         ST    R7,AREC                                                          
         USING AORREC,R7                                                        
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         LA    R2,AORELS                                                        
         DROP  R7                                                               
*                                                                               
GAO6     DS    0H                                                               
         CLI   0(R2),X'02'         ADDRESS ELEM                                 
         BE    GAO7                                                             
         CLI   0(R2),X'03'         AOR INFO ELEM                                
         BE    GAO8                                                             
         CLI   0(R2),0             EOR                                          
         BE    GAO9                                                             
*                                                                               
GAO6D    DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GAO6                                                             
*                                                                               
GAO7     DS    0H                  AOR ADDRESS ELEM                             
         USING AORADREL,R2                                                      
         MVC   WORK(30),AORLIN1    RETURN 'NAME' IN WORK                        
         B     GAO6D                                                            
*                                                                               
GAO8     DS    0H                  AOR INFO ELEM                                
         B     GAO6D               SKIP                                         
         USING AORELEM,R2                                                       
*                                                                               
GAO9     DS    0H                                                               
         MVC   KEY,KEY2            RESTORE SEQ                                  
         GOTO1 HIGH                                                             
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   GETPGRP                                                                     
*                                                                               
*     NOTE: ON ENTRY, FIELD 'THREE' CONTAINS PRDGRP CODE                        
*           ON EXIT,  FIELD 'FULL' CONTAINS FORMATTED PRDGRP CODE               
*                     FIELD 'WORK' CONTAINS PRDGRP NAME                         
***********************************************************************         
         SPACE 2                                                                
GETPGRP  NMOD1 0,GETPGRP                                                        
         LA    RC,SPACEND                                                       
*                                                                               
         MVC   FULL,SPACES         WILL CONTAIN PRDGRP CODE                     
         MVC   WORK,SPACES         WILL CONTAIN PRDGRP NAME                     
*                                                                               
         MVC   KEY2,KEY            PRESERVE THE CURRENT KEY                     
*                                                                               
         LA    R5,KEY              BUILD PRDGRP DEFINITION KEY                  
         USING PRGKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'     PRDGRP RECORD CODE                           
         L     RF,ADCLT                                                         
         MVC   PRGKAGMD,1(RF)      AGY/MED                                      
         MVC   PRGKCLT,2(RF)       CLIENT                                       
         MVC   PRGKID,THREE        PRDGRP ID (SCHEME)                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     MUST BE THERE                                
         BNE   GPGRPX              PGRDEF KEY NOT FOUND                         
*                                                                               
         L     R7,ADBUY            USE STATION BUCKET AREA                      
         ST    R7,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         MVI   ELCODE,X'01'        LOOK FOR BREAK DESCRIPTION ELEMENT           
         BRAS  RE,GETEL                                                         
         BNE   GPGRPX                                                           
         USING PRGEL01,R7                                                       
         ZIC   R0,PRGBK1LN         BREAK 1 LENGTH                               
         ZIC   RE,PRGBK2LN         BREAK 2 LENGTH                               
         AR    RE,R0                                                            
         MVC   FULL(1),THREE       PRDGRP ID                                    
         UNPK  DUB,THREE+1(2)      PRDGRP NUMBER                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FULL+1(0),DUB+3     PRINTABLE PRDGRP NUMBER                      
         MVC   BYTE,PRGBK2LN       HANG ONTO BREAK2 LENGTH                      
         DROP  R7                                                               
*                                                                               
         MVC   PRGKGRP,THREE+1     PUT PRDGRP NUMBER INTO KEY                   
         DROP  R5                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DID WE FIND THE PRDGRP KEY?                  
         BNE   GPGRPX              NO                                           
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         L     R7,ADBUY                                                         
         MVI   ELCODE,X'10'        LOOK FOR BREAK NAMES ELEMENT                 
         BRAS  RE,GETEL                                                         
         BNE   GPGRPX                                                           
         USING PRGEL10,R7                                                       
         CLI   BYTE,0              IS 2ND BREAK NAME PRESENT?                   
         BE    *+14                NO, SO USE 1ST BREAK NAME                    
         MVC   WORK(L'PRGNAM2),PRGNAM2                                          
         B     GPGRPX                                                           
         MVC   WORK(L'PRGNAM1),PRGNAM1                                          
         DROP  R7                                                               
*                                                                               
GPGRPX   DS    0H                                                               
         MVC   KEY,KEY2            RESTORE SEQ                                  
         GOTO1 HIGH                                                             
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   PRNT - PRINTING ROUTINE                                                     
***********************************************************************         
PRNT     NMOD1 0,PRNT                                                           
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   CNTRY,C'C'          CANADA GETS DIFFERENT SPROG                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,50         FOR GST                                      
*                                                                               
         CLC   P,SPACES                                                         
         BNE   PRNT1                                                            
         CLI   MODE,OFCLAST     IF P EMPTY AND MODE IS OFFICE LAST              
         BE    PRNTX            DON'T PRINT ANYTHING                            
*                                                                               
PRNT1    MVC   HEAD1+53(23),=C'NETPAK NISSAN INTERFACE'                         
         MVC   HEAD2+53(23),=C'-----------------------'                         
         CLI   NETPAKSW,C'Y'                                                    
         BE    PRNT1N                                                           
         MVC   HEAD1+52(4),=C'SPOT'                                             
         MVI   HEAD2+52,C'-'                                                    
         CLI   QMED,C'N'                                                        
         BNE   PRNT1N                                                           
         MVC   HEAD1+52(07),=C'NETWORK'                                         
*                                                                               
PRNT1N   CLI   MODE,REQLAST      AT REQLAST DON'T SHOW AN OFFICE                
         BE    PRNT2                                                            
*                                                                               
         CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BNE   PRNT2                                                            
         MVC   HEAD2(6),=C'OFFICE'                                              
         MVC   HEAD2+9(1),OFFICE                                                
         CLI   SV00APRF+2,C'Y'     TEST ALWAYS PRINT AS CHAR                    
         BE    PRNT2                                                            
*                                                                               
         MVC    HEAD2+9(2),SAVCOFF                                              
*                                                                               
******   GOTO1 =V(OFFOUT),DMCB,OFFICE,HEXOUT,HEAD2+9                            
*                                                                               
PRNT2    DS    0H                                                               
         CLI   QOPT5,C'A'         TEST AOR ONLY                                 
         BNE   *+10                                                             
         MVC   HEAD6(18),=C'**AOR BILLS ONLY**'                                 
*                                                                               
         CLI   QOPT5,C'B'         AOR AND AOR/CLIENT                            
         BNE   *+10                                                             
         MVC   HEAD6(24),=C'**AOR AND CLIENT BILLS**'                           
*                                                                               
         CLI   QOPT5,C'X'         NON=AOR BILLS ONLY                            
         BNE   *+10                                                             
         MVC   HEAD6(22),=C'**NON-AOR BILLS ONLY**'                             
*                                                                               
         CLI   QOPT5,C'C'         COMMISSION ONLY BILLS                         
         BNE   *+10                                                             
         MVC   HEAD6(25),=C'**COMMISSION ONLY BILLS**'                          
*                                                                               
         CLI   QOPT5,C'N'         EXCLUDE COMMISSION ONLY BILLS                 
         BNE   *+10                                                             
         MVC   HEAD6(34),=C'**COMMISSION ONLY BILLS EXCLUDED**'                 
*                                                                               
         CLI   QOPT5,C'S'         SOON BILLS ONLY?                              
         BNE   *+10                                                             
         MVC   HEAD6(19),=C'**SOON BILLS ONLY**'                                
*                                                                               
         CLI   QOPT4,C'M'         MOS DATES                                     
         BNE   *+10                                                             
         MVC   HEAD4+49(32),=C'**MONTH OF SERVICE DATE FILTER**'                
*                                                                               
         LA    R4,HEAD3+49                                                      
         MVC   0(23,R4),=C'PERIOD FROM          TO'                             
         GOTO1 DATCON,DMCB,SVQST,(8,12(R4))                                     
         GOTO1 DATCON,DMCB,SVQEND,(8,24(R4))                                    
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
PRNTX    XIT1                                                                   
         EJECT                                                                  
*                             HEADHOOK ROUTINE                                  
         SPACE 2                                                                
HHROUT   NTR1                                                                   
*                                                                               
         CLC   P,SPACES                                                         
         BNE   HHR1                                                             
         CLI   MODE,OFCLAST     IF P EMPTY AND MODE IS OFFICE LAST              
         BE    HHR4             DON'T PRINT ANYTHING                            
*                                                                               
HHR1     CLI   MODE,REQLAST        SKIP AT REQLAST                              
         BE    HHR4                                                             
*                                                                               
         CLC   =C'ALL',QCLT        FIX CLIENT HEADLINE                          
         BNE   HHR2                                                             
         CLI   PROGPROF+0,C'Y'     UNLESS SEPARATE PAGE                         
         BE    HHR2                                                             
         MVC   HEAD3+9(3),=C'ALL'                                               
         MVC   HEAD3+13(24),SPACES                                              
*                                                                               
HHR2     DS    0H                                                               
         MVC   HEAD4(07),=C'PRODUCT'                                            
         CLC   =C'ALL',QPRD        FIX PRODUCT HEADLINE                         
         BNE   HHR2B                                                            
         CLI   PROGPROF+1,C'Y'     UNLESS SEPARATE PAGE                         
         BE    HHR2B                                                            
         MVC   HEAD4+9(3),=C'ALL'                                               
         MVC   HEAD4+13(24),SPACES                                              
*                                                                               
HHR2B    DS    0H                                                               
         MVC   HEAD5(08),=C'ESTIMATE'                                           
         CLC   =C'ALL',QEST        FIX ESTIMATE HEADLINE                        
         BNE   HHR2C                                                            
         MVC   HEAD5+9(3),=C'ALL'                                               
         MVC   HEAD5+13(24),SPACES                                              
*                                                                               
HHR2C    CLI   MODE,OFCLAST        LAST FOR OFFICE                              
         BE    HHR4                                                             
         CLI   MODE,CLTLAST        UNLESS AFTER CLIENT LAST                     
         BH    HHR4                PUT CLIENT DATA IN MIDLINE                   
         CLC   SVMID+8(3),CLT      IF STILL WITH SAME CLIENT                    
         BNE   HHR4                                                             
         CLC   P,SVMID             DON'T REPEAT THE CLIENT LINE                 
         BE    HHR4                                                             
***      CLC   P,SPACES            EMPTY LINE?                                  
***      BE    HHR4                                                             
         MVC   MID1,SVMID                                                       
*                                                                               
HHR4     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PSTRT    DS    0X                                                               
         DC    X'00010203042A35380000000000000000'   00-0F                      
         DC    X'00050607000000000000000000000000'   10-1F                      
         DC    X'0008090A1F393A000000000000000000'   20-2F                      
         DC    X'000B0C21003B3C000000000000000000'   30-3F                      
         DC    X'000D0E0F101112132B00000000000000'   40-4F                      
         DC    X'0014151617182D2E3031373D3E3F4041'   50-5F                      
         DC    X'00222324252600000000000000000000'   60-6F                      
         DC    X'00203233340000000000000000000000'   70-7F                      
         DC    X'00000000000000000000000000000000'   80-8F                      
         DC    X'00000000000000000000000000000000'   90-9F                      
         DC    X'00000000000000000000000000000000'   A0-AF                      
         DC    X'001C0000000000000000000000000000'   B0-BF                      
         DC    X'1D002728292C2F000000000000000000'   C0-CF                      
         DC    X'00000000000000000000000000000000'   D0-DF                      
         DC    X'00000000000000000000000000000000'   E0-EF                      
         DC    X'191A1B00000000000000000000003600'   F0-FF                      
         PRINT OFF                                                              
         EJECT                                                                  
         MACRO                                                                  
         DSPEC &COL,&LEN,&DTYPE,&MEDIA_FILTER=,&ROUTINE=,              +        
               &FORMAT=,&LITERAL=,&SPECIAL=                                     
         LCLA  &EL,&DC,&FLV,&FL                                                 
         LCLC  &MF,&THREEB                                                      
.*                                                                              
&EL      SETA  8                                                                
&FLV     SETA  0                                                                
&FL      SETA  &LEN                                                             
.*                                                                              
&MF      SETC  ' '                                                              
         AIF   (T'&MEDIA_FILTER EQ 'O').P10                                     
&MF      SETC  '&MEDIA_FILTER'                                                  
.*                                                                              
.P10     AIF   (T'&ROUTINE EQ 'O').P20                                          
&DC      SETA  254                                                              
         AGO   .P60                                                             
.*                                                                              
.P20     AIF   (T'&FORMAT EQ 'O').P40                                           
.*                                                                              
         AIF   (T'&FORMAT NE 'N').P30                                           
&FLV     SETA  &FORMAT                                                          
         AGO   .P40                                                             
.*                                                                              
.P30     AIF   ('&FORMAT' EQ 'YYMMDD').P40                                      
         AIF   ('&FORMAT' EQ 'YYMM').P40                                        
&FLV     SETA  32                                                               
         AIF   ('&FORMAT' EQ 'MMDDYY').P40                                      
&FLV     SETA  33                                                               
         AIF   ('&FORMAT' EQ 'MMYY').P40                                        
&FLV     SETA  34                                                               
         AIF   ('&FORMAT' EQ 'CYYMMDD').P40                                     
&FLV     SETA  34                                                               
         AIF   ('&FORMAT' EQ 'CYYMM').P40                                       
&FLV     SETA  34                                                               
         AIF   ('&FORMAT' EQ 'CYY').P40                                         
&FLV     SETA  35                                                               
         AIF   ('&FORMAT' EQ 'CMMYY').P40                                       
&FLV     SETA  1                                                                
         AIF   ('&FORMAT' EQ 'PACKED').P40                                      
&FLV     SETA  2                                                                
         AIF   ('&FORMAT' EQ 'BINARY').P40                                      
&FLV     SETA  3                                                                
         AIF   ('&FORMAT' EQ 'UNPACKED').P40                                    
&FLV     SETA  4                                                                
         AIF   ('&FORMAT' EQ 'EBCDIC').P40                                      
&FLV     SETA  5                                                                
         AIF   ('&FORMAT' EQ 'LEADING_MINUS_SIGN').P40                          
&FLV     SETA  6                                                                
         AIF   ('&FORMAT' EQ 'TRAILING_MINUS_SIGN').P40                         
         MNOTE 8,'INVALID DATA FORMAT'                                          
         MEXIT                                                                  
.*                                                                              
.P40     AIF   (T'&SPECIAL EQ 'O').P50                                          
&DC      SETA  &SPECIAL                                                         
         AGO   .P60                                                             
.*                                                                              
.P50     AIF   (T'&LITERAL EQ 'O').P60                                          
&DC      SETA  241                                                              
&EL      SETA  &EL+K'&LITERAL-2                                                 
&FL      SETA  K'&LITERAL-2                                                     
.*                                                                              
.P60     ANOP                                                                   
&THREEB  SETC  'AL2(&COL),AL1(&FL)'                                             
         AIF   (T'&ROUTINE EQ 'O').P70                                          
&THREEB  SETC  'AL3(&ROUTINE)'                                                  
.*                                                                              
.P70     AIF   (T'&DTYPE EQ 'O').P80                                            
&DC      SETA  1                                                                
         AIF   ('&DTYPE' EQ 'AGENCY').P80                                       
&DC      SETA  2                                                                
         AIF   ('&DTYPE' EQ 'MEDIA').P80                                        
&DC      SETA  3                                                                
         AIF   ('&DTYPE' EQ 'OFFICE').P80                                       
&DC      SETA  4                                                                
         AIF   ('&DTYPE' EQ 'MKTGROUP').P80                                     
&DC      SETA  5                                                                
         AIF   ('&DTYPE' EQ 'RETAIL').P80                                       
&DC      SETA  6                                                                
         AIF   ('&DTYPE' EQ 'SUBMEDIA').P80                                     
&DC      SETA  7                                                                
         AIF   ('&DTYPE' EQ 'COSTTYPE').P80                                     
&DC      SETA  17                                                               
         AIF   ('&DTYPE' EQ 'CLTCODE').P80                                      
&DC      SETA  18                                                               
         AIF   ('&DTYPE' EQ 'CLTNAME').P80                                      
&DC      SETA  19                                                               
         AIF   ('&DTYPE' EQ 'CLTNUM').P80                                       
&DC      SETA  33                                                               
         AIF   ('&DTYPE' EQ 'PRDCODE').P80                                      
&DC      SETA  34                                                               
         AIF   ('&DTYPE' EQ 'PRDNAME').P80                                      
&DC      SETA  35                                                               
         AIF   ('&DTYPE' EQ 'PRDNUM').P80                                       
&DC      SETA  36                                                               
         AIF   ('&DTYPE' EQ 'DIVISION').P80                                     
&DC      SETA  37                                                               
         AIF   ('&DTYPE' EQ 'PUSER1').P80                                       
&DC      SETA  38                                                               
         AIF   ('&DTYPE' EQ 'PUSER2').P80                                       
&DC      SETA  49                                                               
         AIF   ('&DTYPE' EQ 'ESTCODE').P80                                      
&DC      SETA  50                                                               
         AIF   ('&DTYPE' EQ 'ESTNAME').P80                                      
&DC      SETA  51                                                               
         AIF   ('&DTYPE' EQ 'ESTFILTS').P80                                     
&DC      SETA  53                                                               
         AIF   ('&DTYPE' EQ 'EUSER1').P80                                       
&DC      SETA  54                                                               
         AIF   ('&DTYPE' EQ 'EUSER2').P80                                       
&DC      SETA  65                                                               
         AIF   ('&DTYPE' EQ 'INVNUM2').P80                                      
&DC      SETA  66                                                               
         AIF   ('&DTYPE' EQ 'INVNUM4').P80                                      
&DC      SETA  67                                                               
         AIF   ('&DTYPE' EQ 'INVNUM6').P80                                      
&DC      SETA  68                                                               
         AIF   ('&DTYPE' EQ 'RUNDATE').P80                                      
&DC      SETA  69                                                               
         AIF   ('&DTYPE' EQ 'INVDATE').P80                                      
&DC      SETA  70                                                               
         AIF   ('&DTYPE' EQ 'DUEDATE').P80                                      
&DC      SETA  71                                                               
         AIF   ('&DTYPE' EQ 'MOS').P80                                          
&DC      SETA  72                                                               
         AIF   ('&DTYPE' EQ 'TODAY').P80                                        
&DC      SETA  81                                                               
         AIF   ('&DTYPE' EQ 'GROSS').P80                                        
&DC      SETA  82                                                               
         AIF   ('&DTYPE' EQ 'NET').P80                                          
&DC      SETA  83                                                               
         AIF   ('&DTYPE' EQ 'ACTUAL').P80                                       
&DC      SETA  84                                                               
         AIF   ('&DTYPE' EQ 'GROSS_COMMISSION').P80                             
&DC      SETA  85                                                               
         AIF   ('&DTYPE' EQ 'ACTUAL_COMMISSION').P80                            
&DC      SETA  86                                                               
         AIF   ('&DTYPE' EQ 'TAX').P80                                          
&DC      SETA  87                                                               
         AIF   ('&DTYPE' EQ 'GROSS_MINUS_TAX').P80                              
&DC      SETA  88                                                               
         AIF   ('&DTYPE' EQ 'FEE').P80                                          
&DC      SETA  89                                                               
         AIF   ('&DTYPE' EQ 'MEDIA_ACTUAL').P80                                 
&DC      SETA  90                                                               
         AIF   ('&DTYPE' EQ 'GST').P80                                          
&DC      SETA  91                                                               
         AIF   ('&DTYPE' EQ 'PST').P80                                          
&DC      SETA  93                                                               
         AIF   ('&DTYPE' EQ 'MARKET').P80                                       
&DC      SETA  94                                                               
         AIF   ('&DTYPE' EQ 'PST_ALONE').P80                                    
&DC      SETA  95                                                               
         AIF   ('&DTYPE' EQ 'HST_ALONE').P80                                    
&DC      SETA  99                                                               
         AIF   ('&DTYPE' EQ 'ESTIMATE_ACTUAL').P80                              
&DC      SETA  102                                                              
         AIF   ('&DTYPE' EQ 'PGRNAME').P80                                      
&DC      SETA  113                                                              
         AIF   ('&DTYPE' EQ 'REVISION_STATUS').P80                              
&DC      SETA  114                                                              
         AIF   ('&DTYPE' EQ 'AOR_STATUS').P80                                   
&DC      SETA  115                                                              
         AIF   ('&DTYPE' EQ 'BILLTYPE').P80                                     
&DC      SETA  240                                                              
         AIF   ('&DTYPE' EQ 'ZEROES').P80                                       
&DC      SETA  242                                                              
         AIF   ('&DTYPE' EQ 'SPACES').P80                                       
         MNOTE 8,'INVALID DATATYPE'                                             
         MEXIT                                                                  
.*                                                                              
.P80     DC    X'05',AL1(&EL),AL1(&DC),&THREEB,C'&MF',AL1(&FLV)                 
         AIF   (T'&LITERAL EQ 'O').P90                                          
         DC    C&LITERAL                                                        
.P90     ANOP                                                                   
         SPACE 1                                                                
         MEXIT                                                                  
         MEND                                                                   
         EJECT                                                                  
         PRINT ON                                                               
*        SPEC TABLES                                                            
         SPACE 2                                                                
DSELEM   DSECT                     DATA SPEC ELEM                               
         DS    XL1'05'             ELEM CODE                                    
         DS    AL1(0)              ELEM LENGTH                                  
DSECODE  DS    XL1                 DATA CODE                                    
DSEPROC  DS    AL3                 A(AGENCY ROUTINE)                            
         ORG   DSEPROC                                                          
DSEPOS   DS    XL2                 POSITION IN OUTPUT                           
DSELEN   DS    XL1                 DATA LENGTH                                  
DSEMEDF  DS    CL1                 MEDIA FILTER                                 
DSEFMT   DS    XL1                 FORMAT CONTROL                               
DSEDATA  DS    0X                  DATA (FOR LITERALS)                          
*                                                                               
         ORG   DSEPOS              REDEFINE FOR FILTERS                         
DSEFTYP  DS    CL1                                                              
DSEFILT  DS    0X                                                               
         ORG                                                                    
*                                                                               
*        DATA CODES-                                                            
*        X'01'=AGENCY                                                           
*        X'02'=MEDIA                                                            
*        X'03'=OFFICE                                                           
*        X'04'=MARKET GROUP                                                     
*        X'11'=CLT CODE                                                         
*        X'12'=CLT NAME                                                         
*        X'13'=CLT NUMBER                                                       
*        X'21'=PRD CODE                                                         
*        X'22'=PRD NAME                                                         
*        X'23'=PRD NUMBER                                                       
*        X'24'=DIVISION (FROM PRD HDR)                                          
*        X'31'=EST CODE                                                         
*        X'32'=EST NAME                                                         
*        X'33'=EST FILTERS                                                      
*        X'41'=INV NO(2)                                                        
*        X'42'=INV NO(4)                                                        
*        X'43'=INV NO(6)                                                        
*        X'44'=BILL RUN DATE                                                    
*        X'45'=INVOICE DATE                                                     
*        X'46'=DUE DATE                                                         
*        X'47'=MONTH OF SERVICE                                                 
*        X'48'=TODAY'S DATE                                                     
*        X'51'=GROSS                                                            
*        X'52'=NET                                                              
*        X'53'=ACTUAL                                                           
*        X'54'=AGYCOM (GROSS-NET)                                               
*        X'55'=AGYCOM (ACTUAL-NET)                                              
*        X'56'=ACTUAL-TAX                                                       
*        X'71'=REVISION STATUS                                                  
*        X'72'=AOR STATUS                                                       
*        X'73'=BILL TYPE                                                        
*        X'C0'=SPECIAL (JW)                                                     
*        X'C2'=SPECIAL (COMPTON NET/CABLE)                                      
*        X'C3'=SPECIAL (YNR MEDIA)                                              
*        X'C4'=SPECIAL (DDB AGENCY CODES)                                       
*        X'C5'=SPECIAL (DDB CLIENT ESTIMATES)                                   
*        X'B1'=FILTERS                                                          
*        X'F0'=ZEROS                                                            
*        X'F1'=LITERAL                                                          
*        X'F2'=SPACES                                                           
         SPACE 2                                                                
SPIN02   CSECT                                                                  
         EJECT                                                                  
         SPACE 2                                                                
INSPECS  DS    0X                                                               
*                                                                               
*        NOTE - $ FIELDS ARE SET TO PACKED HERE                                 
*        IN INOUT THEY WILL BE CONVERTED TO THEIR PROPER FORMAT                 
*        PACKED FIELDS ARE NEEDED HERE FOR ACCUMULATING $                       
*        FOR AN INVOICE WITH MORE THAT ONE MOS                                  
*                                                                               
         DSPEC 1,LITERAL='L'                                                    
**OLD *  DSPEC 8,6,LITERAL='631000'   DEFAULT ACCOUNT                           
         DSPEC 29,8,ACTUAL,FORMAT=PACKED                                        
         DSPEC 37,8,GST,FORMAT=PACKED                                           
         DSPEC 45,8,PST,FORMAT=PACKED                                           
*                                                                               
*        FORMAT OF EST USER 1 IS CCCC-NNNNN                                     
*        CCCC=COST CENTER, NNNNN=ORDER #                                        
*                                                                               
* NOTE-  INIPROC WILL BREAKOUT EUSER1 DATA                                      
*                                                                               
         DSPEC 160,32,PUSER1                                                    
         DSPEC 182,16,PUSER2                                                    
         DSPEC 198,32,EUSER1                                                    
         DSPEC 230,16,EUSER2                                                    
*                                                                               
*        FIELDS BELOW NEEDED FOR INVOICE HEADER INFO                            
*        H AND FIRST L RECORDS                                                  
*                                                                               
*        20 CENTURY ASSUMED (Y2K!)                                              
*                                                                               
         DSPEC 340,6,DUEDATE,FORMAT=MMDDYY                                      
         DSPEC 348,2,LITERAL='20'                                               
         DSPEC 350,6,INVDATE,FORMAT=YYMMDD                                      
         DSPEC 356,2,LITERAL='20'                                               
         DSPEC 358,6,RUNDATE,FORMAT=YYMMDD                                      
         DSPEC ROUTINE=INIPROC                                                  
         DC    X'0000'                                                          
         EJECT                                                                  
*        GSCA (GQ)                                                              
         PRINT OFF                                                              
         MACRO                                                                  
&NAME    AGSPC &AGENCY=,&MEDIA=ALL,&TAPE=,&BLKSIZE=,&LRECL=,           +        
               &SPECS=NONE,&ESTIMATE_AMOUNTS=NO,&RECFM=FB,&CLIENT=ALL           
         LCLC  &TC,&MED,&CTRL                                                   
         LCLA  &CTRL1,&CTRL2,&CTRL3,&CTRL4                                      
.*                                                                              
&CTRL1   SETA  0                                                                
&CTRL2   SETA  0                                                                
&CTRL3   SETA  0                                                                
&CTRL4   SETA  0                                                                
.*                                                                              
&TC      SETC  ' '                                                              
         AIF   (T'&TAPE EQ 'O').A10                                             
&TC      SETC  '&TAPE'                                                          
.*                                                                              
.A10     ANOP                                                                   
&MED     SETC  'Z'                                                              
         AIF   ('&MEDIA' EQ 'ALL').A15                                          
&MED     SETC  '&MEDIA'                                                         
.*                                                                              
.A15     ANOP                                                                   
&CLT     SETC  '   '                                                            
         AIF   ('&CLIENT' EQ 'ALL').A20                                         
&CLT     SETC  '&CLIENT'                                                        
.*                                                                              
.A20     AIF   ('&ESTIMATE_AMOUNTS' NE 'YES').A30                               
&CTRL1   SETA  &CTRL1+128                                                       
.*                                                                              
.A30     ANOP                                                                   
&RFM     SETC  'F'                                                              
         AIF   ('&RECFM' NE 'VB').A40                                           
&RFM     SETC  'V'                                                              
.*                                                                              
.A40     ANOP                                                                   
&NAME    DC    C'&AGENCY&MED&TC',AL2(&BLKSIZE,&LRECL)                           
         DC    C'&RFM'                                                          
         AIF   ('&SPECS' NE 'NONE').A50                                         
         DC    AL3(0)                                                           
         AGO   .A60                                                             
.A50     DC    AL3(&SPECS)                                                      
.A60     DC    AL1(&CTRL1,&CTRL2,&CTRL3,&CTRL4)                                 
         DC    CL3'&CLT'                                                        
         SPACE 1                                                                
         MEXIT                                                                  
         MEND                                                                   
         EJECT                                                                  
         PRINT ON                                                               
AGYTABD  DSECT                                                                  
AGYTAGY  DS    CL2                 AGENCY CODE                                  
AGYTMED  DS    C                   MEDIA (C'Z' = ALL)                           
AGYTAPE  DS    C                   TAPE CODE (C' ' = NONE)                      
AGYTBLKS DS    AL2                 BLKSIZE                                      
AGYTLRCL DS    AL2                 LRECL                                        
AGYRECFM DS    C                   'F' = FB, 'V' = VB                           
AGYTSPCS DS    AL3                 A(DATA SPECS)                                
AGYTCTRL DS    XL4                 SPECIAL CONTROLS                             
*                                    CNTRL1 X'80' = 'ESTIMATE AMOUNTS'          
*                                               (MCANN- ATT)                    
AGYTCLT  DS    CL3                 CLIENT (C'   ' = ALL)                        
AGYTABL  EQU   *-AGYTABD                                                        
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
*********************************************************************           
* DSECT TO COVER STARCOM CLIENT ACCOUNT CODES                                   
*********************************************************************           
                                                                                
H7ACCD   DSECT                                                                  
H7CLI    DS    CL3                 CLIENT                                       
H7FUND   DS    CL3                 FUND                                         
H7GLSR   DS    CL5                 G/L ACCOUNT RADIO                            
H7GLSX   DS    CL5                             NETWORK RADIO                    
H7GLST   DS    CL5                             TV                               
H7GLNN   DS    CL5                             NETWORK TV                       
H7GLPO   DS    CL5                             OUTDOOR                          
H7GLPM   DS    CL5                             MAGAZINE/NEWSPAPER               
H7GLCOM  DS    CL5                             COMMISSION                       
H7AGID   DS    CL5                 AGENCY ID                                    
H7AGNME  DS    AL3                 AGENCY NAME                                  
H7ACLNQ  EQU   *-H7ACCD                                                         
         EJECT                                                                  
SPIN02   CSECT                                                                  
AGYTAB   DS    0D                                                               
                                                                                
*                               OMDTOA - NISSAN CANADA                          
         AGSPC AGENCY=OU,BLKSIZE=112,LRECL=112,SPECS=INSPECS,TAPE=N             
                                                                                
*                               OMDUSEC- NISSAN  USA                            
         AGSPC AGENCY=OO,BLKSIZE=112,LRECL=112,SPECS=INSPECS,TAPE=N             
                                                                                
*                               SJR TEST  - NISSAN                              
         AGSPC AGENCY=SJ,BLKSIZE=112,LRECL=112,SPECS=INSPECS,TAPE=N             
                                                                                
*                                                                               
         DC    X'FFFF'             END-OF-TABLE                                 
*                                                                               
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R7,24,ELCODE                                                     
         PRINT NOGEN                                                            
         SPACE 3                                                                
INVTAB   DS    0D                                                               
         ORG   *+(INVMAX*3)                                                     
         DC    X'00'                                                            
         EJECT                                                                  
BLINED   DSECT                                                                  
BLINE    DS    0CL132                                                           
         DS    CL3                                                              
BLPRD    DS    CL3                                                              
         DS    CL1                                                              
BLPNUM   DS    CL5                                                              
         DS    CL1                                                              
BLEST    DS    CL3                                                              
         DS    CL1                                                              
BLPER    DS    CL6                                                              
         DS    CL1                                                              
BLINVNO  DS    CL7                                                              
         DS    CL1                                                              
BLRUND   DS    CL8                                                              
         DS    CL1                                                              
BLINVD   DS    CL8                                                              
         DS    CL1                                                              
BLDUED   DS    CL8                                                              
         DS    CL1                                                              
BLTYPE   DS    CL3                                                              
BLGROSS  DS    CL14                                                             
BLACTUAL DS    CL14                                                             
BLNET    DS    CL14                                                             
BLAC     DS    CL14                                                             
BLGST    DS    CL14                                                             
         ORG   BLGST+132           PST ON 2ND LINE                              
BLPST    DS    CL14                                                             
*                                  FOR NON-CANADIAN AGENCIES, GST/PST           
*                                  ISN'T NEEDED, SO WIDEN $ COLUMNS             
         ORG   BLGROSS                                                          
         DS    C                                                                
BLGRSWID DS    CL15                GROSS                                        
         DS    C                                                                
BLACTWID DS    CL15                ACTUAL                                       
         DS    C                                                                
BLNETWID DS    CL15                NET                                          
         DS    C                                                                
BLACWIDE DS    CL15                AGENCY COMMISSION                            
         DS    CL6                 SPARE                                        
         ORG                                                                    
*                                                                               
SPINWRKD DSECT                                                                  
*                                                                               
VSPFMTIN DS    V                   V(SPFMTINO)                                  
MVOFFICE DS    V                   V(OFFICER)                                   
ASPECS   DS    A                                                                
ATOTS    DS    A                                                                
AG7MED   DS    A                                                                
ASCMED   DS    A                                                                
AINKEY   DS    A                                                                
AMCMED   DS    A                                                                
ATPFMT   DS    A                                                                
*                                                                               
EATOTGRS DS    PL6                                                              
EATOTAC  DS    PL6                                                              
EATOTACT DS    PL6                                                              
EATOTNET DS    PL6                                                              
*                                                                               
RECLEN   DS    H                                                                
OMDUSASW DS    CL1                 Y=OMDUSA                                     
ELCODE   DS    X                                                                
MYDUB    DS    PL8                                                              
BIGDUB   DS    PL12                                                             
PL16     DS    PL16                                                             
OPENSW   DS    C                                                                
RETAIL   DS    C                                                                
REVSW    DS    C                   REVISION SWITCH                              
BYTE2    DS    X                                                                
FISCAL   DS    CL4                                                              
LSTBLKY  DS    XL13                LAST BILL KEY                                
B1XPROF  DS    CL16                                                             
B1PROF   DS    CL16                                                             
SV00APRF DS    CL16                                                             
DINVMED  DS    CL2                 INVOICE MEDIA PART                           
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                FULL FORMAT INVOICE NUMBER                   
CONTROLS DS    0XL4                                                             
CONTROL1 DS    X                                                                
CONTROL2 DS    X                                                                
CONTROL3 DS    X                                                                
CONTROL4 DS    X                                                                
OFFICE   DS    CL1                                                              
SAVCOFF  DS    CL2                 OFFICER OUTPUT                               
SVQST    DS    CL6                                                              
SVQEND   DS    CL6                                                              
STARTMOS DS    XL2                 START MOS FILTER (BINARY YM)                 
ENDMOS   DS    XL2                 END MOS FILTER (BINARY YM)                   
CNTRY    DS    C                                                                
TESTMQ   DS    C                                                                
SCSFTP   DS    C                                                                
INSFTP   DS    C                                                                
BKSFTP   DS    C                   Y=DOING BURGER KING SFTP                     
*                                                                               
BKFISCAL DS    CL4                 FISCAL YEAR                                  
BKBYR    DS    CL4                 BILLING MOS - YEAR                           
BKBMTH   DS    CL2                 BILLING MOS - MONTH                          
*                                  FOR MINDSHARE BURGER KING MQ MSG             
CTODAY   DS    CL8                 YYYYMMDD                                     
MCTODAY  DS    CL10                MM/DD/YYYY                                   
TIMEOFD  DS    CL8                 HH.MM.SS                                     
TIMEOFDF DS    CL6                 HHMMSS                                       
TODAYC   DS    XL2                 TODAY COMPRESSED                             
SVMID    DS    CL132                                                            
ELEM     DS    CL200                                                            
*                                                                               
MQMAPNM  DS    CL14                SFTPDISK.PROD.                               
*                                                                               
DSNAME   DS    CL35  DSN -  BIL.SYS.AGID.DYYYMMDD.THHMMSS                       
*                    FOR MINDSHARE BURGER KING AGID = H7BK                      
*                                                                               
NEWCLT   DS    C                                                                
NETPAKSW DS    C                                                                
SKIPBILL DS    X                                                                
JWOASW   DS    C                                                                
YEARDIG  DS    XL1                 YEAR DIGIT                                   
DECADE   DS    XL1                                                              
WIAPLSW  DS    C                   WESTERN/APL SWITCH                           
WIQASW   DS    C                   WESTERN/APL QA SWITCH                        
WAPLBTSW DS    C                   YET ANOTHER MODE                             
*                                                                               
BKCOM    DS    PL6                 COMMISSION AMOUNT                            
BKACCT   DS    CL10                BURGER KING ACCOUNT                          
BKCOMACC DS    CL10                BURGER KING COMMISSION ACCOUNT               
BKCOSTC  DS    CL10                BURGER KING COST CENTER                      
BKMAR#   DS    CL23                BURGER KING MAR NUMBER                       
*                                                                               
G7SW     DS    CL1                 SET TO 1 FOR GSTX REQS                       
MCCSW    DS    CL1                 SET TO 1 FOR MCCANN + TYPE C                 
SCSW     DS    CL1                 SET TO 1 FOR MSNY + TYPE S                   
SCLMOS   DS    XL2       YM        LOWEST MOS ON THE FILE                       
SCHMOS   DS    XL2       YM        HIGHEST MOS ON THE FILE                      
*                                                                               
INSW     DS    CL1                 SET TO 1 FOR OMD + TYPE N                    
*                                                                               
SVQOPT3  DS    CL1                 SAVED QOPT3                                  
*                                                                               
CLFILT   DS    CL3                                                              
MEFILT   DS    CL1                                                              
*                                                                               
SAVER1   DS    F                                                                
SAVER4   DS    F                                                                
MYFULL   DS    F                                                                
*                                                                               
LOWINV   DS    H            LOW INVOICE NUMBER FILTER                           
HIINV    DS    H            HI INVOICE NUMBER FILTER                            
*                                                                               
INVPARS  DS    6F                  INVOICE BINSRCH PARS                         
*                                                                               
INVMAX   EQU   100000              MAX INVOICES PER CLT (WAS 50000)             
*                                                                               
EOT      EQU   X'FF'                                                            
*                                                                               
GBINVALS DS    0F                                                               
GBPAR1   DS    XL4                ADDRESS OF RECORD                             
*                                 HIGH ORDER BYTE IS ACTION X'01'=ADD           
GBPAR2   DS    A                  ADDRESS OF TABLE WHERE REC IS TO BE           
G7RECNT  DS    F                  NUMBER OF RECORDS ADDED                       
GBPAR4   DS    F                  LEN OF RECORD                                 
GBPAR5   DS    F                  KEY SIZE                                      
GBPAR6   DS    F                  MAX NUMBER OF RECORDS                         
*                                                                               
AOFG7T   DS    A                  ADDRESS OF G7TABLE                            
*                                                                               
G7KEY    DS    0XL12                                                            
G7MED    DS    CL1                                                              
G7CLI    DS    CL3                                                              
G7INUMB  DS    0XL3                                                             
G7INVMO  DS    XL1           INVOICE MONTH                                      
G7INVN   DS    XL2           INVOICE NUMBER                                     
G7PRO    DS    CL3                                                              
G7EST    DS    XL2                                                              
*            ______                                                             
*              12                                                               
G7REC    DS    CL200                                                            
ENDG7R   DS    0C                                                               
*                                                                               
G7COUNT  DS    PL3                                                              
*                                                                               
*        SC JOHNSON INTERFACE (H7)                                              
*                                                                               
SCINVALS DS    0F                                                               
SCPAR1   DS    XL4                ADDRESS OF RECORD                             
*                                 HIGH ORDER BYTE IS ACTION X'01'=ADD           
SCPAR2   DS    A                  ADDRESS OF TABLE WHERE REC IS TO BE           
SCRECNT  DS    F                  NUMBER OF RECORDS ADDED                       
SCPAR4   DS    F                  LEN OF RECORD                                 
SCPAR5   DS    F                  KEY SIZE                                      
SCPAR6   DS    F                  MAX NUMBER OF RECORDS                         
*                                                                               
AOFSCT   DS    A                  ADDRESS OF SCTABLE                            
*                                                                               
SCKEY    DS    0XL12                                                            
SCMED    DS    CL1                                                              
SCCLI    DS    CL3                                                              
SCINUMB  DS    0XL3                                                             
SCINVMO  DS    XL1           INVOICE MONTH                                      
SCINVN   DS    XL2           INVOICE NUMBER                                     
SCPRO    DS    CL3                                                              
SCEST    DS    XL2                                                              
*            ______                                                             
*              12                                                               
SCREC    DS    CL200                                                            
ENDSCR   DS    0C                                                               
*                                                                               
SCCOUNT  DS    PL3                                                              
*                                                                               
ININVALS DS    0F                                                               
INPAR1   DS    XL4                ADDRESS OF RECORD                             
*                                 HIGH ORDER BYTE IS ACTION X'01'=ADD           
INPAR2   DS    A                  ADDRESS OF TABLE WHERE REC IS TO BE           
INRECNT  DS    F                  NUMBER OF RECORDS ADDED                       
INPAR4   DS    F                  LEN OF RECORD                                 
INPAR5   DS    F                  KEY SIZE                                      
INPAR6   DS    F                  MAX NUMBER OF RECORDS                         
*                                                                               
AOFINT   DS    A                  ADDRESS OF INTABLE                            
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
*                                                                               
MBINVALS DS    0F                                                               
MCPAR1   DS    XL4                ADDRESS OF RECORD                             
*                                 HIGH ORDER BYTE IS ACTION X'01'=ADD           
MCPAR2   DS    A                  ADDRESS OF TABLE WHERE REC IS TO BE           
MCRECNT  DS    F                  NUMBER OF RECORDS ADDED                       
MCPAR4   DS    F                  LEN OF RECORD                                 
MCPAR5   DS    F                  KEY SIZE                                      
MCPAR6   DS    F                  MAX NUMBER OF RECORDS                         
*                                                                               
AOFMCT   DS    A                  ADDRESS OF MCTABLE                            
*                                                                               
MCKEY    DS    0XL12                                                            
MCMED    DS    CL1                                                              
MCCLI    DS    CL3                                                              
MCINUMB  DS    0XL3                                                             
MCINVMO  DS    XL1           INVOICE MONTH                                      
MCINVN   DS    XL2           INVOICE NUMBER                                     
MCPRO    DS    CL3                                                              
MCEST    DS    XL2                                                              
*            ______                                                             
*              12                                                               
MCREC    DS    CL341                                                            
ENDMCR   DS    0C                                                               
*                                                                               
MCCOUNT  DS    PL3                                                              
*                                                                               
CLTZINV  DS    F                   CLIENT ZERO INVOICES                         
CLTINVS  DS    F                   CLIENT INVOICES                              
OFFINVS  DS    F                   OFFICE INVOICES                              
REQINVS  DS    F                   REQUEST INVOICES                             
RUNINVS  DS    F                   RUN INVOICES                                 
*                                                                               
BAMTS    DS    (NAMTS)PL6          BILL TOTALS                                  
SVBAMTS  DS    (NAMTS)PL6          SAVED TOTALS                                 
PAMTS    DS    (NAMTS)PL6          PRODUCT TOTS                                 
CAMTS    DS    (NAMTS)PL6          CLIENT                                       
OAMTS    DS    (NAMTS)PL6          OFFICE                                       
RQAMTS   DS    (NAMTS)PL6          REQUEST                                      
RAMTS    DS    (NAMTS)PL6          RUN                                          
MYGST    DS    PL6                 SAVE CANADIAN TAXES                          
MYHST    DS    PL6                                                              
MYPST    DS    PL6                                                              
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
         EJECT                                                                  
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
SVBILL   DS    XL256                                                            
*                                                                               
TPRECRDW DS    F                   RDW (FOR VARIABLE LENGTH RECORDS)            
TPREC    DS    XL1200                                                           
         ORG   TPREC+600                                                        
SVTPREC  DS    XL600                                                            
*                                                                               
******   VERY IMPORTANT NOTE  ******                                            
*        DO NOT USE SVTPREC WHEN YOUR LRECL IS OVER 600                         
*        DO NOT TRY TO EXPAND LENGTH OF SVTPREC                                 
*                                                                               
*        CURRENTLY ONLY IPGSPECS FOR SEVERAL AGENCIES                           
*        HAS A LRECL OVER 600 - 1150                                            
*        IT DOES NOT USE SVTPREC                                                
*                                                                               
         SPACE 3                                                                
AMOUNTSD DSECT                                                                  
AMTGRS   DS    PL6                 GROSS                                        
AMTAC    DS    PL6                 ACTUAL COMMISSION                            
AMTACT   DS    PL6                 ACTUAL                                       
AMTNET   DS    PL6                 NET                                          
AMTCNT   DS    PL6                 COUNT                                        
AMTGST   DS    PL6                 GST                                          
AMTPST   DS    PL6                 PST                                          
AMTHST   DS    PL6                 HST                                          
NAMTS    EQU   (*-AMOUNTSD)/6                                                   
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         ORG   QAREA+49                                                         
QMOS     DS    0CL8                REQUESTED MONTH-OF-SERVICE RANGE             
QMOSSTRT DS    CL4                 START MOS (YYMM)                             
QMOSEND  DS    CL4                 END MOS (YYMM)                               
         ORG   Q2USER                                                           
*                                  QAREA2 COL 21-COL 28                         
QINVNO1  DS    CL4                 START INVOICE #                              
QINVNO2  DS    CL4                 END INVOICE #                                
         ORG                                                                    
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAOR                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE DDUCOMD                                                        
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDGETPROFD                                                     
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NECOMBLOK                                                      
         PRINT ON                                                               
*                                                                               
INTABLE  CSECT                                                                  
***      DS    1500CL390                                                        
         DS    1500CL(INRECLEN)                                                 
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREPIN02 01/07/19'                                      
         END                                                                    
