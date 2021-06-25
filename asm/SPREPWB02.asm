*          DATA SET SPREPWB02  AT LEVEL 014 AS OF 08/12/16                      
*PHASE SPWB02A                                                                  
*INCLUDE DDUCOM                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'SPWB02 - SPOTPAK WARNER BROS. INTERFACE'                        
                                                                                
***********************************************************************         
*  SPECIAL NOTES:  ONLY ONE CLIENT SHOULD BE REQUESTED                          
*                  PER JOB STREAM (ID)                                          
*                  AS THE TRANSMISSION FILE NAME                                
*                  IS ONLY SENT ONCE AND THE NAME MIGHT BE                      
*                  DIFFERENT BY CLIENT.                                         
*                                                                               
***********************************************************************         
*  QOPT1 -   N = NO TAPE                                                        
*  QOPT2 -   TAPE SPEC CODE                                                     
*  QOPT3 -   Y = PRINT TAPE RECORD TRACE                                        
*  QOPT4 -   M=START-END DATES ARE MOS                                          
*  QOPT5 -   C = DO ONLY COMMISSION ONLY BILLS, AND PRESERVE NET                
*            A = AOR ONLY, B=AOR AND AOR/CLIENT, X = NON-AOR ONLY               
*            2 = USE COS2 BILLS                                                 
*  QOPT5+1 - * = SHOW NETPAK SUB-MED INSTEAD OF CLIENT NUMBER                   
*            X = FILTER ON NETPAK SUB-MED=X                                     
*                                                                               
*  QOPT5+2   B = SUPPRESS BDE HEADERS                                           
*                                                                               
***********************************************************************         
* USER     JIRA       DATE                  CHANGE LOG                *         
* ---- ------------- ------- -----------------------------------------*         
* AKAT SPEC-4880     8/10/16 NEED CHANGES TO WB BILLING FILE FOR OMG  *         
* AKAT SPEC-2406     8/10/16 NEED CHANGES TO WB BILLING FILE FOR OMD  *         
* AKAT CUSTENH-33280 3/28/16 OMG REQUIRES GROSS                       *         
***********************************************************************         
         PRINT NOGEN                                                            
SPWB02   CSECT                                                                  
         NMOD1 0,SPWB02,R6,R8                                                   
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SPWBWRKD,RC                                                      
*                                                                               
         RELOC RELO                                                             
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
         MVI   ERRORSW,0                                                        
         XC    RUNINVS,RUNINVS     CLEAR RUN INVOICE TOTALS                     
*                                                                               
         MVI   DOWNACT,C'N'                                                     
         LA    R3,RAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         MVI   DHEADSW,C'N'        SET HEADER NOT SENT                          
         ZAP   WBINVS,=P'0'        CLEAR TOTALS FOR END OF FILE HEADER          
         ZAP   WBACT,=P'0'                                                      
         ZAP   WBNET,=P'0'                                                      
         ZAP   WBGROSS,=P'0'                                                    
         ZAP   WBTAX,=P'0'                                                      
         ZAP   WBAGC,=P'0'                                                      
         ZAP   WBCTAX,=P'0'                                                     
*                                                                               
*        BUILD BDE HEADERS                                                      
*        WILL BE SENT WITH FILE TO THE PRINTQUE                                 
*                                                                               
         MVC   BDEH1,SPACES                                                     
         MVC   BDEH2,SPACES                                                     
         MVC   BDEH3,SPACES                                                     
         MVC   BDEH4,SPACES                                                     
         MVC   BDEH1+4(17),=C'*HDR*EDICT=OMGXWD'                                
         MVI   BDEH1+34,C'W'      FOR WIDE                                      
         MVC   BDEH2(5),=C'++DDS'                                               
         MVC   BDEH2+6(08),=C'SPBWBTRN'                                         
         MVC   BDEH3(5),=C'++DDS'                                               
         MVC   BDEH3+11(03),=C'SUB'                                             
         MVC   BDEH3+15(17),=C'WARNER BROS. FILE '                              
         MVC   BDEH4(5),=C'++DDS'                                               
         MVC   BDEH4+11(03),=C'FIL'                                             
*******  MVC   BDEH4+15(17),=C'F_XN_AGENCY_TEST_'                               
         MVC   BDEH4+15(5),=C'F_XN_'                                            
*                                                                               
         MVI   BDEHSW,0         SET HEADERS NOT SENT                            
         B     EXIT                                                             
         SPACE 3                                                                
*        REQUEST FIRST                                                          
         SPACE 2                                                                
REQF     DS    0H                                                               
*                                                                               
         L     RF,=V(DLFLD)                                                     
         A     RF,RELO                                                          
         ST    RF,VDLFLD                                                        
         L     RF,=V(DDUCOM)                                                    
         A     RF,RELO                                                          
         ST    RF,VDDUCOM                                                       
         L     RF,=A(DOWNLD)                                                    
         A     RF,RELO                                                          
         ST    RF,VDOWNLD                                                       
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+8                                                              
         MVI   NETPAKSW,C'N'                                                    
******   MVC   VREMOTEC,MCVREMOT                                                
         DROP  RF                                                               
*                                                                               
******   USING REMOTED,R1                                                       
******   L     R1,VREMOTEC                                                      
******   MVC   REMOTJID,=C'SWB'                                                 
******   CLI   NETPAKSW,C'Y'                                                    
******   BNE   *+8                                                              
******   MVI   REMOTJID,C'N'      NWB                                           
******   DROP  R1                                                               
*                                                                               
         CLI   QOPT5+2,C'B' SEE IF SUPPRESSING BDE FILE HEADERS                 
         BNE   *+8          (IF TRANSFER NOT READY)                             
         MVI   BDEHSW,1      SO PROGRAM WILL THINK THEY'RE ALREADY SENT         
*                            AND NOT SEND THEM                                  
*                                                                               
*        FINISH FIXING BDE HEADERS                                              
*                                                                               
******   CLI   NETPAKSW,C'Y'                                                    
******   BNE   *+16                                                             
******   MVC   BDEH2+6(2),=C'NE'   NETPAK BDE                                   
******   MVC   BDEH4+15(3),=C'NET'                                              
******                                                                          
******   GOTO1 DATCON,DMCB,TODAY,(5,BDEH4+19)                                   
******   L     RF,ADAGY                                                         
******   MVC   BDEH4+28(L'AGYNAME),AGYNAME-AGYHDR(RF)                           
*                                  SET OFF VARIOUS WESTERN/APL SWITCHES         
*                                                                               
REQF1J   DS    0H                                                               
         XC    REQINVS,REQINVS   CLEAR REQUEST                                  
         XC    OFFINVS,OFFINVS   OFFICE                                         
         XC    CLTINVS,CLTINVS   AND CLIENT INVOICE TOTALS                      
*                                                                               
         XC    PPGRECNT,PPGRECNT    CLEAR BINSRCH COUNTER                       
*                                                                               
         L     RF,ADAGY                                                         
         MVC   CNTRY,AGYPROF+7-AGYHDR(RF)   SET COUNTRY                         
         MVI   RCSUBPRG,0                                                       
         CLI   CNTRY,C'C'          CANADA GETS DIFFERENT SPROG                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,50         FOR GST                                      
         L     RF,=A(HHROUT)                                                    
         ST    RF,HEADHOOK                                                      
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
         GOTO1 VDOWNLD,DMCB,(RA)   INITIALIZE DOWNLOAD                          
         MVI   DOWNACT,C'Y'                                                     
         B     REQF16                                                           
*                                                                               
REQF10   DS    0H                                                               
         MVC   P(38),=C'**NO TAPE SPECS FOUND - REQ BYPASSED**'                 
         BRAS  RE,PRNT                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
*                                                                               
*                                                                               
REQF13X  DS    0H                                                               
         L     RE,=A(MULTMSG)                                                   
         MVC   P(52),0(RE)                                                      
         BRAS  RE,PRNT                                                          
         GOTO1 AENDREQ                                                          
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
         XC    OFFICE,OFFICE       SET NO OFFICE                                
*                                  CLEAR SAVED TAPE REC AREA                    
         XC    SVBILL,SVBILL       AND SAVED BILL AREA                          
         LA    R3,SVBAMTS          AND SAVED BILL VALUES                        
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        FIRST FOR OFFICE                                                       
         SPACE 2                                                                
FBILO    DS    0H                                                               
*                                                                               
         XC    CLTINVS,CLTINVS     CLEAR CLIENT                                 
         XC    OFFINVS,OFFINVS     AND OFFICE INVOICE TOTALS                    
*                                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'S'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,SVAGY                                                
         MVC   OFFD.OFCOFC,COFFICE-CLTHDR(RF)                                   
*                                                                               
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         L     RF,VOFFICER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(C'2',WORK),(0,ACOMFACS)                               
         CLI   0(R1),0                                                          
         BNE   FBILOX                                                           
         MVC   OFFICE,OFFD.OFCOFC2                                              
*                                                                               
         DROP  OFFD                                                             
*                                                                               
FBILOX   MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 3                                                                
*        FIRST FOR CLI                                                          
         SPACE 2                                                                
FBILC    DS    0H                                                               
         BAS   RE,GETVEN                                                        
         CLI   SAVBDEH,C'N'      NO BDE HEADERS?                                
         BNE   *+8                                                              
         MVI   BDEHSW,1          SO THEY WON'T GET SENT                         
*                                                                               
*        FINISH FIXING BDE HEADERS  - FILE NAME                                 
*                                                                               
         MVC   BDEH4+20(22),SAVFILE                                             
*                                                                               
FBILC3G  DS    0H                                                               
*                                                                               
FBILC7B  DS    0H                                                               
         SR    R0,R0               SET INVOICE LIST BINSRCH PARS                
         L     R1,=A(INVTAB)                                                    
         SR    R2,R2                                                            
         LHI   R3,3                                                             
         LHI   R4,0                                                             
         L     R5,=A(INVMAX)                                                    
         STM   R0,R5,INVPARS                                                    
         XC    CLTINVS,CLTINVS                                                  
*                                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,CLRTOTS                                                       
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
*    NOW ALWAYS PASS OFFICE DATA                                                
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
*    NOW ALWAYS PASS OFFICE DATA                                                
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
*                                                                               
LBILC5   LA    R3,CAMTS                                                         
         LA    R4,OAMTS            ROLL TO OFFICE TOTS                          
         CLI   QCLT,C'$'           IF IN OFFICE LIST MODE                       
         BE    *+8                                                              
         LA    R4,RQAMTS           ELSE TO REQUEST TOTS                         
         BAS   RE,PBROLL                                                        
*                                                                               
         MVC   CLTINVS,INVPARS+8   INVOICE COUNT                                
         L     R0,OFFINVS                                                       
         LA    RE,OFFINVS                                                       
         CLI   QCLT,C'$'           IF IN OFFICE LIST MODE                       
         BE    *+12                                                             
         L     R0,REQINVS                                                       
         LA    RE,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,0(RE)                                                         
*                                                                               
         BRAS  RE,PRNT                                                          
         MVC   P+10(19),=C'** CLIENT TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  CLTINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         XC    CLTINVS,CLTINVS                                                  
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
*                                                                               
         L     R2,PPGRECNT                                                      
         LTR   R2,R2                                                            
         BZ    LREQC5                                                           
*                                                                               
*        I HAVE SOMETHING TO SEND                                               
*                                                                               
         CLI   BDEHSW,1           BDE HEADERS ALREADY SENT?                     
         BE    LREQC3                                                           
*                                                                               
         MVC   P,BDEH1                                                          
         MVC   P2,BDEH2                                                         
         MVC   P3,BDEH3                                                         
         MVC   P4,BDEH4                                                         
*                                                                               
         MVC   SVLINE,LINE                                                      
         MVC   SVFORCEH,FORCEHED                                                
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
         MVC   LINE,SVLINE             RESTORE LINE                             
         MVC   FORCEHED,SVFORCEH       AND FORCEHED                             
         MVI   RCWHATPR,1     RESET TO FIRST                                    
         MVI   BDEHSW,1           SET BDE HEADERS SENT                          
*                                                                               
LREQC3   L     R3,AOFPPGT         SEND ENTRIES TO DOWNLOAD                      
         LA    R3,L'PPGKEY(R3)     PAST PSUEDO KEY                              
LREQC4   GOTO1 VDOWNLD,DMCB,(RA),(R3)                                           
         LA    R3,PPGTLEN(R3)      NEXT ENTRY                                   
         BCT   R2,LREQC4                                                        
*                                                                               
LREQC5   DS    0H                                                               
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
         CLI   ERRORSW,0        ANY ERRORS?                                     
         BE    RUNL10                                                           
         MVC   P+1(36),=C'*** ERRORS HAVE BEEN ENCOUNTERED ***'                 
         BRAS  RE,PRNT                                                          
         TM    ERRORSW,X'01'     ANY MISSING ACCOUNT NUMBERS?                   
         BNO   RUNL3                                                            
         MVC   P+3(31),=C'*** MISSING ACCOUNT NUMBERS ***'                      
         BRAS  RE,PRNT                                                          
RUNL3    TM    ERRORSW,X'02'     ANY MISSING INTERNAL ORDER NOS.                
         BNO   RUNL4                                                            
         MVC   P+3(35),=C'*** MISSING INTERNAL ORDER NOS. ***'                  
         BRAS  RE,PRNT                                                          
RUNL4    TM    ERRORSW,X'04'     ANY MISSING LEGAL ENTITIES                     
         BNO   RUNL5                                                            
         MVC   P+3(30),=C'*** MISSING LEGAL ENTITIES ***'                       
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL5    TM    ERRORSW,X'08'   MIXED LEGAL ENTITES ON AN INVOICE                
         BNO   RUNL6                                                            
         MVC   P+3(42),=C'*** INVOICES WITH MIXED LEGAL ENTITIES ***'           
         BRAS  RE,PRNT                                                          
RUNL6    TM    ERRORSW,X'80'   MISSING POL ESTIMATE                             
         BNO   RUNL8                                                            
         MVC   P+3(29),=C'*** MISSING POL ESTIMATES ***'                        
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL8    DS    0H                                                               
         MVC   P+1(35),=C'**WARNING -OUTPUT FILE HAS ERRORS**'                  
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL10   CLI   DOWNACT,C'Y'                                                     
         BNE   EXIT                                                             
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         B     EXIT                                                             
         EJECT                                                                  
*        PROCESS BILL  **NOTE- BILL RECORD NOT READ YET**                       
         SPACE 2                                                                
PRBL     DS    0H                                                               
         MVI   SKIPBILL,0                                                       
         CLI   KEY+BKEYEST-BKEY,0  SKIP EST 0 BILLS (BILLING BUG)               
         BE    EXIT                                                             
         CLC   KEY(10),LSTBLKY     IF FIRST FOR EST/MOS                         
         BE    PRB1                                                             
         MVC   SVKEY,KEY           MUST SAVE KEY AND KEYSAVE                    
         BAS   RE,PUGTPOLE         MUST READ POL EST                            
         MVC   KEY(64),SVKEY       RESTORE                                      
         BE    PRB0                                                             
         OI    ERRORSW,X'80'                                                    
         MVC   P+3(32),=C'*** POL ESTIMATE NOT ON FILE ***'                     
         BRAS  RE,PRNT                                                          
*                                  TO USE ITS USER FIELDS                       
PRB0     GOTO1 HIGH                RESTORE SEQ READ                             
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
         BNZ   *+12                YES, OK                                      
         CLI   QOPT5,C'C'          NO, TEST TO SKIP OTHERS                      
         BE    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'A'          AOR BILLS ONLY?                              
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BZ    EXIT                                                             
         CLI   QOPT5,C'B'          AOR AND AOR/CLIENT BILLS                     
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ+BSTCAORQ                                        
         BZ    EXIT                                                             
         CLI   QOPT5,C'X'          NON-AOR BILLS ONLY?                          
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BNZ   EXIT                                                             
         CLC   =C'*SOON',QUESTOR   IF WAS AUTOREQUESTED BY SOON                 
         BNE   PB6                                                              
         TM    BILSTAT3,BSTSOONQ   WAS REC GENERATED BY SOON ?                  
         BZ    EXIT                                                             
         CLC   BILLUID,RCORIGID    PROCESS ONLY FOR REQUESTING USER ID          
         BNE   EXIT                                                             
*                                  NB- POST TO 'ESTIMATE' AMTS                  
*                                  WHETHER PASSES DATE FILTERS OR NOT           
PB6      DS    0H                  SET BILL AMOUNTS                             
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
*                                                                               
         LA    R3,BAMTS            CLEAR BILL AMOUNTS                           
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         USING AMOUNTSD,R3                                                      
         ZAP   AMTGRS,SPBVGRSP     EFFECTIVE GROSS                              
         ZAP   AMTACT,SPBVACTP     ACTUAL                                       
         ZAP   AMTNET,SPBVNETP     EFFECTIVE NET                                
*                                                                               
         L     R0,SPBVETAX         US TAX                                       
         CVD   R0,DUB              CONVERT TO DECIMAL                           
         ZAP   AMTTAX,DUB          US TAX                                       
*                                                                               
         L     R0,SPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTGST,DUB          GST                                          
         L     R0,SPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTPST,DUB          PST'S (INCL HSTS)                            
         L     R0,SPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTHST,DUB          HST'S ALONE                                  
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
         BZ    *+18                                                             
         CLI   QOPT5,C'C'          TEST LEAVE NET                               
         BE    *+10                YES                                          
         ZAP   AMTNET,=P'0'        ELSE CLEAR NET (AC = RCVBL)                  
*                                                                               
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
         MVI   SPACING,2                                                        
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
PB33D    DS    0H                                                               
         MVC   DINVNO,SPACES                                                    
         MVC   WBINV,SPACES                                                     
         L     RF,ADCONLST                                                      
         L     RF,VSPFMINO-SPADCONS(,RF)   A(SPFMTINO)                          
         GOTO1 (RF),DMCB,(C'B',BDATE),(6,BINVNO),(QMED,B1PROF),B1XPROF,X        
               ADBILL                                                           
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)      FULL FORMAT INVOICE NUMBER                   
*                                                                               
         MVC   WBINV,DINVFULL                                                   
         B     PB33X                                                            
*                                                                               
*        CODE BELOW WILL STRIP OFF DASHES                                       
*        THEY CHANGED THEIR MIND AND NOW WANT DASHES                            
*                                                                               
**       LA    RF,DINVFULL                                                      
**       LA    RE,WBINV            WBINV - NO DASHES                            
**       LHI   R0,10                                                            
**33I    CLI   0(RF),C'-'                                                       
**       BNE   PB33J                                                            
**       LA    RF,1(RF)            SKIP DASHES                                  
**       B     PB33K                                                            
**33J    MVC   0(1,RE),0(RF)                                                    
**       LA    RF,1(RF)                                                         
**       LA    RE,1(RE)                                                         
**33K    BCT   R0,PB33I                                                         
**                                                                              
PB33X    L     RF,DMCB+4           FORMAT MN-NNNN (SHORT INVOICE NO.)           
         LA    RE,DINVNO                                                        
         LHI   R0,7                                                             
                                                                                
PB34     DS    0H                                                               
         CLI   0(RF),C'-'                                                       
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PB34                                                          
                                                                                
         MVC   BLINVNO(2),DINVNO                                                
         MVI   BLINVNO+2,C'-'                                                   
         MVC   BLINVNO+3(4),DINVNO+2                                            
*                                                                               
         CLI   CNTRY,C'C'          IF CANADA                                    
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
PS40     DS    0H                                                               
         BAS   RE,POSTB           POST TO TABLE                                 
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
PSX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        PUGTPOLE - PRTUSER, GET POL ESTIMATE HEADER                            
*        READ INTO ADBUY                                                        
*                                                                               
PUGTPOLE NTR1                                                                   
         XC    UCOMDATA,UCOMDATA   CLEAR UCOMM DATA                             
*                                                                               
         L     RF,ADEST                                                         
         MVC   KEY,0(RF)                                                        
         L     R7,ADBUY                                                         
         ST    R7,AREC                                                          
         MVC   KEY+EKEYPRD-ESTHDR(3),=C'POL'                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NEQXIT                                                           
         GOTO1 GET                                                              
         CLI   DMCB+8,0                                                         
         BNE   NEQXIT                                                           
*                                                                               
*        CALL DDUCOM TO GET PRD POL'S ESTIMATE'S FIRST UCOMM                    
*                                                                               
         MVC   USAVKEY,KEY   SAVE MY KEY                                        
         LA    R5,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R5                                                       
*                                                                               
         MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         MVC   UCSAM,BAGYMD      AGENCY/MEDIA                                   
         MVC   UCSCLT,BCLT       PACKED CLIENT                                  
         MVC   UCPRD,=C'POL'  **TEMPORARY USE OF POL**                          
*                             **UNTIL SPOT SFM ALLOWS AAA**                     
*                                DO UCOMM FOR PRD AAA                           
         OI    UCOPT,UCOEST     RETURN ESTIMATE UCOMMS                          
         L     RF,ADBUY            REALLY POL ESTIMATE                          
         MVC   UCSEST,EKEYEST-ESTHDR(RF)                                        
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK    NEW UCOM CALL SINCE GOTO MACRO                
         CLI   UCERROR,0         TRASHED WRKING STORAGE USED BY DDUCOM          
         BNE   PGETPOLX     ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BO    PGETPOLX                                                         
         XC    UCTTLS(UCALL),UCTTLS                                             
         L     R4,UCETTLS     EST TITLES                                        
         MVC   UCTTLS,0(R4)   SAVE INFO IN MY STORAGE                           
         LA    R4,UCTTLS      AS OPPOSED TO RD CHANE                            
         L     R7,UCEDATA     EST DATA                                          
         MVC   UCOMDATA,0(R7)                                                   
***                                                                             
         DROP  R5                                                               
*                                                                               
PGETPOLX DS    0H                                                               
         B     EQXIT                                                            
*                                                                               
NEQXIT   LTR   RD,RD               EXIT WITH CC NOT =                           
         B     *+6                                                              
EQXIT    CR    RD,RD               EXIT WITH CC =                               
         XIT1                                                                   
         EJECT                                                                  
POSTB    NTR1                                                                   
         LA     R3,BAMTS                                                        
         USING  AMOUNTSD,R3                                                     
         LA     R4,PPGREC                                                       
         XC     0(PPGRECL,R4),0(R4)    CLEAR IT                                 
         USING  PPGRECD,R4                                                      
         L      RF,ADEST                                                        
         L      R2,ADBILL                                                       
*******  L      R5,ADBUY         POL ESTIMATE                                   
         L      R5,ADEST         ESUER1 NOW FROM BRAND EST                      
*                                                                               
         USING  BILLREC,R2                                                      
         MVC    PPGIDATE,BQDATE       DATE PRINTED ON BILL                      
         CLI    PPGIDATE,0            IS THERE ONE?                             
         BNE    *+10                                                            
         MVC    PPGIDATE,BDATE      ELSE USE RUN DATE                           
*                                                                               
         GOTO1  DATCON,DMCB,(3,BDUEDATE),(0,PPGDDATE)                           
*                                                                               
         MVC    PPGINVF,WBINV         FULL INVOICE NUMBER DASHLESS              
         MVC    PPGMOS,BKEYYSRV       MOS YR+MN                                 
         MVC    PPGWBF,BLFLT          WB FLIGHT                                 
*                                                                               
*        IF QAGY IS OU (OMDTOA)                                                 
*        MUST READ ESTIMATE AND GET FLIGTH FOR FIRST                            
*        ESTIMATE USER FIELD                                                    
*                                                                               
         CLC    QAGY,=C'OU'           SEE IF OMDTOA                             
         BNE    POSTB2                                                          
         L      RF,ADEST                                                        
         USING  ESTHDR,RF                                                       
         MVC    PPGWBF,EUSER1         WB FLIGHT IN FIRST EST USER               
         OC     PPGWBF,SPACES                                                   
         DROP   RF                                                              
*                                                                               
POSTB2   DS     0H                                                              
         CLI    QOPT1,C'N'            PRODUCING OUTPUT?                         
         BE     POSTBX                JUST EXIT                                 
*                                                                               
*        NOTE THAT A TEST RUN WILL BE ABLE TO FIND                              
*        LEGAL ENTITY MISMATCHES                                                
*                                                                               
         ZAP    PPGBAMT,BAMTS+12(6)   AMOUNT DUE (INCLUDES TAXES?)              
         AP     PPGBAMT,AMTGST                                                  
         AP     PPGBAMT,AMTPST                                                  
         ZAP    PPGBGRS,BAMTS(6)      GROSS BILLED                              
         ZAP    PPGBNET,BAMTS+18(6)   NET BILLED                                
         ZAP    PPGBAGC,BAMTS+6(6)    ACTUAL AGY COMM                           
         ZAP    PPGBTAX,AMTTAX        US TAX                                    
*                                                                               
         ZAP    PPGGST,AMTGST         BILLS GST                                 
         AP     PPGGST,AMTHST         ADD HST                                   
         ZAP    PPGPST,AMTPST         PST                                       
         SP     PPGPST,AMTHST         EXCLUDE HST                               
*                                                                               
*  AT THIS POINT MUST ADD DATA RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         MVC    PPGKMED,QMED                                                    
         MVC    PPGKSMED,QMED       SUB MEDIA                                   
         CLI    NETPAKSW,C'Y'       SEE IF DOING NETPAK                         
         BNE    POSTB3                                                          
         CLI    BLMED,C' '          SUB MEDIA PRESENT                           
         BNH    *+10                                                            
         MVC    PPGKSMED,BLMED                                                  
POSTB3   MVC    PPGKCLI,CLT                                                     
         MVC    PPGCLI,CLT          CLIENT ALSO IN RECORD                       
         MVC    PPGKPRO,BKEYPRD     PRODUCT IN KEY                              
         MVC    PPGPRO,BKEYPRD      PRODUCT ALSO IN RECORD                      
         MVC    PPGMED,PPGKSMED     SUB-MEDIA ALSO IN RECORD                    
         L      RF,ADEST           RESET RF TO ESTIMATE                         
         MVC    PPGKEST+1(1),EKEYEST-ESTHDR(RF) +1 SPOT/NET 1 BYTE EST          
         MVC    PPGEST+1(1),EKEYEST-ESTHDR(RF) BOTH PLACES                      
         MVC    PPGINVMO,BKEYMBIL      BILLING MONTH                            
         NI     PPGINVMO,X'0F'        SET OFF YEAR HALF-BYTE                    
         MVC    PPGINVN,BKEYINV                                                 
*                                                                               
         DROP   R2                                                              
*                                                                               
         L      R2,AOFPPGT        ADDRESS OF PPGTAB                             
         PRINT  GEN                                                             
         GOTO1  =V(BINSRCH),BINVALS                                             
         PRINT  NOGEN                                                           
*                                                                               
         CLI    BINVALS,1          RECORD INSERTED                              
         BE     GOTOXIT                                                         
*******  OC     BINVALS+1(3),BINVALS+1 IF ZERO TABLE IS FULL                    
*******  BNZ    *-2                                                             
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         DC     H'0'        DUPLICATES SHOULD NOT BE POSSIBLE                   
*******  L      RF,BINVALS               ADDRESS OF FOUND RECORD                
*******  LA     RF,L'PPGKEY(RF)          PAST KEY                               
*******  CLC    LEDIS(L'PPGPU1,RF),PPGPU1   LEGAL ENTITIES                      
*******  BE     POSTB5                                                          
*******  OI     ERRORSW,X'08'   MISMATCH ENCOUNTERED                            
*******  MVC    P+1(33),=C'**ERROR-LEGAL WARNING MISMATCH ON'                   
*******  MVC    P+35(L'PPGINVF),PPGINVF                                         
*******  BRAS   RE,PRNT                                                         
*                                                                               
POSTB5   DS     0H                                                              
*******  AP     AMTDIS(6,RF),PPGBAMT                                            
*******  AP     GSTDIS(6,RF),PPGGST                                             
*******  AP     PSTDIS(6,RF),PPGPST                                             
*                                                                               
GOTOXIT  DS     0H                                                              
         MVC    BINVALS,=A(PPGKMED)                                             
         MVI    BINVALS,1                                                       
POSTBX   XIT1                                                                   
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
BINVALS  DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(PPGKMED)       RECORD TO BE ADDED                            
         DC    A(PPGTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
PPGRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    AL4(PPGTLEN)       LEN OF RECORD                                 
         DC    AL4(L'PPGKEY)      KEY SIZE                                      
         DC    F'7000'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFPPGT  DC    A(PPGTABLE)                                                      
PPGKEY   DS    0XL13                                                            
PPGKMED  DS    CL1                                                              
PPGKCLI  DS    CL3                                                              
PPGKSMED DS    CL1           SUB-MEDIA                                          
PPGKPRO  DS    CL3           PRODUCT                                            
PPGINUMB DS    0XL3                                                             
PPGINVMO DS    XL1           INVOICE MONTH                                      
PPGINVN  DS    XL2           INVOICE NUMBER                                     
PPGKEST  DS    XL2           EST IN LAST BYTE FOR SPOT/NET                      
*            ______                                                             
*              13                                                               
PPGREC   DS    0C                                                               
         ORG   *+PPGRECL                                                        
*                                                                               
ENDPPGR  DS    0C                                                               
*                                                                               
PPGTLEN  EQU   ENDPPGR-PPGKEY                                                   
*                                                                               
***********************                                                         
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
         B     EXIT                                                             
         SPACE 3                                                                
CLRTOTS  NTR1                                                                   
         LHI   R0,NAMTS                                                         
CLRT2    DS    0H                                                               
         ZAP   0(6,R3),=P'0'                                                    
         LA    R3,6(R3)                                                         
         BCT   R0,CLRT2                                                         
         B     EXIT                                                             
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
         BAS   RE,TPFMT                                                         
         BRAS  RE,PRNT                                                          
         XIT1                                                                   
         SPACE 3                                                                
TPFMT    NTR1                                                                   
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
         B     EXIT                                                             
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
         EJECT                                                                  
GETVEN   NTR1                                                                   
         XC    SAVVEND,SAVVEND                                                  
         XC    SAVCNTY,SAVCNTY                                                  
         XC    SAVCURR,SAVCURR                                                  
*                                                                               
         MVC   WORK(2),QAGY                                                     
         LA    R5,VENTAB                                                        
         CLI   NETPAKSW,C'Y'   SEE IF NETPAK                                    
         BNE   GETV5                                                            
         LA    R5,NVENTAB      USE NET TABLE                                    
*                                                                               
GETV5    CLI   0(R5),X'FF'     END OF TABLE                                     
         BNE   *+6                                                              
         DC    H'0'            INVALID AGENCY                                   
         CLC   0(2,R5),WORK                                                     
         BE    GETV10                                                           
         LA    R5,VENTABL(R5)                                                   
         B     GETV5                                                            
*                                                                               
GETV10   MVC   SAVVEND,2(R5)   SAVE VENDOR CODE                                 
         MVC   SAVCNTY,5(R5)    COUNTRY                                         
         MVC   SAVCURR,7(R5)    CURRENCY CODE                                   
         MVC   SAVBDEH,10(R5)   BDE HEADER CONTROL                              
         MVC   SAVFILE,11(R5)   FILE NAME                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VENDOR CODE TABLE                                                      
*        AGY CODE - VENDER, COUNTRY, CURRENCY                                   
*        FILE NAME                                                              
*                                                                               
VENTABL  EQU   33        ENTRY LENGTH                                           
*                                                                               
VENTAB   DS    0H                                                               
         DC    C'OO',C'OMG',C'US',C'USD',C' '    OMDUSEC                        
         DC    CL22'OMG_WB_SPT_NY.BWB'                                          
         DC    C'OU',C'OMG',C'CA',C'CAD',C' '    OMDTOA                         
         DC    CL22'OMG_WB_SPT_TO.BWB'                                          
**** FOR TESTING ***                                                            
         DC    C'SJ',C'SJR',C'US',C'USD',C'N'                                   
         DC    CL22'AGENCY_TEST_DDS_SPT_NY'                                     
         DC    C'*B',C'DDB',C'US',C'USD',C'N'                                   
         DC    CL22'AGENCY_TEST_DDS_SPT_NY'                                     
         DC    X'FFFF'                  END OF TABLE                            
*                                                                               
NVENTAB  DS    0H                                                               
         DC    C'OO',C'OMG',C'US',C'USD',C' '  OMDUSEC                          
         DC    CL22'OMG_WB_NET_NY.BWB'                                          
**** FOR TESTING ***                                                            
         DC    C'SJ',C'SJR',C'US',C'USD',C'N'                                   
         DC    CL22'AGENCY_TEST_DDS_NET_NY'                                     
         DC    C'*B',C'DDB',C'US',C'USD',C'N'                                   
         DC    CL22'AGENCY_TEST_DDS_NET_NY'                                     
         DC    X'FFFF'                  END OF TABLE                            
*                                                                               
SBITAPE  DCB   DDNAME=SBITAPE,DSORG=PS,MACRF=PM                                 
*                                                                               
MULTMSG  DC    C'**MULTIPLE TAPE DESCRIPTIONS IN ONE JOB - BYPASSED**'          
*                                                                               
*                                                                               
NUFLIST  DC    CL8'UUNTFIL'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL10'X'                                                          
*                                                                               
         LTORG                                                                  
         DROP  R7                                                               
***********************************************************************         
*   PRNT - PRINTING ROUTINE                                                     
***********************************************************************         
PRNT     NMOD1 0,PRNT                                                           
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         MVC   HEAD1+47(37),=C'NETPAK WARNER BROS. BILLING INTERFACE'           
         MVC   HEAD2+47(37),=C'-------------------------------------'           
         CLI   MEDIA,C'N'                                                       
         BE    *+14                                                             
         MVC   HEAD1+46(4),=C'SPOT'                                             
         MVI   HEAD2+46,C'-'                                                    
*                                                                               
         CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BNE   PRNT2                                                            
         MVC   HEAD2(6),=C'OFFICE'                                              
         MVC   HEAD2+7(2),OFFICE                                                
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
         CLI   QOPT4,C'M'         MOS DATES                                     
         BNE   *+10                                                             
         MVC   HEAD4+49(32),=C'**MONTH OF SERVICE DATE FILTER**'                
*                                                                               
         LA    R4,HEAD3+49                                                      
         MVC   0(23,R4),=C'PERIOD FROM MMMDD/YY TO'                             
         GOTO1 DATCON,DMCB,SVQST,(8,12(R4))                                     
         GOTO1 DATCON,DMCB,SVQEND,(8,24(R4))                                    
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             HEADHOOK ROUTINE                                  
         SPACE 2                                                                
HHROUT   NTR1                                                                   
         CLC   =C'ALL',QCLT        FIX CLIENT HEADLINE                          
         BNE   HHR2                                                             
         CLI   PROGPROF+0,C'Y'     UNLESS SEPARATE PAGE                         
         BE    HHR2                                                             
         MVC   HEAD3+9(3),=C'ALL'                                               
         MVC   HEAD3+13(24),SPACES                                              
*                                                                               
HHR2     DS    0H                                                               
         CLC   =C'ALL',QPRD        FIX PRODUCT HEADLINE                         
         BNE   HHR2B                                                            
         CLI   PROGPROF+1,C'Y'     UNLESS SEPARATE PAGE                         
         BE    HHR2B                                                            
         MVC   HEAD4+9(3),=C'ALL'                                               
         MVC   HEAD4+13(24),SPACES                                              
*                                                                               
HHR2B    DS    0H                                                               
         CLC   =C'ALL',QEST        FIX ESTIMATE HEADLINE                        
         BNE   *+16                                                             
         MVC   HEAD5+9(3),=C'ALL'                                               
         MVC   HEAD5+13(24),SPACES                                              
*                                                                               
         CLI   MODE,CLTLAST        UNLESS AFTER CLIENT LAST                     
         BH    HHR4                PUT CLIENT DATA IN MIDLINE                   
         CLC   SVMID+8(3),CLT      IF STILL WITH SAME CLIENT                    
         BNE   HHR4                                                             
         CLC   P,SVMID             DON'T REPEAT THE CLIENT LINE                 
         BE    HHR4                                                             
         MVC   MID1,SVMID                                                       
*                                                                               
HHR4     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         SPACE 3                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
NETBLK   DS    2000X                                                            
         EJECT                                                                  
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
         EJECT                                                                  
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
         L     RA,0(R1)                                                         
         USING SPWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING SPWBWRKD,RC                                                      
         L     R4,4(R1)        ADDRESS OF PPGREC                                
         USING PPGRECD,R4                                                       
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         XC    DLCB(L'DLCB),DLCB   CLEAR IT                                     
*                                                                               
         MVC   DLCBFLD,SPACES    CLEAR FIELD                                    
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         CLI   MODE,RUNLAST       SEE IF END OF REPORT                          
         BE    DNP70                                                            
*                                                                               
         CLI   MODE,REQFRST       SEE IF I NEED TO INTIALIZE                    
         BE    DNP80                                                            
*****                                                                           
*****    DOWNLOAD BILLING INFO HERE                                             
*****                                                                           
         CLI   DHEADSW,C'Y'   HAVE I DONE THE HEADER?                           
         BE    DNP20                                                            
         MVC   DLCBFLD(3),SAVVEND         SENDER                                
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(2),SAVCNTY          COUNTRY                              
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),SAVCURR          CURRENCY                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL     SEND END OF LINE                             
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DHEADSW,C'Y'                                                     
*                                                                               
DNP20    DS    0H                                                               
         ST    R1,SAVER1                                                        
         GOTO1 DATCON,DMCB,(0,PPGIDATE),(X'20',WORK)                            
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(2),WORK+2     MONTH                                      
         MVI   DLCBFLD+2,C'/'                                                   
         MVC   DLCBFLD+3(2),WORK+4   DAY                                        
         MVI   DLCBFLD+5,C'/'                                                   
         MVC   DLCBFLD+8(2),WORK        YEAR                                    
         MVC   DLCBFLD+6(2),=C'20'     CENTURY                                  
         CLC   WORK(2),=C'80'                                                   
         BL    *+10                                                             
         MVC   DLCBFLD+6(2),=C'19' IF YEAR IS GREATER THAN 80                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
******                         ASSUME LAST CENTURY                              
******  Y3K NOTE: CODE WILL WORK UNTIL 2080- I'LL BE DEAD                       
******                                                                          
*                                  LEGAL ENTITY                                 
         MVI   WORK,C'S'           SYSTEM SPOT                                  
         MVC   WORK+1(1),PPGMED                                                 
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+8                                                              
         MVI   WORK,C'N'        FOR NET PPGMED IS SUB-MEDIA                     
         MVC   DLCBFLD(2),WORK                                                  
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),PPGCLI  CLIENT                                        
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBFLD,C','       EMPTY DIVISION CODE                           
         MVC   DLCBFLD+1(3),PPGPRO  PRODUCT                                     
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         LA    R3,PPGEST          ESTIMATE                                      
         BAS   RE,CVD                                                           
         MVC   DLCBFLD(3),WORK+2                                                
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(10),PPGWBF   WB FLIGHT                                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   WORK(2),PPGMOS        MOS                                        
         MVI   WORK+2,X'01'   DUMMY DAY                                         
         ST    R1,SAVER1                                                        
         GOTO1 DATCON,DMCB,(3,WORK),(X'20',WORK+6)                              
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(2),WORK+8                                                
         MVI   DLCBFLD+2,C'/'                                                   
         MVC   DLCBFLD+5(2),WORK+6                                              
         MVC   DLCBFLD+3(2),=C'20'     CENTURY                                  
         CLC   WORK(2),=C'80'                                                   
         BL    *+10                                                             
         MVC   DLCBFLD+3(2),=C'19' IF YEAR IS GREATER THAN 80                   
*                                  ASSUME 19TH CENTURY                          
*                                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                            FULL INVOICE NUMBER                                
         MVC   DLCBFLD(L'PPGINVF),PPGINVF                                       
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  INVOICE AMOUNT                               
         ST    R1,SAVER1                                                        
         EDIT  (P6,PPGBAMT),(12,WORK),2,ALIGN=LEFT,FLOAT=-                      
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 DUE DATE                                      
         ST    R1,SAVER1                                                        
         GOTO1 DATCON,DMCB,(0,PPGDDATE),(X'20',WORK)                            
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(2),WORK+2     MONTH                                      
         MVI   DLCBFLD+2,C'/'                                                   
         MVC   DLCBFLD+3(2),WORK+4   DAY                                        
         MVI   DLCBFLD+5,C'/'                                                   
         MVC   DLCBFLD+8(2),WORK        YEAR                                    
         MVC   DLCBFLD+6(2),=C'20'     CENTURY                                  
         CLC   WORK(2),=C'80'                                                   
         BL    *+10                                                             
         MVC   DLCBFLD+6(2),=C'19' IF YEAR IS GREATER THAN 80                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
******                         ASSUME LAST CENTURY                              
******  Y3K NOTE: CODE WILL WORK UNTIL 2080- I'LL BE DEAD                       
******                                                                          
*                                  NET AMOUNT                                   
         ST    R1,SAVER1                                                        
         EDIT  (P6,PPGBNET),(12,WORK),2,ALIGN=LEFT,FLOAT=-                      
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  AGY COMMISSION                               
         ST    R1,SAVER1                                                        
         EDIT  (P6,PPGBAGC),(11,WORK),2,ALIGN=LEFT,FLOAT=-                      
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  GROSS BILLED                                 
         ST    R1,SAVER1                                                        
         EDIT  (P6,PPGBGRS),(12,WORK),2,ALIGN=LEFT,FLOAT=-                      
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLC   AGY,=C'OO'         OMG?                                          
         BNE   DNP30              NO                                            
         ST    R1,SAVER1                                                        
         EDIT  (P6,PPGBTAX),(12,WORK),2,ALIGN=LEFT,FLOAT=-                      
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP30    CLI   CNTRY,C'C'          SEE IF CANADIAN AGY                          
         BNE   DNP40                                                            
         ST    R1,SAVER1                                                        
         ZAP   MYDUB,PPGGST                                                     
         AP    MYDUB,PPGPST                                                     
         EDIT  (P8,MYDUB),(11,WORK),2,ALIGN=LEFT,FLOAT=-                        
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),WORK                                                 
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP40    MVI   DLCBACT,DLCBEOL     SEND END OF LINE                             
         GOTO1 VDLFLD                                                           
         AP    WBINVS,=P'1'        BUMP INVOICE COUNT                           
         AP    WBACT,PPGBAMT       AMOUNT DUE                                   
         AP    WBNET,PPGBNET       NET BILLED                                   
         AP    WBGROSS,PPGBGRS     GROSS BILLED                                 
         AP    WBTAX,PPGBTAX       GROSS BILLED                                 
         AP    WBAGC,PPGBAGC       COMMISSION                                   
         AP    WBCTAX,PPGGST       (GST+HST)                                    
         AP    WBCTAX,PPGPST                                                    
         B     DNPX                                                             
*                                                                               
DNP70    DS    0H                                                               
         CP    WBINVS,=P'0'        ANY INVOICES?                                
         BE    DNP75                                                            
*                                                                               
         MVC   DLCBFLD(3),=C'EOF'     END OF FILE                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         EDIT  (P6,WBINVS),(6,DLCBFLD),0,ALIGN=LEFT                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 TOTAL AMT DUE                                 
         ST    R1,SAVER1                                                        
         EDIT  (P6,WBACT),(12,WORK),2,ALIGN=LEFT,FLOAT=-                        
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 TOTAL NET DUE                                 
         ST    R1,SAVER1                                                        
         EDIT  (P6,WBNET),(12,WORK),2,ALIGN=LEFT,FLOAT=-                        
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 TOTAL AGY COMM                                
         ST    R1,SAVER1                                                        
         EDIT  (P6,WBAGC),(11,WORK),2,ALIGN=LEFT,FLOAT=-                        
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 TOTAL GROSS DUE                               
         ST    R1,SAVER1                                                        
         EDIT  (P6,WBGROSS),(12,WORK),2,ALIGN=LEFT,FLOAT=-                      
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLC   AGY,=C'OO'         OMG?                                          
         BNE   DNP71              NO                                            
*                                                                               
         ST    R1,SAVER1                                                        
         EDIT  (P6,WBTAX),(12,WORK),2,ALIGN=LEFT,FLOAT=-                        
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),WORK                                                 
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP71    CLI   CNTRY,C'C'          SEE IF CANADIAN AGY                          
         BNE   DNP72                                                            
         ST    R1,SAVER1                                                        
         EDIT  (P6,WBCTAX),(11,WORK),2,ALIGN=LEFT,FLOAT=-                       
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),WORK                                                 
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP72    MVI   DLCBACT,DLCBEOL     SEND END OF LINE                             
         GOTO1 VDLFLD                                                           
*                                                                               
DNP75    MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'R'        SET END OF REPORT                            
         GOTO1 VDLFLD                                                           
*                                                                               
DNP80    DS    0H                                                               
*                                 AND RETURN IN SAVVEND                         
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         GOTO1 VDLFLD                                                           
DNPX     DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
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
*                                                                               
DNPRINT  NTR1                                                                   
         MVC   SVLINE,LINE                                                      
         MVC   SVFORCEH,FORCEHED                                                
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
         MVC   LINE,SVLINE             RESTORE LINE                             
         MVC   FORCEHED,SVFORCEH       AND FORCEHED                             
         MVI   RCWHATPR,1     RESET TO FIRST                                    
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DMTHSW   DS    CL1         M IF PROCESSING MOS LINE                             
SAVER1   DS    F                                                                
MYWORK   DS    CL12                                                             
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    C','        FIELD DELIMITER - COMMA                              
EOTCHR   DC    X'00'       END OF TEXT FIELD DELIMITER                          
EOTALT   DC    X'00'       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'00'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    X'00'       END OF REPORT CHR                                    
*                                                                               
*        NORMAL DOWNLOAD FIELDS ARE:                                            
*        C' ',C'"',C'''',X'5E',C':'     5E IS SEMI-COLON                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
SPWB02   CSECT                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
         MACRO                                                                  
&NAME    AGSPC &AGENCY=,&MEDIA=ALL,&TAPE=,&BLKSIZE=,&LRECL=,           +        
               &SPECS=NONE,&ESTIMATE_AMOUNTS=NO,&RECFM=FB                       
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
         AIF   ('&MEDIA' EQ 'ALL').A20                                          
&MED     SETC  '&MEDIA'                                                         
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
         SPACE 1                                                                
         MEXIT                                                                  
         MEND                                                                   
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
INVTAB   DS    0D                                                               
         ORG   *+(INVMAX*3)                                                     
         DC    X'00'                                                            
*                                                                               
PPGTABLE CSECT                                                                  
         ORG   *+(7000*PPGTLEN)                                                 
         DC    X'00'                                                            
PPGRECD  DSECT                                                                  
PPGMED   DS    CL1           MEDIA (ALSO IN KEY)                                
PPGCLI   DS    CL3           CLIENT (ALSO IN KEY)                               
PPGPRO   DS    CL3           PRODUCT (ALSO IN KEY)                              
PPGEST   DS    XL2           ESTIMATE (ALSO IN KEY)                             
PPGIDATE DS    XL6           INVOICE DATE                                       
PPGINVF  DS    CL10          FULL INVOICE NUMBER                                
PPGMOS   DS    XL2           FROM BKEYYSRV AND BKEYMSRV                         
PPGDDATE DS    XL6           DUE DATE                                           
PPGWBF   DS    CL10          WB FLIGHT                                          
*        THE FIELDS WILL BE THE VALUES FOR THE FIRST BILL                       
*        FOR THE BINSRCH KEY                                                    
*                                                                               
*        THE FIELDS BELOW ARE THE TOTALS FOR THE INVOICE                        
*        (BINSRCH KEY)                                                          
PPGBAMT  DS    PL6                AMOUNT DUE                                    
PPGBNET  DS    PL6                NET BILLED                                    
PPGBAGC  DS    PL6                AGENCY COMMISSION                             
*                                 (AMT DUE - NET)                               
PPGBGRS  DS    PL6                GROSS BILLED FOR OMG                          
PPGGST   DS    PL6                                                              
PPGPST   DS    PL6                                                              
PPGBTAX  DS    PL6                                                              
PPGRECL  EQU   *-PPGMED                                                         
*                                                                               
AMTDIS   EQU   PPGBAMT-PPGMED      DISPLACEMENTS                                
NETDIS   EQU   PPGBNET-PPGMED                                                   
AGCDIS   EQU   PPGBAGC-PPGMED                                                   
*                                                                               
GSTDIS   EQU   PPGGST-PPGMED                                                    
PSTDIS   EQU   PPGPST-PPGMED                                                    
*                                                                               
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
         EJECT                                                                  
SPWBWRKD DSECT                                                                  
*                                                                               
VDLFLD   DS    A                                                                
VDOWNLD  DS    A                                                                
VDDUCOM  DS    A                                                                
ASPECS   DS    A                                                                
ATOTS    DS    A                                                                
RELO     DS    A                                                                
SVBAMT   DS    PL6                                                              
TAXSW    DS    XL1                                                              
ERRORSW  DS    XL1                                                              
SAVVEND  DS    CL03         VENDOR CODE                                         
SAVCNTY  DS    CL02         COUNTRY                                             
SAVCURR  DS    CL03         CURRENCY CODE                                       
SAVBDEH  DS    CL1          BDE HEADER CONTROL                                  
SAVFILE  DS    CL22                                                             
*                                                                               
SVMOS    DS    CL4                                                              
MYSVEST  DS    CL3                                                              
SEQNUM   DS    CL1                                                              
SVLINE   DS    X                                                                
SVFORCEH DS    CL1                                                              
MYPPOS   DS    PL6                                                              
MYPACK   DS    PL6                                                              
*                                                                               
MYDUB    DS    PL8                                                              
*                           DOWNLOAD FILE TOTALS                                
WBINVS   DS    PL6          INVOICES                                            
WBACT    DS    PL6          TOTAL AMT DUE                                       
WBNET    DS    PL6          TOTAL NET BILLED                                    
WBAGC    DS    PL6          TOTAL AGENCY COMMISSION                             
WBGROSS  DS    PL6          TOTAL GROSS BILLED FOR OMG                          
WBCTAX   DS    PL6          TOTAL CANADIAN TAXES                                
WBTAX    DS    PL6          TOTAL US TAXES                                      
*                                                                               
EATOTGRS DS    PL6                                                              
EATOTAC  DS    PL6                                                              
EATOTACT DS    PL6                                                              
EATOTNET DS    PL6                                                              
*                                                                               
SVKEY    DS    CL64         KEY AND KEYSAVE                                     
*                                                                               
RECLEN   DS    H                                                                
ELCODE   DS    X                                                                
DOWNACT  DS    C                                                                
DHEADSW  DS    C                                                                
RETAIL   DS    C                                                                
REVSW    DS    C                   REVISION SWITCH                              
BYTE2    DS    X                                                                
LSTBLKY  DS    XL13                LAST BILL KEY                                
B1XPROF  DS    CL16                                                             
B1PROF   DS    CL16                                                             
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                FULL FORMAT INVOICE NUMBER                   
WBINV    DS    CL10                NOW WITH DASHES                              
*                                                                               
CONTROLS DS    0XL4                                                             
CONTROL1 DS    X                                                                
CONTROL2 DS    X                                                                
CONTROL3 DS    X                                                                
CONTROL4 DS    X                                                                
OFFICE   DS    CL2                 WAS CL1                                      
SVQST    DS    CL6                                                              
SVQEND   DS    CL6                                                              
STARTMOS DS    XL2                 START MOS FILTER (BINARY YM)                 
ENDMOS   DS    XL2                 END MOS FILTER (BINARY YM)                   
CNTRY    DS    C                                                                
SVMID    DS    CL132                                                            
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
CLFILT   DS    CL3                                                              
MEFILT   DS    CL1                                                              
*                                                                               
VREMOTEC DS    F                                                                
*                                                                               
INVPARS  DS    6F                  INVOICE BINSRCH PARS                         
*                                                                               
INVMAX   EQU   50000               MAX INVOICES PER CLT                         
*                                                                               
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
*                                                                               
BDEHSW   DS    XL1                                                              
BDEH1    DS    CL132                                                            
BDEH2    DS    CL132                                                            
BDEH3    DS    CL132                                                            
BDEH4    DS    CL132                                                            
         EJECT                                                                  
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
SVBILL   DS    XL256                                                            
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
UCALL    EQU   *-UCTTLS                                                         
USAVKEY  DS    XL13             TO SAVE CURRENT READ SEQUENCE                   
UCOMQ    EQU   *-UCOMBLK                                                        
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
AMTTAX   DS    PL6                 US TAX                                       
NAMTS    EQU   (*-AMOUNTSD)/6                                                   
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         ORG   QAREA+49                                                         
QMOS     DS    0CL8                REQUESTED MONTH-OF-SERVICE RANGE             
QMOSSTRT DS    CL4                 START MOS (YYMM)                             
QMOSEND  DS    CL4                 END MOS (YYMM)                               
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
       ++INCLUDE DDUCOMD                                                        
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NECOMBLOK                                                      
         PRINT ON                                                               
       ++INCLUDE DDOFFICED                                                      
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPWB02 08/12/16'                                      
         END                                                                    
