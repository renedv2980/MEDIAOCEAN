*          DATA SET SPREPJW02  AT LEVEL 015 AS OF 01/22/15                      
*          DATA SET SPREPJW02  AT LEVEL 091 AS OF 01/31/11                      
*PHASE SPJW02A                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE DDUCOM                                                                 
*INCLUDE NETCOM                                                                 
*INCLUDE BINSRCH2                                                               
         TITLE 'SPJW02 - SPOT/NET JWT BILLING INTERFACE'                        
         SPACE 1                                                                
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
         SPACE 1                                                                
         PRINT NOGEN                                                            
SPJW02   CSECT                                                                  
         NMOD1 0,SPJW02,R6,R8                                                   
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SPJWWRKD,RC                                                      
*                                                                               
JACTCDN  EQU   16216           ID THAT GETS 'CAD' IN THE HEADER                 
JGEOCDN  EQU   17004           ID THAT GETS 'CAD' IN THE HEADER                 
*                              INSTEAD OF 'USD'                                 
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
         RELOC RELO                                                             
*                                                                               
         ZAP   JWAORDUE,=P'0'      USED TO ACCUMULATE AOR BILLS                 
*                                                                               
         XC    RUNINVS,RUNINVS     CLEAR RUN INVOICE TOTALS                     
         MVI   BKSFTP,C'N'         SET OFF DOING BURGER SFTP                    
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
         B     EXIT                                                             
*        REQUEST FIRST                                                          
         SPACE 2                                                                
REQF     DS    0H                                                               
*                                                                               
         MVI   CAD$SW,C'N'        SET CANADIAN $ SWITCH                         
         CLC   RCORIGID,=Y(JACTCDN)    SEE IF REQUESTED FOR CANADIAN $          
         BNE   *+8                                                              
         MVI   CAD$SW,C'Y'                                                      
         CLC   RCORIGID,=Y(JGEOCDN)    SEE IF REQUESTED FOR CANADIAN $          
         BNE   *+8                                                              
         MVI   CAD$SW,C'Y'                                                      
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
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+8                                                              
         MVI   NETPAKSW,C'N'                                                    
         DROP  RF                                                               
*                                                                               
REQF1J   DS    0H                                                               
         XC    REQINVS,REQINVS   CLEAR REQUEST                                  
         XC    OFFINVS,OFFINVS   OFFICE                                         
         XC    CLTINVS,CLTINVS   AND CLIENT INVOICE TOTALS                      
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
         LM    R3,R5,AGYLST                                                     
         A     R3,RELO                                                          
         A     R5,RELO                                                          
         MVC   WORK(4),SPACES                                                   
         MVC   WORK(2),QAGY                                                     
*                                                                               
         CLC   WORK(3),0(R3)                                                    
         BE    *+8                                                              
         BXLE  R3,R4,*-10                                                       
*                                                                               
         MVC   QOPT5,3(R3)        SET AOR OPTIOON FOR AGYLST                    
*                                                                               
         L     RF,4(R3)                                                         
         LTR   RF,RF               TEST HAVE TAPE PROC                          
         BNZ   FBLR2D                                                           
         CLI   QOPT4,C' '         OK IF NO TAPE SPEC                            
         BNH   FBLR2D                                                           
*                                                                               
         MVC   P(80),QPROG                                                      
         MVC   P2(35),=C'**INVALID TAPE SPEC- REQ BYPASSED**'                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVI   MODE,REQLAST                                                     
         B     EXIT                                                             
*                                                                               
FBLR2D   DS    0H                                                               
         A     RF,RELO                                                          
         ST    RF,AOUTP                                                         
*                                                                               
         CLI   OPENSW,C'N'                                                      
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
FBLR1C   DS    0H                                                               
         MVC   13(2,R3),QAGY                                                    
         CLC   QAGY,=C'O$'                                                      
         BNE   *+10                                                             
         MVC   DYNDSN+13(2),=C'OS'      O$ NOT ALLOWED AS DSN                   
*                                                                               
         MVC   DYNDSN,0(R3)                                                     
         MVC   DYNDDN,=CL8'SJWTAPE'                                             
*                                                                               
REQF13B  DS    0H                                                               
         B     REQF14                                                           
*                                                                               
SPDYNDSN DC    CL(DSNLENQ)'SPTTAPE.SP0JWAG1'                                    
NEDYNDSN DC    CL(DSNLENQ)'NETTAPE.NE0JWAG1'                                    
DSNLENQ  EQU   20                                                               
MAJORNAM DC    C'SJWTAPE '         MAJOR RESOURCE NAME FOR ENQ                  
*                                                                               
REQF13X  DS    0H                                                               
         L     RE,=A(MULTMSG)                                                   
         MVC   P(52),0(RE)                                                      
         BRAS  RE,PRNT                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
REQF14   DS    0H                                                               
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
         L     RF,ADCLT            SAVE OFFICE CODE                             
         MVC   OFFICE,COFFICE-CLTHDR(RF)                                        
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 3                                                                
*        FIRST FOR CLI                                                          
         SPACE 2                                                                
FBILC    DS    0H                                                               
*                                                                               
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
         MVC   OFFD.OFCOFC,0(RF)     FORM CLIENT HEADER                         
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
*                                                                               
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
         MVC   JWCLTNAM,CLTNM  MUST ALTER COMMAS FOR INTERFACE                  
         LA    R1,JWCLTNAM                                                      
         LA    R2,20                                                            
FBILC7F  CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'-'      CHANGE A COMMA TO A DASH                         
         LA    R1,1(R1)                                                         
         BCT   R2,FBILC7F                                                       
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
*                                                                               
         MVC   JWPRDNAM,PRDNM      CHANGE ANY COMMAS TO DASHES                  
         LA    R1,JWPRDNAM                                                      
         LA    R2,20                                                            
FBILP5   CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)                                                         
         BCT   R2,FBILP5                                                        
*                                                                               
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
         L     R0,OFFINVS                                                       
         LA    RE,OFFINVS                                                       
         CLI   QCLT,C'$'           IF IN OFFICE LIST MODE                       
         BE    *+12                                                             
         L     R0,REQINVS                                                       
         LA    RE,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,0(RE)                                                         
*                                                                               
         MVC   P+10(19),=C'** CLIENT TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  CLTINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         MVI   ALLOWLIN,2                                                       
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
         LA    R3,RAMTS                                                         
         BAS   RE,TOTPRNT                                                       
*                                                                               
         CLI   OPENSW,C'T'                                                      
         BNE   EXIT                                                             
*                                                                               
         L     RF,AOUTP                                                         
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         BASR  RE,RF                                                            
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
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
         MVC   BILLYRMN,FULL        SAVE BILL'S YEAR/MONTH                      
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
*                                                                               
*        SPECIAL HANDLING OF GSTX (G7) BILLS FOR CLIENTS LLB AND MCI            
*        FOR THESE COST 2 CLIENTS SET BNETP TO BNET2P (COST2 NET)               
*        SO THAT NO COMMISSION WILL BE REPORTED                                 
*                                                                               
         CLC   AGY,=C'G7'    GSTX                                               
         BNE   PB6AX                                                            
         CLC   CLT,=C'LLB'          MUST BE EITHER CLIENT LLB OR MCI            
         BE    PB6A                                                             
         CLC   CLT,=C'MCI'                                                      
         BNE   PB6AX                                                            
*                                                                               
PB6A     TM    BILSTAT2,BSTC2Q     MUST BE COST 2 BILL                          
         BZ    PB6AX                                                            
         MVC   BNETP,BNET2P        SET BNET TO COST 2 VALUE                     
*                                                                               
PB6AX    GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
*                                                                               
         LA    R3,BAMTS            CLEAR BILL AMOUNTS                           
         BRAS  RE,CLRTOTS                                                       
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
         CLC   =C'WI',QAGY         ALSO LEAVE NET FOR WI, TAPE CODE "A"         
         BNE   *+12                                                             
         CLI   QOPT2,C'A'                                                       
         BE    PB7                                                              
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
*                                 MVI   SPACING,2   WAS HERE                    
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
         CLC   QAGY,=C'OM'         SPECIAL FOR OM                               
         BNE   PB33D                                                            
         TM    BILSTAT,BSTTAORQ    AOR BILLS                                    
         BZ    PB33D                                                            
         MVC   BINVNO,SVBILL+BINVNO-BILLREC  USE CLT INV BILL NUMBER            
         CLC   BILLREC(11),SVBILL  SHOULD ALWAYS BE RIGHT BILL                  
         BE    *+10                                                             
         MVC   BINVNO,=6C'000000'  IF NOT CLEAR BILL NUMBER                     
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
*                                                                               
         TM    BILSTAT,BSTTAORQ    SEE IF AOR BILL                              
         BZ    *+10                                                             
         AP    JWAORDUE,AMTACT                                                  
*                                                                               
         BRAS  RE,PRNT                                                          
         LA    R4,PAMTS            ROLL TO PRODUCT TOTALS                       
         BAS   RE,PBROLL                                                        
         DROP  R3                                                               
*                                                                               
         CLI   QOPT1,C'N'                                                       
         BE    PSX                 SKIP TAPE                                    
         B     PS40                                                             
*                                                                               
PS40     DS    0H                                                               
         L     RF,AOUTP            SPECIAL INTERFACE MODULE                     
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         CLI   QOPT4,C'N'           SEE IF NOT PUTTING ON TAPE                  
         BE    EXIT                                                             
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
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
PSX      DS    0H                                                               
         B     EXIT                                                             
         DROP R2                                                                
         EJECT                                                                  
*                                                                               
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
*                                                                               
         GETEL R7,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
AGYLST   DS    0F                AGENCY TABLE                                   
         DC    A(AGYLST+12)                                                     
         DC    A(8)                                                             
         DC    A(AGYLSTX-1)                                                     
*                                                                               
         DC    CL4'FR  ',A(JWT)                                                 
         DC    CL4'H7  ',A(JWT)                                                 
         DC    CL4'O$  ',A(JWT)                                                 
         DC    CL4'JS  ',A(JWT)                                                 
         DC    CL4'SJ  ',A(JWT)      SJR FOR TESTING ONLY                       
         DC    CL4'H0 X',A(JWT)      MSHTOA  (CANADIAN)  NO AOR BILLS           
         DC    CL4'HY X',A(JWT)      OUTTO   (CANADIAN)  NO AOR BILLS           
*                                                                               
AGYLSTX  EQU   *                                                                
         DC    CL4'XX',A(0)                                                     
*                                                                               
         SPACE 2                                                                
*                                                                               
MULTMSG  DC    C'**MULTIPLE TAPE DESCRIPTIONS IN ONE JOB - BYPASSED**'          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
SEMICOL  EQU   X'5E'                                                            
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
         EJECT                                                                  
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
         UNPK  DUB,THREE+1         PRDGRP NUMBER                                
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
PRNT1    MVC   HEAD1+55(19),=C'NETPAK BILLING LIST'                             
         MVC   HEAD2+55(19),=C'-------------------'                             
         CLI   MEDIA,C'N'                                                       
         BE    *+14                                                             
         MVC   HEAD1+54(4),=C'SPOT'                                             
         MVI   HEAD2+54,C'-'                                                    
*                                                                               
         CLI   MODE,REQLAST      AT REQLAST DON'T SHOW AN OFFICE                
         BE    PRNT2                                                            
*                                                                               
         CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BNE   PRNT2                                                            
         MVC   HEAD2(6),=C'OFFICE'                                              
         MVC   HEAD2+9(1),OFFICE                                                
         CLI   SV00APRF+2,C'Y'     TEST ALWAYS PRINT AS CHAR                    
         BE    PRNT2                                                            
         MVC   HEAD2+9(2),SAVCOFF  SET BY OFFICER                               
*                                                                               
*****    GOTO1 =V(OFFOUT),DMCB,OFFICE,HEXOUT,HEAD2+9                            
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
         CLC   =C'ALL',QPRD        FIX PRODUCT HEADLINE                         
         BNE   HHR2B                                                            
         CLI   PROGPROF+1,C'Y'     UNLESS SEPARATE PAGE                         
         BE    HHR2B                                                            
         MVC   HEAD4(07),=C'PRODUCT'                                            
         MVC   HEAD4+9(3),=C'ALL'                                               
         MVC   HEAD4+13(24),SPACES                                              
*                                                                               
HHR2B    DS    0H                                                               
         CLC   =C'ALL',QEST        FIX ESTIMATE HEADLINE                        
         BNE   HHR2C                                                            
         MVC   HEAD5(08),=C'ESTIMATE'                                           
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
         MVC   MID1,SVMID                                                       
*                                                                               
HHR4     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
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
         SPACE 2                                                                
SPJW02   CSECT                                                                  
*          'STANDARD' TAPE SPECS - (LIKE PRODUCTION)                            
         SPACE 2                                                                
SPSPECS  DS    0X                                                               
         DSPEC 1,2,AGENCY                                                       
         DSPEC 3,LITERAL='S'                                S-SYSTEM            
         DSPEC 4,1,MEDIA                                                        
         DSPEC 5,3,CLTCODE                                                      
         DSPEC 8,3,PRDCODE                                                      
         DSPEC 11,3,ESTCODE                                                     
         DSPEC 17,1,OFFICE                                                      
         DSPEC 18,6,INVNUM6                                                     
         DSPEC 24,6,INVDATE,FORMAT=YYMMDD                                       
         DSPEC 30,10,ACTUAL,FORMAT=UNPACKED                                     
         DSPEC 40,10,NET,FORMAT=UNPACKED                                        
         DSPEC 50,10,ACTUAL_COMMISSION,FORMAT=UNPACKED                          
         DSPEC 60,10,ZEROES,FORMAT=UNPACKED                                     
         DSPEC 70,4,MOS,FORMAT=YYMM                                             
         DSPEC 82,6,DUEDATE,FORMAT=YYMMDD                                       
         DSPEC 88,4,PRDNUM                                                      
         DC    X'0000'                                                          
         EJECT                                                                  
         SPACE 2                                                                
JWT      NMOD1 0,JWPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
JWPR1    DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BE    JWPRL                                                            
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         MVI   PBIREC,C' '                                                      
         MVC   PBIREC+1(L'PBIREC-1),PBIREC                                      
*                                                                               
*     - BILL INV DATE (MM/DD/YYYY)                                              
         GOTO1 DATCON,DMCB,(0,BQDATE),(20,WORK)        INV DATE                 
         MVC   PBIREC+0(2),WORK+4                                               
         MVI   PBIREC+2,C'/'                                                    
         MVC   PBIREC+3(2),WORK+6                                               
         MVI   PBIREC+5,C'/'                                                    
         MVC   PBIREC+6(4),WORK                                                 
         MVI   PBIREC+10,C','            DELIMITER                              
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(20,WORK) DATE DUE                      
         MVC   PBIREC+11(2),WORK+4                                              
         MVI   PBIREC+13,C'/'                                                   
         MVC   PBIREC+14(2),WORK+6                                              
         MVI   PBIREC+16,C'/'                                                   
         MVC   PBIREC+17(4),WORK                                                
         MVI   PBIREC+21,C','            DELIMITER                              
*                           2 POSITIONS LINE NUMBER WILL GO HERE                
         MVC   PBIREC+22(10),DINVFULL   THEY SEEM TO USE SHORTER INV            
         MVI   PBIREC+32,C','            DELIMITER                              
*                                        LINE NUMBER WILL BE HERE               
         MVI   PBIREC+36,C','                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(5,WORK)     RUN DATE                      
         MVC   BRUNMTH,WORK              SAVE RUN MONTH FOR LATER               
*                                                                               
         GOTO1 DATCON,DMCB,(3,BKEYYSRV),(5,WORK)                                
         CLI   BKEYYSRV+1,12       SPECIAL PERIOD                               
         BNH   JWTR2                                                            
         ZIC   R1,BKEYYSRV+1                                                    
         EDIT  (R1),(2,WORK+1)                                                  
         MVI   WORK,C' '                                                        
*                                                                               
JWTR2    DS    0H                                                               
         MVC   PBIREC+37(3),WORK         MMM                                    
         MVI   PBIREC+40,C'/'                                                   
         MVC   PBIREC+41(2),WORK+6       YY                                     
         MVI   PBIREC+43,C','            DELIMITER                              
         MVC   PBIREC+44(6),=C'RADIO,'                                          
*                                                                               
*        AFTER THIS FIELD USE R6                                                
*                                                                               
         LA    R6,PBIREC+50                                                     
         CLI   QMED,C'R'                                                        
         BE    JWTR3                                                            
         MVC   PBIREC+44(11),=C'TELEVISION,'                                    
         LA    R6,PBIREC+55                                                     
         CLI   QMED,C'T'                                                        
         BE    JWTR3                                                            
         MVC   PBIREC+44(14),=C'NETWORK RADIO,'                                 
         LA    R6,PBIREC+58                                                     
         CLI   QMED,C'X'                                                        
         BE    JWTR3                                                            
*                                                                               
         MVC   PBIREC+44(14),SPACES        CLEAR ANYTHING LEFT                  
*                                    NETPAK MEDIA                               
         MVC   PBIREC+44(6),=C'CABLE,'                                          
         LA    R6,PBIREC+50                                                     
         CLI   BLMED,C'C'       NETPAK SUBMEDIA                                 
         BE    JWTR3                                                            
         MVC   PBIREC+44(07),=C'CINEMA,'                                        
         LA    R6,PBIREC+51                                                     
         CLI   BLMED,C'O'       OTHER IS CINEMA FOR THEM                        
         BE    JWTR3                                                            
         MVC   PBIREC+44(08),=C'NETWORK,'                                       
         LA    R6,PBIREC+52                                                     
         CLI   BLMED,C' '       NO SUBMEDIA                                     
         BE    JWTR3                                                            
         CLI   BLMED,C'N'       OR NETWORK                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(12),=C'SYNDICATION,'                                   
         LA    R6,PBIREC+56                                                     
         CLI   BLMED,C'S'                                                       
         BE    JWTR3                                                            
         MVC   PBIREC+44(14),=C'NETWORK RADIO,'                                 
         LA    R6,PBIREC+58                                                     
         CLI   BLMED,C'D'                                                       
         BE    JWTR3                                                            
         DC    H'0'      UNKNOWN MEDIA                                          
*                                                                               
JWTR3    DS    0H                                                               
*                                                                               
         MVC   0(3,R6),CLT                                                      
         CLI   CLT+2,C' '         SEE IF 2 CHARACTER CLT                        
         BH    JWTR3C                                                           
         MVI   2(R6),C','                                                       
         LA    R6,3(R6)                                                         
         B     JWTR3D                                                           
*                                                                               
JWTR3C   MVI   3(R6),C','         DELIMITER                                     
         LA    R6,4(R6)                                                         
JWTR3D   MVC   0(20,R6),JWCLTNAM                                                
         LA    R6,20(R6)                                                        
JWTR4    CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR6                                                            
         BCT   R6,JWTR4                                                         
JWTR6    MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
*                                                                               
         MVC   0(3,R6),BKEYPRD                                                  
         LA    R6,2(R6)                                                         
         CLI   0(R6),C' '      CHECK FOR 2 CHARACTER PRD CODE                   
         BE    *+8                                                              
         LA    R6,1(R6)                                                         
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
         MVC   0(20,R6),JWPRDNAM                                                
         LA    R6,20(R6)                                                        
JWTR8    CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR10                                                           
         BCT   R6,JWTR8                                                         
JWTR10   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R6),DUB                                                      
         MVI   3(R6),C','       DELIMITER                                       
         LA    R6,4(R6)                                                         
*                                                                               
         MVC   0(3,R6),BRUNMTH    RUN MONTH                                     
         MVI   3(R6),C','         DELIMITER                                     
         LA    R6,4(R6)                                                         
*                                                                               
         LA    R3,BAMTS                                                         
         USING AMOUNTSD,R3                                                      
*                                                                               
         MVI   0(R6),C'0'                                                       
         CP    AMTACT,=P'0'                                                     
         BE    JWTR14                                                           
         TM    BILSTAT,BSTTAORQ  SEE IF AN AOR BILL                             
         BNZ   JWTR14            SKIP THIS FIELD                                
*                                                                               
         ZAP   MYDUB,AMTACT                                                     
         AP    MYDUB,AMTGST                                                     
         AP    MYDUB,AMTPST                                                     
*                                                                               
         EDIT  MYDUB,(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                            
         LA    R6,12(R6)                                                        
JWTR12   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR14                                                           
         BCT   R6,JWTR12                                                        
JWTR14   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
         MVI   0(R6),C'0'                                                       
         CP    AMTNET,=P'0'                                                     
         BE    JWTR18                                                           
         TM    BILSTAT,BSTTAORQ  SEE IF AN AOR BILL                             
         BNZ   JWTR18            SKIP THIS FIELD                                
*                                                                               
         EDIT  AMTNET,(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                           
         LA    R6,12(R6)                                                        
JWTR16   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR18                                                           
         BCT   R6,JWTR16                                                        
JWTR18   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
         TM    BILSTAT,BSTTAORQ  SEE IF AN AOR BILL                             
         BZ    JWTR19            IF SO, SHOW ACTUAL HERE                        
         MVI   0(R6),C'0'                                                       
         ZAP   MYDUB,AMTACT                                                     
         B     JWT19C                                                           
*                                                                               
JWTR19   MVI   0(R6),C'0'                                                       
         ZAP   MYDUB,AMTACT          RECIEVABLE                                 
         SP    MYDUB,AMTNET          LESS NET                                   
JWT19C   CP    MYDUB,=P'0'                                                      
         BE    JWTR22                                                           
         EDIT  MYDUB,(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                            
         LA    R6,12(R6)                                                        
JWTR20   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR22                                                           
         BCT   R6,JWTR20                                                        
JWTR22   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
**                              ZERO AOR FEE FIELD                              
         MVI   0(R6),C'0'                                                       
         MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
         TM    BILSTAT,BSTTAORQ  SEE IF AN AOR BILL                             
         BZ    JWTR23X     IF SO SHOW AMOUNT DUE IN AOR NET FIELD               
         MVI   0(R6),C'0'                                                       
         CP    AMTACT,=P'0'                                                     
         BE    JWTR23A                                                          
         ZAP   DUB,AMTACT   REVERSE THE SIGN FOR THIS FIELD                     
         CVB   R0,DUB                                                           
         LCR   R0,R0                                                            
         CVD   R0,MYDUB                                                         
         EDIT  MYDUB,(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                            
         LA    R6,12(R6)                                                        
JWTR23A  CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR23B                                                          
         BCT   R6,JWTR23A                                                       
JWTR23B  MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
         B     JWTR23XX                                                         
                                                                                
*                                                                               
JWTR23X  MVI   0(R6),C'0'                                                       
         MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
*        NO CD,PROD,PHONE + DELIVERY,ADMIN FEES                                 
*                                                                               
JWTR23XX MVC   0(7,R6),=C'0,0,0,0'    4 EMPTY FIELDS AT END                     
*                                                                               
         LA    R6,7(R6)                                                         
*                                                                               
         CLI   CNTRY,C'C'          IF CANADIAN AGENCY                           
         BNE   JWPR50                                                           
*                                                                               
         MVI   0(R6),C','      DELIMITER                                        
         LA    R6,1(R6)                                                         
*                                                                               
         SP    MYPST,MYHST      REMOVE MYHST FROM MYPST                         
*                                                                               
         CP    MYPST,=P'0'     CHECK FOR QST (WORKS FOR NOW)                    
         BE    JWTR30                                                           
         ZAP   DOUBLE,MYGST       COMBINE GST AND PST (QST)                     
         AP    DOUBLE,MYPST                                                     
         EDIT  (P8,DOUBLE),(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                      
         LA    R6,12(R6)                                                        
JWTR28   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR29                                                           
         BCT   R6,JWTR28                                                        
JWTR29   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
         MVC   0(3,R6),=C'QST'                                                  
         B     JWPR50                                                           
*                                                                               
JWTR30   ZAP   DOUBLE,MYGST     ONLY MYGST OR MYHST                             
         AP    DOUBLE,MYHST     SHOULD BE NON-ZERO (NOT BOTH)                   
         EDIT  (P8,DOUBLE),(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                      
         LA    R6,12(R6)                                                        
JWTR32   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR34                                                           
         BCT   R6,JWTR32                                                        
JWTR34   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
         MVC   0(3,R6),=C'GST'                                                  
         CP    MYHST,=P'0'     ANY HST PRESENT                                  
         BE    JWPR50                                                           
         MVC   0(3,R6),=C'HST' REPORT AS HST                                    
         B     JWPR50                                                           
*                                                                               
         DROP  R3                                                               
*                             NOTE - NO COMMA AFTER LAST FIELD                  
*                                                                               
JWPR50   DS    0H                                                               
         B     JWLAR1           PATCH OUT FOR TESTING                           
*                                                                               
*        FILE RECORDS CREATED AT RUNLAST                                        
*                                                                               
*        RECORDS HERE ARE JUST GOING TO BINSRCH                                 
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   JWLAR1                                                           
         CP    COUNT,=P'50'                                                     
         BH    JWLAR1                                                           
         AP    COUNT,=P'1'                                                      
         MVC   P(18),=C'***BILLING RECORD'                                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,BILLREC,P,60,0                                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,BILLREC+60,P,60,0                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(15),=C'***TAPE RECORD'                                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(100),PBIREC                                                    
         MVC   P2(80),PBIREC+100                                                
         GOTO1 REPORT                                                           
         MVC   P(21),=C'***TAPE RECORD - HEX'                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC,P,60,0                                        
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC+60,P,60,0                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC+120,P,60,0                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     *+6                                                              
COUNT    DC    PL2'0'                                                           
*                                                                               
*                                                                               
*                                                                               
JWLAR1   DS    0H                                                               
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         MVC    JWTMED,QMED                                                     
*                                                                               
         CLI    BLMED,C' '    USE NETPAK SUB MEDIA IF PRESENT                   
         BE     *+10                                                            
         MVC    JWTMED,BLMED                                                    
*                                                                               
         MVC    JWTCLI,CLT                                                      
         MVC    JWTINVMO,BILLYRMN+1    BILLING MONTH (WITHOUT YEAR)             
         MVC    JWTINVN,BKEYINV                                                 
         MVC    JWTMOS,BKEYYSRV        YM                                       
         MVC    JWTPRO,BKEYPRD                                                  
         MVC    JWTEST+1(1),BKEYEST                                             
*                                                                               
         MVC    JWTREC,PBIREC                                                   
*                                                                               
         L      R2,AOFJWTT        ADDRESS OF JWTTAB                             
         PRINT  GEN                                                             
         GOTO1 =V(BINSRCH),BINVALS                                              
         PRINT  NOGEN                                                           
*                                                                               
         CLI    BINVALS,1          RECORD INSERTED                              
         BE     GOTOXIT                                                         
         OC     BINVALS+1(3),BINVALS+1 IF ZERO TABLE IS FULL                    
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS   SHOULD NOT HAPPEN                          
*                                                                               
         DC      H'0'                                                           
**       L       RF,BINVALS               ADDRESS OF FOUND RECORD               
**       LA      RF,L'JWTKEY(RF)          PAST KEY                              
**       AP      45(9,RF),PBIREC+45(9)     CD                                   
**       AP      34(11,RF),PBIREC+34(11)     GROSS                              
**       AP      54(11,RF),PBIREC+54(11)                                        
**       AP      65(11,RF),PBIREC+65(11)                                        
**       AP      87(9,RF),PBIREC+87(9)      AOR FEE                             
*                                                                               
GOTOXIT  DS    0H                                                               
         MVC   BINVALS,=A(JWTMED)                                               
         MVI   BINVALS,1                                                        
         XIT1                                                                   
*                                                                               
         DROP  R7                                                               
*                                                                               
JWPRL    DS    0H                                                               
*                                                                               
         L     R2,JWTRECNT         FOR BCT    RECORD COUNT                      
         CH    R2,=H'0'            NO RECORDS                                   
         BE    ZEROX               NO OPEN NOR CLOSE NEEDED                     
*                                                                               
         CLI   SVQOPT3,C'Y'       TEST RUN - NO TAPE                            
         BE    JWPRL5                                                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,DYNDDN),(0,DYNDSN)                              
         OPEN  (JWSBIT1,(OUTPUT))                                               
*                                                                               
JWPRL5   DS    0H                                                               
         L     R2,JWTRECNT         FOR BCT    RECORD COUNT                      
         CH    R2,=H'0'            NO RECORDS                                   
         BE    ZEROCNT                                                          
*                                                                               
*        FIRST CREATE AND SEND HEADER RECORD                                    
*                                                                               
         MVI   PBIREC,C' '                                                      
         MVC   PBIREC+1(L'PBIREC-1),PBIREC                                      
*                                                                               
         MVC   PBIREC(8),=C'HDR,USD,'                                           
         CLI   CAD$SW,C'Y'                                                      
         BNE   *+10                                                             
         MVC   PBIREC+4(3),=C'CAD'                                              
*                                                                               
         CLI   CNTRY,C'C'          CANADIAN AGENCY                              
         BNE   *+10                                                             
         MVC   PBIREC+4(3),=C'CAD'                                              
*                                                                               
         LA    R6,PBIREC+8                                                      
         EDIT  (B4,JWTRECNT),(5,0(R6)),0,ALIGN=LEFT                             
         LA    R6,5(R6)                                                         
JWPRL2   CLI   0(R6),C' '          SCAN BACKWARD FOR NON-SPACE                  
         BH    JWPRL4                                                           
         BCT   R6,JWPRL2                                                        
JWPRL4   MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
         EDIT  (B4,RUNINVS),(5,0(R6)),0,ALIGN=LEFT                              
         LA    R6,5(R6)                                                         
JWPRL6   CLI   0(R6),C' '          SCAN BACKWARD FOR NON-SPACE                  
         BH    JWPRL8                                                           
         BCT   R6,JWPRL6                                                        
JWPRL8   MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
*                                                                               
         ZAP   MYDUB,RAMTS+12(6)    TOTAL AMOUNT DUE                            
         AP    MYDUB,RAMTS+30(6)    GST                                         
         AP    MYDUB,RAMTS+36(6)    PST  (INCLUDES HST)                         
*                                                                               
         SP    MYDUB,JWAORDUE       REMOVE AOR $                                
*                                                                               
         MVI   0(R6),C'0'                                                       
         CP    MYDUB,=P'0'                                                      
         BE    JWPRL12                                                          
         EDIT  MYDUB,(13,0(R6)),2,ALIGN=LEFT,FLOAT=-                            
         LA    R6,13(R6)                                                        
JWPRL10  CLI   0(R6),C' '          SCAN BACKWARD FOR NON-SPACE                  
         BH    JWPRL12                                                          
         BCT   R6,JWPRL10                                                       
JWPRL12  DS    0H                   NOTE - NO COMMA AFTER LAST FIELD            
*                                                                               
*        SET RECORD LENGTH IN PBIRECL HERE                                      
*                                                                               
         CLI   SVQOPT3,C'Y'        DUMPING OUTPUT                               
         BNE   JWPRL15                                                          
         MVC   P(19),=C'** HEADER RECORD **'                                    
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
         MVC   P(100),PBIREC                                                    
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(16),=C'** HEADER HEX **'                                       
         BRAS  RE,PRNT                                                          
         GOTO1 HEXOUT,DMCB,PBIRECL,P,60,0                                       
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
*                                                                               
         B     JWPRL20                                                          
*                                                                               
JWPRL15  LA    R1,JWSBIT1                                                       
         LA    R0,PBIREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
*                                  DETAILS                                      
JWPRL20  DS    0H                                                               
         XC    JLASTINV,JLASTINV                                                
         ZAP   ITMCNT,=P'0'                                                     
         L     R3,AOFJWTT                                                       
         LA    R3,L'JWTKEY(R3)     BUMP PAST PSUEDO KEY                         
*                                                                               
JWTAGN   DS    0H                                                               
JWTAGN5  DS    0H                                                               
         SH    R3,=H'14'         BACK UP R3 TO KEY                              
         CLC   0(7,R3),JLASTINV  SAME INVOICE #?  MED/CLT/INV                   
         BE    JWTAGN5B                                                         
         ZAP   ITMCNT,=P'1'        RESET TO 1                                   
         MVC   JLASTINV,0(R3)      SAVE MED/CLT/INV                             
         AH    R3,=H'14'           RE-BUMP PAST KEY                             
         B     JWTAGN5C                                                         
*                                                                               
JWTAGN5B DS    0H                                                               
         AH    R3,=H'14'           RE-BUMP PAST KEY                             
         AP    ITMCNT,=P'1'                                                     
JWTAGN5C UNPK  33(3,R3),ITMCNT                                                  
***      CLI   33(R3),C'0'     SUPPRESSION OF LEADING ZEROS                     
***      BNE   *+8             NO-OPTED                                         
***      MVI   33(R3),C' '     NO LEADING ZERO                                  
***      CLI   34(R3),C'0'                                                      
***      BNE   *+8                                                              
***      MVI   34(R3),C' '     NO LEADING ZERO                                  
         OI    35(R3),X'F0'    NOTE - MAY HAVE TO RESTORE                       
***                            IF FIELD CAN'T START WITH A BLANK                
***                            IF LEADING ZEROS NO BLANKS                       
***                            NOT ALLOWED - SHIFT RECORD HERE                  
***                                                                             
         MVC   SAVREC(180),0(R3)  SAVE ORIGINAL RECORD                          
         CLI   33(R3),C'0'    ITEM NUMBER OVER 99?                              
         BH    JWTAGN6                                                          
         MVC   33(180-33,R3),SAVREC+34                                          
         MVI   179(R3),C' '                                                     
         CLI   33(R3),C'0'    ITEM NUMBER OVER 9?                               
         BH    JWTAGN6                                                          
         MVC   33(180-34,R3),SAVREC+35                                          
         MVI   178(R3),C' '                                                     
*                                                                               
JWTAGN6  DS    0H                                                               
         CLI   SVQOPT3,C'Y'        TEST RUN - NO TAPE                           
         BE    JWTAGN7                                                          
*                                                                               
         LA    R1,JWSBIT1                                                       
         PUT   (1),(3)                                                          
*                                                                               
JWTAGN7  DS    0H                                                               
         CLI   SVQOPT3,C'Y'                                                     
         BNE   BINGO                                                            
         CP    COUNT,=P'100'                                                    
         BH    BINGO                                                            
         AP    COUNT,=P'1'                                                      
         MVC   P(9),=C'***OUTPUT'                                               
         GOTO1 REPORT                                                           
         MVC   P(75),0(R3)                                                      
         MVC   P2(75),75(R3)                                                    
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
         MVC   P(14),=C'***OUTPUT- HEX'                                         
         BRAS  RE,PRNT                                                          
         GOTO1 HEXOUT,DMCB,(R3),P,65,0                                          
         BRAS  RE,PRNT                                                          
         LA    RF,65(R3)                                                        
         GOTO1 HEXOUT,DMCB,(RF),P,65,0                                          
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
         LA    RF,130(R3)                                                       
         GOTO1 HEXOUT,DMCB,(RF),P,50,0                                          
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
*                                                                               
BINGO    LA    R3,194(R3)   TO NEXT RECORD IN TABLE                             
         BCT   R2,JWTAGN                                                        
*                                                                               
ZEROCNT  DS    0H                                                               
*                                                                               
         CLI   SVQOPT3,C'Y'     TEST RUN - NO TAPE                              
         BE    ZEROX                                                            
*                                                                               
         LA    R2,JWSBIT1                                                       
         CLOSE ((2),)                                                           
ZEROX    B     GOTOXIT                                                          
*                                                                               
JWSBIT1  DCB   DDNAME=SJWTAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00180,                                            X        
               BLKSIZE=00180,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
         LTORG                                                                  
ITMCNT   DS    PL2                                                              
JLASTINV DS    CL7                                                              
         EJECT                                                                  
*                                                                               
NEXTELM  NTR1  BASE=*,LABEL=*                                                   
NEXTELM1 ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELMX                                                         
         CLC   0(1,R2),ELCODE                                                   
         BE    *+10                                                             
         B     NEXTELM1                                                         
NEXTELMX LTR   RE,RE       NOT FOUND -                                          
*                                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
BINVALS  DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(JWTMED)        RECORD TO BE ADDED                            
         DC    A(JWTTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
JWTRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    F'194'             LEN OF RECORD                                 
         DC    F'14'              KEY SIZE                                      
         DC    F'4000'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFJWTT  DC    A(JWTTABLE)                                                      
JWTKEY   DS    0XL14                                                            
JWTMED   DS    CL1                                                              
JWTCLI   DS    CL3                                                              
JWTINUMB DS    0XL3                                                             
JWTINVMO DS    XL1           INVOICE MONTH                                      
JWTINVN  DS    XL2           INVOICE NUMBER                                     
JWTPRO   DS    CL3                                                              
JWTEST   DS    XL2                                                              
JWTMOS   DS    XL2           MONTH OF SERVICE                                   
*            ______                                                             
*              14                                                               
JWTREC   DS    CL180                                                            
ENDJWTR  DS    0C                                                               
***********************                                                         
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
NEXTELD  NTR1  BASE=*,LABEL=*                                                   
NEXTELD1 ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELDX                                                         
         CLC   0(1,R2),ELCODE                                                   
         BE    *+10                                                             
         B     NEXTELD1                                                         
NEXTELDX LTR   RE,RE       NOT FOUND -                                          
*                                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
*        JWT                                                                    
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
         EJECT                                                                  
SPBT02   CSECT                                                                  
*                                                                               
         EJECT                                                                  
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
         EJECT                                                                  
SPJWWRKD DSECT                                                                  
*                                                                               
VSPFMTIN DS    V                   V(SPFMTINO)                                  
MVOFFICE DS    V                   V(OFFICER)                                   
ASPECS   DS    A                                                                
ATOTS    DS    A                                                                
AG7MED   DS    A                                                                
RELO     DS    A                                                                
*                                                                               
EATOTGRS DS    PL6                                                              
EATOTAC  DS    PL6                                                              
EATOTACT DS    PL6                                                              
EATOTNET DS    PL6                                                              
*                                                                               
RECLEN   DS    H                                                                
ELCODE   DS    X                                                                
MYDUB    DS    PL8                                                              
PL16     DS    PL16                                                             
OPENSW   DS    C                                                                
RETAIL   DS    C                                                                
REVSW    DS    C                   REVISION SWITCH                              
BYTE2    DS    X                                                                
CAD$SW   DS    CL1                 SET TO Y FOR ID JACTCDN                      
*                                  OR JGEOCDN                                   
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
BKSFTP   DS    C                   Y=DOING BURGER KING SFTP                     
*                                                                               
CTODAY   DS    CL8                 YYYYMMDD                                     
TIMEOFD  DS    CL8                 HH.MM.SS                                     
SVMID    DS    CL132                                                            
*                                                                               
JWPRDNAM DS    CL20      ALTERED PRODUCT NAME                                   
*                                                                               
JWCLTNAM DS    CL20      ALTERED CLIENT NAME                                    
*                        (COMMAS TO DASHES)                                     
SAVREC   DS    CL180                                                            
*                                                                               
MQMAPNM  DS    CL14                SFTPDISK.PROD.                               
*                                                                               
DSNAME   DS    CL35  DSN -  BIL.SYS.AGID.DYYYMMDD.THHMMSS                       
*                    FOR MINDSHARE BURGER KING AGID = H7BK                      
*                                                                               
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
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
SVQOPT3  DS    CL1                 SAVED QOPT3                                  
*                                                                               
BILLYRMN DS    XL2          BILL'S YEAR AND MONTH                               
*                                                                               
CLFILT   DS    CL3                                                              
MEFILT   DS    CL1                                                              
BRUNMTH  DS    CL3                                                              
*                                                                               
AOUTP    DS    F            ADDRESS OF BILL PROCESS ROUTINE                     
*                                                                               
SAVER1   DS    F                                                                
MYFULL   DS    F                                                                
*                                                                               
JWAORDUE DS    PL8          RUN TOTAL OF AOR BILLS                              
*                           SUBTRACTED FOR HDR TOTAL                            
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
         DS    0H               FOR ALIGNMENT                                   
PBIRECL  DS    CL2              RECORD LENGTH                                   
PBIREC   DS    CL200                                                            
*                                                                               
SVBILL   DS    XL256                                                            
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
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NECOMBLOK                                                      
         PRINT ON                                                               
*                                                                               
JWTTABLE CSECT                                                                  
         DS    4000CL194                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPJW02 01/22/15'                                      
         END                                                                    
