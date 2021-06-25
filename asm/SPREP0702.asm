*          DATA SET SPREP0702  AT LEVEL 107 AS OF 09/12/05                      
*PHASE SP0702A                                                                  
*INCLUDE BUFFERIN                                                               
*INCLUDE PRTREC                                                                 
         TITLE 'SPREP0702 - SPOTPAK UNBILLING'                                  
***********************************************************************         
*                                                                               
*   QOPT1: (SEE DETAILED EXPLANATION BELOW)                                     
*          'S'   = SUBMITTED AS UPDATIVE SOON VIA REQUEST PROGRAM               
*          'L'   = LIVE (OVERNIGHT) REQUEST FROM REQUEST FILE                   
*          'P'   = LIVE "UPDATIVE SOON" UNBILLING ISSUED VIA TSO                
*          ' '   = DRAFT UNBILLING ISSUED VIA TSO                               
*   QOPT2: 'T' = PRINT TRACE OF FILE UPDATES                                    
*   QOPT3: 'Y' = REMOVE UNBILLED ELEMENTS FROM STABUCK RECORDS                  
*                 (RATHER THAN SIMPLY MARKING ELEMENTS AS UNBILLED)             
*   QOPT4: 'D' = CREATE DDS UNBILLING RECORDS                                   
*                                                                               
* THE UNBILLING PROGRAM CAN RUN IN MANY DIFFERENT MODES, ALL OF WHICH           
* ARE DRIVEN BY QOPT1. (NOTE THAT REGARDLESS OF THE QOPT1 SETTING,              
* IF A WRITE=NO CARD IS PRESENT, NO UPDATE WILL OCCUR TO ANY FILE.)             
*                                                                               
* TYPICALLY, THE WAY UNBILLING WORKS IS THIS: DATA CONTROL MAKES AN             
* UNBILLING REQUEST VIA THE REQUEST PROGRAM, ASKING IT TO RUN "SOON".           
* THE REQUEST PROGRAM GENERATES AN UPDATIVE SOON REQUEST WITH AN "S" IN         
* QOPT1. THE UNBILLING PROGRAM COMPARES THE AMOUNTS IT FOUND TO UNBILL          
* WITH THE AMOUNT(S) ENTERED BY DATA CONTROL IN THE REQUEST (THIS ACTS          
* AS A CHECKSUM). IF THEY MATCH, AN UNBILLING REQUEST IS WRITTEN TO THE         
* REQUEST FILE TO RUN *OVERNIGHT* WITH QOPT1 SET TO "L". IF NECESSARY,          
* AN OVERNIGHT UNPOSTING REQUEST IS ALSO WRITTEN TO THE REQUEST FILE.           
* WHEN THE OVERNIGHT UNBILLING REQUEST IS RUN (WITH QOPT1 SET TO "L"),          
* THE LIVE UNBILLING ACTUALLY TAKES PLACE.                                      
*                                                                               
* THERE ARE ALSO TWO SETTINGS FOR QOPT1 AVAILABLE ONLY TO PROGRAMMERS           
* (WHEN SUBMITTING UNBILLING REQUESTS VIA TSO). IF QOPT1 IS A BLANK,            
* THEN THE PROGRAM RUNS IN "DRAFT" MODE, AND MERELY PRODUCES A REPORT.          
*                                                                               
* IF A PROGRAMMER WISHES TO RUN A LIVE UNBILLING DURING THE DAY, AS AN          
* "UPDATIVE SOON" SUBMITTED VIA TSO, THAT IS DONE BY SETTING QOPT1 TO           
* "P". (THE PROGRAMMER MUST SET UP THE JCL CORRECTLY, WITH A RECOVER=W          
* CARD.) THIS WILL CAUSE THE UNBILLING TO OCCUR LIVE ON THE FILES, AND          
* IT WILL ALSO ADD AN UNPOSTING REQUEST TO THE REQUEST FILE IF NEEDED,          
* WHICH WILL RUN OVERNIGHT. THE PROGRAMMER HAS THE OBLIGATION TO INFORM         
* DATA CONTROL OF THE UNBILLING (USUALLY BY PROVIDING THEM WITH A COPY          
* OF THE UNBILLING REPORT).                                                     
*                                                                               
* NOTE: RUNNING WITH QOPT1 SET TO "P" IS ONLY INTENDED TO BE USED IN            
*       "EMERGENCY" SITUATIONS, WHERE THE UNBILLING CANNOT WAIT UNTIL           
*       OVERNIGHT.                                                              
*                                                                               
* REGARDLESS OF WHEN THE LIVE UNBILLING TAKES PLACE, THE PROGRAM WILL           
* ALSO ADD "DDS UNBILLING" RECORDS WHEN QOPT4 IS SET TO 'D'. THESE DDS          
* UNBILLING RECORDS MUST BE ADDED *IF AND ONLY IF* :                            
*    1) THE UNBILLING TAKES PLACE IN A CALENDAR YEAR/MONTH *AFTER* THE          
*        ORIGINAL BILLING RUN, *AND*                                            
*    2) THE DDS BILLING EXTRACT HAS ALREADY RUN FOR THE BILLING MONTH           
*        IN QUESTION.                                                           
* THE DDS UNBILLING RECORDS CONTAIN THE GROSS AND NET UNBILLED DOLLARS.         
* THESE RECORDS ARE SUBSEQUENTLY READ BY THE DDS BILLING EXTRACT                
* PROGRAM (SBL) IN ORDER TO PROVIDE A CORRECTION TO THE PC-BASED DDS            
* BILLING DATABASE.                                                             
*                                                                               
***********************************************************************         
         EJECT                                                                  
SP0702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP0702,R8                                                      
*                                                                               
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING BILWRKD,RC                                                       
*                                  SAVE BASE REGS FOR HEADHOOK                  
         L     RF,=A(SAVERA)                                                    
         ST    RA,0(RF)                                                         
         L     RF,=A(SAVER9)                                                    
         ST    R9,0(RF)                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,PRDLAST                                                     
         BE    PRDL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        RUNFRST                                                                
         SPACE 2                                                                
RUNF     DS    0H                                                               
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   RUNF10                                                           
*                                                                               
         L     R4,ADBUY            USE AS WORK AREA FOR OPEN                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',XSPFLIST,(R4)                     
*                                                                               
         B     RUNF10                                                           
*                                                                               
XSPFLIST DC    CL8'UXSPFIL'                                                     
         DC    CL8'UXSPDIR'                                                     
         DC    CL10'X'                                                          
*                                                                               
RUNF10   DS    0H                                                               
         MVC   HEADHOOK,=A(HDHOOK)                                              
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   VSPFMTIN,VSPFMINO   A(SPFMTINO)                                  
         DROP  RF                                                               
*                                                                               
         LA    RF,REPTOTS          CLEAR REPORT TOTALS                          
         USING TOTALSD,RF                                                       
         ZAP   GRSSTAB,=P'0'                                                    
         ZAP   NETSTAB,=P'0'                                                    
         ZAP   GRS2STAB,=P'0'                                                   
         ZAP   NET2STAB,=P'0'                                                   
         ZAP   GRSBLHD,=P'0'                                                    
         ZAP   NETBLHD,=P'0'                                                    
         ZAP   GRS2BLHD,=P'0'                                                   
         ZAP   NET2BLHD,=P'0'                                                   
         DROP  RF                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        REQFRST                                                                
         SPACE 2                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
*                                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFAINI',BUFFM),0,ACOMFACS                  
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BUFFERIN INITIALIZATION UNSUCCESSFUL         
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FF'-X'02'  "RECORD IS DELETED" IS OK ERROR            
*                                                                               
         MVI   POSTED,C'N'         ASSUME BILLS NOT POSTED TO ACC               
         MVI   ACTIVITY,C'N'       ASSUME NOTHING TO UNBILL                     
*                                                                               
         LA    RF,REQTOTS          CLEAR REQUEST TOTALS                         
         USING TOTALSD,RF                                                       
         ZAP   GRSSTAB,=P'0'                                                    
         ZAP   NETSTAB,=P'0'                                                    
         ZAP   GRS2STAB,=P'0'                                                   
         ZAP   NET2STAB,=P'0'                                                   
         ZAP   GRSBLHD,=P'0'                                                    
         ZAP   NETBLHD,=P'0'                                                    
         ZAP   GRS2BLHD,=P'0'                                                   
         ZAP   NET2BLHD,=P'0'                                                   
         DROP  RF                                                               
*                                  STANDARDIZE QEST (ALL + BLANK = NO)          
         CLC   QEST,=C'ALL'                                                     
         BE    *+12                                                             
         CLI   QEST,C' '                                                        
         BNE   *+10                                                             
*                                                                               
         MVC   QEST,=C'NO '                                                     
*                                                                               
         MVC   QEND,QSTART                                                      
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QSTART,(3,BQSTART)                                   
         GOTO1 DATCON,DMCB,QEND,(2,BQENDP)                                      
         GOTO1 DATCON,DMCB,QEND,(3,BQEND)                                       
*                                                                               
*                                  SET VALUES FOR RUN-DATE FILTERING            
         GOTO1 DATCON,DMCB,TODAY,(3,TODAYB)                                     
         ZIC   R1,TODAYB                                                        
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         STC   R1,DECADE           80,90,00,ETC.                                
         MVC   YEARDIG,TODAY+1     GET YEAR WITHIN DECADE                       
         NI    YEARDIG,X'FF'-X'F0' ISOLATE YEAR DIGIT                           
*                                                                               
         XC    INVFILT,INVFILT                                                  
         CLI   QINVNUM,C' '        INVOICE NUMBER GIVEN?                        
         BE    REQF5                                                            
         GOTO1 VSPFMTIN,DMCB,,(C'P',QINVNUM)                                    
         L     RF,DMCB+4                                                        
         MVC   INVFILT,0(RF)       YES: UNBILL ONLY THIS INVOICE                
*                                                                               
REQF5    DS    0H                                                               
         XC    MOSFILT,MOSFILT                                                  
         CLI   QMOSFILT,C' '       MONTH OF SERVICE FILTER GIVEN?               
         BE    REQF10                                                           
         GOTO1 DATCON,DMCB,QMOSFILT,(3,DUB)                                     
         MVC   MOSFILT,DUB         KEEP Y/M (TOSS DAY)                          
*                                                                               
REQF10   DS    0H                                                               
         CLI   QOPT1,C'S'          SOON REQUEST?                                
         BNE   REQFX                                                            
         PACK  HEADER$P,QHEADER$+1(L'QHEADER$-1) TOTAL HEAD. $ EXPECTED         
         NI    HEADER$P+L'HEADER$P-1,X'FC'  ASSUME POSITIVE                     
         CLI   QHEADER$,C'-'                CREDIT AMOUNT?                      
         BNE   *+8                                                              
         OI    HEADER$P+L'HEADER$P-1,X'01'  YES: MAKE NEGATIVE                  
*                                                                               
         MVC   DETAIL$P,HEADER$P   ASSUME DETAILS SHOULD MATCH                  
         CLI   QDETAIL$+L'QDETAIL$-1,C' '   EXPECTED DETAIL $ GIVEN?            
         BE    REQFX                                                            
         PACK  DETAIL$P,QDETAIL$+1(L'QDETAIL$-1)  WE'RE OUT OF BALANCE          
         NI    DETAIL$P+L'DETAIL$P-1,X'FC'  ASSUME POSITIVE                     
         CLI   QDETAIL$,C'-'                CREDIT AMOUNT?                      
         BNE   REQFX                                                            
         OI    DETAIL$P+L'DETAIL$P-1,X'01'  YES: MAKE NEGATIVE                  
*                                                                               
REQFX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        CLTFRST                                                                
         SPACE 2                                                                
CLTF     DS    0H                                                               
*                                                                               
         L     R4,PRDLIST                                                       
         L     R3,ADCLT                                                         
         USING CLTHDRD,R3                                                       
         LA    R5,CLIST                                                         
*                                                                               
CLTF10   DS    0H                                                               
         CLI   0(R5),0             EOL                                          
         BE    CLTF20                                                           
         XC    0(2,R4),0(R4)                                                    
         MVC   2(4,R4),0(R5)       MNEMONIC AND CODE                            
         LA    R5,4(R5)                                                         
         LA    R4,6(R4)                                                         
         B     CLTF10                                                           
*                                                                               
CLTF20   DS    0H                                                               
         XC    PROFB1X,PROFB1X     READ B1X PROFILE                             
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING PROFKD,R5                                                        
         MVI   PROFKSYS,C'S'-X'40' LOWER-CASE 'S'                               
         MVC   PROFKPGM,=C'B1X'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLT                                                     
         MVI   PROFKOI2,C'*'       SET OFFICE CODE                              
         MVC   PROFKOCD,COFFICE                                                 
         GOTO1 GETPROF,DMCB,WORK,PROFB1X,DATAMGR                                
         DROP  R3                                                               
*                                                                               
         XC    PROFB1,PROFB1       READ B1 PROFILE                              
         MVI   PROFKSYS,C'S'                                                    
         MVC   PROFKPGM,=C'OB1'                                                 
         GOTO1 GETPROF,DMCB,WORK,PROFB1,DATAMGR                                 
         DROP  R5                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        PRDLAST                                                                
         SPACE 2                                                                
PRDL     DS    0H                                                               
         MVC   KPRD,BPRD                                                        
         BRAS  RE,RNBILL                                                        
         BRAS  RE,ROBILL                                                        
         B     EXIT                                                             
         EJECT                                                                  
*        REQLAST                                                                
         SPACE 2                                                                
REQL     DS    0H                                                               
*                                                                               
         BRAS  RE,REPRT                                                         
*                                                                               
         MVI   RCSUBPRG,2                                                       
*                                                                               
         LA    R3,REPTOTS          ADD REQUEST TOTALS TO REPORT TOTALS          
REPT     USING TOTALSD,R3                                                       
         LA    R4,REQTOTS                                                       
         USING TOTALSD,R4                                                       
*                                                                               
         AP    REPT.GRSSTAB,GRSSTAB                                             
         AP    REPT.NETSTAB,NETSTAB                                             
         AP    REPT.GRS2STAB,GRS2STAB                                           
         AP    REPT.NET2STAB,NET2STAB                                           
         AP    REPT.GRSBLHD,GRSBLHD                                             
         AP    REPT.NETBLHD,NETBLHD                                             
         AP    REPT.GRS2BLHD,GRS2BLHD                                           
         AP    REPT.NET2BLHD,NET2BLHD                                           
         DROP  REPT                                                             
*                                                                               
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVC   LINV(18),=C'**REQUEST TOTALS**'                                  
         MVC   LGRS-10(6),=C'BILLS='                                            
         EDIT  GRSBLHD,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETBLHD,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2BLHD,LGRS2,2,COMMAS=YES,MINUS=YES                            
         EDIT  NET2BLHD,LNET2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
*                                                                               
         MVC   LGRS-10(8),=C'DETAILS='                                          
         EDIT  GRSSTAB,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETSTAB,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2STAB,LGRS2,2,COMMAS=YES,MINUS=YES                            
         EDIT  NET2STAB,LNET2,2,COMMAS=YES,MINUS=YES                            
         DROP  R2                                                               
*                                                                               
         CP    GRSSTAB,GRSBLHD                                                  
         BNE   REQL10                                                           
         CP    NETSTAB,NETBLHD                                                  
         BNE   REQL10                                                           
         CP    GRS2STAB,GRS2BLHD                                                
         BNE   REQL10                                                           
         CP    NET2STAB,NET2BLHD                                                
         BE    *+10                                                             
REQL10   MVC   P(21),=C'***ERROR*** IMBALANCE'                                  
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVI   P1,0                                                             
         MVC   P2(18),=C'UNPOSTING REQUIRED'                                    
         MVI   P3,0                                                             
         CLI   POSTED,C'Y'         ANY BILLS POSTED TO ACC?                     
         BE    *+10                NO                                           
         MVC   P2(46),=C'NO INVOICES POSTED TO ACC: NO UNPOSTING NEEDED+        
               '                                                                
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QOPT1,C'P'          PROGRAMMER SPECIAL (VIA TSO)?                
         BE    REQL20              YES: DON'T BOTHER CHECKING $                 
         CLI   QOPT1,C'S'          ONLY CHECK $ ON SOON REQUESTS                
         BNE   REQLX                                                            
*                                                                               
         CP    GRSBLHD,HEADER$P                                                 
         BNE   *+14                                                             
         CP    GRSSTAB,DETAIL$P                                                 
         BE    REQL20                                                           
         DROP  R4                                                               
*                                                                               
         MVC   P1(44),=C'********************************************'          
         MVC   P2(44),=C'*** COMPUTED AND EXPECTED TOTALS UNEQUAL ***'          
         MVC   P3(44),=C'***  NO LIVE REQUESTS HAVE BEEN WRITTEN  ***'          
         MVC   P4(44),=C'********************************************'          
         GOTO1 REPORT              PRINT WARNING FOR DATA CONTROL               
         B     REQLX                                                            
*                                                                               
REQL20   DS    0H                                                               
         CLI   QOPT1,C'S'          SUBMITTED VIA REQUEST PROGRAM?               
         BE    *+12                YES: WRITE OVERNIGHT REQUESTS                
         CLI   QOPT1,C'P'          PROGRAMMER SPECIAL (VIA TSO)?                
         BNE   REQLX               YES: WRITE UNPOSTING OVERNIGHT RQST          
*                                                                               
         CLI   ACTIVITY,C'Y'       ANY RECORDS TO UPDATE?                       
         BE    REQL30              YES                                          
*                                                                               
         MVC   P1(34),=C'**********************************'                    
         MVC   P2(34),=C'*** NO RECORDS FOUND TO UNBILL ***'                    
         MVC   P3(34),=C'**********************************'                    
         GOTO1 REPORT              PRINT WARNING FOR DATA CONTROL               
         B     REQLX                                                            
*                                                                               
REQL30   DS    0H                  BUILD UNBILLING REQUEST...                   
         CLI   QOPT1,C'P'                                                       
         BE    REQL50              ...BUT NOT FOR PROGRAMMER SPECIAL            
*                                                                               
         XC    REQHDR,REQHDR       26-BYTE REQUEST HEADER                       
         MVC   REQUEST,QRECORD             COPY THE SOON REQUEST...             
         MVI   QOPT1-QAREA+REQUEST,C'L'    ...SET THE 'LIVE' FLAG               
         MVI   QCONTREQ-QAREA+REQUEST,C' ' ...SINGLE-CARD REQUEST               
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   REQL40              NO                                           
         GOTO1 DATAMGR,DMCB,DMADD,=C'REQUEST',WORK,REQHDR                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P1(41),=C'*****************************************'             
         MVC   P2(41),=C'*** LIVE UNBILLING WILL RUN OVERNIGHT ***'             
         MVC   P3(41),=C'*****************************************'             
         MVI   P4,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
REQL40   DS    0H                                                               
         MVC   P1(24),=C'UNBILLING REQUEST CARD: '                              
         MVC   P1+24(80),REQUEST                                                
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
REQL50   DS    0H                                                               
         CLI   POSTED,C'Y'         ANY BILLS POSTED TO ACC?                     
         BNE   REQLX               NO                                           
*                                                                               
         XC    REQHDR,REQHDR       BUILD UNPOSTING REQUEST                      
         MVC   REQDEST,=AL2(17)    FORCE UNPOSTING REPORT TO SJR                
         MVC   REQUEST,SPACES                                                   
         MVC   QCODE-QAREA+REQUEST,=C'MY'     SET TO 'MY' REQUEST               
         MVC   QAGY-QAREA+REQUEST,QAGY        AGENCY                            
         MVC   QMED-QAREA+REQUEST,QMED        MEDIA                             
         MVC   QCLT-QAREA+REQUEST,QCLT        CLIENT                            
         MVC   QPRD-QAREA+REQUEST,=C'ALL'     ALL PRODUCTS                      
         MVC   QSTAUTO-QAREA+REQUEST,=C'ALL'  AUTO-REQUEST START DATE           
         MVI   QOPT1-QAREA+REQUEST,C'R'       'REVERSE' OPTION                  
         MVI   QOPT4-QAREA+REQUEST,C'U'       'UNPOST' OPTION                   
         MVC   QUESTOR-QAREA+REQUEST,QUESTOR  REQUESTOR NAME                    
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   REQL60              NO                                           
         GOTO1 DATAMGR,DMCB,DMADD,=C'REQUEST',WORK,REQHDR                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P1(41),=C'*****************************************'             
         MVC   P2(41),=C'*** LIVE UNPOSTING WILL RUN OVERNIGHT ***'             
         MVC   P3(41),=C'*****************************************'             
         MVI   P4,0                                                             
*                                                                               
REQL60   DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P1(24),=C'UNPOSTING REQUEST CARD: '                              
         MVC   P1+24(80),REQUEST                                                
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
REQLX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        RUNLAST                                                                
         SPACE 2                                                                
RUNL     DS    0H                                                               
*                                                                               
         LA    R3,REPTOTS          PRINT REPORT TOTALS                          
         USING TOTALSD,R3                                                       
*                                                                               
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   LINV(17),=C'**REPORT TOTALS**'                                   
         MVC   LGRS-10(6),=C'BILLS='                                            
         EDIT  GRSBLHD,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETBLHD,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2BLHD,LGRS2,2,COMMAS=YES,MINUS=YES                            
         EDIT  NET2BLHD,LNET2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
*                                                                               
         MVC   LGRS-10(8),=C'DETAILS='                                          
         EDIT  GRSSTAB,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETSTAB,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2STAB,LGRS2,2,COMMAS=YES,MINUS=YES                            
         EDIT  NET2STAB,LNET2,2,COMMAS=YES,MINUS=YES                            
         DROP  R2                                                               
*                                                                               
         CP    GRSSTAB,GRSBLHD                                                  
         BNE   RUNL10                                                           
         CP    NETSTAB,NETBLHD                                                  
         BNE   RUNL10                                                           
         CP    GRS2STAB,GRS2BLHD                                                
         BNE   RUNL10                                                           
         CP    NET2STAB,NET2BLHD                                                
         BE    *+10                                                             
RUNL10   MVC   P(21),=C'***ERROR*** IMBALANCE'                                  
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFACLO',BUFFM),0,ACOMFACS                  
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                UNSUCCESSFUL BUFFERIN CLOSE                  
*                                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* READ NEW BILL RECORDS                                                         
*                                                                               
RNBILL   NTR1                                                                   
*                                                                               
         MVC   KEY1,KEY            SAVE KEY                                     
         XC    KEY,KEY                                                          
*                                                                               
KEY@     USING STABUCKD,KEY                                                     
         MVC   KEY@.STABKCOD,=X'0E01'  RECORD TYPE                              
         MVC   KEY@.STABKAM,BAGYMD                                              
         MVC   KEY@.STABKCLT,BCLT                                               
         CLI   KPRD,0                                                           
         BE    RNB20               SKIP REST OF KEY NOW IF MULTI-PRD            
         MVC   KEY@.STABKPRD,KPRD                                               
*                                                                               
RNB10    DS    0H                                                               
         MVC   KEY@.STABKEST,BEST                                               
         XC    KEY@.STABKMKT,KEY@.STABKMKT                                      
         XC    KEY@.STABKSTA,KEY@.STABKSTA                                      
         MVI   KEY@.STABKCUR,0                                                  
*                                                                               
RNB20    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RNB40                                                            
*                                                                               
RNB30    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
RNB40    DS    0H                                                               
         CLC   KEY(5),KEYSAVE      AGENCY/MEDIA/CLIENT                          
         BNE   RNBX                                                             
         CLI   KPRD,0                                                           
         BE    RNB60                                                            
         CLC   KPRD,KEY@.STABKPRD  ONE PRODUCT MUST BE EQUAL                    
         BNE   RNBX                                                             
         B     RNB60                                                            
*                                  MULTI-PRODUCT STIUATION                      
RNB50    DS    0H                                                               
         ZIC   RF,KEY@.STABKPRD    BUMP TO NEXT PRODUCT                         
         AHI   RF,1                                                             
         STC   RF,KEY@.STABKPRD                                                 
         B     RNB10               SET KEY FROM ESTIMATE DOWN                   
*                                                                               
RNB60    DS    0H                  ESTIMATE                                     
         CLI   BEST,0                                                           
         BE    RNB70                                                            
*                                  ONE ESTIMATE OR SERIES                       
         CLC   KEY@.STABKEST,BEST                                               
         BL    RNB10                                                            
         BE    RNB70                                                            
         CLI   BESTEND,0                                                        
         BE    *+14                                                             
         CLC   KEY@.STABKEST,BESTEND                                            
         BNH   RNB70                                                            
*                                                                               
         CLI   KPRD,0              ESTIMATE NOT OK                              
         BE    RNB50               NEXT PRODUCT IF MULTI-PRODUCT                
         B     RNBX                ELSE DONE                                    
         DROP  KEY@                                                             
*                                                                               
*                                  HAVE GOOD KEY                                
RNB70    DS    0H                                                               
         L     R7,=A(STABUCKC)                                                  
         ST    R7,AREC                                                          
         USING STABUCK,R7                                                       
         GOTO1 GET                                                              
*                                                                               
         LA    R2,STABELEM                                                      
         USING STABELEM,R2                                                      
         MVI   RECSW,0             CLEAR RECORD ALTERED SWITCH                  
         B     RNB90                                                            
*                                                                               
RNB80    DS    0H                  NEXT ELEMENT                                 
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
RNB90    DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BE    RNB140                                                           
*                                                                               
         CLI   STABELEM,X'0E'      NO: LOOK FOR BILLING ELEMENTS                
         BNE   RNB80                                                            
*                                                                               
         CLC   STABBDT,BQSTARTP    DATE MUST MATCH                              
         BNE   RNB80                                                            
*                                                                               
         OC    MOSFILT,MOSFILT     IF MOS FILTER GIVEN...                       
         BZ    *+14                                                             
         CLC   STABPER,MOSFILT     ... THEN IT MUST MATCH                       
         BNE   RNB80                                                            
*                                  SPECIFIC INVOICE NUMBER?                     
         OC    INVFILT,INVFILT                                                  
         BZ    RNB100                                                           
         MVC   HALF,STABINV                                                     
         NI    HALF,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                   
         CLC   HALF,INVFILT                                                     
         BNE   RNB80                                                            
*                                                                               
RNB100   DS    0H                                                               
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
*                                                                               
         ZAP   INVGRS,=P'0'                                                     
         ZAP   INVNET,=P'0'                                                     
         ZAP   INVGRS2,=P'0'                                                    
         ZAP   INVNET2,=P'0'                                                    
*                                                                               
         MVC   INVINV,STABINV                                                   
         NI    INVINV,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                 
*                                  FIND PRODUCT CODE                            
         L     R3,PRDLIST                                                       
RNB110   DS    0H                                                               
         CLC   STABKPRD,5(R3)                                                   
         BE    *+12                                                             
         LA    R3,6(R3)                                                         
         B     RNB110                                                           
*                                                                               
         MVC   INVCLT,STABKCLT     CLIENT CODE                                  
         MVC   INVCLTNM,CLTNM      CLIENT NAME                                  
         MVC   INVPRD,2(R3)        PRODUCT CODE                                 
         MVC   INVEST,STABKEST     ESTIMATE                                     
         MVC   INVPER,STABPER      MOS                                          
         MVC   INVMKSTA,STABKMKT   MARKET/STATION                               
         GOTO1 DATCON,DMCB,(2,STABBDT),INVBDATE                                 
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'E',STABELEM),SPBVALD,0                            
*                                                                               
         CLI   STABKCUR,0          NORMAL CURRENCY?                             
         BNE   RNB120                                                           
         ZAP   INVGRS,SPBVGRSP     YES                                          
         ZAP   INVNET,SPBVNETP                                                  
         ZAP   INVGRS2,=P'0'                                                    
         ZAP   INVNET2,=P'0'                                                    
         B     RNB130                                                           
*                                                                               
RNB120   DS    0H                  NO: COS2                                     
         ZAP   INVGRS,=P'0'                                                     
         ZAP   INVNET,=P'0'                                                     
         ZAP   INVGRS2,SPBVGRSP                                                 
         ZAP   INVNET2,SPBVNETP                                                 
*                                                                               
RNB130   DS    0H                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFAPUT',BUFFM),BUFFREC,ACOMFACS            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BUFFERIN PUT UNSUCCESSFUL                    
*                                                                               
         MVI   STABELEM,X'1E'      SET ELEMENT UNBILLED                         
         MVI   RECSW,1                                                          
         B     RNB80                                                            
         DROP  R2                                                               
*                                  NO MORE BUCKET ELEMENTS                      
*                                  END OF RECORD                                
RNB140   DS    0H                                                               
         CLI   RECSW,1             RECORD ALTERED?                              
         BNE   RNB30               NO                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,STABLEN                                                     
         AR    RF,R7                                                            
         MVI   0(RF),0             SET EOR                                      
*                                                                               
         CLI   QOPT3,C'Y'          DELETE UNBILLED ELEMENTS?                    
         BNE   RNB160              NO                                           
         SR    R0,R0                                                            
         LA    R2,STABELEM         R2 = A(FIRST ELEMENT)                        
RNB150   CLI   0(R2),0             EOR?                                         
         BE    RNB160              YES                                          
         CLI   0(R2),X'1E'         ELEMENT UNBILLED?                            
         BE    *+14                YES: DELETE IT                               
         IC    R0,1(R2)            NO: BUMP TO NEXT ELEMENT                     
         AR    R2,R0                                                            
         B     RNB150                                                           
         GOTO1 RECUP,DMCB,(C'S',AREC),(R2),0,0                                  
         B     RNB150                                                           
*                                                                               
RNB160   DS    0H                                                               
         MVI   ACTIVITY,C'Y'       WE NEED TO UPDATE THE FILE                   
*                                                                               
         CLI   QOPT2,C'T'          PRINT TRACE OF UPDATES?                      
         BNE   RNB170                                                           
         GOTO1 HEXOUT,DMCB,KEY+14,TRCMSG1+33,4,=C'TOG'                          
         CLC   =F'8',DMCB+16       PRINT DISK ADDRESS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRTREC),DMCB,AREC,(24,13),PRINT,HEXOUT,C'DOME',      +        
               ('TRCMSG1X',TRCMSG1)                                             
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
RNB170   DS    0H                                                               
         CLI   QOPT1,C'L'          LIVE OVERNIGHT RUN?                          
         BE    *+12                YES                                          
         CLI   QOPT1,C'P'          LIVE PROGRAMMER'S RUN (VIA TSO)?             
         BNE   RNB30               NO: DON'T ACTUALLY UNBILL                    
*                                                                               
         GOTO1 PUT                                                              
         B     RNB30                                                            
         DROP  R7                                                               
*                                                                               
RNBX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* READ OLD BILL RECORDS                                                         
*                                                                               
ROBILL   NTR1                                                                   
*                                                                               
         MVC   KEY1,KEY            SAVE KEY                                     
         XC    KEY,KEY                                                          
*                                                                               
KEY@     USING BKEY,KEY                                                         
         MVC   KEY@.BKEYAM,BAGYMD                                               
         MVC   KEY@.BKEYCLT,BCLT                                                
         CLI   KPRD,0                                                           
         BE    ROB20               SKIP REST OF KEY IF MULTI-PRODUCT            
         MVC   KEY@.BKEYPRD,PRD                                                 
*                                                                               
ROB10    DS    0H                                                               
         MVC   KEY@.BKEYEST,BEST                                                
         MVI   KEY@.BKEYYSRV,0                                                  
         MVI   KEY@.BKEYMSRV,0                                                  
         MVI   KEY@.BKEYMBIL,0                                                  
         XC    KEY@.BKEYINV,KEY@.BKEYINV                                        
*                                                                               
ROB20    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     ROB40                                                            
*                                                                               
ROB30    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
ROB40    DS    0H                                                               
         CLC   KEY(4),KEYSAVE      AGENCY/MEDIA/CLIENT                          
         BNE   ROBX                                                             
*                                                                               
         CLI   KPRD,0                                                           
         BE    *+14                                                             
         CLC   PRD,KEY@.BKEYPRD    ONE PRODUCT MUST BE EQUAL                    
         BNE   ROBX                                                             
*                                  MULTI-PRODUCT SITUATION                      
         CLI   KEY@.BKEYYSRV,0     BYPASS IF NOT A BILL                         
         BNE   ROB50                                                            
         CLI   KEY@.BKEYMSRV,0                                                  
         BNE   ROB50                                                            
         CLI   KEY@.BKEYMBIL,0                                                  
         BNE   ROB50                                                            
         OC    KEY@.BKEYINV,KEY@.BKEYINV                                        
         BZ    ROB110                                                           
*                                                                               
ROB50    DS    0H                                                               
         OC    INVFILT,INVFILT     SPECIFIC INVOICE NUMBER?                     
         BZ    *+14                                                             
         CLC   INVFILT,KEY@.BKEYINV                                             
         BNE   ROB110                                                           
*                                                                               
         OC    MOSFILT,MOSFILT     SPECIFIC MONTH OF SERVICE?                   
         BZ    *+14                                                             
         CLC   MOSFILT,KEY@.BKEYYSRV                                            
         BNE   ROB110                                                           
*                                                                               
         CLI   BEST,0                                                           
         BE    ROB60                                                            
*                                  ONE ESTIMATE OR SERIES                       
         CLC   KEY@.BKEYEST,BEST                                                
         BL    ROB10                                                            
         BE    ROB60                                                            
*                                                                               
         CLI   BESTEND,0                                                        
         BE    *+14                                                             
         CLC   KEY@.BKEYEST,BESTEND                                             
         BNH   ROB60                                                            
*                                  ESTIMATE NOT OK                              
         CLI   KPRD,0                                                           
         BNE   ROBX                DONE UNLESS MULTI-PRODUCT                    
*                                                                               
         ZIC   RF,KEY@.BKEYPRD+2   BUMP TO NEXT PRODUCT                         
         AHI   RF,1                                                             
         STC   RF,KEY@.BKEYPRD+2                                                
         B     ROB10               SET KEY FROM ESTIMATE DOWN                   
*                                                                               
ROB60    DS    0H                  BILL DATE                                    
         ZIC   R3,KEY@.BKEYMBIL                                                 
         SRL   R3,4                YEAR DIGIT OF BILL                           
         ZIC   RE,DECADE                                                        
         CLM   R3,1,YEARDIG        COMPARE TO YEAR OF TODAY                     
         BNH   *+8                 IF NOT HIGH, OK                              
         SHI   RE,10               ELSE BACK UP TO PREVIOUS DECADE              
         AR    RE,R3                                                            
         STC   RE,HALF             CALCULATED YEAR OF BILL                      
*                                                                               
         MVC   HALF+1(1),KEY@.BKEYMBIL                                          
         NI    HALF+1,X'FF'-X'F0'                                               
         DROP  KEY@                                                             
*                                                                               
         CLC   HALF,BQSTART        DATE MUST MATCH                              
         BNE   ROB110                                                           
*                                                                               
         TM    KEY+13,X'80'        DELETED?                                     
         BO    ROB110              YES -- SKIP                                  
         SPACE 3                                                                
*                                  HAVE GOOD KEY                                
*                                  PASS DATA TO SORT                            
         GOTO1 GETBILL                                                          
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         CLC   BDATE,QSTART        RUN DATE MUST MATCH                          
         BNE   ROB110                                                           
*                                                                               
         CLI   BTYPE,C'B'          MUST BE A NEW BILLING BILL                   
         BNE   ROB110                                                           
*                                                                               
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
*                                                                               
         ZAP   INVGRS,=P'0'                                                     
         ZAP   INVNET,=P'0'                                                     
         ZAP   INVGRS2,=P'0'                                                    
         ZAP   INVNET2,=P'0'                                                    
*                                                                               
         MVC   INVINV,BKEYINV      INVOICE NUMBER                               
*                                                                               
         MVC   INVCLT,BKEYCLT      CLIENT CODE                                  
         MVC   INVCLTNM,CLTNM      CLIENT NAME                                  
         MVC   INVPRD,BKEYPRD      PRODUCT CODE                                 
         MVC   INVEST,BKEYEST      ESTIMATE                                     
         MVC   INVPER,BKEYYSRV     MOS                                          
         MVC   INVBDATE,BDATE                                                   
         MVC   INVRVRS#,BLREVINO   REVERSED BILL NUMBER                         
         MVC   INVRVRDT,BLREVDAT   REVERSED BILL DATE                           
*                                                                               
         MVC   INVMKSTA,=X'FFFFFFFFFF' FORCES BILL HEADERS TO SORT LAST         
*                                                                               
         MVC   INVBTYPE(2),BTYPE   BILLTYPE (B4, B5, B6, B7)                    
         MVC   INVBTYPE+2(2),SPACES                                             
*                                                                               
         TM    BILSTAT,BSTMANQ     MANUAL BILL?                                 
         BZ    *+16                                                             
         MVC   INVBTYPE(3),=C'MAN'                                              
         MVC   INVBTYPE+3(1),BTYPE+1                                            
*                                                                               
         TM    BILSTAT,BSTTAORQ    AOR BILL?                                    
         BZ    *+16                                                             
         MVC   INVBTYPE(3),=C'AOR'                                              
         MVC   INVBTYPE+3(1),BTYPE+1                                            
*                                                                               
         TM    BILSTAT,BSTSCOMQ    SEP COMM BILL?                               
         BZ    *+16                                                             
         MVC   INVBTYPE(3),=C'UFC'                                              
         MVC   INVBTYPE+3(1),BTYPE+1                                            
*                                                                               
         TM    BILSTAT,BSTSNETQ    SEP NET BILL?                                
         BZ    *+16                                                             
         MVC   INVBTYPE(3),=C'NET'                                              
         MVC   INVBTYPE+3(1),BTYPE+1                                            
*                                                                               
         MVI   INVBTYPE+3,C'S'                                                  
         CLI   BRETAIL,X'41'       SUMMARY                                      
         BE    ROB70                                                            
         MVI   INVBTYPE+3,C'C'                                                  
         CLI   BRETAIL,X'81'       CORP CONTROL                                 
         BE    ROB70                                                            
         MVI   INVBTYPE+3,C' '                                                  
*                                                                               
ROB70    DS    0H                                                               
         TM    BILSTAT3,BSTSOONQ   UPDATIVE SOON BILL?                          
         BZ    *+8                                                              
         OI    INVSTAT,INVSOONQ    YES                                          
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
*                                                                               
         ZAP   INVGRS,SPBVGRSP     EFFECTIVE GROSS                              
         CLC   =C'AOR',INVBTYPE    FOR AOR BILLS                                
         BE    *+10                SKIP NET (USED FOR SOMETHING ELSE)           
         ZAP   INVNET,SPBVNETP     ELSE EFFECTIVE NET                           
*                                                                               
         CLI   BRETAIL,0           RETAIL BILLS USE COS2 FIELDS...              
         BNE   *+16                ...FOR OTHER PURPOSES                        
         ZAP   INVGRS2,BGRS2P      SET COS2                                     
         ZAP   INVNET2,BNET2P                                                   
         DROP  R4                                                               
*                                                                               
         OC    BILPOST,BILPOST     TRANSFERRED TO ACC?                          
         BZ    *+8                 NO                                           
         MVI   POSTED,C'Y'         ... AND SET GLOBAL FLAG                      
*                                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFAPUT',BUFFM),BUFFREC,ACOMFACS            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BUFFERIN PUT UNSUCCESSFUL                    
*                                                                               
         OI    BCNTRL,X'80'        SET RECORD AND POINTER DELETED               
         OI    KEY+13,X'80'                                                     
         TM    BCNTRL,X'01'        IF RECORD NOT POSTED TO ACC...               
         BZ    *+12                ...THEN SIMPLY LEAVE IT DELETED              
         OI    BCNTRL,X'40'        ...ELSE MARK CLOSED-OUT AND THE              
         OI    KEY+13,X'40'        ...UNPOST PROGRAM WILL SUBSEQUENTLY          
*                                  ...TURN OFF THE CLOSED-OUT BIT               
*                                                                               
         MVI   ACTIVITY,C'Y'       WE NEED TO UPDATE THE FILE                   
*                                                                               
         CLI   QOPT2,C'T'          PRINT TRACE OF UPDATES?                      
         BNE   ROB80                                                            
         GOTO1 HEXOUT,DMCB,KEY+14,TRCMSG2+30,4,=C'TOG'                          
         CLC   =F'8',DMCB+16       PRINT DISK ADDRESS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,BLEN           RECORD LENGTH                                
         GOTO1 PRNTBL,DMCB,('TRCMSG2X',TRCMSG2),ADBILL,C'DUMP',(R0),   +        
               =C'1D'                                                           
*                                                                               
ROB80    DS    0H                                                               
         CLI   QOPT1,C'L'          LIVE OVERNIGHT RUN?                          
         BE    *+12                YES                                          
         CLI   QOPT1,C'P'          LIVE PROGRAMMER'S RUN (VIA TSO)?             
         BNE   ROB90               NO: DON'T ACTUALLY UNBILL                    
*                                                                               
         GOTO1 WRITE                                                            
         GOTO1 PUTBILL                                                          
*                                                                               
ROB90    DS    0H                                                               
         CLI   QOPT4,C'D'          GENERATE DDS UNBILLING RECORDS?              
         BNE   ROB110              NO                                           
*                                                                               
         CLI   BRETAIL,X'41'       NOT FOR RETAIL BILLS                         
         BE    ROB110                                                           
*                                                                               
         L     R5,AREC             BUILD DDS UNBILLING RECORD                   
         USING UNBRECD,R5                                                       
         XC    UNBRECD(256),UNBRECD    RECORD WILL HAVE NO ELEMENTS             
         MVI   UNBKSYS,UNBKSYSQ    X'0E08' RECORDS                              
         MVI   UNBKSTYP,UNBKSTYQ                                                
         MVC   UNBKAM,BKEYAM       AGY/MED                                      
         MVC   UNBKCLT,BKEYCLT     CLIENT                                       
         MVC   UNBKPRD,BKEYPRD     PRODUCT                                      
         MVC   UNBKEST,BKEYEST     ESTIMATE                                     
         MVC   UNBKUYM,TODAYB      UNBILLING DATE (YR/MNTH)                     
         MVC   UNBKUDAY,TODAYB+2   UNBILLING DATE (DAY NUMBER)                  
         MVC   UNBKYSRV,BKEYYSRV   YEAR/MONTH OF SERVICE (BINARY)               
         MVC   UNBKMSRV,BKEYMSRV                                                
         GOTO1 DATCON,DMCB,BDATE,(2,UNBKBDAT)   BILLING DATE                    
         MVC   UNBKINV,BKEYINV     INVOICE NUMBER                               
         LHI   RE,UNBELDQ          LENGTH OF FIXED PORTION OF RECORD            
         AHI   RE,1                PLUS ONE FOR EOR MARKER                      
         STH   RE,UNBRECLN         RECORD LENGTH                                
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'B',(R7)),SPBVALD,0                                
         CP    SPBVGRSP,=P'0'                                                   
         BNE   *+14                                                             
         CP    SPBVNETP,=P'0'                                                   
         BE    ROB110                                                           
*                                                                               
         ZAP   UNBKGRS,SPBVGRSP    GROSS                                        
         ZAP   UNBKNET,SPBVNETP    NET                                          
*                                                                               
         XC    FULL,FULL                                                        
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   ROB100                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFIL',0,(R5),0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,DMCB+8         DISK ADDRESS OF ADDED RECORD                 
*                                                                               
ROB100   DS    0H                                                               
         CLI   QOPT2,C'T'          PRINT TRACE OF UPDATES?                      
         BNE   ROB110                                                           
         GOTO1 HEXOUT,DMCB,FULL,TRCMSG3+30,4,=C'TOG'                            
         CLC   =F'8',DMCB+16       PRINT DISK ADDRESS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,UNBRECLN       RECORD LENGTH                                
         GOTO1 PRNTBL,DMCB,('TRCMSG3X',TRCMSG3),(R5),C'DUMP',(R0),     +        
               =C'1D'                                                           
         DROP  R5                                                               
*                                                                               
ROB110   DS    0H                                                               
         B     ROB30                                                            
*                                                                               
ROBX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
* REPRT - PRINT REPORT FROM BUFFERIN RECORDS                                    
*                                                                               
REPRT    NTR1                                                                   
*                                                                               
         LA    RF,BUFFM                                                         
         OC    BUFFNREC-BUFFD(,RF),BUFFNREC-BUFFD(RF)                           
         BZ    REPX                NOTHING TO UNBILL                            
*                                                                               
         LA    R2,P                                                             
         USING LINED,R2                                                         
         XC    OLDKEY,OLDKEY                                                    
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
         GOTO1 =V(BUFFERIN),DMCB,('BUFFARDH',BUFFM),BUFFREC,ACOMFACS            
         B     REP20                                                            
*                                                                               
REP10    DS    0H                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFASEQ',BUFFM),BUFFREC,ACOMFACS            
*                                                                               
REP20    DS    0H                                                               
         TM    4(R1),BUFFEEOF      AT EOF...                                    
         BZ    REP30                                                            
         MVI   BUFFREC,X'FF'       ...FORCE A "RECORD" OF X'FF'S                
         MVC   BUFFREC+1(L'BUFFREC-1),BUFFREC                                   
         B     REP40                                                            
*                                                                               
REP30    DS    0H                                                               
*                                                                               
         LA    R3,OLDKEY                                                        
OKEY     USING INVD,R3                                                          
         CLC   INVCLT,OKEY.INVCLT  SAME CLT/PRD/EST/PERIOD?                     
         BNE   REP40                                                            
         CLC   INVPRD,OKEY.INVPRD                                               
         BNE   REP40                                                            
         CLC   INVEST,OKEY.INVEST                                               
         BNE   REP40                                                            
         CLC   INVPER,OKEY.INVPER                                               
         BE    REP70               YES                                          
*                                                                               
REP40    DS    0H                                                               
         OC    OLDKEY,OLDKEY       FIRST TIME                                   
         BZ    REP60                                                            
*                                  NO: FINISH LAST INVOICE                      
         LA    R5,INVTOTS                                                       
         USING TOTALSD,R5                                                       
         CP    GRSSTAB,GRSBLHD                                                  
         BNE   *+14                                                             
         CP    NETSTAB,NETBLHD                                                  
         BE    REP50                                                            
         MVC   P(21),=C'***ERROR*** IMBALANCE'                                  
         MVC   LGRS-17(15),=C'SUM OF DETAILS='                                  
         EDIT  GRSSTAB,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETSTAB,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2STAB,LGRS2,2,COMMAS=YES,MINUS=YES                            
         EDIT  NET2STAB,LNET2,2,COMMAS=YES,MINUS=YES                            
         DROP  R5                                                               
*                                                                               
         CLI   HAVEBILL,C'Y'                                                    
         BE    *+10                                                             
         MVC   P+12(14),=C'NO BILL HEADER'                                      
         CLI   HAVEDETS,C'Y'                                                    
         BE    *+10                                                             
         MVC   P+12(15),=C'NO DETAIL ITEMS'                                     
         GOTO1 REPORT                                                           
*                                                                               
REP50    DS    0H                                                               
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
REP60    DS    0H                                                               
         MVI   HAVEBILL,C'N'       ASSUME NO BILL HEADERS WILL BE SEEN          
         MVI   HAVEDETS,C'N'       ASSUME NO BILL DETAILS WILL BE SEEN          
*                                                                               
         CLI   INVINV,X'FF'                                                     
         BE    REPX                                                             
*                                                                               
         CLC   INVCLT,OKEY.INVCLT  SAME CLIENT?                                 
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       NO: EJECT PAGE                               
         DROP  OKEY                                                             
*                                                                               
         LA    RF,INVTOTS          CLEAR INVOICE TOTALS                         
         USING TOTALSD,RF                                                       
         ZAP   GRSSTAB,=P'0'                                                    
         ZAP   NETSTAB,=P'0'                                                    
         ZAP   GRS2STAB,=P'0'                                                   
         ZAP   NET2STAB,=P'0'                                                   
         ZAP   GRSBLHD,=P'0'                                                    
         ZAP   NETBLHD,=P'0'                                                    
         ZAP   GRS2BLHD,=P'0'                                                   
         ZAP   NET2BLHD,=P'0'                                                   
         DROP  RF                                                               
*                                                                               
REP70    DS    0H                                                               
         MVC   OLDKEY,BUFFREC                                                   
*                                                                               
         GOTO1 VSPFMTIN,DMCB,INVBDATE,(2,INVINV),(QMED,PROFB1),PROFB1X          
*        L     RF,DMCB+4                                                        
         L     RF,DMCB                                                          
         MVC   LINV,0(RF)          INVOICE NUMBER                               
*                                                                               
         MVC   LPRD,INVPRD         PRODUCT                                      
*                                                                               
         ZIC   RF,INVEST           ESTIMATE                                     
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
*                                                                               
*                                  PERIOD                                       
         CLI   INVPER+1,12                                                      
         BH    REP80                                                            
*                                  1 - 12 TREAT AS MONTH                        
         GOTO1 DATCON,DMCB,(3,INVPER),(6,WORK)                                  
         MVC   LPER,WORK                                                        
         B     REP90                                                            
*                                                                               
REP80    DS    0H                  SPECIAL BILLING PERIOD                       
         ZIC   RF,INVPER                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LPER+4(2),DUB                                                    
         MVI   LPER+3,C'/'                                                      
         ZIC   RF,INVPER+1                                                      
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LPER+1(2),DUB                                                    
*                                                                               
REP90    DS    0H                                                               
*                                  MARKET-STATION                               
         CLC   INVMKSTA,=X'FFFFFFFFFF'  BILL HEADER RECORD?                     
         BNE   REP100              NO                                           
*                                                                               
         MVI   HAVEBILL,C'Y'       REMEMBER THAT WE HAVE A BILL HEADER          
         MVC   LBILHEAD,=C'BILL'                                                
*                                                                               
         MVC   LBILTYP,INVBTYPE    BILL TYPE                                    
*                                                                               
         TM    INVSTAT,INVSOONQ    UPDATIVE SOON BILL?                          
         BZ    *+10                                                             
         MVC   LBILSOON,=C'SOON'   YES                                          
*                                                                               
         OC    INVRVRS#,INVRVRS#   UNBILLING A REVERSAL?                        
         BZ    REP110              NO                                           
         MVC   LBILRVSL,=C'UNREVERSE YM-NNNN:MMMDD/YY'                          
         GOTO1 DATCON,DMCB,(2,INVRVRDT),WORK                                    
         GOTO1 VSPFMTIN,DMCB,WORK,(2,INVRVRS#),(QMED,PROFB1),PROFB1X            
         L     RF,DMCB+4                                                        
         MVC   LBILRVSL+10(7),0(RF) REVERSED INVOICE NUMBER                     
         GOTO1 DATCON,DMCB,(2,INVRVRDT),(8,LBILRVSL+18)                         
         B     REP110                                                           
*                                                                               
REP100   DS    0H                                                               
         MVI   HAVEDETS,C'Y'       REMEMBER THAT WE HAVE BILL DETAILS           
         GOTO1 MSUNPK,DMCB,(X'80',INVMKSTA),LMKT,LSTA                           
*                                                                               
REP110   DS    0H                                                               
         EDIT  INVGRS,LGRS,2,COMMAS=YES,MINUS=YES                               
         EDIT  INVNET,LNET,2,COMMAS=YES,MINUS=YES                               
         EDIT  INVGRS2,LGRS2,2,COMMAS=YES,MINUS=YES                             
         EDIT  INVNET2,LNET2,2,COMMAS=YES,MINUS=YES                             
         GOTO1 REPORT                                                           
*                                  ADD TO TOTALS                                
         CLC   =C'AOR',INVBTYPE    NOT FOR AOR BILLS                            
         BE    REP120                                                           
         CLI   INVBTYPE+3,C'S'     OR RETAIL SUMMARY BILLS                      
         BE    REP120                                                           
*                                                                               
         CLC   INVMKSTA,=X'FFFFFFFFFF'  DATA FROM BILL HEADER RECORD?           
         BE    REP115              YES                                          
*                                                                               
         USING TOTALSD,RF                                                       
         LA    RF,INVTOTS                                                       
         AP    GRSSTAB,INVGRS      NO: ADD TO STABUCK DETAIL TOTALS             
         AP    NETSTAB,INVNET                                                   
         AP    GRS2STAB,INVGRS2                                                 
         AP    NET2STAB,INVNET2                                                 
         LA    RF,REQTOTS                                                       
         AP    GRSSTAB,INVGRS                                                   
         AP    NETSTAB,INVNET                                                   
         AP    GRS2STAB,INVGRS2                                                 
         AP    NET2STAB,INVNET2                                                 
         B     REP120                                                           
*                                                                               
REP115   DS    0H                  ADD TO BILL HEADER TOTALS                    
         LA    RF,INVTOTS                                                       
         AP    GRSBLHD,INVGRS      NO: ADD TO BILL HEADER TOTALS                
         AP    NETBLHD,INVNET                                                   
         AP    GRS2BLHD,INVGRS2                                                 
         AP    NET2BLHD,INVNET2                                                 
         LA    RF,REQTOTS                                                       
         AP    GRSBLHD,INVGRS                                                   
         AP    NETBLHD,INVNET                                                   
         AP    GRS2BLHD,INVGRS2                                                 
         AP    NET2BLHD,INVNET2                                                 
         DROP  RF                                                               
*                                                                               
REP120   DS    0H                                                               
         B     REP10                                                            
*                                                                               
REPX     DS    0H                                                               
         MVI   FORCEHED,C'Y'       EJECT PAGE                                   
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                  BUFFERIN PARAMETERS                          
BUFFM    BUFFD TYPE=P,KEYLEN=INVKL,COMLEN=INVCL,COLUMNS=4,FILE=BUFFWK, +        
               BUFFERS=255                                                      
         SPACE 3                                                                
TRCMSG1  DC    C'PUT STATION BUCKET RECORD: D/A = XXXXXXXX'                     
TRCMSG1X EQU   *-TRCMSG1                                                        
         SPACE 3                                                                
TRCMSG2  DC    C'PUT BILL HEADER RECORD: D/A = XXXXXXXX'                        
TRCMSG2X EQU   *-TRCMSG2                                                        
         SPACE 3                                                                
TRCMSG3  DC    C'ADD DDS UNBILLING REC:  D/A = XXXXXXXX'                        
TRCMSG3X EQU   *-TRCMSG3                                                        
         SPACE 3                                                                
         DS    0D                                                               
STABUCKC DS    4000C                                                            
         DC    X'00'                                                            
         EJECT                                                                  
HDHOOK   NMOD1 0,HEADHOOK                                                       
         L     RA,SAVERA                                                        
         L     R9,SAVER9                                                        
         LA    RC,SPACEND                                                       
         B     HDHK10                                                           
*                                                                               
SAVERA   DS    F                                                                
SAVER9   DS    F                                                                
*                                                                               
HDHK10   DS    0H                                                               
         CLI   RCSUBPRG,1                                                       
         BNE   HDHKX                                                            
*                                                                               
         MVC   HEAD4(6),=C'CLIENT'                                              
         GOTO1 CLUNPK,DMCB,INVCLT,HEAD4+9                                       
         MVC   HEAD4+13(L'INVCLTNM),INVCLTNM                                    
*                                                                               
         MVC   HEAD6(13),=C'BILLING DATE-'                                      
         GOTO1 DATCON,DMCB,QSTART,(5,HEAD6+15)                                  
*                                                                               
         SPACE 2                                                                
HDHKX    DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
BILWRKD  DSECT                                                                  
*                                                                               
VSPFMTIN DS    V                   V(SPFMTINO)                                  
*                                                                               
PROFB1   DS    CL16                B1 PROFILE                                   
PROFB1X  DS    CL16                B1X PROFILE                                  
*                                                                               
         DS    0D                                                               
INVTOTS  DS    XL(TOTALSLQ)        INVOICE-LEVEL TOTALS                         
REQTOTS  DS    XL(TOTALSLQ)        REQUEST-LEVEL TOTALS                         
REPTOTS  DS    XL(TOTALSLQ)        REPORT-LEVEL TOTALS                          
*                                                                               
BUFFREC  DS    XL(INVRLQ)                                                       
OLDKEY   DS    XL(INVRLQ)                                                       
KPRD     DS    X                                                                
RECSW    DS    X                                                                
DECADE   DS    X                                                                
YEARDIG  DS    X                                                                
INVFILT  DS    H                   SPECIFIC INVOICE NUMBER TO UNBILL            
MOSFILT  DS    XL2                 MONTH OF SERVICE (BINARY Y/M)                
*                                                                               
POSTED   DS    C                   'Y' = 1 OR MORE BILLS POSTED TO ACC          
ACTIVITY DS    C                   'Y' = WE NEED TO UPDATE THE FILE             
HAVEBILL DS    C                   'Y' = BILL HEADER EXISTS FOR INVOICE         
HAVEDETS DS    C                   'Y' = DETAIL(S) EXIST FOR INVOICE            
*                                                                               
HEADER$P DS    PL8                 EXPECTED TOTAL BILL HEADER $                 
DETAIL$P DS    PL8                 EXPECTED TOTAL STABUCK $                     
         EJECT                                                                  
REQHDR   DS    0XL26                                                            
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    CL80                BUILD OVERNIGHT REQUEST CARD HERE            
         EJECT                                                                  
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
LINED    DSECT                                                                  
LINV     DS    CL10                INVOICE NUMBER                               
         DS    C                                                                
LPRD     DS    CL3                 PRODUCT                                      
         DS    C                                                                
LEST     DS    CL3                 ESTIMATE                                     
         DS    C                                                                
LPER     DS    CL6                 MONTH OF SERVICE                             
         DS    C                                                                
LMKT     DS    CL4                 MARKET                                       
         DS    C                                                                
LSTA     DS    CL8                 STATION                                      
         DS    CL29                SPARE                                        
         DS    C                                                                
         ORG   LMKT                                                             
LBILHEAD DS    CL4                 'BILL'                                       
         DS    C                                                                
LBILTYP  DS    CL4                 BILL TYPE                                    
         DS    C                                                                
LBILSOON DS    CL4                 'SOON' (FOR UPDATIVE SOON BILLS)             
         DS    CL2                                                              
LBILRVSL DS    CL26                'UNREVERSE YM-NNNN:MMMDD/YY'                 
         DS    C                                                                
         ORG                                                                    
LGRS     DS    CL15                GROSS                                        
         DS    C                                                                
LNET     DS    CL15                NET                                          
         DS    C                                                                
LGRS2    DS    CL15                GROSS (COS2)                                 
         DS    C                                                                
LNET2    DS    CL15                NET (COS2)                                   
         SPACE 3                                                                
*                                  DSECT FOR TABLE ENTRY                        
INVD     DSECT                                                                  
INVCLT   DS    XL2                 CLIENT                                       
INVPRD   DS    CL3                 PRODUCT                                      
INVEST   DS    HL1                 ESTIMATE                                     
INVPER   DS    XL2                 MOS                                          
INVMKSTA DS    XL5                 MARKET/STATION                               
INVINV   DS    HL2                 INVOICE NUMBER                               
INVBTYPE DS    CL4                 BILL TYPE (BILL HEADERS ONLY)                
INVKL    EQU   *-INVD              LENGTH OF BUFFERIN KEY                       
INVCOMM  EQU   *                                                                
INVBDATE DS    CL6                 BILL DATE                                    
INVCLTNM DS    CL24                CLIENT NAME                                  
INVRVRS# DS    HL2                 REVERSED BILL NUMBER                         
INVRVRDT DS    XL2                 REVERSED BILL DATE                           
INVSTAT  DS    X                   VARIOUS STATUS FLAGS                         
INVSOONQ EQU   X'80'                -GENERATED VIA UPDATIVE SOON                
INVCL    EQU   *-INVCOMM           LENGTH OF BUFFERIN "COMMENT"                 
INVGRS   DS    PL8                 GROSS                                        
INVNET   DS    PL8                 NET                                          
INVGRS2  DS    PL8                 GROSS (COS2)                                 
INVNET2  DS    PL8                 NET (COS2)                                   
INVRLQ   EQU   *-INVD                                                           
         SPACE 3                                                                
TOTALSD  DSECT                                                                  
GRSSTAB  DS    PL8                 BILL DETAILS GROSS                           
NETSTAB  DS    PL8                 BILL DETAILS NET                             
GRS2STAB DS    PL8                 BILL DETAILS GROSS (COS2)                    
NET2STAB DS    PL8                 BILL DETAILS NET (COS2)                      
GRSBLHD  DS    PL8                 BILL HEADER GROSS                            
NETBLHD  DS    PL8                 BILL HEADER NET                              
GRS2BLHD DS    PL8                 BILL HEADER GROSS (COS2)                     
NET2BLHD DS    PL8                 BILL HEADER NET (COS2)                       
TOTALSLQ EQU   *-TOTALSD                                                        
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE SPGENUNBIL                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         EJECT                                                                  
       ++INCLUDE DDGETPROFD                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
*                                                                               
* NOTE: ONLY THE *FIRST* REQUEST CARD IS WRITTEN TO THE REQUEST FILE            
*       FOR THE OVERNIGHT (LIVE) UNBILLINGS!!!                                  
*                                                                               
         ORG   QAREA+49                                                         
QINVNUM  DS    CL4                 INVOICE NUMBER (OPTIONAL)                    
QMOSFILT DS    CL4                 MONTH OF SERVICE (OPTIONAL)                  
         ORG   Q2USER                                                           
QHEADER$ DS    CL12                BILL HEADER EXPECTED TOTAL                   
QDETAIL$ DS    CL12                DETAIL EXPECTED TOTAL                        
         ORG                                                                    
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107SPREP0702 09/12/05'                                      
         END                                                                    
