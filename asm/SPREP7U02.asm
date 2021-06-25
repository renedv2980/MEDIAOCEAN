*          DATA SET SPREP7U02  AT LEVEL 026 AS OF 08/07/13                      
*PHASE SP7U02A                                                                  
*INCLUDE BUFFERIN                                                               
*INCLUDE PRTREC                                                                 
         TITLE 'SPREP7U02 - NETPAK UNBILLING'                                   
**********************************************************************          
*                                                                               
*    QOPT1:  (SEE DETAILED EXPLANATION BELOW)                                   
*            'S' = SUBMITTED AS UPDATIVE SOON VIA REQUEST PROGRAM               
*            'L' = LIVE (OVERNIGHT) REQUEST FROM REQUEST FILE                   
*            'P' = LIVE "UPDATIVE SOON" UNBILLING ISSUED VIA TSO                
*            ' ' = DRAFT UNBILLING ISSUED VIA TSO                               
*    QOPT2:  'T' = PRINT TRACE OF FILE UPDATES                                  
*    QOPT4:  'D' = CREATE DDS UNBILLING RECORDS                                 
*                                                                               
* THE UNBILLING PROGRAM CAN RUN IN MANY DIFFERENT MODES, ALL OF WHICH           
* ARE DRIVEN BY QOPT1. (NOTE THAT REGARDLESS OF THE QOPT1 SETTING,              
* IF A WRITE=NO CARD IS PRESENT, NO UPDATE WILL OCCUR TO ANY FILE.)             
*                                                                               
* TYPICALLY, THE WAY UNBILLING WORKS IS THIS: DATA CONTROL MAKES AN             
* UNBILLING REQUEST VIA THE REQUEST PROGRAM, ASKING IT TO RUN "SOON."           
* THE REQUEST PROGRAM GENERATES AN UPDATIVE SOON REQUEST WITH AN "S" IN         
* QOPT1. THE UNBILLING PROGRAM COMPARES THE AMOUNTS IT FOUND TO UNBILL          
* WITH THE AMOUNT(S) ENTERED BY DATA CONTROL IN THE REQUEST (THIS ACTS          
* AS A CHECKSUM). IF THEY MATCH, AN UNBILLING REQUEST IS WRITTEN TO THE         
* REQUEST FILE TO RUN *OVERNIGHT* WITH QOPT1 SET TO "L". IF NECESSARY,          
* OVERNIGHT UNPOSTING REQUESTS ARE ALSO WRITTEN TO THE REQUEST FILE.            
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
* IT WILL ALSO ADD ANY NEEDED UNPOSTING REQUEST(S) TO THE REQUEST FILE,         
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
**********************************************************************          
         EJECT                                                                  
SP7U02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP7U02                                                         
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
         CLI   MODE,CLTLAST                                                     
         BE    CLTL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
*        RUNFRST                                                                
         SPACE 2                                                                
RUNF     DS    0H                                                               
*                                                                               
         MVC   HEADHOOK,=A(HDHOOK)                                              
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   VSPFMTIN,VSPFMINO   A(SPFMTINO)                                  
         DROP  RF                                                               
*                                                                               
         LA    RF,REPTOTS          CLEAR REPORT TOTALS                          
         USING TOTALSD,RF                                                       
         ZAP   GRSUNIT,=P'0'                                                    
         ZAP   NETUNIT,=P'0'                                                    
         ZAP   GRS2UNIT,=P'0'                                                   
         ZAP   GRSUBIL,=P'0'                                                    
         ZAP   NETUBIL,=P'0'                                                    
         ZAP   GRS2UBIL,=P'0'                                                   
         ZAP   GRSBLHD,=P'0'                                                    
         ZAP   NETBLHD,=P'0'                                                    
         ZAP   GRS2BLHD,=P'0'                                                   
         ZAP   GRSSTAB,=P'0'                                                    
         ZAP   NETSTAB,=P'0'                                                    
         ZAP   GRS2STAB,=P'0'                                                   
         DROP  RF                                                               
*                                                                               
         J     XIT                                                              
         SPACE 3                                                                
*        REQFRST                                                                
         SPACE 2                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
*                                                                               
         XC    PROFB1S,PROFB1S     READ AGENCY-LEVEL B1S PROFILE                
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING PROFKD,R5                                                        
         MVI   PROFKSYS,C'S'-X'40' LOWER-CASE 'S'                               
         MVC   PROFKPGM,=C'B1S'                                                 
         MVC   PROFKAGN,AGY                                                     
         GOTO1 GETPROF,DMCB,(X'C0',WORK),PROFB1S,DATAMGR                        
         DROP  R5                                                               
*                                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFAINI',A(BUFFM)),0,ACOMFACS               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BUFFERIN INITIALIZATION UNSUCCESSFUL         
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FF'-X'02'  "RECORD IS DELETED" IS OK ERROR            
*                                                                               
         L     RF,=A(MYREQTAB)     A(UNPOST REQUESTS)                           
         ST    RF,MYP2ATAB         BINSRCH P2                                   
         LHI   R0,MAXREQS                                                       
         XC    0(80,RF),0(RF)      CLEAR THE TABLE                              
         LA    RF,80(RF)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         XC    MYP3#REC,MYP3#REC   NO RECORDS IN TABLE SO FAR                   
         MVC   MYP4LREC,=F'80'     L'REQUEST CARD                               
         MVI   MYP5DKEY,0          DISP TO KEY                                  
         MVC   MYP5LKEY,=FL3'80'   L'KEY                                        
         MVC   MYP6#MAX,=A(MAXREQS)                                             
*                                                                               
         MVI   ACTIVITY,C'N'       ASSUME NOTHING TO UNBILL                     
*                                                                               
         LA    RF,REQTOTS          CLEAR REQUEST TOTALS                         
         USING TOTALSD,RF                                                       
         ZAP   GRSUNIT,=P'0'                                                    
         ZAP   NETUNIT,=P'0'                                                    
         ZAP   GRS2UNIT,=P'0'                                                   
         ZAP   GRSUBIL,=P'0'                                                    
         ZAP   NETUBIL,=P'0'                                                    
         ZAP   GRS2UBIL,=P'0'                                                   
         ZAP   GRSBLHD,=P'0'                                                    
         ZAP   NETBLHD,=P'0'                                                    
         ZAP   GRS2BLHD,=P'0'                                                   
         ZAP   GRSSTAB,=P'0'                                                    
         ZAP   NETSTAB,=P'0'                                                    
         ZAP   GRS2STAB,=P'0'                                                   
         DROP  RF                                                               
*                                  STANDARDIZE QEST (ALL + BLANK = NO)          
         CLC   QEST,=C'ALL'                                                     
         BE    *+12                                                             
         CLI   QEST,C' '                                                        
         BNE   *+10                                                             
         MVC   QEST,=C'NO '                                                     
*                                                                               
         MVI   FCRDBUYS,C'N'                                                    
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QSTART,(3,BQSTART)                                   
*                                                                               
*                                 SET VALUES FOR RUN-DATE FILTERING             
         GOTO1 DATCON,DMCB,TODAY,(3,TODAYB)                                     
         ZIC   R1,TODAYB                                                        
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         STC   R1,DECADE           80,90,00,ETC.                                
         MVC   YEARDIG,TODAY+1     GET YEAR WITHIN DECADE                       
         NI    YEARDIG,X'0F'       ISOLATE YEAR DIGIT                           
*                                                                               
         XC    INVFILT,INVFILT                                                  
         CLI   QINVNUM,C' '        INVOICE NUMBER GIVEN?                        
         BE    REQF10                                                           
         GOTO1 VSPFMTIN,DMCB,,(C'P',QINVNUM)                                    
         L     RF,DMCB+4                                                        
         MVC   INVFILT,0(RF)       YES: UNBILL ONLY THIS INVOICE                
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
         MVI   QDETTYPE,QDETUNIT            ASSUME UNIT DETAILS GIVEN           
         CLI   PROFB1S+3,C'Y'                                                   
         BNE   *+8                                                              
         MVI   QDETTYPE,QDETUBIL            UNLESS AGENCY IS CONVERTED          
         MVC   DETAIL$P,HEADER$P            ASSUME DETAILS SHOULD MATCH         
         CLI   QDETAIL$+L'QDETAIL$-1,C' '   EXPECTED DETAIL $ GIVEN?            
         BE    REQFX                        NO                                  
*                                                                               
         PACK  DETAIL$P,QDETAIL$+1(L'QDETAIL$-1)  WE'RE OUT OF BALANCE          
         NI    DETAIL$P+L'DETAIL$P-1,X'FC'  ASSUME POSITIVE                     
         CLI   QDETAIL$,C'-'                CREDIT AMOUNT?                      
         BNE   REQFX                                                            
         OI    DETAIL$P+L'DETAIL$P-1,X'01'  YES: MAKE NEGATIVE                  
*                                                                               
REQFX    DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
*        CLTFRST                                                                
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
         J     XIT                                                              
         SPACE 2                                                                
*        CLTLAST                                                                
         SPACE 2                                                                
CLTL     DS    0H                                                               
         MVI   KPRD,0                                                           
         XC    KPRDC,KPRDC                                                      
         CLC   =C'ALL',QPRD                                                     
         BE    CLTL20                                                           
         CLC   =C'POL',QPRD                                                     
         BE    CLTL20                                                           
         MVC   KPRD,BPRD                                                        
         MVC   KPRDC,PRD                                                        
*                                                                               
CLTL20   BRAS  RE,NETNU                                                         
         BRAS  RE,ROBILL                                                        
         BRAS  RE,RNBILL                                                        
*                                                                               
         J     XIT                                                              
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
         AP    REPT.GRSUNIT,GRSUNIT                                             
         AP    REPT.NETUNIT,NETUNIT                                             
         AP    REPT.GRS2UNIT,GRS2UNIT                                           
         AP    REPT.GRSUBIL,GRSUBIL                                             
         AP    REPT.NETUBIL,NETUBIL                                             
         AP    REPT.GRS2UBIL,GRS2UBIL                                           
         AP    REPT.GRSBLHD,GRSBLHD                                             
         AP    REPT.NETBLHD,NETBLHD                                             
         AP    REPT.GRS2BLHD,GRS2BLHD                                           
         AP    REPT.GRSSTAB,GRSSTAB                                             
         AP    REPT.NETSTAB,NETSTAB                                             
         AP    REPT.GRS2STAB,GRS2STAB                                           
         DROP  REPT                                                             
*                                                                               
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVC   P+15(18),=C'**REQUEST TOTALS**'                                  
         MVC   P+35(14),=C'BILL HEADERS ='                                      
         EDIT  GRSBLHD,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETBLHD,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2BLHD,LGRS2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
         MVC   P+35(21),=C'UNIT RECORD DETAILS ='                               
         EDIT  GRSUNIT,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETUNIT,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2UNIT,LGRS2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
         MVC   P+35(22),=C'UNIT BILLING DETAILS ='                              
         EDIT  GRSUBIL,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETUBIL,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2UBIL,LGRS2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
         MVC   P+35(21),=C'MANUAL BILL DETAILS ='                               
         EDIT  GRSSTAB,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETSTAB,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2STAB,LGRS2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
*                                                                               
         CLI   PROFB1S+3,C'Y'      NEW UNIT BILLING RECORDS MANDATORY?          
         BE    REQL10              YES: DON'T EXPECT UNITS TO MATCH             
*                                                                               
         ZAP   DUB,GRSUNIT         BILL HEADERS MUST MATCH...                   
         AP    DUB,GRSSTAB         ...UNIT DETAILS PLUS MANUAL STABUCKS         
         CP    GRSBLHD,DUB                                                      
         BNE   REQL20                                                           
         ZAP   DUB,NETUNIT                                                      
         AP    DUB,NETSTAB                                                      
         CP    NETBLHD,DUB                                                      
         BNE   REQL20                                                           
         ZAP   DUB,GRS2UNIT                                                     
         AP    DUB,GRS2STAB                                                     
         CP    GRS2BLHD,DUB                                                     
         BNE   REQL20                                                           
*                                                                               
REQL10   DS    0H                                                               
         CLI   PROFB1S+3,C'Y'                                                   
         BE    *+12                                                             
         CLI   PROFB1S+3,C'R'                                                   
         BNE   REQL30                                                           
*                                                                               
         ZAP   DUB,GRSUBIL         BILL HEADERS MUST MATCH...                   
         AP    DUB,GRSSTAB         ...UBILL RECS PLUS MANUAL STABUCKS           
         CP    GRSBLHD,DUB                                                      
         BNE   REQL20                                                           
         ZAP   DUB,NETUBIL                                                      
         AP    DUB,NETSTAB                                                      
         CP    NETBLHD,DUB                                                      
         BNE   REQL20                                                           
         ZAP   DUB,GRS2UBIL                                                     
         AP    DUB,GRS2STAB                                                     
         CP    GRS2BLHD,DUB                                                     
         BE    REQL30                                                           
*                                                                               
REQL20   MVC   P(21),=C'***ERROR*** IMBALANCE'                                  
         GOTO1 REPORT                                                           
         DROP  R2                                                               
*                                                                               
REQL30   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVI   P1,0                                                             
         MVC   P2(18),=C'UNPOSTING REQUIRED'                                    
         MVI   P3,0                                                             
         L     RF,=A(MYREQTAB)     A(UNPOST REQUESTS)                           
         CLI   0(RF),0             ANY BILLS POSTED TO ACC?                     
         BNE   *+10                NO                                           
         MVC   P2(46),=C'NO INVOICES POSTED TO ACC: NO UNPOSTING NEEDED+        
               '                                                                
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QOPT1,C'P'          PROGRAMMER SPECIAL (VIA TSO)?                
         BE    REQL50              YES: DON'T BOTHER CHECKING $                 
         CLI   QOPT1,C'S'          ONLY CHECK $ ON SOON REQUESTS                
         BNE   REQLX                                                            
*                                                                               
         CP    GRSBLHD,HEADER$P                                                 
         BNE   REQL40              EXPECTED HEADER $ DON'T MATCH                
         CLI   QDETTYPE,QDETUNIT   UNIT RECORD DETAILS PROVIDED?                
         BNE   REQL37                                                           
         ZAP   DUB,GRSUNIT                                                      
         AP    DUB,GRSSTAB                                                      
         CP    DETAIL$P,DUB                                                     
         BE    REQL50                                                           
         B     REQL40              EXPECTED UNIT DETAILS DON'T MATCH            
REQL37   CLI   QDETTYPE,QDETUBIL   UNIT BILLING RECORD DETAILS?                 
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,GRSUBIL                                                      
         AP    DUB,GRSSTAB                                                      
         CP    DETAIL$P,DUB        EXPECTED UNIT BILLING DETAILS MATCH?         
         BE    REQL50                                                           
         DROP  R4                                                               
*                                                                               
REQL40   DS    0H                                                               
         MVC   P1(44),=C'********************************************'          
         MVC   P2(44),=C'*** COMPUTED AND EXPECTED TOTALS UNEQUAL ***'          
         MVC   P3(44),=C'***  NO LIVE REQUESTS HAVE BEEN WRITTEN  ***'          
         MVC   P4(44),=C'********************************************'          
         GOTO1 REPORT              PRINT WARNING FOR DATA CONTROL               
         B     REQLX                                                            
*                                                                               
REQL50   DS    0H                                                               
         CLI   QOPT1,C'S'          SUBMITTED VIA REQUEST PROGRAM?               
         BE    *+12                YES: WRITE OVERNIGHT REQUESTS                
         CLI   QOPT1,C'P'          PROGRAMMER SPECIAL (VIA TSO)?                
         BNE   REQLX               YES: WRITE UNPOSTING OVERNIGHT RQST          
*                                                                               
         CLI   ACTIVITY,C'Y'       ANY RECORDS TO UPDATE?                       
         BE    REQL60              YES                                          
*                                                                               
         MVC   P1(34),=C'**********************************'                    
         MVC   P2(34),=C'*** NO RECORDS FOUND TO UNBILL ***'                    
         MVC   P3(34),=C'**********************************'                    
         GOTO1 REPORT              PRINT WARNING FOR DATA CONTROL               
         B     REQLX                                                            
*                                                                               
REQL60   DS    0H                  BUILD UNBILLING REQUEST...                   
         CLI   QOPT1,C'P'                                                       
         BE    REQL75              ...BUT NOT FOR PROGRAMMER SPECIAL            
*                                                                               
         XC    REQHDR,REQHDR       26-BYTE REQUEST HEADER                       
         MVC   REQUEST,QRECORD             COPY THE SOON REQUEST...             
         MVI   QOPT1-QAREA+REQUEST,C'L'    ...SET THE 'LIVE' FLAG               
         MVI   QCONTREQ-QAREA+REQUEST,C' ' ...SINGLE-CARD REQUEST               
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   REQL70              NO                                           
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
REQL70   DS    0H                                                               
         MVC   P1(24),=C'UNBILLING REQUEST CARD: '                              
         MVC   P1+24(80),REQUEST                                                
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
REQL75   L     R4,=A(MYREQTAB)     A(UNPOST REQUESTS)                           
REQL80   CLI   0(R4),0             EOT?                                         
         BE    REQLX               YES                                          
*                                                                               
         XC    REQHDR,REQHDR       CLEAR UNPOSTING REQUEST HEADER               
         MVC   REQDEST,=AL2(17)    FORCE UNPOSTING REPORT TO SJR                
         MVC   REQUEST,0(R4)       SET UNPOSTING REQUEST CARD                   
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   REQL90              NO                                           
         GOTO1 DATAMGR,DMCB,DMADD,=C'REQUEST',WORK,REQHDR                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P1(41),=C'*****************************************'             
         MVC   P2(41),=C'*** LIVE UNPOSTING WILL RUN OVERNIGHT ***'             
         MVC   P3(41),=C'*****************************************'             
         MVI   P4,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
REQL90   DS    0H                                                               
         MVC   P1(24),=C'UNPOSTING REQUEST CARD: '                              
         MVC   P1+24(80),REQUEST                                                
         GOTO1 REPORT                                                           
*                                                                               
         LA    R4,80(R4)                                                        
         B     REQL80                                                           
*                                                                               
REQLX    DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
*        RUNLAST                                                                
RUNL     DS    0H                                                               
         LA    R2,P                                                             
         USING LINED,R2                                                         
         LA    R3,REPTOTS                                                       
         USING TOTALSD,R3                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P+15(17),=C'**REPORT TOTALS**'                                   
         MVC   P+35(14),=C'BILL HEADERS ='                                      
         EDIT  GRSBLHD,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETBLHD,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2BLHD,LGRS2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
         MVC   P+35(21),=C'UNIT RECORD DETAILS ='                               
         EDIT  GRSUNIT,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETUNIT,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2UNIT,LGRS2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
         MVC   P+35(22),=C'UNIT BILLING DETAILS ='                              
         EDIT  GRSUBIL,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETUBIL,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2UBIL,LGRS2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
         MVC   P+35(21),=C'MANUAL BILL DETAILS ='                               
         EDIT  GRSSTAB,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETSTAB,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2STAB,LGRS2,2,COMMAS=YES,MINUS=YES                            
         GOTO1 REPORT                                                           
*                                                                               
         CLI   PROFB1S+3,C'Y'      NEW UNIT BILLING RECORDS MANDATORY?          
         BE    RUNL10              YES: DON'T EXPECT UNITS TO MATCH             
*                                                                               
         ZAP   DUB,GRSUNIT         BILL HEADERS MUST MATCH...                   
         AP    DUB,GRSSTAB         ...UNIT DETAILS PLUS MANUAL STABUCKS         
         CP    GRSBLHD,DUB                                                      
         BNE   RUNL20                                                           
         ZAP   DUB,NETUNIT                                                      
         AP    DUB,NETSTAB                                                      
         CP    NETBLHD,DUB                                                      
         BNE   RUNL20                                                           
         ZAP   DUB,GRS2UNIT                                                     
         AP    DUB,GRS2STAB                                                     
         CP    GRS2BLHD,DUB                                                     
         BNE   RUNL20                                                           
*                                                                               
RUNL10   DS    0H                                                               
         CLI   PROFB1S+3,C'Y'                                                   
         BE    *+12                                                             
         CLI   PROFB1S+3,C'R'                                                   
         BNE   RUNLX                                                            
*                                                                               
         ZAP   DUB,GRSUBIL         BILL HEADERS MUST MATCH...                   
         AP    DUB,GRSSTAB         ...UBILL RECS PLUS MANUAL STABUCKS           
         CP    GRSBLHD,DUB                                                      
         BNE   RUNL20                                                           
         ZAP   DUB,NETUBIL                                                      
         AP    DUB,NETSTAB                                                      
         CP    NETBLHD,DUB                                                      
         BNE   RUNL20                                                           
         ZAP   DUB,GRS2UBIL                                                     
         AP    DUB,GRS2STAB                                                     
         CP    GRS2BLHD,DUB                                                     
         BE    RUNLX                                                            
*                                                                               
RUNL20   MVC   P(21),=C'***ERROR*** IMBALANCE'                                  
         GOTO1 REPORT                                                           
*                                                                               
RUNLX    DS    0H                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFACLO',A(BUFFM)),0,ACOMFACS               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                UNSUCCESSFUL BUFFERIN CLOSE                  
*                                                                               
         J     XIT                                                              
         SPACE 2                                                                
         DROP  R2,R3                                                            
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'RNBILL - READ NEW BILL RECORDS'                                 
RNBILL   NMOD1 0,RNBILL                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         MVC   DATADISP,=H'24'                                                  
*                                                                               
         MVC   KEY1,KEY            SAVE KEY                                     
         XC    KEY,KEY                                                          
*                                                                               
KEY@     USING STABUCKD,KEY                                                     
KS       USING STABUCKD,KEYSAVE                                                 
         MVC   KEY@.STABPCOD,=X'0E81'   RECORD TYPE                             
         MVC   KEY@.STABPAM,BAGYMD                                              
         MVC   KEY@.STABPCLT,BCLT                                               
         OC    KPRDC,KPRDC                                                      
         BZ    RNB20               SKIP REST OF KEY NOW IF MULTI-PRD            
         MVC   KEY@.STABPPRD,KPRDC                                              
*                                                                               
RNB10    DS    0H                                                               
         MVC   KEY@.STABPEST,BEST                                               
*        XC    KEY@.STABKMKT,KEY@.STABKMKT                                      
*        XC    KEY@.STABKSTA,KEY@.STABKSTA                                      
*        MVI   KEY@.STABKCUR,0                                                  
*                                                                               
RNB20    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RNB40                                                            
*                                                                               
RNB30    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
RNB40    DS    0H                                                               
         CLC   KEY@.STABPCOD,KS.STABPCOD      MUST BE SAME AGY/MED/CLT          
         BNE   RNBX                                                             
         CLC   KEY@.STABPAM,KS.STABPAM                                          
         BNE   RNBX                                                             
         CLC   KEY@.STABPCLT,KS.STABPCLT                                        
         BNE   RNBX                                                             
*                                                                               
         OC    KPRDC,KPRDC                                                      
         BZ    RNB60                                                            
         CLC   KPRDC,KEY@.STABPPRD  ONE PRODUCT MUST BE EQUAL                   
         BNE   RNBX                                                             
         B     RNB60                                                            
*                                  MULTI-PRODUCT SITUATION                      
RNB50    DS    0H                                                               
         ZIC   RF,KEY@.STABPPRD    BUMP TO NEXT PRODUCT                         
         AHI   RF,1                                                             
         STC   RF,KEY@.STABPPRD                                                 
         B     RNB10               SET KEY FROM ESTIMATE DOWN                   
*                                                                               
RNB60    DS    0H                  ESTIMATE                                     
         CLI   BEST,0                                                           
         BE    RNB70                                                            
*                                  ONE ESTIMATE OR SERIES                       
         CLC   KEY@.STABPEST,BEST                                               
         BL    RNB10                                                            
         BE    RNB70                                                            
         CLI   BESTEND,0                                                        
         BE    *+14                                                             
         CLC   KEY@.STABPEST,BESTEND                                            
         BNH   RNB70                                                            
*                                                                               
         OC    KPRDC,KPRDC         ESTIMATE NOT OK                              
         BZ    RNB50               NEXT PRODUCT IF MULTI-PRODUCT                
         B     RNBX                ELSE DONE                                    
         DROP  KEY@                                                             
         DROP  KS                                                               
         SPACE 3                                                                
*                                  HAVE GOOD KEY                                
RNB70    DS    0H                                                               
         L     R7,=A(STABUCKC)                                                  
         ST    R7,AREC                                                          
         USING STABUCK,R7                                                       
         GOTO1 GET                                                              
*                                                                               
         MVI   ELCODE,X'0E'                                                     
         MVI   RECSW,0             CLEAN RECORD ALTERED SWITCH                  
         LA    R2,STABELEM                                                      
         USING STABELEM,R2                                                      
RNB80    DS    0H                                                               
         CLC   ELCODE,0(R2)                                                     
         BE    RNB100                                                           
*                                                                               
RNB90    DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   RNB130                                                           
*                                                                               
RNB100   DS    0H                                                               
         CLC   STABBDT,BQSTARTP    MATCH ON RUN DATE?                           
         BNE   RNB90               NO                                           
*                                                                               
         OC    INVFILT,INVFILT     SPECIFIC INVOICE NUMBER?                     
         BZ    RNB110                                                           
         MVC   HALF,STABINV                                                     
         NI    HALF,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                   
         CLC   HALF,INVFILT                                                     
         BNE   RNB90                                                            
*                                                                               
RNB110   DS    0H                                                               
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
*                                                                               
         MVC   INVNUM,STABINV                                                   
         NI    INVNUM,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                 
*                                                                               
*                                  FIND PRODUCT CODE                            
*        L     R3,PRDLIST                                                       
*NB120   DS    0H                                                               
*        CLC   STABKPRD,5(R3)                                                   
*        BE    *+12                                                             
*        LA    R3,6(R3)                                                         
*        B     RNB120                                                           
*                                                                               
         MVC   INVCLT,STABKCLT     CLIENT CODE                                  
         MVC   INVCLTNM,CLTNM      CLIENT NAME                                  
         MVC   INVPRD,STABKSTA     PRODUCT (IN STATION FIELD)                   
         MVC   INVEST,STABKEST                                                  
         MVC   INVPER,STABPER                                                   
         MVC   INVSTA,=X'D6B5A4'     X'D6B5A4' (STATION ZZZZ)                   
         MVI   INVSRC,INVSRCMQ       FROM MANUAL DETAIL                         
         GOTO1 DATCON,DMCB,(2,STABBDT),INVBDATE                                 
*                                                                               
         ICM   R1,15,STABGRS                                                    
         CVD   R1,DUB                                                           
         ZAP   INVGRS,DUB                                                       
         ICM   R1,15,STABNET                                                    
         CVD   R1,DUB                                                           
         ZAP   INVNET,DUB                                                       
         ZAP   INVGRS2,=P'0'       NO COS2 FOR MANUAL BILLS (IN NETPAK)         
         DROP  R4                                                               
*                                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFAPUT',A(BUFFM)),BUFFREC,ACOMFACS         
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BUFFERIN PUT UNSUCCESSFUL                    
*                                                                               
         MVI   0(R2),X'1E'         SET ELEMENT UNBILLED                         
         MVI   RECSW,1                                                          
         B     RNB80                                                            
*                                  NO MORE BUCKET ELEMENTS                      
*                                  END OF RECORD                                
RNB130   DS    0H                                                               
         CLI   RECSW,1             RECORD ALTERED?                              
         BNE   RNB30               NO                                           
         MVC   HALF,STABLEN                                                     
         LH    RF,HALF                                                          
         AR    RF,R7                                                            
         MVI   0(RF),0             SET EOR                                      
*                                                                               
         CLI   QOPT2,C'T'          PRINT TRACE OF UPDATES?                      
         BNE   RNB140                                                           
         GOTO1 HEXOUT,DMCB,KEY+14,TRCMSG1+33,4,=C'TOG'                          
         CLC   =F'8',DMCB+16       PRINT DISK ADDRESS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRTREC),DMCB,AREC,(24,13),PRINT,HEXOUT,C'DOME',      +        
               ('TRCMSG1X',TRCMSG1)                                             
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
RNB140   DS    0H                                                               
         CLI   QOPT1,C'L'          LIVE OVERNIGHT RUN?                          
         BE    *+12                YES                                          
         CLI   QOPT1,C'P'          LIVE PROGRAMMER'S RUN (VIA TSO)?             
         BNE   RNB30               NO: DON'T ACTUALLY UNBILL                    
*                                                                               
         GOTO1 PUT                                                              
*                                                                               
         B     RNB30                                                            
*                                                                               
RNBX     DS    0H                                                               
         J     XIT                                                              
         SPACE 2                                                                
         DROP  R7,R2                                                            
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
TRCMSG1  DC    C'PUT STATION BUCKET RECORD: D/A = XXXXXXXX'                     
TRCMSG1X EQU   *-TRCMSG1                                                        
         TITLE 'ROBILL  - READ OLD BILL RECORDS'                                
ROBILL   NMOD1 0,ROBILL                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         MVC   KEY1,KEY            SAVE KEY                                     
         XC    KEY,KEY                                                          
*                                                                               
KEY@     USING BKEY,KEY                                                         
KS       USING BKEY,KEYSAVE                                                     
         MVC   KEY@.BKEYAM,BAGYMD                                               
         MVC   KEY@.BKEYCLT,BCLT                                                
         OC    KPRDC,KPRDC                                                      
         BZ    ROB20               SKIP REST OF KEY IF MULTI-PRODUCT            
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
         CLC   KEY@.BKEYTYPE,KS.BKEYTYPE      MUST BE SAME AGY/MED/CLT          
         BNE   ROBX                                                             
         CLC   KEY@.BKEYAM,KS.BKEYAM                                            
         BNE   ROBX                                                             
         CLC   KEY@.BKEYCLT,KS.BKEYCLT                                          
         BNE   ROBX                                                             
*                                                                               
         OC    KPRDC,KPRDC                                                      
         BZ    *+14                                                             
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
         BZ    ROB160                                                           
*                                                                               
ROB50    DS    0H                                                               
         OC    INVFILT,INVFILT     ONE INVOICE NUMBER?                          
         BZ    ROB70                                                            
         CLC   INVFILT,KEY@.BKEYINV                                             
         BNE   ROB160                                                           
         B     ROB70                                                            
*                                                                               
ROB60    DS    0H                                                               
         ZIC   RF,KEY@.BKEYPRD+2   BUMP TO NEXT PRODUCT                         
         AHI   RF,1                                                             
         STC   RF,KEY@.BKEYPRD+2                                                
         B     ROB10               SET KEY FROM ESTIMATE DOWN                   
*                                                                               
ROB70    DS    0H                                                               
         CLI   BEST,0                                                           
         BE    ROB80                                                            
*                                  ONE ESTIMATE OR SERIES                       
         CLC   KEY@.BKEYEST,BEST                                                
         BL    ROB10                                                            
         BE    ROB80                                                            
*                                                                               
         CLI   BESTEND,0                                                        
         BE    *+14                                                             
         CLC   KEY@.BKEYEST,BESTEND                                             
         BNH   ROB80                                                            
*                                                                               
*                                  ESTIMATE NOT OK                              
         OC    KPRDC,KPRDC                                                      
         BZ    ROB60               NEXT PRODUCT IF MULTI-PRODUCT                
         B     ROBX                ELSE DONE                                    
*                                                                               
ROB80    DS    0H                  BILL DATE                                    
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
         NI    HALF+1,X'0F'                                                     
         DROP  KEY@                                                             
         DROP  KS                                                               
*                                                                               
         CLC   HALF,BQSTART        MATCH ON RUN DATE?                           
         BNE   ROB160              NO                                           
*                                                                               
         TM    KEY+13,X'80'        DELETED                                      
         BO    ROB160              YES-SKIP                                     
*                                                                               
*                                  HAVE GOOD KEY                                
*                                  PASS DATA TO SORT                            
         GOTO1 GETBILL                                                          
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         CLC   BDATE,QSTART        MATCH ON RUN DATE?                           
         BNE   ROB160              NO                                           
*                                                                               
         CLI   BTYPE,C'B'          MUST BE A NEW BILLING BILL                   
         BNE   ROB160                                                           
*                                                                               
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
*                                                                               
         MVC   INVNUM,BKEYINV      INVOICE NUMBER                               
*                                                                               
         MVC   INVCLT,BKEYCLT      CLIENT CODE                                  
         MVC   INVCLTNM,CLTNM      CLIENT NAME                                  
         MVC   INVPRD,BKEYPRD                                                   
         MVC   INVEST,BKEYEST                                                   
         MVC   INVPER,BKEYYSRV                                                  
         MVC   INVBDATE,BDATE                                                   
         MVC   INVSUBM,BLMED       SUB-MEDIA                                    
         MVC   INVRVRS#,BLREVINO   REVERSED BILL NUMBER                         
         MVC   INVRVRDT,BLREVDAT   REVERSED BILL DATE                           
         MVI   INVSRC,INVSRCHQ     FROM BILL HEADER                             
*                                                                               
         MVC   INVSTA,=X'FFFFFF'   FORCES BILL HEADERS TO SORT LAST             
*                                                                               
         MVC   INVBTYPE(2),BTYPE   BILLTYPE (B4, MAN, AOR, ETC.)                
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
         BE    ROB90                                                            
         MVI   INVBTYPE+3,C'C'                                                  
         CLI   BRETAIL,X'81'       CORP CONTROL                                 
         BE    ROB90                                                            
         MVI   INVBTYPE+3,C' '                                                  
*                                                                               
ROB90    DS    0H                                                               
         ZAP   INVGRS,BGRSP        GROSS                                        
         ZAP   INVGRS2,BGRS2P      GROSS (COS2)                                 
*                                                                               
         OC    BILPOST,BILPOST     TRANSFERRED TO ACC?                          
         BZ    ROB100              NO                                           
*                                                                               
         MVC   REQUEST,SPACES                                                   
         LA    RF,REQUEST                                                       
         MVC   QCODE-QAREA(,RF),=C'MY'        SET TO 'MY' REQUEST               
         MVC   QAGY-QAREA(,RF),QAGY           AGENCY                            
         MVC   QMED-QAREA(,RF),QMED           MEDIA                             
         MVC   QCLT-QAREA(,RF),QCLT           CLIENT                            
         MVC   QPRD-QAREA(,RF),=C'ALL'        ALL PRODUCTS                      
         MVC   QSTAUTO-QAREA(,RF),=C'ALL'     AUTO-REQUEST START DATE           
         MVI   QOPT1-QAREA(RF),C'R'           'REVERSE' OPTION                  
         MVI   QOPT2-QAREA(RF),C'N'           DEFAULT SUB-MEDIA TO 'N'          
         CLI   BLMED,C' '                                                       
         BNH   *+10                                                             
         MVC   QOPT2-QAREA(,RF),BLMED         SUB-MEDIA                         
         MVI   QOPT4-QAREA(RF),C'U'           'UNPOST' OPTION                   
         MVC   QUESTOR-QAREA(,RF),QUESTOR     REQUESTOR NAME                    
*                                                                               
         MVI   MYP1HOB,X'01'       ADD REQUEST IF NOT THERE ALREADY             
         STCM  RF,7,MYP1AREC       A(REQUEST CARD)                              
         GOTO1 BINSRCH,MYPARMS                                                  
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                'MY' REQUEST TABLE FULL                      
*                                                                               
ROB100   DS    0H                                                               
         ZAP   INVNET,=P'0'                                                     
         TM    BILSTAT,BSTTAORQ    FOR TRUE AOR BILLS                           
         BNZ   *+10                SKIP NET (SOMETHING ELSE IS THERE)           
         ZAP   INVNET,BNETP                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFAPUT',A(BUFFM)),BUFFREC,ACOMFACS         
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
         BNE   ROB110                                                           
         GOTO1 HEXOUT,DMCB,KEY+14,TRCMSG2+30,4,=C'TOG'                          
         CLC   =F'8',DMCB+16       PRINT DISK ADDRESS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,BLEN           RECORD LENGTH                                
         GOTO1 PRNTBL,DMCB,('TRCMSG2X',TRCMSG2),ADBILL,C'DUMP',(R0),   +        
               =C'1D'                                                           
*                                                                               
ROB110   DS    0H                                                               
         CLI   QOPT1,C'L'          LIVE OVERNIGHT RUN?                          
         BE    *+12                YES                                          
         CLI   QOPT1,C'P'          LIVE PROGRAMMER'S RUN (VIA TSO)?             
         BNE   ROB120              NO: DON'T ACTUALLY UNBILL                    
*                                                                               
         GOTO1 WRITE                                                            
         GOTO1 PUTBILL                                                          
*                                                                               
ROB120   DS    0H                                                               
         CLI   QOPT4,C'D'          GENERATE DDS UNBILLING RECORDS?              
         BNE   ROB160                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(9,=C'20030401'),DUB                                 
         CLC   BDATE,DUB           PRIOR TO APR01/2003...                       
         BL    ROB160              ...NO SUB-MEDIA DATA IS AVAILABLE            
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
         LA    R3,SUBMEDTB         NETPAK SUB-MEDIA TABLE                       
ROB130   CLI   0(R3),X'FF'         EOT?                                         
         BE    ROB160                                                           
*                                                                               
         ZIC   R0,0(R3)            SUB-MEDIA                                    
         GOTO1 SPBVAL,DMCB,(C'B',(R7)),((R0),SPBVALD),0                         
*                                                                               
         CP    SPBVGRSP,=P'0'      MIGHT NOT BE ANY DATA...                     
         BNE   *+14                ...FOR THIS SUB-MEDIA                        
         CP    SPBVNETP,=P'0'                                                   
         BE    ROB150                                                           
         MVC   UNBKSUBM,0(R3)      SUB-MEDIA                                    
         ZAP   UNBKGRS,SPBVGRSP    GROSS FOR THIS SUB-MEDIA ONLY                
         ZAP   UNBKNET,SPBVNETP    NET FOR THIS SUB-MEDIA ONLY                  
*                                                                               
         XC    FULL,FULL                                                        
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   ROB140                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFIL',0,(R5),0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,DMCB+8         DISK ADDRESS OF ADDED RECORD                 
*                                                                               
ROB140   DS    0H                                                               
         CLI   QOPT2,C'T'          PRINT TRACE OF UPDATES?                      
         BNE   ROB150                                                           
         GOTO1 HEXOUT,DMCB,FULL,TRCMSG6+30,4,=C'TOG'                            
         CLC   =F'8',DMCB+16       PRINT DISK ADDRESS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,UNBRECLN       RECORD LENGTH                                
         GOTO1 PRNTBL,DMCB,('TRCMSG6X',TRCMSG6),(R5),C'DUMP',(R0),     +        
               =C'1D'                                                           
*                                                                               
ROB150   DS    0H                                                               
         LA    R3,1(R3)            BUMP TO NEXT SUB-MEDIA                       
         B     ROB130                                                           
         DROP  R5                                                               
*                                                                               
ROB160   DS    0H                                                               
         B     ROB30               GET NEXT BILL HEADER                         
*                                                                               
ROBX     DS    0H                                                               
         J     XIT                                                              
         DROP  R7                                                               
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
TRCMSG2  DC    C'PUT BILL HEADER RECORD: D/A = XXXXXXXX'                        
TRCMSG2X EQU   *-TRCMSG2                                                        
TRCMSG6  DC    C'ADD DDS UNBILLING REC:  D/A = XXXXXXXX'                        
TRCMSG6X EQU   *-TRCMSG6                                                        
         SPACE 3                                                                
SUBMEDTB DC    C'NCSODV'           NETPAK SUB-MEDIA TABLE                       
         DC    X'FF'               EOT                                          
         SPACE 3                                                                
         TITLE 'NETNU - PROCESS NETWORK UNITS'                                  
NETNU    NMOD1 0,NETNU                                                          
         LA    RC,SPACEND                                                       
*                                                                               
         BC    0,NTU10             ONCE ONLY (SELF-MODIFYING CODE)              
         OI    *-3,X'F0'                                                        
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BNE   NTU6                                                             
         LA    RF,NUFLIST                                                       
NTU5     CLI   0(RF),C'X'          IF IN NON-UPDATIVE MODE                      
         BE    NTU6                                                             
         MVI   0(RF),C'N'          SET FILES AS READ-ONLY                       
         LA    RF,8(RF)                                                         
         B     NTU5                                                             
*                                                                               
NTU6     L     R4,ADBUY            USE AS WORK AREA FOR OPEN                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',NUFLIST,(R4)                      
*                                                                               
         B     NTU10                                                            
*                                                                               
NUFLIST  DC    CL8'UUNTFIL'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL8'UXSPFIL'                                                     
         DC    CL8'UXSPDIR'                                                     
         DC    CL10'X'                                                          
         EJECT                                                                  
NTU10    DS    0H                                                               
         L     R8,=A(NETBLK)                                                    
         USING NETBLOCK,R8                                                      
*                                                                               
         LA    RE,NETBLOCK                                                      
         LHI   RF,NBBLKEND-NETBLOCK                                             
         XCEFL                                                                  
*                                                                               
         LA    RF,DMWORKU          SET A(DMWORK)                                
         ST    RF,NBADMWRK                                                      
*                                  SET SELECT OPTIONS                           
         MVC   NBSELAGY,QAGY       AGENCY                                       
         MVC   NBSELMED,QMED       MEDIA                                        
         MVC   NBSELCLI,CLT        CLIENT                                       
         MVC   NBSELPRD,=C'POL'    ALWAYS ASK FOR ALL PRODUCTS                  
         MVC   NBSELEST,BEST       ESTIMATE RANGE START                         
         MVC   NBSELESE,BESTEND    ESTIMATE RANGE END                           
         CLC   =C'NO',QEST                                                      
         BNE   *+10                                                             
         MVC   NBSELEFL,QESTEND    ESTIMATE FILTER                              
         CLC   NBSELEFL,SPACES                                                  
         BNE   *+10                                                             
         XC    NBSELEFL,NBSELEFL                                                
*                                  SET DATA OPTIONS                             
         MVI   NBDATA,C'U'         UNITS                                        
         MVC   NBTRCOPT,RCTRACE    TRACE                                        
         MVI   NBFUNCT,NBFNORM     NORMAL FUNCTION                              
         MVC   NBNOWRIT,RCWRITE                                                 
         OI    NBSBKEND,NBNODPT2   DONT EXTRACT DAYPARTS                        
*                                                                               
         MVC   NBAIO,=A(STABUCKC)  USE STATION BUCKET AREA                      
         MVC   NBPRINT,PRINT                                                    
         MVC   NBLOADER,LOADER                                                  
         MVC   NBACOM,ACOMFACS                                                  
*                                                                               
NTU20    DS    0H                                                               
         GOTO1 NETIO,DMCB,NETBLOCK                                              
         CLI   NBERROR,0                                                        
         BE    NTU30                                                            
         MVC   P(46),=C'**NETIO ERROR NNN, - GOING ON TO NEXT CLIENT**'         
         EDIT  (B1,NBERROR),(3,P+14)                                            
         GOTO1 REPORT                                                           
         B     NTUX                                                             
*                                                                               
NTU30    DS    0H                                                               
         CLI   NBMODE,NBPROCUN                                                  
         BE    NTU40                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    NTUX                                                             
         CLI   NBMODE,NBVALCLI                                                  
         BNE   *+8                                                              
         MVI   NBUSER+13,C'N'      SET TO PASS PRE-EMPTS                        
         B     NTU20                                                            
         SPACE 3                                                                
*        PROCESS UNIT                                                           
         SPACE 2                                                                
NTU40    DS    0H                                                               
         MVI   RECSW,0             SET OFF RECORD ALTERED SWITCH                
         MVC   WORK(4),NBACTNET                                                 
         MVI   WORK+4,C'N'                                                      
         GOTO1 MSPACK,DMCB,=C'0000',WORK,BMKTSTA                                
*                                                                               
         BAS   RE,WRTUBILL         WRITE UNIT BILLING RECORDS                   
*                                                                               
         L     R7,NBAIO                                                         
         USING NURECD,R7                                                        
         LA    R2,NUDATA                                                        
         MVC   DATADISP,=H'27'                                                  
         MVI   ELCODE,X'10'        BILL ELEMENT                                 
         USING NUBILD,R2                                                        
*                                                                               
NTU50    DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   NTU80                                                            
*                                                                               
         TM    NUBILST,NUBILUBQ    UNBILLED?                                    
         BO    NTU50               YES                                          
*                                                                               
         CLI   NUBILLEN,29                                                      
         BL    *+24                                                             
         OC    NUBILPRC,NUBILPRC   IS THERE 3 CHAR PRODUCT?                     
         BZ    *+14                                                             
         MVC   WPRDC,NUBILPRC                                                   
         B     NTU55                                                            
*                                                                               
         L     RF,VCLIST                                                        
         CLC   NUBILPRD,3(RF)                                                   
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     *-14                                                             
                                                                                
         MVC   WPRDC,0(RF)                                                      
NTU55    BAS   RE,CKPRD            CHECK FOR RIGHT PRODUCT                      
         BNE   NTU50                                                            
*                                                                               
         CLC   NUBILDAT,BQSTARTP   MATCH ON RUN DATE?                           
         BNE   NTU50               NO                                           
*                                                                               
         OC    INVFILT,INVFILT     SPECIFIC INVOICE NUMBER?                     
         BZ    NTU60                                                            
         GOTO1 VSPFMTIN,DMCB,,(C'P',NUBILNUM)                                   
         L     RF,DMCB+4                                                        
         CLC   INVFILT,0(RF)       RIGHT INVOICE NUMBER?                        
         BNE   NTU50               NO-SKIP                                      
*                                                                               
NTU60    DS    0H                                                               
         OI    NUBILST,NUBILUBQ                                                 
         MVI   RECSW,1             SET RECORD ALTERED                           
*                                                                               
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
         XC    BUFFREC,BUFFREC                                                  
*                                                                               
         MVC   INVCLT,NBACTCLI     CLIENT CODE                                  
         MVC   INVCLTNM,CLTNM      CLIENT NAME                                  
         MVC   INVPRD,WPRDC                                                     
         MVC   INVEST,NBACTEST                                                  
         GOTO1 DATCON,DMCB,(2,NUBILDAT),INVBDATE                                
         GOTO1 VSPFMTIN,DMCB,,(C'P',NUBILNUM)                                   
         L     RF,DMCB+4                                                        
         MVC   INVNUM,0(RF)        BINARY INVOICE NUMBER                        
         MVC   INVSTA,BMKTSTA+2    NETWORK (IN MSPACK FORMAT)                   
         MVC   INVPKG,NBPACK                                                    
         MVC   INVUPGM,NBACTPRG                                                 
         MVC   INVUDAT,NBACTDAT                                                 
         MVC   INVULIN,NBACTSUB                                                 
         MVC   INVCTYP,NUBILTYP                                                 
         MVI   INVSRC,INVSRCUQ     FROM UNIT RECORD                             
*                                                                               
         ICM   R1,15,NUBILGRS      GROSS                                        
         CVD   R1,DUB                                                           
         ZAP   INVGRS,DUB                                                       
         ICM   R1,15,NUBILNET      NET                                          
         CVD   R1,DUB                                                           
         ZAP   INVNET,DUB                                                       
*                                                                               
         ZAP   INVGRS2,=P'0'       GROSS (COS2)                                 
         CLI   NUBILLEN,28                                                      
         BL    NTU70                                                            
         ICM   R1,15,NUBILGR2                                                   
         CVD   R1,DUB                                                           
         ZAP   INVGRS2,DUB                                                      
         DROP  R4                                                               
*                                                                               
NTU70    DS    0H                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFAPUT',A(BUFFM)),BUFFREC,ACOMFACS         
         CLI   4(R1),0                                                          
         BE    NTU50               NEXT ELEMENT                                 
         DC    H'0'                BUFFERIN PUT UNSUCCESSFUL                    
*                                                                               
NTU80    DS    0H                                                               
         CLI   RECSW,1             RECORD ALTERED?                              
         BNE   NTU20               NO - SKIP                                    
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   NTU90                                                            
         CLI   QOPT1,C'L'          LIVE OVERNIGHT RUN?                          
         BE    *+12                YES                                          
         CLI   QOPT1,C'P'          LIVE PROGRAMMER'S RUN (VIA TSO)?             
         BNE   NTU90               NO: DON'T ACTUALLY UNBILL                    
*                                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,=C'UNTFILE',NBKEY+21,NBAIO,DMWORKU           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NTU90    DS    0H                                                               
         CLI   QOPT2,C'T'          PRINT TRACE OF UPDATES?                      
         BNE   NTU100                                                           
         GOTO1 HEXOUT,DMCB,NBKEY+21,TRCMSG3+23,4,=C'TOG'                        
         CLC   =F'8',DMCB+16       PRINT DISK ADDRESS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRTREC),DMCB,NBAIO,(27,20),PRINT,HEXOUT,C'DOME',     +        
               ('TRCMSG3X',TRCMSG3)                                             
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
NTU100   DS    0H                                                               
         B     NTU20               NEXT UNIT                                    
*                                                                               
NTUX     DS    0H                                                               
         J     XIT                                                              
         SPACE 3                                                                
TRCMSG3  DC    C'PUT UNIT RECORD: D/A = XXXXXXXX'                               
TRCMSG3X EQU   *-TRCMSG3                                                        
         EJECT                                                                  
WRTUBILL NTR1                                                                   
*                                                                               
         L     RE,=A(REVERTAB)     CLEAR 'UNREVERSE' TABLE                      
         LHI   RF,MAXREVS                                                       
         MHI   RF,REVERSDQ                                                      
         XCEFL                                                                  
         XC    NUMREVS,NUMREVS     NO REVERSALS YET                             
*                                                                               
         XC    XSPKEY,XSPKEY                                                    
         LA    R3,XSPKEY           BUILD UNIT BILL RECORD KEY                   
*                                                                               
         CLI   PROFB1S+5,C'Y'      X'0E0A' IS ACTIVE KEY?                       
         BNE   WRTU10              NO: READ X'0E06' KEYS                        
*                                                                               
         USING NUBKEY,R3                                                        
         MVI   NUBK0SYS,NUBK0SYQ   X'0E0A' RECORDS                              
         MVI   NUBK0STY,NUBK0STQ                                                
         MVC   NUBK0AM,NBACTAM     AGENCY/MEDIA                                 
         MVC   NUBK0CLI,NBACTCLI   CLIENT                                       
         MVC   NUBK0DAT,NBACTDAT   AIR DATE                                     
         MVC   NUBK0TIM,NBACTSQH   START 1/4 HOUR                               
         MVC   NUBK0NET,NBACTNET   NETWORK                                      
         MVC   NUBK0PRG,NBACTPRG   PROGRAM                                      
         MVC   NUBK0EST,NBACTEST   ESTIMATE                                     
         MVC   NUBK0SUB,NBACTSUB   SUB-LINE                                     
         MVC   NUBK0DPT,NBACTDP    DAYPART                                      
         MVC   NUBK0BDT,BQSTARTP   RUN DATE                                     
         B     WRTU15                                                           
         DROP  R3                                                               
*                                                                               
WRTU10   DS    0H                                                               
         USING NUBKEY,R3                                                        
         MVI   NUBKSYS,NUBKSYSQ    X'0E06' RECORDS                              
         MVI   NUBKSTYP,NUBKSTYQ                                                
         MVC   NUBKAM,NBACTAM      AGENCY/MEDIA                                 
         MVC   NUBKCLI,NBACTCLI    CLIENT                                       
         MVC   NUBKNET,NBACTNET    NETWORK                                      
         MVC   NUBKPROG,NBACTPRG   PROGRAM                                      
         MVC   NUBKDATE,NBACTDAT   AIR DATE                                     
         MVC   NUBKEST,NBACTEST    ESTIMATE                                     
         MVC   NUBKSUB,NBACTSUB    SUB-LINE                                     
         MVC   NUBKDPT,NBACTDP     DAYPART                                      
         MVC   NUBKBDAT,BQSTARTP   RUN DATE                                     
         DROP  R3                                                               
*                                                                               
WRTU15   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'XSPDIR',XSPKEY,XSPKEY                     
         TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                FATAL DATAMGR ERROR                          
         TM    DMCB+8,X'10'                                                     
         BO    WRTUX               NOTHING BILLED FOR THIS UNIT/DATE            
*                                                                               
         MVI   UBILCHGD,C'N'       ASSUME NOTHING TO UNBILL HERE                
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XSPKEY+36,A(XSPIO),      +        
               DMWORKX                                                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,=A(XSPIO)        UNIT BILL RECORD                             
         MVC   BILLDATE,(NUBKBDAT-NUBKEY)(R2) ASSUME ACTIVE KEY IS 0E06         
         CLI   1(R2),NUBK0STQ      2ND BYTE IS X'0A' ?                          
         BNE   *+10                                                             
         MVC   BILLDATE,(NUBK0BDT-NUBK0KEY)(R2) YES: ACTIVE KEY IS 0E0A         
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,NBILELQ      LOOK FOR BILLING ELEMENTS                    
         BRAS  RE,GETEL                                                         
         BNE   WRTUX               NONE, SO NOTHING TO EXAMINE                  
*                                                                               
         USING NBILD,R2                                                         
WRTU20   DS    0H                                                               
         TM    NBILST,NBILUBQ      UNBILLED?                                    
         BO    WRTU40              YES                                          
*                                                                               
         OC    NBILPRDC,NBILPRDC   IS THERE 3 CHAR PRODUCT?                     
         BZ    *+14                                                             
         MVC   WPRDC,NBILPRDC                                                   
         B     WRTU22                                                           
*                                                                               
         L     RF,VCLIST                                                        
         CLC   NBILPRD,3(RF)                                                    
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     *-14                                                             
*                                                                               
         MVC   WPRDC,0(RF)                                                      
WRTU22   BAS   RE,CKPRD            CHECK FOR RIGHT PRODUCT                      
         BNE   WRTU40                                                           
*                                                                               
         OC    INVFILT,INVFILT     SPECIFIC INVOICE NUMBER?                     
         BZ    *+14                                                             
         CLC   NBILNUM,INVFILT     YES: RIGHT INVOICE NUMBER?                   
         BNE   WRTU40              NO-SKIP                                      
*                                                                               
         TM    NBILST,NBILRLQ      REVERSAL?                                    
         BZ    WRTU30              NO                                           
*                                                                               
         OC    NBILRVDT,NBILRVDT   IS DATE OF REVERSED BILL PRESENT?            
         BZ    WRTU30              NO -- IT'S A RELATIVELY OLD UNIT.            
*                                  THIS UBILL RECORDS WAS CREATED BY            
*                                  SUSHEEL'S CONVERSION PROGRAM AND             
*                                  NOT BY BILLING. THEREFORE, WE DON'T          
*                                  KNOW THE DATE AND INVOICE NUMBER OF          
*                                  THE 'REVERSEE', SO WE CAN'T MARK IT          
*                                  UNREVERSED.                                  
*                                                                               
         LA    RE,WORK             BUILD TABLE ENTRY IN WORK                    
         USING REVERSD,RE                                                       
         MVC   REVDATE,NBILRVDT    DATE OF REVERSED INVOICE                     
         MVC   REVINVNO,NBILRVIN   REVERSED INVOICE NUMBER                      
         MVC   REVGROSS,NBILGRS    GROSS                                        
         MVC   REVNET,NBILNET      NET                                          
         MVC   REVPRD,NBILPRDC     PRODUCT                                      
         OC    REVPRD,REVPRD       IF NO 3CHAR PRODUCT                          
         BNZ   WRTU25                                                           
         L     RF,ADCLT            GET 1 CHAR AND CONVERT TO 3CHAR              
         LA    RF,CLIST-CLTHDR(RF)                                              
         CLC   NBILPRD,3(RF)                                                    
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     *-14                                                             
*                                                                               
         MVC   REVPRD,0(RF)                                                     
WRTU25   MVC   REVCHGCD,NBILCHGC   SPECIAL CHARGE TYPE CODE                     
         MVC   REVMKSTA,NBILMKST   LOCAL MARKET/STATION                         
         DROP  RE                                                               
*                                                                               
         GOTO1 BINSRCH,DMCB,(X'01',WORK),A(REVERTAB),NUMREVS,REVERSDQ, +        
               REVERSDQ,MAXREVS                                                 
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                MUST INCREASE MAXREVS                        
         MVC   NUMREVS,DMCB+8                                                   
*                                                                               
*                                  OK TO UNBILL                                 
WRTU30   DS    0H                                                               
         OI    NBILST,NBILUBQ                                                   
         MVI   UBILCHGD,C'Y'       RECORD HAS CHANGED                           
*                                                                               
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
         XC    BUFFREC,BUFFREC                                                  
*                                                                               
         MVC   INVCLT,NBACTCLI     CLIENT CODE                                  
         MVC   INVCLTNM,CLTNM      CLIENT NAME                                  
         MVC   INVPRD,WPRDC                                                     
         MVC   INVEST,NBACTEST                                                  
         GOTO1 DATCON,DMCB,(2,BILLDATE),INVBDATE                                
         MVC   INVNUM,NBILNUM      INVOICE NUMBER                               
         MVC   INVSTA,BMKTSTA+2    NETWORK (IN MSPACK FORMAT)                   
         MVC   INVPKG,NBPACK                                                    
         MVC   INVUPGM,NBACTPRG                                                 
         MVC   INVUDAT,NBACTDAT                                                 
         MVC   INVULIN,NBACTSUB                                                 
         MVC   INVCTYP,NBILCHGT                                                 
         MVC   INVLMKST,NBILMKST                                                
         MVC   INVPER,NBILMOS                                                   
         MVI   INVSRC,INVSRCBQ     FROM UNIT BILLING RECORD                     
*                                                                               
         ICM   R1,15,NBILGRS       GROSS                                        
         CVD   R1,DUB                                                           
         ZAP   INVGRS,DUB                                                       
         ICM   R1,15,NBILNET       NET                                          
         CVD   R1,DUB                                                           
         ZAP   INVNET,DUB                                                       
         ICM   R1,15,NBILGR2       GROSS (COS2)                                 
         CVD   R1,DUB                                                           
         ZAP   INVGRS2,DUB                                                      
         DROP  R4                                                               
*                                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFAPUT',A(BUFFM)),BUFFREC,ACOMFACS         
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                BUFFERIN PUT UNSUCCESSFUL                    
*                                                                               
WRTU40   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    WRTU20                                                           
         DROP  R2                                                               
*                                                                               
         CLI   UBILCHGD,C'Y'       WAS RECORD MODIFIED?                         
         BNE   WRTUX               NO                                           
         CLI   QOPT2,C'T'          PRINT TRACE OF UPDATES?                      
         BNE   WRTU50                                                           
         GOTO1 HEXOUT,DMCB,XSPKEY+36,TRCMSG4+31,4,=C'TOG'                       
         CLC   =F'8',DMCB+16       PRINT DISK ADDRESS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRTREC),DMCB,A(XSPIO),(42,32),PRINT,HEXOUT,C'DOME',  +        
               ('TRCMSG4X',TRCMSG4)                                             
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
WRTU50   DS    0H                                                               
         MVI   ACTIVITY,C'Y'       WE NEED TO UPDATE THE FILE                   
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   WRTU60                                                           
         CLI   QOPT1,C'L'          LIVE OVERNIGHT RUN?                          
         BE    *+12                YES                                          
         CLI   QOPT1,C'P'          LIVE PROGRAMMER'S RUN (VIA TSO)?             
         BNE   WRTU60              NO: DON'T ACTUALLY UNBILL                    
*                                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XSPKEY+36,A(XSPIO),      +        
               DMWORKX                                                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRTU60   DS    0H                                                               
         ICM   R4,15,NUMREVS                                                    
         BZ    WRTUX               NO REVERSALS WERE UNBILLED                   
*                                                                               
         B     WRTUX               *** NO LONGER NEEDED ***                     
*                                                                               
         L     R5,=A(REVERTAB)                                                  
         USING REVERSD,R5                                                       
WRTU70   DS    0H                                                               
         USING NUBKEY,R3                                                        
         XC    XSPKEY,XSPKEY                                                    
         MVI   NUBKSYS,NUBKSYSQ    X'0E06' RECORDS                              
         MVI   NUBKSTYP,NUBKSTYQ                                                
         MVC   NUBKAM,NBACTAM      AGENCY/MEDIA                                 
         MVC   NUBKCLI,NBACTCLI    CLIENT                                       
         MVC   NUBKNET,NBACTNET    NETWORK                                      
         MVC   NUBKPROG,NBACTPRG   PROGRAM                                      
         MVC   NUBKDATE,NBACTDAT   AIR DATE                                     
         MVC   NUBKEST,NBACTEST    ESTIMATE                                     
         MVC   NUBKSUB,NBACTSUB    SUB-LINE                                     
         MVC   NUBKDPT,NBACTDP     DAYPART                                      
         MVC   NUBKBDAT,REVDATE    DATE OF REVERSED INVOICE                     
*                                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,=C'XSPDIR',XSPKEY,XSPKEY                     
         TM    DMCB+8,X'FF'-X'10'                                               
         BZ    *+6                                                              
         DC    H'0'                FATAL DATAMGR ERROR                          
         TM    DMCB+8,X'10'        'REVERSEE' RECORD MUST BE THERE --           
         BZ    *+6                 DIE IF IT ISN'T (ALTHOUGH WE REALLY          
         DC    H'0'                COULD JUST SKIP THIS ONE)                    
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFIL',XSPKEY+36,A(XSPIO),      +        
               DMWORKX                                                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRTU80   DS    0H                                                               
         L     R2,=A(XSPIO)        UNIT BILL RECORD                             
         USING NBILD,R2                                                         
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,NBILELQ      LOOK FOR BILLING ELEMENTS                    
         BRAS  RE,GETEL            'REVERSEE' ELEMENT MUST BE THERE --          
         BE    *+6                 DIE IF IT ISN'T (ALTHOUGH WE REALLY          
         DC    H'0'                COULD JUST SKIP THIS ONE)                    
*                                                                               
WRTU90   DS    0H                  FIND MATCHING 'REVERSEE' ELEMENT             
         TM    NBILST,NBILRDQ      MUST BE REVERSED                             
         BZ    WRTU100                                                          
         CLC   NBILNUM,REVINVNO    INVOICE NUMBER                               
         BNE   WRTU100                                                          
         OC    NBILPRDC,NBILPRDC   IS THERE 3 CHAR PRODUCT                      
         BZ    *+18                                                             
         CLC   NBILPRDC,REVPRD     YES, COMPARE AGAINST IT                      
         BNE   WRTU100                                                          
         B     WRTU95                                                           
*                                                                               
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDR(RF)                                              
         CLC   NBILPRD,3(RF)                                                    
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     *-14                                                             
*                                                                               
         CLC   REVPRD,0(RF)                                                     
         BNE   WRTU100                                                          
*                                                                               
WRTU95   CLC   NBILCHGC,REVCHGCD   SPECIAL CHARGE TYPE CODE                     
         BNE   WRTU100                                                          
         CLC   NBILMKST,REVMKSTA   LOCAL MARKET/STATION                         
         BNE   WRTU100                                                          
*                                                                               
         ICM   R0,15,NBILGRS       GROSS MUST MATCH COMPLEMENT OF GROSS         
         LCR   R0,R0                                                            
         C     R0,REVGROSS                                                      
         BNE   WRTU100                                                          
         ICM   R0,15,NBILNET       NET MUST MATCH COMPLEMENT OF NET             
         LCR   R0,R0                                                            
         C     R0,REVNET                                                        
         BE    WRTU110             WE'VE GOT THE 'REVERSEE'                     
*                                                                               
WRTU100  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    WRTU90                                                           
*                                   'REVERSEE' ELEMENT MUST BE THERE --         
*                                   DIE IF IT ISN'T (ALTHOUGH WE REALLY         
*                                   COULD JUST SKIP THIS ONE)                   
         DC    H'0'                'REVERSEE' ELEMENT HAS VANISHED              
*                                                                               
WRTU110  DS    0H                                                               
         NI    NBILST,X'FF'-NBILRDQ    UN-REVERSE THE ORIGINAL REVERSAL         
*                                                                               
         LA    R5,REVERSDQ(R5)     BUMP TO NEXT ENTRY IN REVERSAL TABLE         
         CLC   REVDATE,NUBKBDAT    SAME ORIGINAL INVOICE DATE?                  
         BE    WRTU80              YES -- GO BACK TO START OF RECORD            
         DROP  R2                                                               
         DROP  R3                                                               
*                                                                               
         CLI   QOPT2,C'T'          PRINT TRACE OF FILE UPDATES?                 
         BNE   WRTU120                                                          
         GOTO1 HEXOUT,DMCB,XSPKEY+36,TRCMSG5+37,4,=C'TOG'                       
         CLC   =F'8',DMCB+16       PRINT DISK ADDRESS                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRTREC),DMCB,A(XSPIO),(42,32),PRINT,HEXOUT,C'DOME',  +        
               ('TRCMSG5X',TRCMSG5)                                             
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
WRTU120  DS    0H                                                               
         MVI   ACTIVITY,C'Y'       WE NEED TO UPDATE THE FILE                   
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   WRTU130                                                          
         CLI   QOPT1,C'L'          LIVE OVERNIGHT RUN?                          
         BE    *+12                YES                                          
         CLI   QOPT1,C'P'          LIVE PROGRAMMER'S RUN (VIA TSO)?             
         BNE   WRTU130             NO: DON'T ACTUALLY UNBILL                    
*                                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFIL',XSPKEY+36,A(XSPIO),      +        
               DMWORKX                                                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRTU130  DS    0H                                                               
         OC    REVDATE,REVDATE     EOT?                                         
         BNZ   WRTU70              NO                                           
*                                                                               
WRTUX    DS    0H                                                               
         J     XIT                                                              
         SPACE 2                                                                
         DROP  R5                                                               
         DROP  R8                                                               
         SPACE 3                                                                
TRCMSG4  DC    C'PUT UNIT BILLING RECORD: D/A = XXXXXXXX'                       
TRCMSG4X EQU   *-TRCMSG4                                                        
         SPACE 3                                                                
TRCMSG5  DC    C'UNREVERSE UNIT BILLING RECORD: D/A = XXXXXXXX'                 
TRCMSG5X EQU   *-TRCMSG5                                                        
         EJECT                                                                  
CKPRD    NTR1                                                                   
*                                  CHCK IF WANT THIS PRODUCT (IN WPRDC)         
*                                  SET CC UPON RETURN                           
*                                                                               
*        CLI   WPRD,0                                                           
         OC    WPRDC,WPRDC                                                      
         BE    NO                  SKIP UNALLOCATED                             
         OC    KPRDC,KPRDC         ALL PRODUCTS                                 
         BZ    CKPYES                                                           
*        CLI   KPRD,X'FF'                                                       
         CLC   KPRDC,=C'POL'                                                    
         BE    CKPYES                                                           
*        CLC   KPRD,WPRD           ONE PRODUCT MUST BE EQUAL                    
         CLC   KPRDC,WPRDC         ONE PRODUCT MUST BE EQUAL                    
         BNE   NO                                                               
*                                                                               
CKPYES   DS    0H                                                               
*                                  GET PRODUCT CODE                             
*        L     RF,ADCLT                                                         
*        LA    RF,CLIST-CLTHDR(RF)                                              
*        CLC   WPRD,3(RF)                                                       
*        BE    *+12                                                             
*        LA    RF,4(RF)                                                         
*        B     *-14                                                             
*                                                                               
*        MVC   WPRDC,0(RF)                                                      
         B     YES                 PRODUCT OK                                   
*                                                                               
         ANSR                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'REPRT - PRINT REPORT FROM BUFFERIN RECORDS'                     
REPRT    NMOD1 0,REPRT                                                          
         LA    RC,SPACEND                                                       
*                                                                               
         L     RF,=A(BUFFM)                                                     
         OC    BUFFNREC-BUFFD(,RF),BUFFNREC-BUFFD(RF)                           
         BZ    REPX                NOTHING TO UNBILL                            
*                                                                               
         LA    R2,P                                                             
         USING LINED,R2                                                         
         XC    OLDKEY,OLDKEY                                                    
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
         GOTO1 =V(BUFFERIN),DMCB,('BUFFARDH',A(BUFFM)),BUFFREC,ACOMFACS         
         B     REP20                                                            
*                                                                               
REP10    DS    0H                                                               
         GOTO1 =V(BUFFERIN),DMCB,('BUFFASEQ',A(BUFFM)),BUFFREC,ACOMFACS         
*                                                                               
REP20    DS    0H                                                               
         TM    4(R1),BUFFEEOF      AT EOF...                                    
         BZ    REP30                                                            
         MVI   BUFFREC,X'FF'       ...FORCE A "RECORD" OF X'FF'S                
         MVC   BUFFREC+1(L'BUFFREC-1),BUFFREC                                   
         B     REP40                                                            
*                                                                               
REP30    DS    0H                                                               
         LA    R3,OLDKEY                                                        
OKEY     USING INVD,R3                                                          
         CLC   INVCLT,OKEY.INVCLT  SAME CLT/PRD/EST/INVOICE?                    
         BNE   REP40                                                            
         CLC   INVPRD,OKEY.INVPRD                                               
         BNE   REP40                                                            
         CLC   INVEST,OKEY.INVEST                                               
         BNE   REP40                                                            
         CLC   INVNUM,OKEY.INVNUM                                               
         BE    REP80               YES                                          
*                                                                               
REP40    DS    0H                                                               
         OC    OLDKEY,OLDKEY       FIRST TIME?                                  
         BZ    REP70               YES                                          
*                                                                               
*                                  NO: FINISH LAST INVOICE                      
         LA    R5,INVTOTS          ACCUMULATE INVOICE TOTALS                    
         USING TOTALSD,R5                                                       
*                                                                               
         CLI   PROFB1S+3,C'Y'      NEW UNIT BILLING RECORDS MANDATORY?          
         BE    REP50               YES: DON'T EXPECT UNITS TO MATCH             
*                                                                               
         ZAP   DUB,GRSUNIT         BILL HEADERS MUST MATCH...                   
         AP    DUB,GRSSTAB         ...UNIT DETAILS PLUS MANUAL STABUCKS         
         CP    GRSBLHD,DUB                                                      
         BNE   REP43                                                            
         ZAP   DUB,NETUNIT                                                      
         AP    DUB,NETSTAB                                                      
         CP    NETBLHD,DUB                                                      
         BNE   REP43                                                            
         ZAP   DUB,GRS2UNIT                                                     
         AP    DUB,GRS2STAB                                                     
         CP    GRS2BLHD,DUB                                                     
         BE    REP50                                                            
*                                                                               
REP43    DS    0H                                                               
         MVC   P(21),=C'***ERROR*** IMBALANCE'                                  
         MVC   LGRS-22(20),=C'SUM OF UNIT DETAILS='                             
         EDIT  GRSUNIT,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETUNIT,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2UNIT,LGRS2,2,COMMAS=YES,MINUS=YES                            
*                                                                               
         CP    GRSBLHD,=P'0'                                                    
         BNE   REP45                                                            
         CP    NETBLHD,=P'0'                                                    
         BNE   REP45                                                            
         CP    GRS2BLHD,=P'0'                                                   
         BNE   REP45                                                            
         MVC   P+12(9),=C'NO HEADER'                                            
*                                                                               
REP45    DS    0H                                                               
         CLI   PROFB1S+3,C'Y'      NEW UNIT BILLING RECORDS MANDATORY?          
         BE    REP48               YES: DON'T EXPECT UNIT DETAILS               
         CP    GRSUNIT,=P'0'                                                    
         BNE   REP48                                                            
         CP    NETUNIT,=P'0'                                                    
         BNE   REP48                                                            
         CP    GRS2UNIT,=P'0'                                                   
         BNE   REP48                                                            
         MVC   P+12(22),=C'NO UNIT RECORD DETAILS'                              
*                                                                               
REP48    DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
REP50    DS    0H                                                               
         CLI   PROFB1S+3,C'Y'                                                   
         BE    *+12                                                             
         CLI   PROFB1S+3,C'R'                                                   
         BNE   REP65                                                            
*                                                                               
         ZAP   DUB,GRSUBIL         BILL HEADERS MUST MATCH...                   
         AP    DUB,GRSSTAB         ...UBILL RECS PLUS MANUAL STABUCKS           
         CP    GRSBLHD,DUB                                                      
         BNE   REP55                                                            
         ZAP   DUB,NETUBIL                                                      
         AP    DUB,NETSTAB                                                      
         CP    NETBLHD,DUB                                                      
         BNE   REP55                                                            
         ZAP   DUB,GRS2UBIL                                                     
         AP    DUB,GRS2STAB                                                     
         CP    GRS2BLHD,DUB                                                     
         BE    REP65                                                            
*                                                                               
REP55    DS    0H                                                               
         MVC   P(21),=C'***ERROR*** IMBALANCE'                                  
         MVC   LGRS-23(21),=C'SUM OF UBILL DETAILS='                            
         EDIT  GRSUBIL,LGRS,2,COMMAS=YES,MINUS=YES                              
         EDIT  NETUBIL,LNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  GRS2UBIL,LGRS2,2,COMMAS=YES,MINUS=YES                            
*                                                                               
         CP    GRSBLHD,=P'0'                                                    
         BNE   REP57                                                            
         CP    NETBLHD,=P'0'                                                    
         BNE   REP57                                                            
         CP    GRS2BLHD,=P'0'                                                   
         BNE   REP57                                                            
         MVC   P+12(14),=C'NO BILL HEADER'                                      
*                                                                               
REP57    DS    0H                                                               
         CP    GRSUBIL,=P'0'                                                    
         BNE   REP60                                                            
         CP    NETUBIL,=P'0'                                                    
         BNE   REP60                                                            
         CP    GRS2UBIL,=P'0'                                                   
         BNE   REP60                                                            
         MVC   P+12(23),=C'NO UNIT BILLING RECORDS'                             
         DROP  R5                                                               
*                                                                               
REP60    DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
REP65    DS    0H                                                               
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
REP70    DS    0H                                                               
         CLI   INVNUM,X'FF'        X'FF' "RECORD"?                              
         BE    REPX                YES: WE'RE DONE                              
*                                                                               
         CLC   INVCLT,OKEY.INVCLT  SAME CLIENT?                                 
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       NO: EJECT PAGE                               
         DROP  OKEY                                                             
*                                                                               
         LA    RF,INVTOTS          CLEAR INVOICE TOTALS                         
         USING TOTALSD,RF                                                       
         ZAP   GRSUNIT,=P'0'       CLEAR INVOICE TOTALS                         
         ZAP   NETUNIT,=P'0'                                                    
         ZAP   GRS2UNIT,=P'0'                                                   
         ZAP   GRSUBIL,=P'0'                                                    
         ZAP   NETUBIL,=P'0'                                                    
         ZAP   GRS2UBIL,=P'0'                                                   
         ZAP   GRSBLHD,=P'0'                                                    
         ZAP   NETBLHD,=P'0'                                                    
         ZAP   GRS2BLHD,=P'0'                                                   
         ZAP   GRSSTAB,=P'0'                                                    
         ZAP   NETSTAB,=P'0'                                                    
         ZAP   GRS2STAB,=P'0'                                                   
         DROP  RF                                                               
*                                                                               
REP80    DS    0H                                                               
         MVC   OLDKEY,BUFFREC                                                   
*                                                                               
*                                  INVOICE NO.                                  
         GOTO1 VSPFMTIN,DMCB,INVBDATE,(2,INVNUM),(QMED,PROFB1),PROFB1X          
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
         CLI   INVSRC,INVSRCUQ     UNIT RECORD?                                 
         BNE   *+14                                                             
         MVC   LPER,=C'*UNIT*'                                                  
         B     REP100                                                           
         CLI   INVSRC,INVSRCBQ     UNIT BILLING RECORD?                         
         BNE   *+14                                                             
         MVC   LPER,=C'*UBIL*'                                                  
         B     REP100                                                           
*                                                                               
         CLI   INVPER,0                                                         
         BE    REP100                                                           
         CLI   INVPER+1,12                                                      
         BH    REP90                                                            
*                                  1 - 12 TREAT AS MONTH                        
         GOTO1 DATCON,DMCB,(3,INVPER),(6,WORK)                                  
         MVC   LPER,WORK                                                        
         B     REP100                                                           
*                                                                               
REP90    DS    0H                  SPECIAL BILLING PERIOD                       
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
REP100   DS    0H                                                               
*                                  MARKET/STATION                               
         CLC   INVSTA,=X'FFFFFF'   BILL HEADER DATA?                            
         BNE   REP105              NO                                           
*                                                                               
*                                                                               
         MVC   LBILHEAD,=C'*HEADER*'                                            
         CLI   INVSUBM,C' '        ANY SUB-MEDIA FILTER ON BILL HEADER?         
         BE    *+18                                                             
         MVI   LBILSUBM,C'('       YES: PRINT IT                                
         MVC   LBILSUBM+1(1),INVSUBM                                            
         MVI   LBILSUBM+2,C')'                                                  
*                                                                               
         MVC   LBILTYP(6),=C'TYPE: '                                            
         MVC   LBILTYP+6(4),INVBTYPE  BILL TYPE                                 
*                                                                               
         OC    INVRVRS#,INVRVRS#   UNBILLING A REVERSAL?                        
         BZ    REP110              NO                                           
         MVC   LBILRVSL,=C'REVERSES YM-NNNN ON MMMDD/YY'                        
         GOTO1 DATCON,DMCB,(2,INVRVRDT),WORK                                    
         GOTO1 VSPFMTIN,DMCB,WORK,(2,INVRVRS#),(QMED,PROFB1),PROFB1X            
         L     RF,DMCB+4                                                        
         MVC   LBILRVSL+9(7),0(RF) REVERSED INVOICE NUMBER                      
         GOTO1 DATCON,DMCB,(2,INVRVRDT),(8,LBILRVSL+20)                         
         B     REP110                                                           
*                                                                               
REP105   DS    0H                                                               
         CLC   INVSTA,=X'D6B5A4'   MANUAL BILL (STATION ZZZZ)?                  
         BNE   *+14                                                             
         MVC   LBILHEAD,=C'*MANUAL*'                                            
         B     REP110                                                           
*                                                                               
         XC    WORK(5),WORK                                                     
         MVC   WORK+2(3),INVSTA                                                 
         GOTO1 MSUNPK,DMCB,WORK,FULL,DUB                                        
         MVC   LSTA,DUB                                                         
*                                                                               
         ZIC   RF,INVPKG           PACKAGE NUMBER                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LPKG,DUB                                                         
*                                                                               
         MVC   LPROG,INVUPGM                                                    
*                                                                               
REP110   DS    0H                                                               
         CLI   INVULIN,1                                                        
         BNH   REP120                                                           
         ZIC   R0,INVULIN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LSUB,DUB                                                         
         MVI   LHYPHEN,C'-'                                                     
*                                                                               
REP120   DS    0H                                                               
         CLI   INVUDAT,0                                                        
         BE    REP130                                                           
         GOTO1 DATCON,DMCB,(2,INVUDAT),(5,LUDAT)                                
*                                                                               
REP130   DS    0H                                                               
         CLI   INVCTYP,0                                                        
         BE    *+10                                                             
         MVC   LCTYP,INVCTYP       COST TYPE                                    
*                                                                               
         OC    INVLMKST,INVLMKST   ANY LOCAL STATION DATA?                      
         BZ    REP140              NO                                           
         GOTO1 MSUNPK,DMCB,INVLMKST,LMKT,LLOCSTA                                
*                                                                               
REP140   DS    0H                                                               
         EDIT  INVGRS,LGRS,2,COMMAS=YES,MINUS=YES                               
         EDIT  INVNET,LNET,2,COMMAS=YES,MINUS=YES                               
         EDIT  INVGRS2,LGRS2,2,COMMAS=YES,MINUS=YES                             
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLC   INVSTA,=X'FFFFFF'   BILL HEADER DATA?                            
         BNE   REP150                                                           
         USING TOTALSD,RF                                                       
         LA    RF,INVTOTS                                                       
         AP    GRSBLHD,INVGRS      YES: ADD TO BILL HEADER INVOICE TOTS         
         AP    NETBLHD,INVNET                                                   
         AP    GRS2BLHD,INVGRS2                                                 
         LA    RF,REQTOTS          AND BILL HEADER REQUEST TOTALS               
         AP    GRSBLHD,INVGRS                                                   
         AP    NETBLHD,INVNET                                                   
         AP    GRS2BLHD,INVGRS2                                                 
         B     REP10                                                            
*                                                                               
REP150   DS    0H                                                               
         CLI   INVSRC,INVSRCUQ     UNIT RECORD DETAILS?                         
         BNE   REP160                                                           
         LA    RF,INVTOTS                                                       
         AP    GRSUNIT,INVGRS      YES: ADD TO UNIT RECORD INVOICE TOTS         
         AP    NETUNIT,INVNET                                                   
         AP    GRS2UNIT,INVGRS2                                                 
         LA    RF,REQTOTS          AND UNIT RECORD REQUEST TOTALS               
         AP    GRSUNIT,INVGRS                                                   
         AP    NETUNIT,INVNET                                                   
         AP    GRS2UNIT,INVGRS2                                                 
         B     REP10                                                            
*                                                                               
REP160   DS    0H                                                               
         CLC   INVSTA,=X'D6B5A4'   MANUAL BILL (STATION ZZZZ)?                  
         BNE   REP165                                                           
         LA    RF,INVTOTS                                                       
         AP    GRSSTAB,INVGRS      YES: ADD TO MANUAL BILL STABUCK TOTS         
         AP    NETSTAB,INVNET                                                   
         AP    GRS2STAB,INVGRS2                                                 
         LA    RF,REQTOTS          AND MANUAL STABUCK REQUEST TOTALS            
         AP    GRSSTAB,INVGRS                                                   
         AP    NETSTAB,INVNET                                                   
         AP    GRS2STAB,INVGRS2                                                 
         B     REP10                                                            
*                                                                               
REP165   DS    0H                  MUST BE UNIT BILLING RECORD DETAILS          
         LA    RF,INVTOTS                                                       
         AP    GRSUBIL,INVGRS      ADD TO UBILL RECORD INVOICE TOTALS           
         AP    NETUBIL,INVNET                                                   
         AP    GRS2UBIL,INVGRS2                                                 
         LA    RF,REQTOTS          AND UBILL RECORD REQUEST TOTALS              
         AP    GRSUBIL,INVGRS                                                   
         AP    NETUBIL,INVNET                                                   
         AP    GRS2UBIL,INVGRS2                                                 
         B     REP10                                                            
         DROP  RF                                                               
*                                                                               
REPX     DS    0H                                                               
         MVI   FORCEHED,C'Y'       EJECT PAGE                                   
         J     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         GETEL R2,DATADISP,ELCODE                                               
         SPACE 3                                                                
*                                  BUFFERIN PARAMETERS                          
BUFFM    BUFFD TYPE=P,KEYLEN=INVKL,COMLEN=INVCL,COLUMNS=3,FILE=BUFFWK, +        
               BUFFERS=255                                                      
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*NETBLK*'                                                      
NETBLK   DS    2000X               AREA FOR NETIO WORK                          
*                                                                               
         DS    0D                                                               
         DC    C'*XSPIO**'                                                      
XSPIO    DS    4000X               XSPFIL RECORD I/O AREA                       
*                                                                               
         DS    0D                                                               
         DC    C'*STABUCK'                                                      
STABUCKC DS    4000C               ALSO USED FOR NETIO BUFFER                   
         DC    X'00'                                                            
*                                                                               
         DS    0D                                                               
         DC    C'*REVERS*'                                                      
REVERTAB DS    (MAXREVS)XL(REVERSDQ)  TABLE OF INVOICES TO UNREVERSE            
REVERTBX DC    X'0000'                                                          
MAXREVS  EQU   100                                                              
*                                                                               
         DS    0D                                                               
         DC    C'*UNPOST*'                                                      
MYREQTAB DS    (MAXREQS)CL80       TABLE OF 'MY' REQUESTS TO UNPOST             
MYREQTBX DC    X'00'                                                            
MAXREQS  EQU   10                                                               
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
         USING INVD,R4                                                          
         MVC   HEAD4(6),=C'CLIENT'                                              
         GOTO1 CLUNPK,DMCB,INVCLT,HEAD4+9                                       
         MVC   HEAD4+13(L'INVCLTNM),INVCLTNM                                    
         DROP  R4                                                               
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
DMWORKU  DS    12D                 DMWORK FOR UNTFILE RECORDS                   
DMWORKX  DS    12D                 DMWORK FOR XSPFIL RECORDS                    
*                                                                               
VSPFMTIN DS    V                   V(SPFMTINO)                                  
*                                                                               
DECADE   DS    XL1                                                              
YEARDIG  DS    XL1                                                              
*                                                                               
INVFILT  DS    H                   SPECIFIC INVOICE NUMBER TO UNBILL            
*                                                                               
PROFB1   DS    CL16                B1 PROFILE                                   
PROFB1X  DS    CL16                B1X PROFILE                                  
PROFB1S  DS    CL16                B1S PROFILE                                  
*                                                                               
         DS    0D                                                               
INVTOTS  DS    XL(TOTALSLQ)        INVOICE-LEVEL TOTALS                         
REQTOTS  DS    XL(TOTALSLQ)        REQUEST-LEVEL TOTALS                         
REPTOTS  DS    XL(TOTALSLQ)        REPORT-LEVEL TOTALS                          
*                                                                               
BUFFREC  DS    XL(INVRLQ)                                                       
OLDKEY   DS    XL(INVRLQ)                                                       
*                                                                               
MYPARMS  DS    6F                  BINSRCH PARMS FOR 'MY' REQUEST TABLE         
         ORG   MYPARMS                                                          
MYP1HOB  DS    X                                                                
MYP1AREC DS    AL3                                                              
MYP2ATAB DS    A                                                                
MYP3#REC DS    F                                                                
MYP4LREC DS    F                                                                
MYP5DKEY DS    FL1                                                              
MYP5LKEY DS    FL3                                                              
MYP6#MAX DS    F                                                                
*                                                                               
BILLDATE DS    H                                                                
ACTIVITY DS    C                   'Y' = WE NEED TO UPDATE THE FILE             
UBILCHGD DS    C                   'Y' = UBILL RECORD HAS CHANGED               
KPRD     DS    X                                                                
KPRDC    DS    CL3                                                              
RECSW    DS    X                                                                
ELCODE   DS    X                                                                
WPRD     DS    X                                                                
WPRDC    DS    CL3                                                              
NUMREVS  DS    A                   NUMBER OF ENTRIES IN REVERSAL TABLE          
*                                                                               
HEADER$P DS    PL8                                                              
DETAIL$P DS    PL8                                                              
*                                                                               
XSPKEY   DS    XL40                                                             
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
LSTA     DS    CL4                 NETWORK                                      
         DS    C                                                                
LPKG     DS    CL3                 PACKAGE NUMBER                               
         DS    C                                                                
LPROG    DS    CL6                 PROGRAM                                      
         DS    C                                                                
LUDAT    DS    CL8                 UNIT DATE                                    
LHYPHEN  DS    C                                                                
LSUB     DS    CL2                 SUB-LINE                                     
         DS    C                                                                
LCTYP    DS    C                   COST TYPE                                    
         DS    C                                                                
LMKT     DS    CL4                 LOCAL MARKET                                 
         DS    C                                                                
LLOCSTA  DS    CL5                 LOCAL STATION                                
         DS    CL19                                                             
         ORG   LSTA                                                             
LBILHEAD DS    CL8                 '*HEADER*'                                   
         DS    CL2                                                              
LBILSUBM DS    CL3                 SUB-MEDIA                                    
         DS    CL2                                                              
LBILTYP  DS    CL10                BILL TYPE                                    
         DS    CL2                                                              
LBILRVSL DS    CL28                'REVERSES YM-NNNN ON MMMDD/YY'               
         ORG                                                                    
LGRS     DS    CL15                GROSS                                        
         DS    C                                                                
LNET     DS    CL15                NET                                          
         DS    C                                                                
LGRS2    DS    CL15                GROSS (COS2)                                 
         SPACE 3                                                                
*                                  DSECT FOR TABLE ENTRY                        
INVD     DSECT                                                                  
INVCLT   DS    XL2                 CLIENT                                       
INVPRD   DS    CL3                 PRODUCT                                      
INVEST   DS    HL1                 ESTIMATE                                     
INVNUM   DS    HL2                 INVOICE NUMBER                               
INVSRC   DS    X                   RECORD SOURCE                                
INVSRCUQ EQU   1                    UNIT RECORD                                 
INVSRCBQ EQU   2                    UNIT BILLING RECORD                         
INVSRCMQ EQU   3                    STATION BUCKET RECORD (MANUALS)             
INVSRCHQ EQU   4                    BILL HEADER RECORD                          
INVPER   DS    XL2                 MOS                                          
INVSTA   DS    XL3                 NETWORK STATION (X'FF'S FOR HEADERS)         
INVPKG   DS    HL1                 PACKAGE                                      
INVUPGM  DS    CL6                 PROGRAM                                      
INVUDAT  DS    XL2                 AIR DATE                                     
INVULIN  DS    HL1                 SUB-LINE                                     
INVSUBM  DS    C                   SUB-MEDIA (BILL HEADERS ONLY)                
INVBTYPE DS    CL4                 BILL TYPE (BILL HEADERS ONLY)                
INVCTYP  DS    C                   COST TYPE                                    
INVLMKST DS    0XL5                LOCAL MARKET/STATION                         
INVLMKT  DS    HL2                                                              
INVLSTA  DS    XL3                                                              
INVKL    EQU   *-INVD              LENGTH OF BUFFERIN KEY                       
INVCOMM  EQU   *                                                                
INVBDATE DS    CL6                 BILL DATE                                    
INVCLTNM DS    CL24                CLIENT NAME                                  
INVRVRS# DS    HL2                 REVERSED BILL NUMBER                         
INVRVRDT DS    XL2                 REVERSED BILL DATE                           
INVCL    EQU   *-INVCOMM           LENGTH OF BUFFERIN "COMMENT"                 
INVGRS   DS    PL8                 GROSS                                        
INVNET   DS    PL8                 NET                                          
INVGRS2  DS    PL8                 GROSS (COS2)                                 
INVRLQ   EQU   *-INVD                                                           
         SPACE 3                                                                
REVERSD  DSECT                                                                  
REVDATE  DS    XL2                 DATE OF REVERSED INVOICE                     
REVINVNO DS    H                   REVERSED INVOICE NUMBER                      
REVGROSS DS    F                   GROSS                                        
REVNET   DS    F                   NET                                          
REVPRD   DS    CL3                 PRODUCT                                      
REVCHGCD DS    X                   SPECIAL CHARGE TYPE CODE                     
REVMKSTA DS    XL5                 LOCAL MARKET/STATION                         
REVERSDQ EQU   *-REVERSD                                                        
         SPACE 3                                                                
TOTALSD  DSECT                                                                  
GRSUNIT  DS    PL8                 GROSS      (UNIT RECORD)                     
NETUNIT  DS    PL8                 NET        (UNIT RECORD)                     
GRS2UNIT DS    PL8                 GROSS COS2 (UNIT RECORD)                     
GRSUBIL  DS    PL8                 GROSS      (UNIT BILLING RECORD)             
NETUBIL  DS    PL8                 NET        (UNIT BILLING RECORD)             
GRS2UBIL DS    PL8                 GROSS COS2 (UNIT BILLING RECORD)             
GRSBLHD  DS    PL8                 GROSS      (BILL HEADER RECORD)              
NETBLHD  DS    PL8                 NET        (BILL HEADER RECORD)              
GRS2BLHD DS    PL8                 GROSS COS2 (BILL HEADER RECORD)              
GRSSTAB  DS    PL8                 GROSS      (STABUCK: FOR MANUALS)            
NETSTAB  DS    PL8                 NET        (STABUCK: FOR MANUALS)            
GRS2STAB DS    PL8                 GROSS COS2 (STABUCK: FOR MANUALS)            
TOTALSLQ EQU   *-TOTALSD                                                        
         SPACE 3                                                                
* SPREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
*                                                                               
* NOTE: ONLY THE *FIRST* REQUEST CARD IS WRITTEN TO THE REQUEST FILE            
*       FOR THE OVERNIGHT (LIVE) UNBILLINGS!!!                                  
*                                                                               
         ORG   QAREA+49                                                         
QINVNUM  DS    CL4                 INVOICE NUMBER (OPTIONAL)                    
         ORG   Q2USER                                                           
QHEADER$ DS    CL12                BILL HEADER EXPECTED TOTAL                   
QDETAIL$ DS    CL12                DETAIL EXPECTED TOTAL                        
QDETTYPE DS    C                   TYPE OF DETAIL PROVIDED                      
QDETUNIT EQU   C'O'                 UNIT RECORD DETAIL                          
QDETUBIL EQU   C'U'                 UNIT BILLING RECORD DETAIL                  
         ORG                                                                    
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUBILL                                                     
         EJECT                                                                  
       ++INCLUDE SPGENUNBIL                                                     
         EJECT                                                                  
         PRINT OFF                                                              
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDGETPROFD                                                     
         EJECT                                                                  
       ++INCLUDE DDBUFFD                                                        
         EJECT                                                                  
         PRINT ON                                                               
SP7U02   CSECT                                                                  
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPREP7U02 08/07/13'                                      
         END                                                                    
