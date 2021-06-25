*          DATA SET PPREPCL02  AT LEVEL 011 AS OF 03/21/07                      
*PHASE PPCL02A                                                                  
*INCLUDE PPFMTINO                                                               
*INCLUDE SORTER                                                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE 03/07    ADD CLIENT CODE CAA - TREAT LIKE CLIENT CODE CP                 
*                                                                               
* SMYE 12/06    SKIP CREDIT (NEGATIVE) INVOICES AND THEIR DETAILS               
*                                                                               
* BPLA 1/06     CHANGES TO ALLOW REQUESTS BY PRODUCT                            
*                                                                               
* SMYE 10/23/01 CHANGES FOR COST CENTER LENGTH - SEE PPCOLINTFD DSECT           
*                                                                               
* KWAN 08/00    NEW PBILREC DSECT                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPCL02 - COLGATE BILLING PRINT INTERFACE'                       
*                                                                               
*QOPT2   B=PRINT BOTH INPUT TO SORT AND SORTED OUTPUT RECORDS                   
*                                                                               
*                                                                               
*QOPT4   T=WILL PRINT KEY INFO IN GAPS IN OUTPUT RECORDS                        
*          (DOES NOT WRITE SORTED OUTPUT TO FILE)                               
*                                                                               
*QOPT5   Y=TESTING ONLY - DO NOT WRITE SORTED OUTPUT TO FILE                    
*                                                                               
PPCL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPCL02                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPCLWRKD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,PROCREQ                                                     
         BE    FIRSTB                                                           
         CLI   MODE,RUNLAST                                                     
         BE    FINAL                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
INITIAL  DS    0H                                                               
         MVI   FRSTSW,C' '         CLEAR FIRST TIME SWITCH                      
         LA    R2,INSCNT           ZAP ACCUMS                                   
         LA    R3,7                                                             
INIT2    ZAP   0(4,R2),=P'0'                                                    
         LA    R2,4(R2)                                                         
         BCT   R3,INIT2                                                         
         ZAP   BILTOT,=P'0'        ZAP BILLING $ ACCUMULATOR                    
         ZAP   DTLTOT,=P'0'        ZAP DETAIL $ ACCUMULATOR                     
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FIRSTB   DS    0H                  ALL MEDIA (*) REQUESTS DIE WHEN              
         CLI   QMEDIA,C'S'         MEDIA "S" COMES UP ON A READ HIGH            
         BE    EXIT                FOR CLIENT CP, WHICH DOES NOT EXIST          
*                                  FOR THIS MEDIA. DO NOT KNOW WHY.             
*                                                                               
FB02     MVI   FORCEHED,C'Y'                                                    
*****    MVI   FCRDACTV,C'Y'                                                    
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   WRITESW,0           CLEAR WRITE-TO-FILE SWITCH                   
*                                                                               
         CLI   FRSTSW,C'Y'         FIRST TIME ?                                 
         BE    FBC10               NO                                           
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD    INITIALIZE SORT              
*                                                                               
         CLI   QOPT5,C'Y'          TEST RUN ?                                   
         BE    FBC10               YES - NO OPEN                                
         CLI   QOPT4,C'T'          TEST RUN (KEY PRINTING) ?                    
         BE    FBC10               YES - NO OPEN                                
*                                                                               
         MVC   PPDYNDSN+13(L'QAGENCY),QAGENCY                                   
         GOTO1 DYNALLOC,DMCB,(0,=C'OUTFILE '),(0,PPDYNDSN)                      
         OPEN  (OUTFILE,OUTPUT)                                                 
*                                                                               
FBC10    DS    0H                                                               
         MVI   FRSTSW,C'Y'         SET FIRST TIME SWITCH                        
*                                                                               
         MVI   ONEPRD,C'N'                                                      
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    FBC10C                                                           
         CLC   QPRODUCT,SPACES                                                  
         BE    FBC10C                                                           
         MVI   ONEPRD,C'Y'        SET FOR ONE PRODUCT                           
*                                                                               
FBC10C   DS    0H                                                               
         GOTO1 DATCON,DMCB,QSTART,(3,BQS)                                       
         GOTO1 (RF),(R1),QEND,(3,BQE)                                           
*                                                                               
         MVC   SVQSTART(12),QSTART                                              
*                                                                               
         MVC   SVOPT2,QOPT2        SAVE FOR RUNLAST                             
         MVC   SVOPT4,QOPT4        SAVE FOR RUNLAST                             
         MVC   SVOPT5,QOPT5                                                     
*                                                                               
         XC    KEY,KEY             GET CLIENT                                   
         LA    R5,KEY                                                           
         USING PCLTKEY,R5                                                       
         MVC   PCLTKAGY(3),QAGENCY    AGENCY/MEDIA                              
         MVI   PCLTKRCD,2             CLIENT RECORD CODE                        
         MVC   PCLTKCLT,QCLIENT       CLIENT CODE                               
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                  GET PROFILES                                 
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   FBC80                                                            
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
FBC80    DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE                          
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         EJECT                                                                  
*  READ AND OUTPUT TO SORTER ALL RELEVANT INVOICE RECORDS FOR CLIENT            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING PBILLKEY,R5                                                      
         MVC   PBILKAGY(3),QAGENCY    AGENCY/MEDIA                              
         MVI   PBILKRCD,8             RECORD CODE                               
         MVC   PBILKCLT,PCLTKCLT      CLIENT CODE                               
         CLI   ONEPRD,C'Y'            ONE PRODUCT REQUEST?                      
         BNE   BILL03                                                           
         MVC   PBILKPRD,QPRODUCT     PUT INTO KEY                               
*                                                                               
BILL03   GOTO1 HIGH                                                             
         B     BILL10                                                           
*                                                                               
BILL05   GOTO1 SEQ                                                              
*                                                                               
BILL10   CLC   KEY(7),KEYSAVE      SAME AGY/MED/RCD/CLIENT ?                    
         BNE   BUYPROC             NO - BILLS DONE - GO DO BUYS                 
         CLI   ONEPRD,C'Y'         ONE PRODUCT REQUEST?                         
         BNE   BILL12                                                           
         CLC   PBILKPRD,QPRODUCT                                                
         BNE   BUYPROC                                                          
*                                                                               
BILL12   TM    PBILLKEY+25,X'80'   DELETED ?                                    
         BNZ   BILL05              YES - NEXT RECORD                            
         OC    PBILKEST,PBILKEST       IGNORE BILLS WITH NO EST                 
         BZ    BILL05                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R0,PBILLREC                                                      
         ST    R0,AREC                                                          
         GOTO1 GETPRT              READ BILL RECORD                             
*                                                                               
         CLC   PBILLDAT,SVQSTART                                                
         BL    BILL05              NEXT RECORD                                  
         CLC   PBILLDAT,SVQEND                                                  
         BH    BILL05              NEXT RECORD                                  
*                                                                               
BILL20   DS    0H                  PROCESS BILL RECORD                          
         MVI   OUTREC,C' '         SPACE-FILL OUTPUT RECORD                     
         MVC   OUTREC+1(L'OUTREC-1),OUTREC                                      
         LA    R4,OUTREC                                                        
         USING PCOL1D,R4           INVOICE HEADER                               
         MVC   PCOL1VID,=C'1252071'    VENDOR ID (Y&R)                          
         CLC   QCLIENT,=C'CP '                                                  
         BE    BILL22                                                           
         CLC   QCLIENT,=C'CAA'                                                  
         BE    BILL22                                                           
         MVC   PCOL1VID,=C'2507029'    VENDOR FOR YNTDE                         
         CLC   QCLIENT,=C'COP'                                                  
         BE    *+6                                                              
         DC    H'0'              INVALID CLIENT CODE                            
*                                                                               
BILL22   DS    0H                                                               
         GOTO1 =V(PPFMTINO),DMCB,PBILLDAT,(2,PBILKBNO),                X        
               (PBILKMED,B1PROF),B1XPROF                                        
*                                                                               
         L     RF,DMCB                                                          
         MVC   PCOL1INV,0(RF)        FULL INVOICE NUMBER                        
*                                                                               
         MVC   PCOL1OID,=X'E850D9'      OPERATOR ID ("Y&R")                     
         CLC   QCLIENT,=C'CP '                                                  
         BE    BILL25                                                           
         CLC   QCLIENT,=C'CAA'                                                  
         BE    BILL25                                                           
         MVC   PCOL1OID,=C'TDE'         OPERATOR ID FOR TDE                     
         CLC   QCLIENT,=C'COP'                                                  
         BE    *+6                                                              
         DC    H'0'              INVALID CLIENT CODE                            
*                                                                               
BILL25   DS    0H                                                               
         MVC   WORK(12),PBILLREC     MUST READ ESTIMATE FOR NAME                
         MVI   WORK+3,X'07'                                                     
         CLC   PESTREC(12),WORK                                                 
         BE    BILL50                HAVE ESTIMATE NAME                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(12),PBILLREC                                                 
         MVI   KEY+3,X'07'         ESTIMATE                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(18),PBILLREC                                                 
         GOTO1 HIGH                  RESTORE SEQ                                
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
BILL50   MVC   PCOL1ENM,PESTNAME     1ST 12 CHAR'S OF ESTIMATE NAME             
         GOTO1 DATCON,DMCB,(3,PBILINVD),(20,WORK)                               
         MVC   PCOL1IDT(4),WORK+4    INVOICE DATE (MMDD)                        
         MVC   PCOL1IDT+4(4),WORK    INVOICE DATE (YYYY)                        
         MVC   PCOL1PT,=C'0030'      PAYMENT TERMS ("0030" FOR Y&R)             
*                                    WAS 0025 UNTIL APR23/01                    
*                                    CHANGED VIA REQUEST FROM JEANNE C.         
         GOTO1 DATCON,DMCB,(3,PBILDUED),(20,WORK)                               
         MVC   PCOL1DD(4),WORK+4        DUE DATE (MMDD)                         
         MVC   PCOL1DD+4(4),WORK        DUE DATE (YYYY)                         
*                                                                               
         ZAP   DUB,PBILLRCV                                                     
         MVI   PCOL1SN,C'-'        AMOUNT DUE SIGN (CREDIT)                     
         MVI   PCOL1IC,C'C'        "C" FOR CREDIT (-)                           
         MVC   PCOL1DT,=C'KG'      "KG" FOR CREDIT (-)                          
         CP    DUB,=P'0'           AMOUNT DUE NEGATIVE ?                        
         BL    BILL54              YES                                          
         MVI   PCOL1SN,C'+'        NO - AMOUNT DUE SIGN (POSITIVE)              
         MVI   PCOL1IC,C'I'        "I" FOR DEBIT (+)                            
         MVC   PCOL1DT,=C'RN'      "RN" FOR DEBIT (+)                           
BILL54   MVI   PCOL1SN,C'+'        ** ALWAYS "+" PER Y&R (6/3/98)               
         MVC   PCOL1AD,ZEROS       ZERO-FILL 18-BYTE FIELD                      
         UNPK  PCOL1AD+2(16),DUB         AMOUNT DUE                             
         OI    PCOL1AD+17,X'F0'                                                 
*                                                                               
         CLI   PCOL1IC,C'C'        CREDIT (-) ?                                 
         BE    *+10                YES - DO NOT ADD TO TOTALS                   
*                                                                               
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) SUM                 
*                                                                               
         MVC   PCOL1CC(3),=C'USD'    CURRENCY CODE                              
         MVC   PCOL1LI,=C'US01'      LEDGER ID                                  
         MVC   PCOL1EN(3),=C'101'    ENTITY ("101" FOR Y&R)                     
         MVI   PCOL1RT,C'1'            RECORD TYPE 1                            
*                                                                               
************************   BELOW FOR TESTING ONLY ****************              
         CLI   SVOPT4,C'T'                                                      
         BNE   BILL60                                                           
         MVC   PCOL1DT+05(11),=C'* SM TEST *'                                   
         MVC   PCOL1DT+17(03),PBILLKEY                                          
         MVC   PCOL1DT+21(06),PBILKCLT                                          
         GOTO1 HEXOUT,DMCB,PBILKEST,PCOL1DT+29,8,0                              
         MVC   PCOL1DT+47(06),PBILLDAT                                          
         MVC   PCOL1DT+56(08),=C'* END **'                                      
************************   ABOVE FOR TESTING ONLY ****************              
*                                       SET KEY                                 
BILL60   MVC   SORTVID,PCOL1VID         VENDOR ID                               
         MVC   SORTINV,PCOL1INV         INVOICE NUMBER                          
         MVC   SORTRT,PCOL1RT           RECORD TYPE                             
         MVC   SOUTREC,OUTREC           RECORD DATA                             
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         AP    INVCNT,=P'1'                                                     
         CLI   SVOPT2,C'B'         PRINT INPUT TO SORT RECORDS ?                
         BNE   BILL05              NO - NEXT BILL RECORD                        
         BAS   RE,WRITE                                                         
         B     BILL05              NEXT BILL RECORD                             
         EJECT                                                                  
*                                                                               
BUYPROC  DS    0H          OUTPUT A REC FOR EACH DATED BILLING ELEM             
*                          OF ALL RELEVANT BUY RECORDS FOR CLIENT               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING PBUYKEY,R5                                                       
         MVC   PBUYKAGY(3),PCLTKAGY   AGENCY/MEDIA                              
         MVI   PBUYKRCD,X'20'         RECORD CODE                               
         MVC   PBUYKCLT,PCLTKCLT      CLIENT CODE                               
         CLI   ONEPRD,C'Y'      ONE PRODUCT REQUEST?                            
         BNE   *+10                                                             
         MVC   PBUYKPRD,QPRODUCT                                                
*                                                                               
         GOTO1 HIGH                                                             
         B     PROC04                                                           
*                                                                               
PROC02   GOTO1 SEQ                                                              
*                                                                               
PROC04   CLC   KEY(7),KEYSAVE      SAME AGY/MED/RCD/CLIENT ?                    
         BNE   EXIT                NO - DONE WITH THESE BUYS                    
*                                                                               
         CLI   ONEPRD,C'Y'         ONE PRODUCT REQUEST?                         
         BNE   PROC04C                                                          
         CLC   PBUYKPRD,QPRODUCT                                                
         BNE   EXIT                                                             
*                                                                               
PROC04C  DS    0H                                                               
         CLC   PBUYKACT,=C'ZZZ'    PASSIVE ?                                    
         BE    PROC02              YES - SKIP                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT              READ BUY RECORD                              
*                                                                               
         AP    INSCNT,=P'1'        NUMBER OF BUY RECORDS LOOKED THRU            
         LA    R2,PBUYREC+33                                                    
PROC10   MVI   ELCODE,X'26'        BILL ELEMENT (PBILELEM)                      
         BAS   RE,NEXTEL                                                        
         BNE   PROC02              NEXT BUY RECORD                              
*                                                                               
         USING PPDUMD02,R2                                                      
         OC    PBLDATE,PBLDATE     SEE IF BILLED                                
         BZ    PROC10              NO - TRY FOR ANOTHER                         
         CLC   PBLDATE,BQS         TEST AGAINST BILL DATE                       
         BL    PROC10                                                           
         CLC   PBLDATE,BQE         TEST AGAINST BILL DATE                       
         BH    PROC10                                                           
*                                                                               
PROC20   DS    0H                                                               
         ST    R2,SVBILEL          SAVE ADDRESS OF PBILELEM                     
         MVI   OUTREC,C' '         SPACE-FILL RECORD                            
         MVC   OUTREC+1(L'OUTREC-1),OUTREC                                      
         LA    R4,OUTREC                                                        
         USING PCOL2D,R4           INVOICE DETAIL RECORD                        
         MVC   PCOL2VID,=C'1252071'    VENDOR ID (Y&R)                          
         CLC   QCLIENT,=C'CP '                                                  
         BE    PROC25                                                           
         CLC   QCLIENT,=C'CAA'                                                  
         BE    PROC25                                                           
         MVC   PCOL2VID,=C'2507029'    VENDOR FOR YNTDE                         
         CLC   QCLIENT,=C'COP'                                                  
         BE    *+6                                                              
         DC    H'0'              INVALID CLIENT CODE                            
*                                                                               
*                                                                               
PROC25   GOTO1 DATCON,DMCB,(3,PBLDATE),(0,WORK)   YMD TO YYMMDD FORMAT          
         GOTO1 =V(PPFMTINO),DMCB,WORK,(2,PBINVNO),                     X        
               (PBUYKMED,B1PROF),B1XPROF                                        
         L     RF,DMCB                                                          
         MVC   PCOL2INV,0(RF)        FULL INVOICE NUMBER                        
*                                                                               
         MVC   PCOL2SBA,=C'101'      SAP BUSINESS AREA                          
         MVI   PCOL2DH1,C'-'                                                    
*NOP*    MVI   PCOL2DH2,C'-'                                                    
*                                    SAP GL ACCOUNT                             
*                                                                               
         MVC   WORK(10),PBUYREC      MUST READ ESTIMATE FOR UDEF INFO           
         MVI   WORK+3,X'07'                                                     
         MVC   WORK+10(2),PBUYKEST                                              
         CLC   PESTREC(12),WORK                                                 
         BE    PROC30              ALREADY HAVE ESTIMATE REC                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(12),WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PBUYREC                                                  
         GOTO1 HIGH                  RESTORE BUY SEQ                            
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
PROC30   LA    R2,PESTREC+33                                                    
         MVI   ELCODE,X'08'        ESTIMATE USER DESCRIPTION ELEMENT            
         BAS   RE,NEXTEL                                                        
         BNE   PROC40              NOT FOUND                                    
         MVC   PCOL2SGA,2(R2)      1ST 6 BYTES OF PEUSER1                       
         MVC   PCOL2OMK,34(R2)     1ST 2 BYTES OF PEUSER2                       
*    PCOL2OMK MAY BE OVERWRITTEN BY PCOL2PU IN DISTRIBUTION DESC. BELOW         
PROC40   DS    0H                  SAP COST CENTER                              
         L     R2,SVBILEL          RESTORE ADDRESS OF PBILELEM                  
*                                                                               
         MVC   WORK(10),PBUYREC      MUST READ PRODUCT FOR UDEF INFO            
         MVI   WORK+3,X'06'                                                     
         CLC   PPRDREC(10),WORK                                                 
         BE    PROC50                ALREADY HAVE PRODUCT REC                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PBUYREC                                                  
         GOTO1 HIGH                  RESTORE BUY SEQ                            
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
PROC50   LA    R2,PPRDREC+33                                                    
         MVI   ELCODE,X'08'        PRODUCT USER DESCRIPTION ELEMENT             
         BAS   RE,NEXTEL                                                        
         BNE   PROC52              NOT FOUND - LEAVE COST CENTER BLANK          
*                                                                               
         CLI   10(R2),C' '         9TH POS'N OF USER FIELD GT SPACE ?           
         BNH   PROC51              NO - ONLY MOVE FIRST SEVEN                   
         MVC   PCOL2SCC,2(R2)      1ST 9 BYTES OF PUSER1                        
         B     PROC52                                                           
PROC51   MVC   PCOL2SCC(7),2(R2)   1ST 7 BYTES OF PUSER1                        
*                                                                               
PROC52   CLC   PCOL2SGA,=C'111052'  IF ACCOUNT IS "111052"                      
         BNE   *+10                                                             
         MVC   PCOL2SCC,ZEROS      MAKE COST CENTER "ALL ZEROS"                 
*                                                                               
         MVC   PCOL2SCM,=C'US01'   SAP COMP CODE ("US01" FOR Y&R)               
*                            DESCRIPTION                                        
         L     R2,SVBILEL          RESTORE ADDRESS OF PBILELEM                  
         ZICM  R0,PBUYKEST,2                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         ZAP   FULL,DUB                                                         
         EDIT  (P4,FULL),(3,PCOL2DES),0,FILL=0                                  
         GOTO1 DATCON,DMCB,(3,PBDBDATE),(20,WORK)                               
         MVC   PCOL2DES+4(2),WORK+4   MM (MONTH)                                
         MVI   PCOL2DES+6,C'/'                                                  
         MVC   PCOL2DES+7(4),WORK     YYYY (YEAR)                               
*                                                                               
*                             DISTRIBUTION LINE                                 
         MVC   FULL,PBGROSS        GROSS AMOUNT BILLED                          
         L     R0,FULL                                                          
         MVC   FULL,PBAGYCOM       AGENCY COMM BILLED                           
         S     R0,FULL                                                          
         MVC   FULL,PBCSHDSC       CASH DISCOUNT BILLED                         
         S     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  PCOL2AMT,DUB        (BILLED AMOUNT - NET FOR Y&R)                
         MVI   PCOL2SN,C'-'          SIGN OF PCOL2AMT (NEGATIVE)                
         CP    DUB,=P'0'           PCOL2AMT NEGATIVE ?                          
         BL    *+8                 YES                                          
         MVI   PCOL2SN,C'+'        NO - SIGN OF PCOL2AMT POSITIVE               
         OI    PCOL2AMT+12,X'F0'                                                
         AP    DTLTOT,DUB          ADD TO DETAIL SUM                            
*                                                                               
         MVC   PCOL2INI,=X'E850D9'   SOURCE INITIALS ("Y&R" FOR Y&R)            
         CLC   QCLIENT,=C'CP '                                                  
         BE    PROC54                                                           
         CLC   QCLIENT,=C'CAA'                                                  
         BE    PROC54                                                           
         MVC   PCOL2INI,=C'TDE'         OPERATOR ID FOR TDE                     
         CLC   QCLIENT,=C'COP'                                                  
         BE    *+6                                                              
         DC    H'0'              INVALID CLIENT CODE                            
*                                                                               
*                                  DISTRIBUTION DESCRIPTION FIELDS              
PROC54   GOTO1 DATCON,DMCB,(3,PBUYKDAT),(20,WORK)                               
         MVC   PCOL2IDT(4),WORK+4    INSERTION DATE (MMDD)                      
         MVC   PCOL2IDT+4(4),WORK    INSERTION DATE (YYYY)                      
*                                                                               
         MVC   WORK(1),PBUYKMED      MUST READ PUBREC FOR DESC INFO             
         MVC   WORK+1(6),PBUYKPUB                                               
         MVC   WORK+7(2),PBUYKAGY                                               
         MVI   WORK+09,X'81'                                                    
         CLC   PUBREC(10),WORK                                                  
         BE    PROC60                ALREADY HAVE PUB RECORD                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),WORK                                                     
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         GOTO1 GETNAME                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PBUYREC                                                  
         GOTO1 HIGH                  RESTORE BUY SEQ                            
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
PROC60   DS    0H                                                               
         CLI   PBUYKMED,C'O'       OUTDOOR MEDIA ?                              
         BE    PROC70              YES - DIFFERENT DESCRIPTION FIELDS           
         MVC   PCOL2PUB(20),PUBNAME  PUB NAME                                   
*                    REPLACE TRAILING "NULLS" IN PUBNAME WITH SPACES            
         LA    R0,19               LOOP COUNTER                                 
         LA    RE,PCOL2PUB+19      END OF FIELD                                 
PROC60C  CLI   0(RE),X'39'                                                      
         BH    PROC65              DONE                                         
         MVI   0(RE),C' '                                                       
         BCTR  RE,0                MOVE TO "LEFT"                               
         BCT   R0,PROC60C                                                       
*                                                                               
PROC65   MVC   PCOL2PU,PBDSPACE      SPACE DESCRIPTION                          
*                    REPLACE TRAILING "NULLS" IN PBDSPACE WITH SPACES           
         LA    R0,10               LOOP COUNTER                                 
         LA    RE,PCOL2PU+9        END OF FIELD                                 
PROC65C  CLI   0(RE),X'39'                                                      
         BH    PROC100             DONE                                         
         MVI   0(RE),C' '                                                       
         BCTR  RE,0                MOVE TO "LEFT"                               
         BCT   R0,PROC65C                                                       
         B     PROC100             DONE WITH THIS PBILELEM                      
*                                                                               
PROC70   DS    0H                  OUTDOOR DISTRIBUTION DESC FIELDS             
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,PCOL2OPB          PUB CODE                 
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'66'        COMMENT ELEMENT (PCOMELEM)                   
         BAS   RE,NEXTEL                                                        
         BNE   PROC100             NO ELEMENT FOUND                             
*                                                                               
         LA    R1,16               MAXIMUM OUTPUT LENGTH                        
         CLI   1(R2),18            COMMENT "DATA" GT 16 ?                       
         BH    PROC76              YES - MOVE MAX 16                            
         ZIC   R1,1(R2)                                                         
         SH    R1,=H'2'            DROP ELEM CODE AND LENGTH                    
PROC76   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PCOL2OVH(0),2(R2)   1ST 16 BYTES OF COMMENT (MAXIMUM)            
*                                                                               
PROC100  DS    0H                                                               
         MVI   PCOL2RT,C'2'            RECORD TYPE 2                            
************************   BELOW FOR TESTING ONLY ****************              
         CLI   SVOPT4,C'T'                                                      
         BNE   PROC110                                                          
         MVC   PCOL2OMK+48(11),=C'* SM TEST *'                                  
         MVC   PCOL2OMK+60(03),PBUYKEY                                          
         MVC   PCOL2OMK+64(06),PBUYKCLT                                         
         GOTO1 HEXOUT,DMCB,PBUYKPUB,PCOL2OMK+71,6,0                             
         GOTO1 HEXOUT,DMCB,PBUYKDAT,PCOL2OMK+84,3,0                             
         GOTO1 HEXOUT,DMCB,PBUYKEST,PCOL2OMK+91,2,0                             
         GOTO1 HEXOUT,DMCB,PBUYKLIN,PCOL2OMK+96,1,0                             
         MVC   PCOL2OMK+101(08),=C'* END **'                                    
************************   ABOVE FOR TESTING ONLY ****************              
*                                     SET KEY                                   
PROC110  MVC   SORTVID,PCOL2VID         VENDOR ID                               
         MVC   SORTINV,PCOL2INV         INVOICE NUMBER                          
         MVC   SORTRT,PCOL2RT           RECORD TYPE                             
         MVC   SOUTREC,OUTREC           RECORD DATA                             
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         AP    INVDCNT,=P'1'       NUMBER OF DETAIL RECS WRITTEN                
*                                                                               
         L     R2,SVBILEL          RESTORE ADDRESS OF PBILELEM                  
*                                                                               
         CLI   SVOPT2,C'B'         PRINT INPUT TO SORT RECORDS ?                
         BNE   PROC10              NO                                           
         BAS   RE,WRITE                                                         
         B     PROC10              LOOK FOR ANOTHER BILL ELEMENT                
*                                                                               
         EJECT                                                                  
FINAL    DS    0H                  OUTPUT CONTROL RECORD                        
         MVI   OUTREC,C' '         SPACE-FILL RECORD                            
         MVC   OUTREC+1(L'OUTREC-1),OUTREC                                      
         LA    R4,OUTREC                                                        
         USING PCOL9D,R4           CONTROL (TOTAL) RECORD                       
         MVI   PCOL9RID,C'9'       SET RECORD ID TO ALL 9'S                     
         MVC   PCOL9RID+1(L'PCOL9RID-1),PCOL9RID                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,WORK)          TODAY'S DATE                
         MVC   PCOL9CD(4),WORK+4    CREATION DATE (MMDD)                        
         MVC   PCOL9CD+4(4),WORK    CREATION DATE (YYYY)                        
*                                                                               
         MVC   PCOL9CT,=C'      '      CREATION TIME (HHMMSS)                   
*                                                                               
         MVC   PCOL9AMT,ZEROS          ZERO-FILL 19-BYTE FIELD                  
         UNPK  PCOL9AMT+3(16),BILTOT   TOTAL BILLING AMOUNT DUE                 
         OI    PCOL9AMT+18,X'F0'                                                
*NOP*    MVC   PCOL9DAM,ZEROS          ZERO-FILL 19-BYTE FIELD                  
*NOP*    UNPK  PCOL9DAM+3(16),DTLTOT   BILLING DETAIL SUM TOTAL                 
*NOP*    OI    PCOL9DAM+18,X'F0'                                                
*                                                                               
         MVI   PCOL9RT,C'9'            RECORD TYPE 9                            
*                                      SET KEY                                  
         MVC   SORTKEY,PCOL9RID         RECORD ID (ALL 9'S)                     
         MVC   SOUTREC,OUTREC           RECORD DATA                             
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         EJECT                                                                  
*                                      "FINAL" OUTPUT AND TOTALS                
         MVI   FORCEHED,C'Y'                                                    
         MVI   WRITESW,1           WRITE TO FILE                                
         MVI   RCSUBPRG,10                                                      
         DROP  R2,R4                                                            
*                                                                               
FINLUP   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R6,15,4(R1)                                                      
         BZ    TOTALS              NO MORE RECS - FINISH UP                     
         MVC   OUTREC,L'SORTKEY(R6)   MOVE SORTED DATA (AFTER KEY)              
         AP    SORTCNT,=P'1'       NUMBER OF SORT RECS READ                     
         CLI   OUTREC+255,C'1'     INVOICE HEADER ?                             
         BE    FLREC1              YES                                          
         CLI   OUTREC+255,C'2'     DETAIL BACK-UP ?                             
         BE    FLREC2              YES                                          
****                               MUST BE '9' REC - INVOICE SUMMARY            
         LA    R4,OUTREC                                                        
         USING PCOL9D,R4           CONTROL (TOTAL) RECORD                       
         ZAP   TOTCNT,INVCNT       INVOICE HEADER COUNT                         
         AP    TOTCNT,INVDCNT      ADD DETAIL RECORD COUNT                      
         UNPK  PCOL9TRC,TOTCNT     TOTAL RECORD COUNT                           
         OI    PCOL9TRC+9,X'F0'                                                 
         UNPK  PCOL9IRC,INVCNT     INVOICE HEADER COUNT                         
         OI    PCOL9IRC+9,X'F0'                                                 
         UNPK  PCOL9DRC,INVDCNT    DETAIL RECORD COUNT                          
         OI    PCOL9DRC+9,X'F0'                                                 
*                                                                               
         MVC   PCOL9DAM,ZEROS          ZERO-FILL 19-BYTE FIELD                  
         UNPK  PCOL9DAM+3(16),DTLTOT   BILLING DETAIL SUM TOTAL                 
         OI    PCOL9DAM+18,X'F0'                                                
*                                                                               
         MVI   WRITESW,1           TURN ON WRITE-RECORD SWITCH                  
         B     FINWRT              WRITE RECORD                                 
         DROP  R4                                                               
*                                                                               
FLREC1   DS    0H                                                               
         XC    TSTINVC,TSTINVC     CLEAR INVOICE-NUMBER-SAVE FIELD              
         LA    R4,OUTREC                                                        
         USING PCOL1D,R4                                                        
         CLC   PCOL1AD,ZEROS       ZERO INVOICE ?                               
         BE    FLREC1D             YES - SKIP                                   
         CLI   PCOL1IC,C'C'        CREDIT INVOICE ?                             
         BNE   FINWRT              NO - GO WRITE REC                            
FLREC1D  DS    0H                  SKIP RECORD                                  
         MVC   TSTINVC,PCOL1INV    SAVE INVOICE NUMBER                          
         SP    INVCNT,=P'1'        ADJUST COUNT                                 
         AP    ZEROINV,=P'1'                                                    
         B     FINLUP              SKIP RECORD (NO OUTPUT)                      
         DROP  R4                                                               
*                                                                               
FLREC2   DS    0H                                                               
         MVI   WRITESW,1           TURN ON WRITE-RECORD SWITCH                  
         OC    TSTINVC,TSTINVC     INVOICE-NUMBER-SAVE FIELD CLEAR ?            
         BZ    FINWRT              YES - WRITE RECORD                           
*                                                                               
         LA    R4,OUTREC           RECORD "SHOULD" BE BYPASSED AS               
         USING PCOL2D,R4             PART OF ZERO INVOICE BUT MAKE              
         CLC   TSTINVC,PCOL2INV      SURE SAME INVOICE NUMBER                   
         BNE   FINWRT              NOT SAME - WRITE RECORD                      
         SP    INVDCNT,=P'1'       ADJUST COUNT                                 
         AP    ZERODTL,=P'1'                                                    
*            NOW ADJUST DTLTOT (REVERSE DETAIL TOTALS PREVIOUSLY ADDED)         
         PACK  DUB,PCOL2AMT        PCOL2AMT ALWAYS POSITIVE                     
         CLI   PCOL2SN,C'-'        NEGATIVE VALUE ?                             
         BNE   FLREC2D             NO                                           
         AP    DTLTOT,DUB          ADJUST DETAIL TOTALS                         
         B     FINLUP              SKIP RECORD (NO OUTPUT)                      
FLREC2D  DS    0H                                                               
         SP    DTLTOT,DUB          ADJUST DETAIL TOTALS                         
         B     FINLUP              SKIP RECORD (NO OUTPUT)                      
*                                                                               
FINWRT   BAS   RE,WRITE            WRITE INTERFACE RECORD (MAYBE)               
         MVI   WRITESW,1           MAKE SURE WRITE-RECORD IS "ON"               
         B     FINLUP              GET NEXT SORTED RECORD                       
         DROP  R4                                                               
*                                                                               
TOTALS   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,90                                                      
         LA    R4,TITLES                                                        
         LA    R5,INSCNT                                                        
         LA    R6,6                FOR BCT                                      
TOT50    MVC   P+7(17),0(R4)                                                    
         EDIT  (P4,0(R5)),(9,P+26),0,COMMAS=YES                                 
         GOTO1 REPORT                                                           
         LA    R4,17(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R6,TOT50                                                         
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P+7(19),=C'TAPE RECORDS OUTPUT'                                  
         EDIT  OUTCNT,(9,P+26),0,COMMAS=YES                                     
         MVI   P+35,C'*'                                                        
         GOTO1 REPORT                                                           
*                                                                               
         CLI   SVOPT5,C'Y'         TEST RUN ?                                   
         BE    EXIT                YES - NO CLOSE                               
         CLI   SVOPT4,C'T'         KEY PRINTING TEST RUN ?                      
         BE    EXIT                YES - NO CLOSE                               
         CLOSE OUTFILE                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
         DC    F'0'                                                             
WRITE    ST    RE,WRITE-4                                                       
*****    CLI   SVOPT2,C'B'                                                      
*****    BE    WRIT10                                                           
*****    CLI   SVOPT2,C'P'                                                      
*****    BNE   WRIT20                                                           
*                                                                               
WRIT10   MVC   P(128),OUTREC                                                    
         MVC   PSECOND(128),OUTREC+128                                          
         GOTO1 REPORT                                                           
*                                                                               
WRIT20   DS    0H                                                               
         CLI   WRITESW,1           WRITE TO FILE NOW ?                          
         BNE   WRIT30              NO                                           
         CLI   SVOPT5,C'Y'         TEST RUN ?                                   
         BE    WRIT30              YES - NO WRITE                               
         CLI   SVOPT4,C'T'         KEY PRINTING TEST RUN ?                      
         BE    WRIT30              YES - NO WRITE                               
         LA    R1,OUTFILE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         AP    OUTCNT,=P'1'                                                     
WRIT30   L     RE,WRITE-4                                                       
         BR    RE                                                               
*                                                                               
TITLES   DS    0C                                                               
         DC    CL17'INSERTIONS TESTED'                                          
         DC    CL17'INS DTL WRITTEN'                                            
         DC    CL17'INVOICE WRITTEN'                                            
         DC    CL17'SORT RECS READ'                                             
         DC    CL17'INVOICE BYPASSED'                                           
         DC    CL17'DETAIL BYPASSED'                                            
         DC    CL17'TOTAL WRITTEN'                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,18,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=274'                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PPDYNDSN DC    CL20'PRTTAPE.PP0CLAG'                                            
*                                                                               
OUTFILE  DCB   DDNAME=OUTFILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=256,                                              X        
               BLKSIZE=256,                                            X        
               MACRF=PM                                                         
         EJECT                                                                  
PPCLWRKD DSECT                                                                  
INSCNT   DS    PL4'0'              BUYS "READ"                                  
INVDCNT  DS    PL4'0'              DETAIL RECORD COUNT                          
INVCNT   DS    PL4'0'              INVOICE HEADER COUNT                         
SORTCNT  DS    PL4'0'              RECORDS READ FROM SORTER                     
ZEROINV  DS    PL4'0'              ZERO OR NEGATIVE INVOICE HEADERS             
ZERODTL  DS    PL4'0'              DETAILS MAKING UP ZERO INVOICES              
OUTCNT   DS    PL4'0'              TOTAL INTERFACE RECORDS WRITTEN              
TOTCNT   DS    PL4'0'                                                           
*                                                                               
BILTOT   DS    D                   BILLING AMOUNT $ TOTAL                       
DTLTOT   DS    D                   SUM OF BILLING ON DETAIL RECS                
*                                                                               
ELCODE   DS    CL1                                                              
ONEPRD   DS    CL1                 SET TO Y IF DOING ONE PRODUCT                
SVBILEL  DS    F                                                                
SVQSTART DS    CL6                                                              
SVQEND   DS    CL6                                                              
BQS      DS    XL3                                                              
BQE      DS    XL3                                                              
ZEROS    DS    CL30                                                             
B1PROF   DS    XL16                                                             
B1XPROF  DS    XL16                                                             
TSTINVC  DS    CL10                INVOICE NUMBER FROM ZERO $ INVOICE           
SVOPT2   DS    CL1                                                              
SVOPT4   DS    CL1                                                              
SVOPT5   DS    CL1                                                              
WRITESW  DS    X                   1=WRITE RECORDS TO OUTPUT FILE               
FRSTSW   DS    X                   Y=FIRST TIME THROUGH HAS HAPPENED            
         DS    F                                                                
OUTREC   DS    CL256                                                            
         DS    0D                                                               
SORTREC  DS    0XL274                                                           
SORTKEY  DS    0XL18                                                            
SORTVID  DS    CL7                 VENDOR ID                                    
SORTINV  DS    CL10                INVOICE NO                                   
SORTRT   DS    CL1                 RECORD TYPE                                  
SOUTREC  DS    CL256               INTERFACE RECORD                             
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PPCOLINTFD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PPREPCL02 03/21/07'                                      
         END                                                                    
