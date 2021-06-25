*          DATA SET ACREPNB04  AT LEVEL 013 AS OF 04/29/14                      
*PHASE ACNB04C                                                                  
ACNB04   TITLE 'PRINT BILLS AND REPORTS'                                        
         PRINT NOGEN                                                            
ACNB04   CSECT                                                                  
         J     *+12                                                             
         DC    CL8'**NB04**'                                                    
         STM   RE,RF,12(RD)                                                     
         XR    RE,RE                                                            
         SLDL  RE,8                HOB OF R15 HAS ROUTINE NUMBER                
         SLL   RE,2                R14 = ROUTINE NUMBER * 4                     
         BASR  RF,0                                                             
         LA    RF,6(RF,RE)         R15 = A(JUMP INSTRUCTION)                    
         L     RE,12(RD)           RESTORE R14 FOR RETURN                       
         BR    RF                                                               
         J     ADDREP                                                           
         J     RUNREP                                                           
         J     TRCE                                                             
         J     IOCNTR                                                           
         EJECT                                                                  
***********************************************************************         
* ADD A SORT RECORD FOR A REPORT                                      *         
***********************************************************************         
         USING NBILD,R8                                                         
ADDREP   NMOD1 0,**ADRP,RA                                                      
         L     RC,BASERC           RESTORE RC                                   
         USING ACWORKD,RC                                                       
         STC   R1,BYTE                                                          
         SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     PRSM                PRODUCT SUMMARY                              
         B     CLSM                CLIENT SUMMARY                               
         B     GRPB                GROUP BILL                                   
         B     BLRG                BILL REGISTER                                
         B     MSRG                MEDIA SUMMARY REGISTER                       
         B     IIBS                INTERNAL INVOICE BILLING SUMMARY             
         B     TGRG                REGISTER OF TARGET ACCOUNST                  
         B     ICRG                INTERNAL INCOME REGISTER                     
         B     SIPR                STUDIO INTERCOMPANY POSTING REGISTER         
         B     ICXR                INTERCOMPANY POSTING EXCEPTION REPRT         
         B     POBR                PERCENT OF BILL REGISTER                     
         B     ASPR                ASP REGISTER                                 
         B     GBTR                GROUP BILL TRACE SUMMARY                     
         B     SNBJ                SUMMARY OF NON-BILLABLE JOBS                 
         EJECT                                                                  
***********************************************************************         
* ADD CLIENT /PRODUCT SUMMARIES                                       *         
***********************************************************************         
PRSM     L     R6,ADGOBLOC                                                      
         CLI   GOPROSUM-GOBLOCKD(R6),NO                                         
         JE    XIT                                                              
         B     SUMR                                                             
*                                                                               
CLSM     L     R6,ADGOBLOC                                                      
         CLI   GOCLISUM-GOBLOCKD(R6),NO                                         
         JE    XIT                                                              
*                                                                               
SUMR     L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRQN,REQNUM        REQUEST NUMBER                               
         MVI   SKRQRT,SKRQSMQ      SUMMARY                                      
         MVI   CPSKSUM,CPSKSUMQ                                                 
         MVC   SRREPTN,BYTE        REPORT NUMBER                                
*                                                                               
         TM    PGMAQ,PGMALL27      TEST 27                                      
         BZ    SUMR2               NO,                                          
         MVI   BILKLVL,X'FF'                                                    
         TM    RQLV,LVA+LVB                                                     
         BZ    *+8                                                              
         MVI   SKRQRT,SKRQGSQ      SUMMARY                                      
         MVI   CPSKGRP+6,X'FF'                                                  
         MVC   CPSKGRP(3),CLI                                                   
         MVI   CPSKGRP+3,X'FF'                                                  
         CLI   SRREPTN,CLISUM      TEST CLIENT SUMMARY                          
         BE    SUMR2                                                            
         MVC   CPSKGRP+3(3),PRD                                                 
*                                                                               
SUMR2    MVC   CPSKCLI,CLI         CLIENT                                       
         MVC   CPSKCLME,MEDIA       MEDIA                                       
         MVI   CPSKCMTO,ALL         TOTAL                                       
         MVC   CPSKPRD,PRD         PRODUCT                                      
         CLI   SRREPTN,CLISUM      TEST CLIENT SUMMARY                          
         BE    SUMR3                                                            
         MVI   CPSKCLME,0          CLEAR CLIENT LEVEL MEDIA                     
         MVI   CPSKCMTO,0          CLEAR CLIENT MEDIA TOTAL                     
         MVC   CPSKPRME,MEDIA      MEDIA                                        
*                                                                               
SUMR3    MVC   CPSKJOB,JOB         JOB                                          
         MVC   CPSKOTH,BILOTHR     OTHER NUMBER                                 
         MVC   CPSKBNUM,LNGBILNO   BILL NUMBER                                  
         CLC   CPSKBNUM(L'DRAFT),DRAFT                                          
         BNE   *+10                                                             
         MVC   CPSKBNUM+2(L'DRAFT),DRAFT                                        
         TM    CTAXOPT,CTAXOPST                                                 
         BNO   *+14                                                             
         LA    RF,CTAXPST          PROVINCE                                     
         MVC   CPSPRVD,CTXPRVD-CTXD(RF)                                         
*                                                                               
         LA    R5,AMTS                                                          
         USING AMTD,R5                                                          
         MVC   CPSBDTE,BILDT1      BILL DATE                                    
         MVC   CPSMED,MEDNS        MEDIA NAME                                   
         MVC   CPSCLIN,CLINAM      CLIENT NAME                                  
         MVC   CPSPRDN,PRDNAM      PRODUCT NAME                                 
         ZAP   CPSNET,DUENET       NET                                          
         ZAP   CPSCOM,DUECOM       COMMISSION                                   
         ZAP   CPSGRS,DUEGRS       GROSS                                        
         ZAP   CPSGST,DUEGST                                                    
         ZAP   CPSPST,DUEPST                                                    
         AP    CPSGRS,CPSGST                                                    
         AP    CPSGRS,CPSPST                                                    
*                                                                               
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         TM    JXSTAT3,JXS3PNET    PAY=NET ?                                    
         BNO   *+10                                                             
         SP    CPSGRS,CPSCOM                                                    
         TM    JXSTAT3,JXS3SUPC    SUPPRESS COMMISSION ?                        
         BNO   *+10                                                             
         ZAP   CPSCOM,PZERO                                                     
         DROP  R9                                                               
*                                                                               
         TM    OPT1,OPT1SNC        SUPPRESS NET & COMMISSION ?                  
         BNO   *+16                                                             
         ZAP   CPSNET,PZERO                                                     
         ZAP   CPSCOM,PZERO                                                     
         TM    OPT8,OPT8PAWC       PRINT AMOUNT WITH COMMAS                     
         BNO   *+8                                                              
         OI    SRSTAT,SRPWC                                                     
         LA    RF,SRLNQ+CPSDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD GROUP BILL RECORD                                               *         
***********************************************************************         
GRPB     L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         LA    R5,AMTS                                                          
         USING AMTD,R5                                                          
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRQN,REQNUM        REQUEST NUMBER                               
         MVI   SKRNRT,SKRQGBQ      GROUP BILL                                   
         MVC   SRREPTN,BYTE        REPORT NUMBER                                
         MVC   GBSKCLI,CLI         CLIENT                                       
         MVC   GBSKPRD,PRD         PRODUCT                                      
         MVC   GBSKJOB,JOB         JOB                                          
         MVI   GBSKPALL,X'FF'                                                   
         MVC   GBSKFMT,OPTGBF      FORMAT                                       
*                                                                               
         MVC   GBSBDTE,BILDT1      BILL DATE                                    
         TM    OPT3,OPT3SDD        SUPPRESS DUE DATE ?                          
         BO    *+10                YES,                                         
         MVC   GBSDDTE,DUEDT2      DUE DATE                                     
         MVC   GBSLBN,LNGBILNO     LONG BILL NUMBER                             
         MVC   GBSSBN,SHTBILNO     SHORT BILL NUMBER                            
         MVI   GBSADR1,C' '                                                     
         MVC   GBSADR1+1((5*L'GBSADR1)-1),GBSADR1                               
         ICM   RF,15,ADLVBADD      GET ADDRESS                                  
         BNZ   *+12                                                             
         ICM   RF,15,ADLVAADD                                                   
         BZ    GRPB2                                                            
         USING ADRELD,RF                                                        
         LLC   R0,ADRNUM                                                        
         LA    RE,GBSADR1                                                       
         LA    RF,ADRADD1                                                       
GRPB1    MVC   0(L'GBSADR1,RE),0(RF)                                            
         LA    RF,L'ADRADD1(RF)                                                 
         LA    RE,L'GBSADR1(RE)                                                 
         BCT   R0,GRPB1                                                         
         DROP  RF                                                               
*                                                                               
GRPB2    MVC   GBSMED,MEDNS        MEDIA NAME                                   
         MVC   GBSCLIN,CLINAM      CLIENT NAME                                  
         MVC   GBSPRDN,PRDNAM      PRODUCT NAME                                 
         MVC   GBSJOBN,JOBNAM      JOB NAME                                     
         L     RF,ADPROFIL                                                      
         USING PPRELD,RF                                                        
         MVC   GBSPONB,PPRBILLP    PRINT ON BILLS                               
         OC    GBSPONB,SPACES                                                   
         DROP  RF                                                               
*                                                                               
         LA    R0,GBSBKN           CLEAR BUCKETS                                
         LA    R1,GBSBK                                                         
         ZAP   0(L'BK$,R1),PZERO                                                
         LA    R1,L'BK$(R1)                                                     
         BCT   R0,*-10                                                          
*                                                                               
         TM    JXBLTYP,ONEL+SPCL   ONE LINE OR SPECIAL                          
         BZ    GRPB3                                                            
         MVC   GBSWRKN,SPACES      JUST TOTAL                                   
         MVC   GBSBLNME(L'JXBLNME),JXBLNME                                      
         ZAP   GBSDUE,DUEGRS       DUE GROSS                                    
*                                                                               
         LA    RF,SRLNQ+GBSDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
*                                                                               
GRPB3    DS    0H                                                               
         L     R0,NBTRN            R0=NUMBER OF TRANSACTIONS IN TABLE           
         L     R2,ABTRN            R3=A(TRANSACTION DETAIL)                     
*                                                                               
         USING BTRND,R2                                                         
GRPB5    MVC   GBSWRKN,SPACES                                                   
         CLC   BTRNWC,WCBILLQ                                                   
         BNE   GRPB6                                                            
         TM    JXBLTYP,PROG        PROGRESSIVE                                  
         BO    GRPB9               YES, NO PREVIOUS                             
         MVC   GBSKWKC,BTRNWC                                                   
         MVC   GBSWRKN(16),=C'PREVIOUS BILLING'                                 
         B     GRPB7                                                            
*                                                                               
GRPB6    CLC   BTRNWC,SPACES                                                    
         BE    GRPB7                                                            
         MVC   WRKCCUR,BTRNWC      GET WORK CODE VALUES                         
         GOTOR AWRKM,WRKMGTQ                                                    
         MVC   GBSKWKC,WRKCWC                                                   
         MVC   GBSWRKN(L'WRKCDSC),WRKCDSC    W/C NAME                           
         TM    OPT3,OPT3PWCL                                                    
         BNO   *+10                                                             
         MVC   GBSWRKN(L'WRKCNME),WRKCNME     W/C LONG NAME                     
*                                                                               
GRPB7    LA    R4,BTRNBK                                                        
         USING JOBD,R4                                                          
         ZAP   GBSNET,JOBNET                                                    
         ZAP   GBSCOM,JOBCOM                                                    
         ZAP   GBSCSD,JOBCD                                                     
*                                                                               
         ZAP   GBSDUE,GBSNET                                                    
         AP    GBSDUE,GBSCOM                                                    
         CLC   GBSKWKC,WCBILLQ                                                  
         BNE   *+10                                                             
         MP    GBSDUE,=P'-1'                                                    
*                                                                               
         LA    RF,SRLNQ+GBSDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
*                                                                               
GRPB9    LA    R2,BTRNLQ(R2)       NEXT TRANSACTION                             
         BCT   R0,GRPB5                                                         
*                                                                               
         CP    DUECSD,PZERO                                                     
         JE    XIT                                                              
*                                                                               
         MVC   GBSKWKC,=X'FAFA'    ADD TOTAL FOR CD                             
         MVC   GBSWRKN,SPACES                                                   
         ZAP   GBSNET,PZERO                                                     
         ZAP   GBSCOM,PZERO                                                     
         ZAP   GBSCSD,PZERO                                                     
         ZAP   GBSDUE,DUECSD                                                    
         MP    GBSDUE,PNEG1                                                     
         LA    RF,SRLNQ+GBSDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R2,R3,R5,R9                                                      
         EJECT                                                                  
***********************************************************************         
* ADD BILLING REGISTER RECORD                                         *         
***********************************************************************         
BLRG     L     R3,ASRTWK           ADD JOB EXCEPTION RECORD                     
         USING SORTD,R3                                                         
         LA    R5,AMTS                                                          
         USING AMTD,R5                                                          
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)  RUN REPORT                                   
         MVI   SKRNRT,SKRNBRQ      BILLING REGISTER                             
         LH    R1,BLREGCNT                                                      
         AHI   R1,1                                                             
         STH   R1,BLREGCNT                                                      
         MVC   BLRKSEQ,BLREGCNT    SEQUENCE                                     
         MVC   BLRKCLI,CLI         CLIENT                                       
         MVC   BLRKMED,MEDIA       MEDIA                                        
         MVC   BLRKPRD,PRD         PRODUCT                                      
         MVC   BLRKJOB,JOB         JOB                                          
         MVC   BLRPRVD,=C'HST'                                                  
*                                                                               
         MVC   BLRBDTE,BILDT1      BILL DATE                                    
         MVC   BLRBNUM,LNGBILNO    BILL NUMBER                                  
         CLC   BLRBNUM(L'DRAFT),DRAFT                                           
         BNE   *+10                                                             
         MVC   BLRBNUM+2(L'DRAFT),DRAFT                                         
         ZAP   BLRACT,DUERCV       ACTUAL                                       
         ZAP   BLRNET,DUENET       NET                                          
         ZAP   BLRCOM,DUECOM       COMMISSION                                   
         ZAP   BLRGRS,DUERCV       GROSS                                        
         ZAP   BLRCSD,DUECSD       CD                                           
         ZAP   BLRGST,DUEGST       GST                                          
         ZAP   BLRPST,DUEPST       PST                                          
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         TM    JXSTAT3,JXS3PNET    PAY=NET ?                                    
         BNO   *+16                                                             
         SP    BLRGRS,BLRCOM                                                    
         SP    BLRACT,BLRCOM                                                    
         TM    JXSTAT3,JXS3SUPC    SUPPRESS COMMISSION ?                        
         BNO   *+10                                                             
         ZAP   BLRCOM,PZERO                                                     
         DROP  R9                                                               
*                                                                               
         LA    RF,SRLNQ+BLRDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD MEDIA SUMMARY RECORDS                                           *         
***********************************************************************         
MSRG     L     R4,ASRTWK           ADD A RECORD FOR EACH AT RUN FIRST           
         USING SORTD,R4                                                         
         LA    R5,AMTS                                                          
         USING AMTD,R5                                                          
         CLI   MODE,RUNFRST                                                     
         BNE   MSRG5                                                            
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)  RUN REPORT                                   
         MVI   SKRNRT,SKRNMSQ      MEDIA SUMMARY                                
         ZAP   MSRACT,PZERO                                                     
         ZAP   MSRNET,PZERO                                                     
         ZAP   MSRCOM,PZERO                                                     
         ZAP   MSRGRS,PZERO                                                     
         ZAP   MSRCSD,PZERO                                                     
         ZAP   MSRGST,PZERO                                                     
         ZAP   MSRPST,PZERO                                                     
         LA    RF,SRLNQ+MSRDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
*                                                                               
         L     R3,ADCOMP                                                        
         MVI   ELCODE,PMDELQ       GET MEDIA ELEMENTS                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
MSRG3    BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         USING PMDELD,R3                                                        
         MVC   MSRKCDE,PMDCODE     MEDIA CODE                                   
         MVC   MSRNAME,PMDDESC     MEDIA NAME                                   
         GOTO1 ASORT,SORTPUTQ                                                   
         J     MSRG3                                                            
         DROP  R3                                                               
*                                                                               
MSRG5    XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)  RUN REPORT                                   
         MVI   SKRNRT,SKRNMSQ      MEDIA SUMMARY                                
         MVC   MSRKCDE,MEDIA       MEDIA CODE                                   
         MVC   MSRNAME,MEDNS       MEDIA NAME                                   
         ZAP   MSRACT,DUERCV       ACTUAL                                       
         ZAP   MSRNET,DUENET       NET                                          
         ZAP   MSRCOM,DUECOM       COMMISSION                                   
         ZAP   MSRGRS,DUERCV       GROSS                                        
         ZAP   MSRCSD,DUECSD       CD                                           
         ZAP   MSRGST,DUEGST       GST                                          
         ZAP   MSRPST,DUEPST       PST                                          
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         TM    JXSTAT3,JXS3PNET    PAY=NET ?                                    
         BNO   *+16                                                             
         SP    MSRACT,MSRCOM                                                    
         SP    MSRGRS,MSRCOM                                                    
         TM    JXSTAT3,JXS3SUPC    SUPPRESS COMMISSION ?                        
         BNO   *+10                                                             
         ZAP   MSRCOM,PZERO                                                     
         DROP  R9                                                               
*                                                                               
         LA    RF,SRLNQ+MSRDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD INTERNAL INCOME BILLING SUMMARY (SK REVERSALS)                  *         
***********************************************************************         
IIBS     TM    OPT4,OPT4SINT       EXCLUDE THIS REGISTER?                       
         JO    XIT                 YES                                          
         L     R4,CBTRN                                                         
         USING BTRND,R4                                                         
         L     R3,ASRTWK            ADD JOB EXCEPTION RECORD                    
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)  RUN REPORT                                   
         MVI   SKRNRT,SKRNIIQ      INTERNAL INCOME REPORT                       
*                                                                               
         MVC   INIKJOB,CPJ         JOB                                          
         MVC   INIKWC,BTRNWC       WC                                           
         MVC   INIACT,ANLINA+3     INCOME ACCOUNT                               
         MVC   INIKOTH,BILOTHR     OTHER NUMBER                                 
         MVC   INIKNUM,LNGBILNO    BILL NUMBER                                  
         CLC   INIKNUM(L'DRAFT),DRAFT                                           
         BNE   *+10                                                             
         MVC   INIKNUM+2(L'DRAFT),DRAFT                                         
*                                                                               
         MVC   INIBDTE,BTRNDTE     DATE                                         
         MVC   INIREF,BTRNREF      REFERENCE                                    
         ZAP   INIAMT,BTRNBK+(JOBNET-JOBD)(L'JOBNET)                            
*                                                                               
         LA    RF,SRLNQ+INIDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ADD REGISTER OF TARGET ACCOUNTS                                     *         
***********************************************************************         
TGRG     TM    OPT4,OPT4PTAR       TEST REGISTER WANTED                         
         JNO   XIT                                                              
         L     R3,ASRTWK           ADD JOB EXCEPTION RECORD                     
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)  RUN REPORT                                   
         MVI   SKRNRT,SKRNTGQ      TARGET REGISTER                              
*                                                                               
         MVC   RTACLI,CLI          CLIENT                                       
         MVC   RTAPRD,PRD          PRODUCT                                      
         MVC   RTAJOB,JOB          JOB                                          
*                                                                               
         MVC   RTABDTE,BILDT1      DATE                                         
         MVC   RTABNUM,LNGBILNO    BILL NUMBER                                  
         CLC   RTABNUM(L'DRAFT),DRAFT                                           
         BNE   *+10                                                             
         MVC   RTABNUM+2(L'DRAFT),DRAFT                                         
         MVC   RTARCV,RECVAC+3     RECEIVABLE ACCOUNT                           
         MVC   RTSCST,COSTAC+3     COSTING ACCOUNT                              
         ICM   RF,15,CRTLR         TEST RETAILER ENTRY                          
         BZ    TGRG3                                                            
         USING RTLD,RF                                                          
         CLI   RTLRECV,C' '                                                     
         BNH   *+10                                                             
         MVC   RTARCV,RTLRECV+3   RECEIVABLE ACCOUNT                            
         CLI   RTLCOST,C' '                                                     
         BNH   *+10                                                             
         MVC   RTSCST,RTLCOST+3   COSTING ACCOUNT                               
         DROP  RF                                                               
*                                                                               
TGRG3    MVC   RTASLS,PRDK+3       PRODUCT KEY                                  
         CLI   SALEAC+3,C' '                                                    
         BNH   *+10                                                             
         MVC   RTASLS,SALEAC+3     SALES ACCOUNT                                
*                                                                               
         LA    RF,SRLNQ+RTSDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ADD INTERNAL INCOME REGISTER                                        *         
***********************************************************************         
ICRG     TM    JFLG,JFPWOI        PREVIOUS BILLS W/O INCOME ELEMENTS            
         JO    XIT                                                              
         L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)  RUN REPORT                                   
         MVI   SKRNRT,SKRNICQ      INTERNAL INCOME REPORT                       
*                                                                               
         MVC   INCKCLI,CLI         CLIENT                                       
         MVC   INCKPRD,PRD         PRODUCT                                      
         MVC   INCKJOB,JOB         JOB                                          
         MVC   INCKNUM,LNGBILNO    BILL NUMBER                                  
         CLC   INCKNUM(L'DRAFT),DRAFT                                           
         BNE   *+10                                                             
         MVC   INCKNUM+2(L'DRAFT),DRAFT                                         
         L     R4,AINTLB                                                        
         USING INTLD,R4                                                         
*                                                                               
ICRG3    CLI   INTLD,EOT                                                        
         JE    XIT                                                              
         MVC   INCKWC,INTLWKC      WC                                           
         MVC   INCCRAC,INTLCRAC    CREDIT ACCOUNT                               
         MVC   INCDRAC,INTLDRAC    DEBIT ACCOUNT                                
         ZAP   INCINC,INTLAMNT     INCOME = CURRENT                             
         ZAP   INCPOST,INTLINAM    POSTING = PREVIOUS                           
*                                                                               
         LA    RF,SRLNQ+INCDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         LA    R4,INTLLNQ(R4)                                                   
         B     ICRG3                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ADD STUDIO INTERCOMPANY POSTING REGISTER                            *         
***********************************************************************         
SIPR     L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)                                               
         MVI   SKRNRT,SKRNSPQ                                                   
*                                                                               
         MVC   SIPKSTUD,AGYSTUD    STUDIO                                       
         MVC   SIPKMED,MEDIA       MEDIA                                        
         MVC   SIPKCLI,CLI         CLIENT                                       
         MVC   SIPKPRD,PRD         PRODUCT                                      
         MVC   SIPKJOB,JOB         JOB                                          
*                                                                               
         MVC   SIPBDTE,BILDT1      DATE                                         
         MVC   SIPBNUM,LNGBILNO    BILL NUMBER                                  
         CLC   SIPBNUM(L'DRAFT),DRAFT                                           
         BNE   *+10                                                             
         MVC   SIPBNUM+2(L'DRAFT),DRAFT                                         
         MVC   SIPAGJOB,AGYLCPJ    AGENCY JOB                                   
         MVC   SIPCRAC,AGYICCR+1   CREDIT ACCOUNT                               
         ZAP   SIPAMT,AGYAMNT      AMOUNT                                       
*                                                                               
         LA    RF,SRLNQ+SIPDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ADD STUDIO INTERCOMPANY POSTING EXCEPTION REPORT                    *         
***********************************************************************         
ICXR     TM    AGYSTAT,AGYSXJOB    IS AGENCY JOB  AN X JOB ?                    
         JNO   XIT                                                              
         MVI   AGYIPERR,AGYIPXJO   NO INTERCOMPANY FOR X-JOB                    
*                                                                               
         L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)                                               
         MVI   SKRNRT,SKRNPXQ                                                   
*                                                                               
         MVC   ICXKSTUD,AGYSTUD    STUDIO                                       
         MVC   ICXKAGYJ,AGYLCPJ    CLIENT/PROD/JOB                              
         MVC   ICXKCLI,CLI         CLIENT                                       
         MVC   ICXKPRD,PRD         PRODUCT                                      
         MVC   ICXKJOB,JOB         JOB                                          
*                                                                               
         MVC   ICXBDTE,BILDT1      DATE                                         
         MVC   ICXBNUM,LNGBILNO    BILL NUMBER                                  
         CLC   ICXBNUM(L'DRAFT),DRAFT                                           
         BNE   *+10                                                             
         MVC   ICXBNUM+2(L'DRAFT),DRAFT                                         
         MVC   ICXWARN,AGYIPERR    WARNING NUMBER                               
*                                                                               
         LA    RF,SRLNQ+ICXDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ADD PERCENT OF BILL RECORD                                          *         
***********************************************************************         
POBR     L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)                                               
         MVI   SKRNRT,SKRNPBQ                                                   
*                                                                               
         MVC   POBRKCLI,CLI        CLIENT                                       
         MVC   POBRKPRD,PRD        PRODUCT                                      
         MVC   POBRKJOB,JOB        JOB                                          
*                                                                               
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         MVC   POBRACC,POBACC+1    VENDOR ACCOUNT                               
         MVC   POBRWRK,POBWRK      WORKCODE                                     
         MVC   POBRNUM,SHTBILNO    BILL NUMBER                                  
         ZAP   POBRAMT,POBCUR      AMOUNT                                       
*                                                                               
         LA    RF,SRLNQ+POBRDLNQ                                                
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ASP BILL RECORD                                                 *         
***********************************************************************         
ASPR     L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)                                               
         MVI   SKRNRT,SKRNASQ                                                   
*                                                                               
         MVC   ASPRKCLI,CLI        CLIENT                                       
         MVC   ASPRKPRD,PRD        PRODUCT                                      
         MVC   ASPRKJOB,JOB        JOB                                          
*                                                                               
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         MVC   ASPRACC,ASPACC+1    INCOME ACCOUNT                               
         MVC   ASPRWRK,ASPWRK      WORKCODE                                     
         MVC   ASPRKNUM,SHTBILNO   BILL NUMBER                                  
         ZAP   ASPRAMT,ASPAMT      AMOUNT                                       
*                                                                               
         LA    RF,SRLNQ+ASPRDLNQ                                                
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ADD GROUP BILL TRACE SUMMARY                                        *         
***********************************************************************         
GBTR     L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         LA    R5,AMTS                                                          
         USING AMTD,R5                                                          
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)  RUN REPORT                                   
         MVI   SKRNRT,SKRNGTQ      GROUP BILL TRACE                             
*                                                                               
         MVC   SRREPTN,BYTE        REPORT NUMBER                                
         MVC   GBCKCLI,CLI         CLIENT                                       
         MVC   GBCKBDTE,BILDT1     BILL DATE                                    
         MVC   GBCKBNUM,BILNUM     BILL NUMBER                                  
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         BZ    GBTR3                                                            
         MVC   GBCKOBN,JXRRNUM     ORIGINAL BILL NUMBER                         
         MVC   GBCKOBD,JXRRDT1     AND DATE                                     
*                                                                               
GBTR3    MVC   GBCFORM,FORMCDE     FORM CODE                                    
         MVC   GBCLVL,FORMLVL      LEVEL                                        
         MVC   GBCFDTE,TTLSDAY2    FROM DATE                                    
         MVC   GBCTDTE,TTLEDAY2    TO DATE                                      
         MVI   GBCTYPE,C'C'        TYPE                                         
         MVC   GBCRDTE,TODAY3      RUN DATE                                     
         MVC   GBCDDTE,DUEDT1      DUE DATE                                     
         MVC   GBCRECA,RECVAC      RECEIVABLE ACCOUNT                           
         MVC   GBCROFF,OFC         RECEIVABLE OFFICE                            
         XC    GBCUBIL,GBCUBIL     UNBILL NUMBER                                
         XC    GBCUDTE,GBCUDTE     UNBILL DATE                                  
*                                                                               
         LA    R0,GBCBKN                                                        
         LA    RF,GBCBK                                                         
         ZAP   0(L'GBCBK,RF),PZERO                                              
         LA    RF,L'GBCBK(RF)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         ZAP   GBCPAY,LVGNET                                                    
         SP    GBCPAY,LVCCSD                                                    
         ZAP   GBCREC,LVGRCV                                                    
         ZAP   GBCINC,LVGCOM                                                    
         ZAP   GBCGRS,LVGGRS                                                    
         AP    GBCGRS,LVGTAX                                                    
         ZAP   GBCCSD,LVGCSD                                                    
         ZAP   GBCINT,LVGINT                                                    
         ZAP   GBCTIM,LVGPSF                                                    
         ZAP   GBCOOP,LVGOOP                                                    
         SP    GBCOOP,LVCCSD                                                    
*                                                                               
         LA    RF,SRLNQ+GBCDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3,R5,R9                                                         
         EJECT                                                                  
***********************************************************************         
* ADD SUMMARY OF NON-BILLABLE JOBS                                    *         
***********************************************************************         
SNBJ     OI    JFLG,JFNONB         SET  NONBILLABLE FLAG                        
         TM    REQOPT,REQNBJ       USER XIX?                                    
         JNZ   *+12                YES                                          
         TM    OPT4,OPT4PNON       NO, NON-BILLABLE REPORT WANTED?              
         JNO   XIT                 NO                                           
         L     R3,ASRTWK           ADD JOB EXCEPTION RECORD                     
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)  RUN REPORT                                   
         MVI   SKRNRT,SKRNNBQ      END EXCEPTION REPORT                         
         MVC   NBJKCLI(L'CPJX),CPJX  CLI/PROD/JOB                               
*                                                                               
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         MVC   NBJERR,JXERRS-JXBLK(R9) NON-BILLABLE REASONS                     
         MVC   NBJOTH,ERRNUM      CODES                                         
         CLI   ERRNUM,ERRACNF     TEST ACCOUNT NOT FOUND                        
         BE    *+12                                                             
         CLI   ERRNUM,ERRACNB     TEST ACCOUNT BALANCE ELEMENT                  
         BNE   SNBJ5                                                            
         MVC   NBJDESC(14),DKEY+1 SET KEY                                       
         DROP  R9                                                               
*                                                                               
SNBJ5    LA    RF,SRLNQ+NBJDLNQ                                                 
         STCM  RF,3,SRLEN                                                       
         GOTO1 ASORT,SORTPUTQ                                                   
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* STORAGE - CONSTANTS - LITERAL POOL                                  *         
***********************************************************************         
                                                                                
BLREGCNT DC    H'0'                BILL REGISTER COUNT                          
                                                                                
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* CONTROL PRINTING OF REPORTS                                         *         
***********************************************************************         
RUNREP   NMOD1 ,**RREP,RA                                                       
         L     RC,BASERC           RESTORE RC                                   
         L     R4,AIO1             USE IO1 TO SAVE LAST RECORD                  
         XC    0(SRLNQ,R4),0(R4)                                                
*                                                                               
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         TM    OPT3,OPT3PNH        PRINT NAME IN 'UPPER' ?                      
         BNO   *+10                                                             
         MVC   UPPER1+33(33),ORIGINAM                                           
         TM    OPT3,OPT3PAH        PRINT ADDRESS IN 'UPPER' ?                   
         BNO   *+10                                                             
         MVC   UPPER2+33(33),ORIGINAD                                           
         B     RUNREP3                                                          
         DROP  RF                                                               
*                                                                               
RUNREP1  GOTO1 ASORT,SORTGETQ      GET NEXT RECORD                              
RUNREP3  L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         MVC   RPTNUM,SKRNRT       SET REPORT NUMBER                            
         CLC   SKRUN,=AL2(SKRUNQ)  TEST END OF RUN                              
         BE    RUNREP5                                                          
         CLI   SRREPTN,0           TEST BILL SUB-REPORT                         
         BE    RUNREP4             NO                                           
         MVC   RPTNUM,SRREPTN      YES, SET REPORT NUMBER                       
         B     RUNREP5             YES, OK TO PROCESS                           
*                                                                               
RUNREP4  MVI   RPTNUM,X'FF'                                                     
*                                                                               
RUNREP5  CLI   RPTNUM,X'FF'        TEST LAST TIME                               
         BE    RUNREP9                                                          
         LLC   R1,RPTNUM           GET INDEX ENTRY                              
         MHI   R1,L'CBINDX                                                      
         LARL  RF,CBINDX                                                        
         AR    R1,RF                                                            
         SR    R5,R5                                                            
         ICM   R5,3,1(R1)          R5=A(CONTROL BREAK ENTRY)                    
         AR    R5,RB                                                            
         ST    R5,ADCBK                                                         
         USING CBKD,R5                                                          
         L     R4,AIO1             R4=A(LAST RECORD)                            
         OC    0(SRLNQ,R4),0(R4)   TEST FIRST TIME                              
         BNZ   RUNREP9                                                          
*                                                                               
RUNREP7  MVC   REPMODE,CBKFRST     SET FIRST TIME BITS                          
         SR    RF,RF                                                            
         ICM   RF,3,CBKROUT                                                     
         AR    RF,RB                                                            
         B     RUNREP17                                                         
*                                                                               
RUNREP9  CLC   RPTNUM,LRPTNUM      TEST SAME REPORT                             
         BE    RUNREP13                                                         
         L     R5,ADLCBK           SET CB FOR LAST REPORT                       
         MVC   REPMODE,CBKLAST                                                  
         SR    RF,RF                                                            
         ICM   RF,3,CBKROUT                                                     
         AR    RF,RB                                                            
         BASR  RE,RF               GO TO REPORT ROUTINE FOR LAST                
         CLI   RPTNUM,X'FF'        TEST LAST TIME                               
         JE    XIT                 YES, EXIT REPORT ROPUTINE                    
         L     R5,ADCBK            SET CB FOR NEW REPORT                        
         B     RUNREP7                                                          
*                                                                               
RUNREP13 SR    RF,RF                                                            
         ICM   RF,3,CBKROUT        GET REPORT ROUTINE                           
         AR    RF,RB                                                            
         MVC   REPMODE,CBKSAME     SET FOR NO CONTROL BREAK                     
         SR    R0,R0                                                            
         ICM   R0,1,CBKNUM         NUMBER OF LEVELS TO CHECK                    
         BZ    RUNREP17                                                         
         LA    RE,CBKL                                                          
         USING CBKL,RE                                                          
         SR    R1,R1                                                            
*                                                                               
RUNREP15 LLC   R2,CBKLKDSP         DISPLACEMENT TO KEY COMPARE                  
         LA    R6,SKVKEY-SORTD(R4)                                              
         AR    R6,R2               R6=KEY DATA (OLD RECORD)                     
         LA    R2,SKVKEY(R2)       R2=KEY DATA (NEW RECORD)                     
         IC    R1,CBKLKCMP         SET LENGTH OF COMPARE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R6)                                                    
         BNE   *+16                                                             
         LA    RE,L'CBKL(RE)       CHECK NEXT BREAK                             
         BCT   R0,RUNREP15                                                      
         B     *+10                                                             
         MVC   REPMODE,CBKLVL      SET BREAKS MODES                             
*                                                                               
RUNREP17 BASR  RE,RF               PROCESS THIS MODE                            
         L     RE,ASRTWK           SAVE LAST RECORD                             
         SR    RF,RF                                                            
         ICM   RF,3,SRLEN-SORTD(RE)                                             
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE RECORD TO IO1                           
         MVC   LRPTNUM,RPTNUM      SAVE LAST REPORT NUMBER                      
         MVC   ADLCBK,ADCBK        SAVE ADDRESS OF LAST CB ENTRY                
         B     RUNREP1                                                          
         DROP  R3,R5,RE                                                         
         EJECT                                                                  
***********************************************************************         
* PRODUCT SUMMARY                                                     *         
***********************************************************************         
PRDREP   NTR1  ,                                                                
         L     R3,AIO1                                                          
         USING SORTD,R3                                                         
         L     R6,ADICO                                                         
         USING DICD,R6                                                          
         TM    REPMODE,PSALLQ      TEST NEW PRODUCT                             
         BO    PRDREP3                                                          
PRDREP1  TM    REPMODE,PSPRHDQ     TEST SET HEADING                             
         BNO   PRDREP3                                                          
         MVI   RCSUBPRG,1                                                       
         CLI   CTAXOPT,0                                                        
         BE    *+8                                                              
         MVI   RCSUBPRG,2          USE TAX HEADLINES                            
         MVI   FORCEHED,YES                                                     
         BAS   RE,CLIPRDH                                                       
         LA    R0,MSRBKN*2                                                      
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),((R0),0)                              
*                                                                               
PRDREP3  TM    REPMODE,PSMETOQ     TEST MEDIA TOTAL                             
         BNO   PRDREP5                                                          
         NI    REPMODE,X'FF'-(PSMETOQ)                                          
         BAS   RE,CLIPRDH                                                       
         GOTO1 ACREPORT                                                         
         GOTOR SETPL,DMCB,(C'T',PSRPTAB),TOTROW                                 
         BAS   RE,TOTFOR           TOTAL FOR MEDIA                              
         MVC   0(L'CPSMED,R1),CPSMED                                            
         BAS   RE,CLIPRDH                                                       
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         LA    R5,TOTROW+(L'BK$*CPSBKN)                                         
         GOTO1 ROWMAIN,DMCB,(C'A',(R5)),('CPSBKN',TOTROW)                       
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),('CPSBKN',0)                          
*                                                                               
PRDREP5  TM    REPMODE,PSPRTOQ     TEST PRODUCT TOTAL                           
         BNO   PRDREP7                                                          
         NI    REPMODE,X'FF'-(PSPRTOQ)                                          
         LA    R5,TOTROW+(L'BK$*CPSBKN)                                         
         GOTOR SETPL,DMCB,(C'T',PSRPTAB),(R5)                                   
         MVC   P+1(L'DD@TPRO),DD@TPRO                                           
         BAS   RE,CLIPRDH                                                       
         GOTO1 ACREPORT                                                         
         TM    REPMODE,PSPRHDQ     TEST SET NEW HEADING                         
         BO    PRDREP1                                                          
*                                                                               
PRDREP7  L     R3,ASRTWK                                                        
         TM    REPMODE,PSMEHDQ     TEST MEDIA HEADLINE                          
         BNO   PRDREP9                                                          
         MVC   MID1+1(12),CPSMED  MEDIA NAME                                    
         GOTO1 AULINE,DMCB,(12,MID1+1),MID2+1                                   
         MVI   FORCEMID,YES                                                     
*                                                                               
PRDREP9  TM    REPMODE,PSDTLQ      TEST PRINT DETAILS                           
         JNO   XIT                                                              
         MVC   PRVD,CPSPRVD        SET PROVINCE (IF ANY)                        
         GOTOR SETPL,DMCB,PSRPTAB,SORTD                                         
         BAS   RE,CLIPRDH                                                       
         GOTO1 ACREPORT                                                         
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),('CPSBKN',CPSBK)                      
         J     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* CLIENT SUMMARY                                                      *         
***********************************************************************         
CLIREP   NTR1  ,                                                                
         L     R3,AIO1                                                          
         USING SORTD,R3                                                         
         L     R6,ADICO                                                         
         USING DICD,R6                                                          
         TM    REPMODE,CSCLHDQ     TEST SET HEADING                             
         BNO   CLIREP3                                                          
         MVI   RCSUBPRG,1                                                       
         CLI   CTAXOPT,0                                                        
         BE    *+8                                                              
         MVI   RCSUBPRG,2          USE TAX HEADLINES                            
         MVI   FORCEHED,YES                                                     
         BAS   RE,CLIPRDH                                                       
         LA    R0,MSRBKN*2                                                      
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),((R0),0)                              
*                                                                               
CLIREP3  TM    REPMODE,CSMETOQ     TEST MEDIA TOTAL                             
         BNO   CLIREP5                                                          
         BAS   RE,CLIPRDH                                                       
         GOTO1 ACREPORT                                                         
         GOTOR SETPL,DMCB,(C'T',CSRPTAB),TOTROW                                 
         BAS   RE,TOTFOR           TOTAL FOR MEDIA                              
         MVC   0(L'CPSMED,R1),CPSMED                                            
         BAS   RE,CLIPRDH                                                       
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         LA    R5,TOTROW+(L'BK$*CPSBKN)                                         
         GOTO1 ROWMAIN,DMCB,(C'A',(R5)),('CPSBKN',TOTROW)                       
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),('CPSBKN',0)                          
*                                                                               
CLIREP5  TM    REPMODE,CSCLTOQ     TEST CLIENT TOTAL                            
         BNO   CLIREP7                                                          
         LA    R5,TOTROW+(L'BK$*CPSBKN)                                         
         GOTOR SETPL,DMCB,(C'T',CSRPTAB),(R5)                                   
         MVC   P+1(L'DD@TCLI),DD@TCLI                                           
         BAS   RE,CLIPRDH                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
CLIREP7  L     R3,ASRTWK                                                        
         TM    REPMODE,CSMEHDQ     TEST MEDIA HEADLINE                          
         BNO   CLIREP9                                                          
         MVC   MID1+1(12),CPSMED  MEDIA NAME                                    
         GOTO1 UNDERLIN,DMCB,(12,MID1+1),MID2+1                                 
         MVI   FORCEMID,YES                                                     
*                                                                               
CLIREP9  TM    REPMODE,CSDTLQ      TEST PRINT DETAILS                           
         JNO   XIT                                                              
         MVC   PRVD,CPSPRVD        SET PROVINCE (IF ANY)                        
         GOTOR SETPL,DMCB,CSRPTAB,SORTD                                         
         BAS   RE,CLIPRDH                                                       
         GOTO1 ACREPORT                                                         
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),('CPSBKN',CPSBK)                      
         J     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GROUP SUMMARY                                                       *         
***********************************************************************         
GBSREP   NTR1  ,                                                                
         L     R3,AIO1             R3 TO 'LAST' RECORD                          
         USING SORTD,R3                                                         
         L     R6,ADICO                                                         
         USING DICD,R6                                                          
         LA    R7,P                                                             
         USING GBPLD,R7                                                         
*                                                                               
         TM    REPMODE,GBWKTQ      TEST WORKCODE TOTAL                          
         JNO   GBSREP3                                                          
         MVI   CURFLG,CURPZRO      PRINT WITH ZEROS                             
         GOTO1 ROWMAIN,DMCB,(C'C',GBSBK),('GBSBKN',0)                           
         LA    R5,TOTROW+GRPWTOT                                                
         GOTO1 ROWMAIN,DMCB,(C'A',GBSBK),('GBSBKN',(R5))                        
         CLC   GBSKWKC,=X'FAFA'    TOTAL FOR CD                                 
         BNE   GBSREP1                                                          
         MVC   GBSKWKC,SPACES                                                   
         MVC   GBSWRKN(L'DD@CSHDS),DD@CSHDS                                     
*                                                                               
GBSREP1  GOTOR SETPL,DMCB,GBRPTAB,(R3)                                          
         OC    P,SPACES                                                         
         CP    GBSDUE,PZERO                                                     
         BE    GBSREP2                                                          
         BAS   RE,GBHEAD                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
GBSREP2  MVC    P,SPACES                                                        
         LA    R4,TOTROW+GRPJTOT   TO JOB                                       
         GOTO1 ROWMAIN,DMCB,(C'A',(R4)),('GBSBKN',0(R5))                        
         GOTO1 ROWMAIN,DMCB,(C'C',(R5)),('GBSBKN',0)                            
*                                                                               
GBSREP3  TM    REPMODE,GBJBTQ      TEST JOB TOTAL                               
         BNO   GBSREP4                                                          
         LA    R5,TOTROW+GRPJTOT                                                
         GOTOR SETPL,DMCB,(C'T',GBRPTAB),(R5)                                   
         MVC   GBPLJBN(L'DD@TAMTD),DD@TAMTD   TOTAL AMOUNT DUE                  
         LA    R0,L'GBPLAMT        SET FOR UNDERLINE                            
         LA    RF,P+(GBPLAMT-GBPLD)                                             
         GOTO1 UNDERLIN,DMCB,((R0),(RF)),L'P(RF)                                
         XC    P,PSECOND           GET THE DASHES ON TOP                        
         XC    PSECOND,P                                                        
         XC    P,PSECOND                                                        
         MVI   SPACING,3                                                        
         BAS   RE,GBHEAD                                                        
         GOTO1 ACREPORT                                                         
         LA    R4,TOTROW+GRPMTOT                                                
         GOTO1 ROWMAIN,DMCB,(C'A',(R4)),('GBSBKN',0(R5))                        
         GOTO1 ROWMAIN,DMCB,(C'C',(R5)),('GBSBKN',0)                            
*                                                                               
GBSREP4  TM    REPMODE,GBMETQ     TEST MEDIA TOTAL                              
         BNO   GBSREP8                                                          
         LA    R5,TOTROW+GRPMTOT                                                
         GOTOR SETPL,DMCB,(C'T',GBRPTAB),(R5)                                   
         MVC   P+1(L'DD@TOTAL),DD@TOTAL                                         
         MVC   P+2+L'DD@TOTAL(L'GBSMED),GBSMED                                  
         BAS   RE,GBHEAD                                                        
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         LARL  R4,GBRTAB                                                        
         XR    R0,R0               ADD TO RECAP TABLE                           
         ICM   R0,1,NRECAP                                                      
         BZ    GBSREP6                                                          
         USING GBRCD,R4                                                         
GBSREP5  CLC   GBRCMED,GBSKMED                                                  
         BE    GBSREP7                                                          
         LA    R4,GBRCLNQ(R4)                                                   
         BCT   R0,GBSREP5                                                       
*                                                                               
GBSREP6  MVC   GBRCMED,GBSKMED     ADD TO RECAP TABLE                           
         MVC   GBRCMEDN,GBSMED                                                  
         ZAP   GBRCDUE,PZERO                                                    
         LLC   RF,NRECAP                                                        
         LA    RF,1(RF)                                                         
         STC   RF,NRECAP                                                        
*                                                                               
GBSREP7  AP    GBRCDUE,0(L'BK$,R5)                                              
         DROP  R4                                                               
*                                                                               
         LA    R4,TOTROW+GRPBTOT                                                
         GOTO1 ROWMAIN,DMCB,(C'A',(R4)),('GBSBKN',0(R5))                        
         GOTO1 ROWMAIN,DMCB,(C'C',(R5)),('GBSBKN',0)                            
         OI    REPMODE,GBMEHDQ                                                  
*                                                                               
GBSREP8  TM    REPMODE,GBPOMQ      PROD/MEDIA                                   
         BNO   GBSREP9                                                          
         LA    R5,TOTROW+GRPBTOT                                                
         GOTOR SETPL,DMCB,(C'T',GBRPTAB),(R5)                                   
         MVC   P+1(L'DD@TOTAL),DD@TOTAL                                         
         MVC   P+2+L'DD@TOTAL(L'DD@PRD),DD@PRD                                  
         BAS   RE,GBHEAD                                                        
         GOTO1 ACREPORT                                                         
         B     GBRECAP                                                          
*                                                                               
GBSREP9  L     R3,ASRTWK                                                        
         TM    REPMODE,GBPMHDQ     TEST SET HEADING                             
         BNO   GBSREP11                                                         
         MVI   NRECAP,0                                                         
         MVI   CURFLG,0                                                         
         MVI   FORCEHED,YES                                                     
         ZAP   GRPDUE,PZERO                                                     
         MVC   PAGE,PAGEONE                                                     
         MVI   RCSUBPRG,15                                                      
         MVI   GBMODE,GBMSBIL                                                   
         BAS   RE,GBHEAD                                                        
         LA    R0,(GBSBKN*4)                                                    
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),((R0),0)                              
         OI    REPMODE,GBMEHDQ                                                  
*                                                                               
GBSREP11 TM    REPMODE,GBMEHDQ     TEST MEDIA HEADLINE                          
         BNO   GBSREP13                                                         
         MVC   GBPLMED,GBSMED      MEDIA NAME                                   
         LLC   RF,LINE                                                          
         AHI   RF,2                                                             
         CLM   RF,1,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEHED,YES                                                     
*                                                                               
GBSREP13 TM    REPMODE,GBJBHDQ     JOB  HEADLINE                                
         BNO   GBSREP15                                                         
         BAS   RE,GBHEAD                                                        
         MVC   GBPLJBC,GBSKJOB     JOB CODE                                     
         MVC   GBPLJBN,GBSJOBN     JOB NAME                                     
         MVC   GBPLJBN+L'P(L'GBSPONB),GBSPONB PRINT ON BILLS                    
         CLI   FORCEHED,YES                                                     
         BE    GBSREP14                                                         
         LA    R0,2                                                             
         CLC   GBSPONB,SPACES                                                   
         BE    *+8                                                              
         AHI   R0,1                                                             
         TM    REPMODE,GBMEHDQ     TEST MEDIA HEADLINE                          
         BNO   *+8                                                              
         AHI   R0,1                                                             
         LLC   RF,LINE                                                          
         AR    RF,R0                                                            
         CLM   RF,1,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEHED,YES                                                     
*                                                                               
GBSREP14 GOTO1 ACREPORT                                                         
*                                                                               
GBSREP15 LA    R5,GBSBK                                                         
         LA    R4,TOTROW+GRPWTOT                                                
         GOTO1 ROWMAIN,DMCB,(C'A',(R4)),('GBSBKN',(R5))                         
*                                                                               
GBSREP19 J     XIT                                                              
         DROP  R7                                                               
*                                                                               
GRPWTOT  EQU   0                   WORKCODE                                     
GRPJTOT  EQU   (1*(L'BK$*GBSBKN))  JOB                                          
GRPMTOT  EQU   (2*(L'BK$*GBSBKN))  MEDIA                                        
GRPBTOT  EQU   (3*(L'BK$*GBSBKN))  BILL                                         
         EJECT                                                                  
***********************************************************************         
* GROUP BILL RECAP                                                    *         
***********************************************************************         
GBRECAP  MVI   CURFLG,0                                                         
         MVI   FORCEHED,YES                                                     
         MVC   PAGE,PAGEONE                                                     
         MVI   RCSUBPRG,15                                                      
         MVI   GBMODE,GBMRCAP                                                   
         LA    R7,P                                                             
         USING GBPLD,R7                                                         
         BAS   RE,GBHEAD                                                        
*                                                                               
         LLC   R0,NRECAP                                                        
         LARL  R4,GBRTAB                                                        
*                                                                               
         USING GBRCD,R4                                                         
GBRECAP3 MVC   GBPLJBC(L'GBRCMEDN),GBRCMEDN                                     
         GOTOR SETPL,DMCB,(C'T',GBRPTAB),GBRCDUE                                
         GOTO1 ACREPORT                                                         
         AP    GRPDUE,GBRCDUE      GET OVERALL TOTAL                            
*                                                                               
         LA    R4,GBRCLNQ(R4)                                                   
         BCT   R0,GBRECAP3                                                      
*                                                                               
         GOTO1 ACREPORT                                                         
         MVC   GBPLJBC(L'DD@TAMTD),DD@TAMTD                                     
         GOTOR SETPL,DMCB,(C'T',GBRPTAB),GRPDUE                                 
         LA    R0,L'GBPLAMT        SET FOR UNDERLINE                            
         LA    RF,P+(GBPLAMT-GBPLD)                                             
         GOTO1 UNDERLIN,DMCB,((R0),(RF)),L'P(RF)                                
         MVC   PTHIRD,PSECOND                                                   
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
*                                                                               
         DROP  R4,R7                                                            
         EJECT                                                                  
***********************************************************************         
* GROUP BILL HEADING                                                  *         
***********************************************************************         
GBHEAD   NTR1  ,                                                                
         MVC   HEAD1+1(L'DD@BILDT),DD@BILDT                                     
         GOTO1 DATCON,DMCB,(1,GBSBDTE),(8,HEAD1+11)                             
         MVC   HEAD1+30(L'DD@RECAP),DD@RECAP                                    
         LA    R0,L'DD@RECAP                                                    
         CLI   GBMODE,GBMRCAP                                                   
         BE    GBHEAD1                                                          
         MVC   HEAD1+30(L'DD@RECAP),SPACES                                      
         MVC   HEAD1+30(L'DD@PRDBL),DD@PRDBL                                    
         LA    R0,L'DD@PRDBL                                                    
*                                                                               
GBHEAD1  GOTO1 UNDERLIN,DMCB,((R0),HEAD1+30),HEAD2+30                           
         MVC   HEAD1+55(L'DD@BLN11),DD@BLN11                                    
         MVC   HEAD1+67(L'GBSSBN),GBSSBN                                        
         TM    PGMSTA,PGMDFT                                                    
         BNO   GBHEAD3                                                          
         MVC   HEAD1+67(L'GBSSBN),SPACES                                        
         MVI   HEAD1+67,C'*'                                                    
         MVC   HEAD1+68(L'DD@DRAFT),DD@DRAFT                                    
         MVI   HEAD1+68+L'DD@DRAFT,C'*'                                         
*                                                                               
GBHEAD3  MVC   HEAD3+1(L'DD@CLINT),DD@CLINT                                     
         MVC   HEAD3+9(3),GBSKCLI         CLIENT CODE                           
         MVC   HEAD3+16(36),GBSCLIN                                             
*                                                                               
         MVC   HEAD4+1(L'DD@PRO),DD@PRO                                         
         MVC   HEAD4+9(3),GBSKPRD         PRODUCT CODE                          
         MVC   HEAD4+16(36),GBSPRDN                                             
*                                                                               
         LA    RF,GBSADR1                                                       
         LA    R4,HEAD3+55                                                      
         LA    R0,5                                                             
*                                                                               
GBHEAD5  MVC   0(26,R4),0(RF)      ADDRESS TO HEADLINES                         
         OC    0(26,R4),SPACES                                                  
         LA    RF,26(RF)                                                        
         LA    R4,132(R4)                                                       
         BCT   R0,GBHEAD5                                                       
*                                                                               
GBHEAD7  OC    GBSDDTE,GBSDDTE                                                  
         BZ    GBHEAD9            SUPPRESS DUE DATE                             
         GOTO1 DATCON,DMCB,(2,GBSDDTE),(8,WORK)                                 
         MVC   HEAD2+55(L'DD@DUEDT),DD@DUEDT                                    
         MVC   HEAD2+55+L'DD@DUEDT+1(8),WORK                                    
*                                                                               
GBHEAD9  CLI   GBMODE,GBMRCAP                                                   
         BE    GBHEAD11                                                         
         MVC   HEAD11+1(L'DD@PRD),DD@PRD                                        
         MVC   HEAD11+22(L'DD@JOBNU),DD@JOBNU                                   
         MVC   HEAD11+40(L'DD@JOBN),DD@JOBN                                     
         MVI   HEAD12+1,C'-'                                                    
         MVC   HEAD12+2(L'DD@PRD-1),HEAD12+1                                    
         MVI   HEAD12+22,C'-'                                                   
         MVC   HEAD12+23(L'DD@JOBNU-1),HEAD12+22                                
         MVI   HEAD12+40,C'-'                                                   
         MVC   HEAD12+41(L'DD@JOBN-1),HEAD12+40                                 
*                                                                               
GBHEAD11 MVC   HEAD11+77(L'DD@AMT),DD@AMT                                       
         LA    R0,L'DD@AMT         SET FOR UNDERLINE                            
         LA    RF,HEAD11+77                                                     
         GOTO1 UNDERLIN,DMCB,((R0),(RF)),L'P(RF)                                
         J     XIT                                                              
*                                                                               
GBMODE   DS    XL1                                                              
GBMSBIL  EQU   1                   SUMMARY BILL                                 
GBMRCAP  EQU   2                   RECAP                                        
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT BILLING REGISTER                                              *         
***********************************************************************         
BLRREP   NTR1  ,                                                                
         L     R3,AIO1                                                          
         USING SORTD,R3                                                         
         TM    REPMODE,BRHDQ       SET HEADLINES                                
         BNO   BLRREP4                                                          
         MVI   TXOPT,0             INIT TAX PRINTING OPTION                     
         MVI   RCSUBPRG,17         SET FOR A27                                  
         TM    PGMAQ,PGMALL27      TEST 27                                      
         BNZ   *+8                 YES, TAX DOESN'T REPLACE CD                  
         MVI   RCSUBPRG,3          ASSUME NO GST - CD IN HEADLINE               
         CLI   CTAXOPT,0                                                        
         BE    BLRREP3                                                          
         TM    OPT3,OPT3PTAX       OPTION TO PRINT TAX NOT CD                   
         BNO   BLRREP1             NO,                                          
         OI    TXOPT,PLTXNCD       SET FOR PRINTING TAX NOT CD                  
         MVI   RCSUBPRG,4          USE TAX HEADLINES                            
         TM    PGMAQ,PGMALL21      TEST 21                                      
         BNZ   *+8                 YES,                                         
         MVI   RCSUBPRG,18         USE A27 TAX HEADLINES                        
         B     BLRREP3                                                          
*                                                                               
BLRREP1  TM    OPT3,OPT3PT2L       OPTION TO PRINT TAX ON 2ND LINE              
         BNO   BLRREP3             NO                                           
         OI    TXOPT,PLTX2ND       SET FOR PRINTING TAX ON 2ND LINE             
*                                                                               
BLRREP3  MVI   CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVI   FORCEHED,YES        FIRST FOR REPORT                             
         MVC   PAGE,PAGEONE                                                     
         BAS   RE,BLRCLR           CLEAR ACCUMUS                                
*                                                                               
BLRREP4  TM    REPMODE,BRCLTOQ     TEST CLIENT TOTAL                            
         BNO   BLRREP7                                                          
         CP    DTLCNT,PONE         TEST MORE THAN ONE ITEM                      
         BNH   BLRREP5                                                          
         GOTOR SETPL,DMCB,(C'T',BLRPTAB),TOTROW                                 
         BAS   RE,TOTFCLI           'TOTAL FOR CLIENT'                          
         MVC   0(L'BLRKCLI,R1),BLRKCLI  CLIENT CODE                             
         GOTO1 ACREPORT                                                         
*                                                                               
BLRREP5  TM    OPT2,OPT2PPCR       TEST PAGE PER CLIENT                         
         BNO   *+8                                                              
         MVI   FORCEHED,YES                                                     
         BAS   RE,BLRCLR           CLEAR ROW                                    
*                                                                               
BLRREP7  TM    REPMODE,BRDTLQ      TEST PRINT DETAILS                           
         JNO   XIT                                                              
         GOTO1 ACREPORT            SKIP A LINE                                  
         L     R3,ASRTWK                                                        
         MVC   PRVD,BLRPRVD        SET PROVINCE (IF ANY)                        
         GOTOR SETPL,DMCB,BLRPTAB,ASRTWK                                        
         GOTO1 ACREPORT                                                         
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),('BLRBKN',BLRBK)                      
         AP    DTLCNT,PONE         COUNT DETAIL LINES                           
         J     XIT                                                              
*                                                                               
BLRCLR   LR    R0,RE                                                            
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),('BLRBKN',0)                          
         ZAP   DTLCNT,PZERO                                                     
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT MEDIA SUMMARY                                                 *         
***********************************************************************         
MSREP    NTR1  ,                                                                
         TM    OPT4,OPT4SMED       EXCLUDE THIS SUMMARY?                        
         JO    XIT                 YES                                          
         L     R3,AIO1                                                          
         USING SORTD,R3                                                         
         TM    REPMODE,MSHDQ                                                    
         BNO   MSREP4                                                           
         MVI   TXOPT,0             INIT TAX PRINTING OPTION                     
         MVI   RCSUBPRG,19         SET FOR A27                                  
         TM    PGMAQ,PGMALL27      TEST 27                                      
         BNZ   *+8                 YES, TAX DOESN'T REPLACE CD                  
         MVI   RCSUBPRG,5          ASSUME NO GST - CD IN HEADLINE               
         CLI   CTAXOPT,0                                                        
         BE    MSREP3                                                           
         TM    OPT3,OPT3PTAX       OPTION TO PRINT TAX NOT CD                   
         BNO   MSREP1              NO,                                          
         OI    TXOPT,PLTXNCD       SET FOR PRINTING TAX NOT CD                  
         MVI   RCSUBPRG,6          USE TAX HEADLINES                            
         TM    PGMAQ,PGMALL21      TEST 21                                      
         BNZ   *+8                 YES,                                         
         MVI   RCSUBPRG,20         USE A27 TAX HEADLINES                        
         B     MSREP3                                                           
*                                                                               
MSREP1   TM    OPT3,OPT3PT2L       OPTION TO PRINT TAX ON 2ND LINE              
         BNO   MSREP3              NO                                           
         OI    TXOPT,PLTX2ND       SET FOR PRINTING TAX ON 2ND LIN              
*                                                                               
MSREP3   MVI   CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVI   FORCEHED,YES        FIRST FOR REPORT                             
**       MVC   PAGE,PAGEONE                                                     
         LA    R0,MSRBKN*2                                                      
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),((R0),0)                              
*                                                                               
MSREP4   TM    REPMODE,MSMETOQ     TEST MEDIA BREAK                             
         BNO   MSREP5                                                           
         GOTOR SETPL,DMCB,MSRPTAB,SORTD   PRINT LAST                            
         GOTOR SETPL,DMCB,(C'T',MSRPTAB),TOTROW                                 
         CLC   P+MSPLACT-MSPLD(L'P-(MSPLACT-MSPLD)),SPACES                      
         BNE   *+10                                                             
         MVC   P+(MSPLGRS-MSPLD+6)(3),=C'NIL'                                   
         GOTO1 ACREPORT                                                         
         LA    R5,TOTROW+(L'BK$*MSRBKN)                                         
         GOTO1 ROWMAIN,DMCB,(C'A',(R5)),('MSRBKN',TOTROW)                       
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),('MSRBKN',0)                          
*                                                                               
MSREP5   TM    REPMODE,MSRNTOQ     TEST RUN TOTALS                              
         BNO   MSREP7                                                           
         GOTO1 ACREPORT            SPACE 1                                      
         LA    R5,TOTROW+(L'BK$*MSRBKN)                                         
         GOTOR SETPL,DMCB,(C'T',MSRPTAB),0(R5)                                  
         L     R6,ADICO                                                         
         MVC   P+1(L'DD@TOTRN),DD@TOTRN-DICD(R6) 'TOTALS FOR RUN'               
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
*                                                                               
MSREP7   TM    REPMODE,MSADDQ      ADD TO MEDIA ACCUMS                          
         JNO   XIT                                                              
         L     R3,ASRTWK                                                        
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),('MSRBKN',MSRBK)                      
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT INTERNAL INVOICE SUMMARY (SK REVERSALS)                       *         
***********************************************************************         
IIREP    NTR1  ,                                                                
         L     R3,AIO1                                                          
         USING SORTD,R3                                                         
         TM    REPMODE,IIHDQ                                                    
         BNO   IIREP3                                                           
         MVI   CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVI   FORCEHED,YES        FIRST FOR REPORT                             
         MVC   PAGE,PAGEONE                                                     
         ZAP   DTLCNT,PZERO        CLEAR DETAIL COUNT                           
         MVI   RCSUBPRG,8                                                       
         LA    R0,INIBKN*2                                                      
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),((R0),0)                              
*                                                                               
IIREP3   TM    REPMODE,IIJOTOQ     TEST JOB TOTAL                               
         BNO   IIREP5                                                           
         CP    DTLCNT,PONE         TEST MORE THAN ONE ITEM                      
         BNH   IIREP4              IF NOT, SKIP PRINT OF TOTAL                  
         GOTO1 ACREPORT                                                         
         GOTOR SETPL,DMCB,(C'T',IIRPTAB),TOTROW                                 
         BAS   RE,TOTFJOB                                                       
         TM    REPMODE,IIJOFIQ     IS THERE ANOTHER JOB ?                       
         BNO   *+8                                                              
         MVI   SPACING,2           SKIP A LINE                                  
         GOTO1 ACREPORT                                                         
IIREP4   LA    R5,TOTROW+(L'BK$*INIBKN)                                         
         GOTO1 ROWMAIN,DMCB,(C'A',(R5)),('INIBKN',TOTROW)                       
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),('INIBKN',0)                          
*                                                                               
IIREP5   L     R3,ASRTWK                                                        
         TM    REPMODE,IIJOFIQ     JOB FIRST                                    
         BNO   IIREP7                                                           
         XC    IIDTENO,IIDTENO                                                  
         ZAP   DTLCNT,PZERO        CLEAR DETAIL COUNT                           
         MVC   P+(IIPLJOB-IIPLD)(L'IIPLJOB),INIKJOB                             
*                                                                               
IIREP7   TM    REPMODE,IIACFIQ     ACCOUNT FIRST                                
         BNO   *+16                                                             
         MVC   P+(IIPLACT-IIPLD)(L'IIPLACT),INIACT                              
         XC    IIDTENO,IIDTENO                                                  
*                                                                               
         TM    REPMODE,IIDTLQ      PRINT A DETAIL LINE                          
         BNO   IIREP9                                                           
         GOTOR SETPL,DMCB,IIRPTAB,ASRTWK                                        
         LA    RF,P                                                             
         USING IIPLD,RF                                                         
         CLC   IIPLDTE(L'IIDTENO),IIDTENO SAME DATE & NUMBER ?                  
         BE    *+14                       YES,                                  
         MVC   IIDTENO,IIPLDTE            NO - SAVE THIS ONE                    
         B     *+10                                                             
         MVC   IIPLDTE(L'IIDTENO),SPACES  DON'T PRINT IT AGAIN                  
         DROP  RF                                                               
*                                                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),('INIBKN',INIBK)                      
         AP    DTLCNT,PONE         ADD TO DETAIL COUNT                          
*                                                                               
IIREP9   TM    REPMODE,IIRNTOQ     TEST RUN TOTALS                              
         JNO   XIT                                                              
         GOTO1 ACREPORT            SPACE BEFORE TOTAL                           
         LA    R5,TOTROW+(L'BK$*INIBKN)                                         
         GOTOR SETPL,DMCB,(C'T',IIRPTAB),0(R5)                                  
         L     R6,ADICO                                                         
         MVC   P+1(L'DD@TOTRN),DD@TOTRN-DICD(R6) 'TOTALS FOR RUN'               
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
         DROP  R3                                                               
*                                                                               
IIDTENO  DS    CL(IIPLREF-IIPLDTE)  SAVED DATE & NUMBER                         
         EJECT                                                                  
***********************************************************************         
* PRINT REGISTER OF TARGET ACCOUNTS                                   *         
***********************************************************************         
TARREP   NTR1  ,                                                                
         TM    REPMODE,TGHDQ       SET HEADLINES                                
         BNO   TARREP3                                                          
         MVI   CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVI   FORCEHED,YES        FIRST FOR REPORT                             
         MVC   PAGE,PAGEONE                                                     
         MVI   RCSUBPRG,7                                                       
*                                                                               
TARREP3  TM    REPMODE,TGDTLQ      TEST PRINT DETAILS                           
         JNO   XIT                                                              
         GOTOR SETPL,DMCB,TARPTAB,ASRTWK                                        
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT INTERNAL INCOME REGISTER                                      *         
***********************************************************************         
                                                                                
ICREP    NTR1  ,                                                                
         TM    REPMODE,ICHDQ                                                    
         BNO   ICREP3                                                           
         MVI   CURFLG,CURPAWC+CURPZRO  PRINT WITH COMMAS, ZERO=NOBLANK          
         MVI   FORCEHED,YES        FIRST FOR REPORT                             
         MVC   PAGE,PAGEONE                                                     
         MVI   RCSUBPRG,9                                                       
         LA    R0,INCBKN*2                                                      
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),((R0),0)                              
*                                                                               
ICREP3   TM    REPMODE,ICJOTOQ     TEST JOB TOTAL                               
         BNO   ICREP5                                                           
         GOTOR SETPL,DMCB,(C'T',ICRPTAB),TOTROW                                 
         BAS   RE,TOTFJOB                                                       
         GOTO1 ACREPORT                                                         
         LA    R5,TOTROW+(L'BK$*INCBKN)                                         
         GOTO1 ROWMAIN,DMCB,(C'A',(R5)),('INCBKN',TOTROW)                       
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),('INCBKN',0)                          
*                                                                               
ICREP5   TM    REPMODE,ICRNTOQ     TEST RUN TOTALS                              
         BNO   ICREP7                                                           
         GOTO1 ACREPORT            SPACE 1                                      
         LA    R5,TOTROW+(L'BK$*INCBKN)                                         
         GOTOR SETPL,DMCB,(C'T',ICRPTAB),0(R5)                                  
         L     R6,ADICO                                                         
         MVC   P+1(L'DD@TOTRN),DD@TOTRN-DICD(R6) 'TOTALS FOR RUN'               
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
*                                                                               
ICREP7   TM    REPMODE,ICDTLQ      PRINT DETAIL LINE                            
         JNO   XIT                                                              
         L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         GOTOR SETPL,DMCB,ICRPTAB,ASRTWK                                        
         TM    REPMODE,ICJOFIQ     FIRST FOR JOB                                
         BO    *+10                                                             
         MVC   P(ICPLNUM-ICPLD),SPACES    CLEAR CLI/PRD/JOB                     
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),('INCBKN',INCBK)                      
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT STUDIO INTERCOMPANY POSTING REGISTER                          *         
***********************************************************************         
SPREP    NTR1  ,                                                                
         TM    REPMODE,SPHDQ                                                    
         BNO   SPREP3                                                           
         MVI   CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVI   FORCEHED,YES        FIRST FOR REPORT                             
         MVC   PAGE,PAGEONE                                                     
         MVI   RCSUBPRG,10                                                      
         LA    R0,SIPBKN*2                                                      
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),((R0),0)                              
*                                                                               
SPREP3   TM    REPMODE,SPSTTOQ     TEST STUDIO TOTAL                            
         BNO   SPREP5                                                           
         GOTOR SETPL,DMCB,(C'T',SPRPTAB),TOTROW                                 
         MVC   P+1(13),=C'*STUDIO TYPE*'                                        
         L     R3,AIO1                                                          
         USING SORTD,R3                                                         
         MVC   P+15(L'SIPKSTUD),SIPKSTUD  STUDIO                                
         GOTO1 ACREPORT                                                         
         LA    R5,TOTROW+(L'BK$*SIPBKN)                                         
         GOTO1 ROWMAIN,DMCB,(C'A',(R5)),('SIPBKN',TOTROW)                       
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),('SIPBKN',0)                          
*                                                                               
SPREP5   TM    REPMODE,SPRNTOQ     TEST RUN TOTALS                              
         BNO   SPREP7                                                           
         GOTO1 ACREPORT            SPACE 1                                      
         LA    R5,TOTROW+(L'BK$*SIPBKN)                                         
         GOTOR SETPL,DMCB,(C'T',SPRPTAB),0(R5)                                  
         L     R6,ADICO                                                         
         MVC   P+1(L'DD@TOTRN),DD@TOTRN-DICD(R6) 'TOTALS FOR RUN'               
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
*                                                                               
SPREP7   L     R3,ASRTWK                                                        
         TM    REPMODE,SPSTFIQ     FIRST FOR STUDIO                             
         BNO   SPREP9                                                           
         MVC   P+1(L'SIPKSTUD),SIPKSTUD  STUDIO                                 
         GOTO1 ACREPORT                                                         
*                                                                               
SPREP9   TM    REPMODE,SPDTLQ      PRINT DETAIL LINE                            
         JNO   XIT                                                              
         GOTOR SETPL,DMCB,SPRPTAB,ASRTWK                                        
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),('SIPBKN',SIPBK)                      
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT INTERCOMPANY POSTING EXCEPTION REPORT                         *         
***********************************************************************         
ICXREP   NTR1  ,                                                                
         TM    REPMODE,IXHDQ                                                    
         BNO   ICXREP3                                                          
         MVI   CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVI   FORCEHED,YES        FIRST FOR REPORT                             
         MVC   PAGE,PAGEONE                                                     
         MVI   RCSUBPRG,11                                                      
*                                                                               
ICXREP3  TM    REPMODE,IXDTLQ                                                   
         JNO   XIT                                                              
         GOTOR SETPL,DMCB,IXRPTAB,ASRTWK                                        
         L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         LLC   R1,ICXWARN                                                       
         BCTR  R1,0                                                             
         MHI   R1,L'ICXWTAB                                                     
         LA    R2,ICXWTAB                                                       
         AR    R2,R1                                                            
         MVC   P+(IXPLWMSG-IXPLD)(L'IXPLWMSG),0(R2)                             
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT PERCENT OF BILLING REPORT                                     *         
***********************************************************************         
POBREP   NTR1  ,                                                                
         TM    REPMODE,PBHDQ                                                    
         BNO   POBREP3                                                          
         MVI   CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVI   FORCEHED,YES        FIRST FOR REPORT                             
         MVC   PAGE,PAGEONE                                                     
         MVI   RCSUBPRG,14                                                      
*                                                                               
POBREP3  TM    REPMODE,PBDTLQ                                                   
         JNO   XIT                                                              
         GOTOR SETPL,DMCB,PBRPTAB,ASRTWK                                        
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT ASP REPORT                                                    *         
***********************************************************************         
ASPREP   NTR1  ,                                                                
         L     R6,ADICO                                                         
         USING DICD,R6                                                          
         TM    REPMODE,ASHDQ                                                    
         BNO   ASPREP3                                                          
         MVI   CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVI   RCSUBPRG,21                                                      
         MVC   PAGE,PAGEONE                                                     
ASPREP1  MVI   FORCEHED,YES        FIRST FOR REPORT                             
         LA    R0,ASPRBKN*2                                                     
*                                                                               
ASPREP2  L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),((R0),0)                              
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),('ASPRBKN',ASPRBK)                    
         J     XIT                                                              
*                                                                               
ASPREP3  L     R3,ASRTWK                                                        
         TM    REPMODE,ASADDQ     TEST SAME KEY                                 
         BNO   ASPREP5                                                          
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),('ASPRBKN',ASPRBK)                    
         J     XIT                                                              
*                                                                               
ASPREP5  TM    REPMODE,ASBILQ     TEST SAME KEY                                 
         JNO   XIT                                                              
         LA    R5,TOTROW+(L'BK$*ASPRBKN)   ADD TO OVERALL TOTAL                 
         GOTO1 ROWMAIN,DMCB,(C'A',(R5)),('ASPRBKN',TOTROW)                      
*                                                                               
         L     R3,AIO1             PRINT BILL TOTALS                            
         USING SORTD,R3                                                         
         GOTOR SETPL,DMCB,ASRPTAB,(R3)                                          
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R0,ASPRBKN                                                       
         TM    REPMODE,ASLSTQ     TEST LAST TIME                                
         BNO   ASPREP2                                                          
*                                                                               
*                                                                               
ASPREPX  LA    RF,P+5                                                           
         MVC   0(L'DD@TFOR,RF),DD@TFOR                                          
         LA    RF,1+L'DD@TFOR(RF)                                               
         MVC   0(L'DD@BLDIO,RF),DD@BLDIO                                        
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         BNO   *+10                                                             
         MVC   0(L'DD@UNBIO,RF),DD@UNBIO                                        
*                                                                               
         LA    R0,TOTROW+(ASPRBKN*L'BK$)                                        
         GOTOR SETPL,DMCB,(C'T',ASRPTAB),(R0)                                   
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
                                                                                
         MVC   P+5(L'DD@TREP),DD@TREP                                           
         GOTOR SETPL,DMCB,(C'T',ASRPTAB),(R0)                                   
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
*                                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GROUP BILL TRACE SUMMARY                                            *         
***********************************************************************         
GBTREP   NTR1  ,                                                                
         TM    REPMODE,GTHDQ                                                    
         BNO   GBTREP3                                                          
         MVI   CURFLG,0                                                         
         MVI   RCSUBPRG,16                                                      
         MVC   PAGE,PAGEONE                                                     
GBTREP1  MVI   FORCEHED,YES        FIRST FOR REPORT                             
*                                                                               
GBTREP2  L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         LA    R0,GBCBKN                                                        
         GOTO1 ROWMAIN,DMCB,(C'C',TOTROW),((R0),0)                              
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),((R0),GBCBK)                          
         J     XIT                                                              
*                                                                               
GBTREP3  L     R3,ASRTWK                                                        
         TM    REPMODE,GTADDQ     TEST SAME KEY                                 
         BNO   GBTREP5                                                          
         LA    R0,GBCBKN                                                        
         GOTO1 ROWMAIN,DMCB,(C'A',TOTROW),((R0),GBCBK)                          
         J     XIT                                                              
*                                                                               
GBTREP5  TM    REPMODE,GTBILQ     TEST SAME KEY                                 
         JNO   XIT                                                              
         L     R3,AIO1                                                          
         USING SORTD,R3                                                         
         MVC   HEAD6+10(L'GBCKCLI),GBCKCLI                                      
         GOTO1 DATCON,DMCB,(1,GBCKBDTE),(10,HEAD6+33)                           
         LA    R2,P                                                             
         USING GBCPD,R2                                                         
         MVC   GBCPBNO,GBCKBNUM                                                 
         GOTOR SETPL,DMCB,(C'T',GBTPTAB),TOTROW                                 
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   GBCBK(L'GBCBK*GBCBKN),TOTROW  MOVE TOTALS TO RECORD              
         TM    RQLV,LVA+LVB       TEST GROUP BILL                               
         BZ    *+8                NO, SKIP  GROUP BILL RECORD                   
         BRAS  RE,UPGBR           UPDATE THE GROUP BILLING RECORD               
*                                                                               
         TM    REPMODE,GTLSTQ     TEST LAST TIME                                
         BO    GBTREPX            YES, DONE                                     
         TM    REPMODE,GTDATQ     TEST DATE CHANGE                              
         BO    GBTREP1            YES, NEW PAGE                                 
         B     GBTREP2                                                          
*                                                                               
GBTREPX  MVC   P+1(39),=C'ALL GROUP BILLING RECORDS ADDED/UPDATED'              
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT NON-BILLABLE REPORT                                           *         
***********************************************************************         
NBJREP   NTR1  ,                                                                
         TM    REPMODE,NBHDQ                                                    
         BNO   NBJREP3                                                          
         MVI   CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVI   FORCEHED,YES        FIRST FOR REPORT                             
         MVC   PAGE,PAGEONE                                                     
         MVI   RCSUBPRG,12                                                      
*                                                                               
NBJREP3  TM    REPMODE,NBDTLQ                                                   
         JNO   XIT                                                              
*                                                                               
NBJREP4  L     R3,ASRTWK           RETRIEVE AND PRINT JOB EXCEPTIONS            
         USING SORTD,R3                                                         
         LA    R2,P                                                             
         USING NBPLD,R2                                                         
         MVC   NBPLCLI,NBJKCLI     CLIENT                                       
         MVC   NBPLPRD,NBJKPRD     PRODUCT                                      
         MVC   NBPLJOB,NBJKJOB     JOB                                          
*                                                                               
         LA    R4,NBJERR           R4=A(EXCEPTION ERROR BITS)                   
         LHI   R0,5                R0=NUMBER OF FIELDS                          
*                                                                               
NBJREP5  DS    0H                                                               
         OC    0(2,R4),0(R4)       TEST ANY ERROR                               
         BZ    NBJREP11                                                         
         LA    R5,WORK                                                          
         USING GETTXTD,R5                                                       
         XC    GETTXTD(L'GTBLOCK),GETTXTD                                       
         MVC   GTMSGNO,0(R4)                                                    
         MVI   GTMSYS,X'06'                                                     
         MVI   GTMAXL,L'NBPLERR                                                 
         LA    RE,NBPLERR                                                       
         STCM  RE,7,GTAOUT                                                      
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1REF+GT1OWRK                                           
         L     RF,ADMASTC                                                       
         L     RF,MCVGETXT-MASTD(RF)                                            
         GOTOR (RF),GETTXTD                                                     
         GOTO1 ACREPORT                                                         
         LA    R4,2(R4)                                                         
         BCT   R0,NBJREP5                                                       
*                                                                               
NBJREP11 CLI   NBJOTH,0                                                         
         BE    NBJREP21                                                         
*                                                                               
         LA    R4,ERRMSGS          LOCAL ERRORS TO PRINT                        
NBJREP13 CLC   0(1,R4),NBJOTH                                                   
         BE    NBJREP15                                                         
         LA    R4,L'ERRMSGS(R4)                                                 
         CLI   0(R4),EOT                                                        
         BE    NBJREP21                                                         
         B     NBJREP13                                                         
*                                                                               
NBJREP15 CLI   NBJOTH,ERRACNF      ACCOUNT NOT FOUND                            
         BE    *+12                                                             
         CLI   NBJOTH,ERRACNB      ACCOUNT DOES NOT HAVE BALANCE                
         BNE   NBJREP16                                                         
         MVC   NBPLERR(14),NBJDESC                                              
         MVC   NBPLERR+15,1(R4)                                                 
         B     NBJREP19                                                         
*                                                                               
NBJREP16 MVC   NBPLERR,1(R4)                                                    
NBJREP19 GOTO1 ACREPORT                                                         
*                                                                               
NBJREP21 DS    0H                  GET THE NEXT RECORD                          
         J     XIT                                                              
*                                                                               
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
*                                                                               
CLIPRDH  LR    R0,RE               CLIENT/PRODUCT TO HEADLINE                   
         CLI   FORCEHED,YES                                                     
         BNE   CLIPRDH3                                                         
         MVC   PAGE,PAGEONE                                                     
         L     R3,ASRTWK                                                        
*                                                                               
         USING SORTD,R3                                                         
CLIPRDH3 L     R6,ADICO                                                         
         USING DICD,R6                                                          
         MVI   CURFLG,0                                                         
         TM    SRSTAT,SRPWC        TEST PRINT WITH COMMAS                       
         BNO   *+8                                                              
         OI    CURFLG,CURPAWC      SET PRINT WITH COMMAS                        
         MVC   HEAD4,SPACES                                                     
         MVC   HEAD5,SPACES                                                     
         MVC   HEAD4+1(L'DD@CLINT),DD@CLINT                                     
         MVC   HEAD4+8(3),CPSKCLI                                               
         MVC   HEAD4+15(36),CPSCLIN                                             
         CLI   RPTNUM,CLISUM                                                    
         BE    CLIPRDHX                                                         
         MVC   HEAD4+55(L'DD@PRO),DD@PRO                                        
         MVC   HEAD4+55+L'DD@PRO+1(3),CPSKPRD                                   
         LA    RF,HEAD4+69                                                      
         GOTO1 CHOPPER,DMCB,(36,CPSPRDN),(19,(RF)),(C'P',2)                     
CLIPRDHX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MAINTAIN ACCUMILATORS                                    *         
*  PARM 1  BYTE 0     C'C' = CLEAR                                    *         
*                     C'A' = ADD                                      *         
*          BYTE 1-3   A(TO BUCKETS)                                   *         
*  PARM 2  BYTE 0     NUMBER OF BUCKETS                               *         
*          BYTE 1-3   A(FROM BUCKETS)                                 *         
***********************************************************************         
ROWMAIN  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         LLC   R0,4(R1)                                                         
*                                                                               
ROWMAIN3 CLI   0(R1),C'C'                                                       
         BNE   *+14                                                             
         ZAP   0(L'BK$,R2),PZERO                                                
         B     *+14                                                             
         AP    0(L'BK$,R2),0(L'BK$,R3)                                          
         LA    R3,L'BK$(R3)                                                     
         LA    R2,L'BK$(R2)                                                     
         BCT   R0,ROWMAIN3                                                      
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET TOTAL FOR IN PRINT LINE                                         *         
***********************************************************************         
*                                 ROUTINE TO SET 'TOTAL FOR                     
TOTFOR   L     R6,ADICO                                                         
         MVC   P+1(L'DD@TFOR),DD@TFOR-DICD(R6) 'TOTAL FOR '                     
         LA    R1,P+1+L'DD@TFOR                                                 
         B     TOTFX                                                            
*                                                                               
TOTFCLI  L     R6,ADICO            'TOTAL FOR CLIENT                            
         MVC   P+1(L'DD@TCLI),DD@TCLI-DICD(R6) 'TOTAL FOR CLIENT'               
         LA    R1,P+1+L'DD@TCLI                                                 
TOTFX    CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,TOTFX                                                         
         LA    R1,2(R1)                                                         
         BR    RE                                                               
*                                                                               
TOTFJOB  MVI   P+1,C'*'                                                         
         L     R6,ADICO                                                         
         MVC   P+2(L'DD@TJOB),DD@TJOB-DICD(R6) 'TOTAL FOR JOB'                  
         LA    R1,P+2+L'DD@TJOB                                                 
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'*'                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* STORAGE - CONSTANTS - LITERAL POOL                                  *         
***********************************************************************         
* PRINT LINE CONTROL TABLES (SEE PLCD)                                          
*                                                                               
PSRPTAB  DC    AL2(CPSBK-CPSK)                                                  
         DC    AL1(0,L'PSPLJOB,0),AL2(PSPLJOB-PSPLD,CPSKJOB-BILK)               
         DC    AL1(0,L'PSPLOTH,0),AL2(PSPLOTH-PSPLD,CPSKOTH-BILK)               
         DC    AL1(0,L'PSPLBIL,0),AL2(PSPLBIL-PSPLD,CPSKBNUM-BILK+2)            
         DC    AL1(0,L'PSPLNET,AMTRQ),AL2(PSPLNET-PSPLD,CPSNET-BILK)            
         DC    AL1(0,L'PSPLCOM,AMTRQ),AL2(PSPLCOM-PSPLD,CPSCOM-BILK)            
         DC    AL1(0,L'PSPLGRS,AMTRQ),AL2(PSPLGRS-PSPLD,CPSGRS-BILK)            
         DC    AL1(0,L'PSPLGST,AMTRQ),AL2(PSPLGST-PSPLD,CPSGST-BILK)            
         DC    AL1(0,L'PSPLPST,AMTRQ),AL2(PSPLPST-PSPLD,CPSPST-BILK)            
         DC    AL1(EOT)                                                         
*                                                                               
CSRPTAB  DC    AL2(CPSBK-CPSK)                                                  
         DC    AL1(0,L'CSPLPRD,0),AL2(CSPLPRD-CSPLD,CPSKPRD-BILK)               
         DC    AL1(0,L'CSPLJOB,0),AL2(CSPLJOB-CSPLD,CPSKJOB-BILK)               
         DC    AL1(0,L'CSPLOTH,0),AL2(CSPLOTH-CSPLD,CPSKOTH-BILK)               
         DC    AL1(0,L'CSPLBIL,0),AL2(CSPLBIL-CSPLD,CPSKBNUM-BILK+2)            
         DC    AL1(0,L'CSPLNET,AMTRQ),AL2(CSPLNET-CSPLD,CPSNET-BILK)            
         DC    AL1(0,L'CSPLCOM,AMTRQ),AL2(CSPLCOM-CSPLD,CPSCOM-BILK)            
         DC    AL1(0,L'CSPLGRS,AMTRQ),AL2(CSPLGRS-CSPLD,CPSGRS-BILK)            
         DC    AL1(0,L'CSPLGST,AMTRQ),AL2(CSPLGST-CSPLD,CPSGST-BILK)            
         DC    AL1(0,L'CSPLPST,AMTRQ),AL2(CSPLPST-CSPLD,CPSPST-BILK)            
         DC    AL1(EOT)                                                         
*                                                                               
GBRPTAB  DC    AL2(GBSBK-GBSK)                                                  
         DC    AL1(0,L'GBPLWKC,0),AL2(GBPLWKC-GBPLD,GBSKWKC-BILK)               
         DC    AL1(0,L'GBPLWKN,0),AL2(GBPLWKN-GBPLD,GBSWRKN-BILK)               
         DC    AL1(0,L'GBPLAMT,AMTRQ),AL2(GBPLAMT-GBPLD,GBSDUE-BILK)            
         DC    AL1(EOT)                                                         
*                                                                               
BLRPTAB  DC    AL2(BLRBK-BLRK)                                                  
         DC    AL1(0,L'BRPLMED,0),AL2(BRPLMED-BRPLD,BLRKMED-BLRK)               
         DC    AL1(0,L'BRPLCLI,0),AL2(BRPLCLI-BRPLD,BLRKCLI-BLRK)               
         DC    AL1(0,L'BRPLPRD,0),AL2(BRPLPRD-BRPLD,BLRKPRD-BLRK)               
         DC    AL1(0,L'BRPLJOB,0),AL2(BRPLJOB-BRPLD,BLRKJOB-BLRK)               
         DC    AL1(0,L'BRPLDTE,DATRQ),AL2(BRPLDTE-BRPLD,BLRBDTE-BLRK)           
         DC    AL1(0,L'BRPLBIL,0),AL2(BRPLBIL-BRPLD,BLRBNUM-BLRK+2)             
         DC    AL1(0,L'BRPLACT,AMTRQ),AL2(BRPLACT-BRPLD,BLRACT-BLRK)            
         DC    AL1(0,L'BRPLNET,AMTRQ),AL2(BRPLNET-BRPLD,BLRNET-BLRK)            
         DC    AL1(0,L'BRPLCOM,AMTRQ),AL2(BRPLCOM-BRPLD,BLRCOM-BLRK)            
         DC    AL1(0,L'BRPLGRS,AMTRQ),AL2(BRPLGRS-BRPLD,BLRGRS-BLRK)            
         DC    AL1(PLNOTX,L'BRPLCSD,AMTRQ)                                      
         DC    AL2(BRPLCSD-BRPLD,BLRCSD-BLRK)                                   
         DC    AL1(PLTXNCD,L'BRPLGST,GSTRQ)                                     
         DC    AL2(BRPLGST-BRPLD,BLRGST-BLRK)                                   
         DC    AL1(PLTXNCD,L'BRPLPST,PSTRQ)                                     
         DC    AL2(BRPLPST-BRPLD,BLRPST-BLRK)                                   
         DC    AL1(PLTX2ND,L'BRPLCSD,AMTRQ)                                     
         DC    AL2(BRPLCSD-BRPLD,BLRCSD-BLRK)                                   
         DC    AL1(PLTX2ND,L'BRPLGST2,GSTRQ)                                    
         DC    AL2(BRPLGST2-BRPLD,BLRGST-BLRK)                                  
         DC    AL1(PLTX2ND,L'BRPLPST2,PSTRQ)                                    
         DC    AL2(BRPLPST2-BRPLD,BLRPST-BLRK)                                  
         DC    AL1(EOT)                                                         
*                                                                               
MSRPTAB  DC    AL2(MSRBK-MSRK)                                                  
         DC    AL1(0,L'MSPLNME,0),AL2(MSPLNME-MSPLD,MSRNAME-MSRK)               
         DC    AL1(0,L'MSPLACT,AMTRQ),AL2(MSPLACT-MSPLD,MSRACT-MSRK)            
         DC    AL1(0,L'MSPLNET,AMTRQ),AL2(MSPLNET-MSPLD,MSRNET-MSRK)            
         DC    AL1(0,L'MSPLCOM,AMTRQ),AL2(MSPLCOM-MSPLD,MSRCOM-MSRK)            
         DC    AL1(0,L'MSPLGRS,AMTRQ),AL2(MSPLGRS-MSPLD,MSRGRS-MSRK)            
         DC    AL1(PLNOTX,L'MSPLCSD,AMTRQ)                                      
         DC    AL2(MSPLCSD-MSPLD,MSRCSD-MSRK)                                   
         DC    AL1(PLTXNCD,L'MSPLGST,GSTRQ)                                     
         DC    AL2(MSPLGST-MSPLD,MSRGST-MSRK)                                   
         DC    AL1(PLTXNCD,L'MSPLPST,PSTRQ)                                     
         DC    AL2(MSPLPST-MSPLD,MSRPST-MSRK)                                   
         DC    AL1(PLTX2ND,L'MSPLCSD,AMTRQ)                                     
         DC    AL2(MSPLCSD-MSPLD,MSRCSD-BLRK)                                   
         DC    AL1(PLTX2ND,L'MSPLGST2,GSTRQ)                                    
         DC    AL2(MSPLGST2-MSPLD,MSRGST-BLRK)                                  
         DC    AL1(PLTX2ND,L'MSPLPST2,PSTRQ)                                    
         DC    AL2(MSPLPST2-MSPLD,MSRPST-BLRK)                                  
         DC    AL1(EOT)                                                         
*                                                                               
IIRPTAB  DC    AL2(INIBK-INIK)                                                  
         DC    AL1(0,L'IIPLDTE,DATR2Q),AL2(IIPLDTE-IIPLD,INIBDTE-INIK)          
         DC    AL1(0,L'IIPLNUM,0),AL2(IIPLNUM-IIPLD,INIKNUM-INIK+2)             
         DC    AL1(0,L'IIPLREF,0),AL2(IIPLREF-IIPLD,INIREF-INIK)                
         DC    AL1(0,L'IIPLWC,0),AL2(IIPLWC-IIPLD,INIKWC-INIK)                  
         DC    AL1(0,L'IIPLAMT,AMTRQ),AL2(IIPLAMT-IIPLD,INIAMT-INIK)            
         DC    AL1(EOT)                                                         
*                                                                               
TARPTAB  DC    AL2(0)                                                           
         DC    AL1(0,L'TGPLMED,0),AL2(TGPLMED-TGPLD,RTAJOB-RTAK)                
         DC    AL1(0,L'TGPLCLI,0),AL2(TGPLCLI-TGPLD,RTACLI-RTAK)                
         DC    AL1(0,L'TGPLPRD,0),AL2(TGPLPRD-TGPLD,RTAPRD-RTAK)                
         DC    AL1(0,L'TGPLJOB,0),AL2(TGPLJOB-TGPLD,RTAJOB-RTAK)                
         DC    AL1(0,L'TGPLDTE,DATRQ),AL2(TGPLDTE-TGPLD,RTABDTE-RTAK)           
         DC    AL1(0,L'TGPLNUM,0),AL2(TGPLNUM-TGPLD,RTABNUM-RTAK+2)             
         DC    AL1(0,L'TGPLRCV,0),AL2(TGPLRCV-TGPLD,RTARCV-RTAK)                
         DC    AL1(0,L'TGPLCST,0),AL2(TGPLCST-TGPLD,RTSCST-RTAK)                
         DC    AL1(0,L'TGPLSLS,0),AL2(TGPLSLS-TGPLD,RTASLS-RTAK)                
         DC    AL1(EOT)                                                         
*                                                                               
ICRPTAB  DC    AL2(INCBK-INCK)                                                  
         DC    AL1(0,L'ICPLCLI,0),AL2(ICPLCLI-ICPLD,INCKCLI-INCK)               
         DC    AL1(0,L'ICPLPRD,0),AL2(ICPLPRD-ICPLD,INCKPRD-INCK)               
         DC    AL1(0,L'ICPLJOB,0),AL2(ICPLJOB-ICPLD,INCKJOB-INCK)               
         DC    AL1(0,L'ICPLNUM,0),AL2(ICPLNUM-ICPLD,INCKNUM-INIK+2)             
         DC    AL1(0,L'ICPLWC,0),AL2(ICPLWC-ICPLD,INCKWC-INIK)                  
         DC    AL1(0,L'ICPLINC,AMTRQ),AL2(ICPLINC-ICPLD,INCINC-INIK)            
         DC    AL1(0,L'ICPLCRAC,0),AL2(ICPLCRAC-ICPLD,INCCRAC-INIK)             
         DC    AL1(0,L'ICPLPOST,AMTRQ),AL2(ICPLPOST-ICPLD,INCPOST-INIK)         
         DC    AL1(0,L'ICPLDRAC,0),AL2(ICPLDRAC-ICPLD,INCDRAC-INIK)             
         DC    AL1(EOT)                                                         
*                                                                               
SPRPTAB  DC    AL2(SIPBK-SIPK)                                                  
         DC    AL1(0,L'SPPLMED,0),AL2(SPPLMED-SPPLD,SIPKMED-SIPK)               
         DC    AL1(0,L'SPPLCLI,0),AL2(SPPLCLI-SPPLD,SIPKCLI-SIPK)               
         DC    AL1(0,L'SPPLPRD,0),AL2(SPPLPRD-SPPLD,SIPKPRD-SIPK)               
         DC    AL1(0,L'SPPLJOB,0),AL2(SPPLJOB-SPPLD,SIPKJOB-SIPK)               
         DC    AL1(0,L'SPPLDTE,DATR2Q),AL2(SPPLDTE-SPPLD,SIPBDTE-SIPK)          
         DC    AL1(0,L'SPPLNUM,0),AL2(SPPLNUM-SPPLD,SIPBNUM-SIPK+2)             
         DC    AL1(0,L'SPPLAMT,AMTRQ),AL2(SPPLAMT-SPPLD,SIPAMT-SIPK)            
         DC    AL1(0,L'SPPLAGJO,0),AL2(SPPLAGJO-SPPLD,SIPAGJOB-SIPK)            
         DC    AL1(0,L'SPPLCRAC,0),AL2(SPPLCRAC-SPPLD,SIPCRAC-SIPK)             
         DC    AL1(EOT)                                                         
*                                                                               
IXRPTAB  DC    AL2(0)                                                           
         DC    AL1(0,L'IXPLCLI,0),AL2(IXPLCLI-IXPLD,ICXKCLI-ICXK)               
         DC    AL1(0,L'IXPLPRD,0),AL2(IXPLPRD-IXPLD,ICXKPRD-ICXK)               
         DC    AL1(0,L'IXPLJOB,0),AL2(IXPLJOB-IXPLD,ICXKJOB-ICXK)               
         DC    AL1(0,L'IXPLSTUD,0),AL2(IXPLSTUD-IXPLD,ICXKSTUD-ICXK)            
         DC    AL1(0,L'IXPLAGYJ,0),AL2(IXPLAGYJ-IXPLD,ICXKAGYJ-ICXK)            
         DC    AL1(0,L'IXPLDTE,DATR2Q),AL2(IXPLDTE-IXPLD,ICXBDTE-ICXK)          
         DC    AL1(0,L'IXPLNUM,0),AL2(IXPLNUM-IXPLD,ICXBNUM-ICXK+2)             
         DC    AL1(EOT)                                                         
*                                                                               
PBRPTAB  DC    AL2(POBRBK-POBRK)                                                
         DC    AL1(0,L'PBPLCLI,0),AL2(PBPLCLI-PBPLD,POBRKCLI-POBRK)             
         DC    AL1(0,L'PBPLPRD,0),AL2(PBPLPRD-PBPLD,POBRKPRD-POBRK)             
         DC    AL1(0,L'PBPLJOB,0),AL2(PBPLJOB-PBPLD,POBRKJOB-POBRK)             
         DC    AL1(0,L'PBPLACC,0),AL2(PBPLACC-PBPLD,POBRACC-POBRK)              
         DC    AL1(0,L'PBPLWRK,0),AL2(PBPLWRK-PBPLD,POBRWRK-POBRK)              
         DC    AL1(0,L'PBPLNUM,0),AL2(PBPLNUM-PBPLD,POBRNUM-POBRK)              
         DC    AL1(0,L'PBPLAMT,AMTRQ),AL2(PBPLAMT-PBPLD,POBRAMT-POBRK)          
         DC    AL1(EOT)                                                         
*                                                                               
ASRPTAB  DC    AL2(ASPRBK-ASPRK)                                                
         DC    AL1(0,L'ASPLCLI,0),AL2(ASPLCLI-ASPLD,ASPRKCLI-ASPRK)             
         DC    AL1(0,L'ASPLPRD,0),AL2(ASPLPRD-ASPLD,ASPRKPRD-ASPRK)             
         DC    AL1(0,L'ASPLJOB,0),AL2(ASPLJOB-ASPLD,ASPRKJOB-ASPRK)             
         DC    AL1(0,L'ASPLACC,0),AL2(ASPLACC-ASPLD,ASPRACC-ASPRK)              
         DC    AL1(0,L'ASPLWRK,0),AL2(ASPLWRK-ASPLD,ASPRWRK-ASPRK)              
         DC    AL1(0,L'ASPLNUM,0),AL2(ASPLNUM-ASPLD,ASPRKNUM-ASPRK)             
         DC    AL1(0,L'ASPLAMT,AMTRQ),AL2(ASPLAMT-ASPLD,ASPRAMT-ASPRK)          
         DC    AL1(EOT)                                                         
*                                                                               
GBTPTAB  DC    AL2(GBCBK-GBCK)                                                  
         DC    AL1(0,L'GBCPPAY,AMTRQ),AL2(GBCPPAY-GBCPD,GBCPAY-GBCK)            
         DC    AL1(0,L'GBCPREC,AMTRQ),AL2(GBCPREC-GBCPD,GBCREC-GBCK)            
         DC    AL1(0,L'GBCPINC,AMTRQ),AL2(GBCPINC-GBCPD,GBCINC-GBCK)            
         DC    AL1(0,L'GBCPGRS,AMTRQ),AL2(GBCPGRS-GBCPD,GBCGRS-GBCK)            
         DC    AL1(0,L'GBCPCSD,AMTRQ),AL2(GBCPCSD-GBCPD,GBCCSD-GBCK)            
         DC    AL1(0,L'GBCPINT,AMTRQ),AL2(GBCPINT-GBCPD,GBCINT-GBCK)            
         DC    AL1(0,L'GBCPTIM,AMTRQ),AL2(GBCPTIM-GBCPD,GBCTIM-GBCK)            
         DC    AL1(0,L'GBCPOOP,AMTRQ),AL2(GBCPOOP-GBCPD,GBCOOP-GBCK)            
         DC    AL1(0,L'GBCPPRE,AMTRQ),AL2(GBCPPRE-GBCPD,GBCPRE-GBCK)            
         DC    AL1(0,L'GBCPRET,AMTRQ),AL2(GBCPRET-GBCPD,GBCRET-GBCK)            
         DC    AL1(EOT)                                                         
*                                                                               
REPMODE  DS    XL1                                                              
PSPRHDQ  EQU   X'80'     PRODUCT SUMMARY - CLIENT/PRODUCT HEADING               
PSMEHDQ  EQU   X'40'                       MEDIA HEADING                        
PSDTLQ   EQU   X'20'                       PRINT DETAIL                         
PSMETOQ  EQU   X'10'                       MEDIA TOTAL                          
PSPRTOQ  EQU   X'08'                       PRODUCT TOTAL                        
PSMECQ   EQU   PSMETOQ+PSMEHDQ+PSDTLQ      MEDIA CHANGE                         
PSALLQ   EQU   PSMECQ+(PSPRTOQ+PSPRHDQ)                                         
*                                                                               
CSCLHDQ  EQU   X'80'     CLIENT SUMMARY - CLIENT HEADING                        
CSMEHDQ  EQU   X'40'                      MEDIA HEADING                         
CSDTLQ   EQU   X'20'                      PRINT DETAIL                          
CSMETOQ  EQU   X'10'                      MEDIA TOTAL                           
CSCLTOQ  EQU   X'08'                      CLIENT TOTAL                          
CSMECQ   EQU   CSMETOQ+CSMEHDQ+CSDTLQ     MEDIA CHANGE                          
*                                                                               
GBPMHDQ  EQU   X'80'     GROUP BILL     - PRODUCTION/MEDIA HEADING              
GBMEHDQ  EQU   X'40'                    - MEDIA HEADING                         
GBJBHDQ  EQU   X'20'                    - JOB HEADING                           
GBWKTQ   EQU   X'10'                      WORKCODE TOTAL                        
GBJBTQ   EQU   X'08'                      JOB TOTAL                             
GBMETQ   EQU   X'04'                      MEDIA TOTAL                           
GBPOMQ   EQU   X'02'                      PRODUCTION/MEDIA TOTAL                
GBJOBCQ  EQU   GBJBTQ+GBJBHDQ+GBWKTQ JOB CHANGE                                 
*                                                                               
BRHDQ    EQU   X'80'     BILL REGISTER   - HEADING                              
BRDTLQ   EQU   X'40'                       PRINT DETAIL                         
BRCLTOQ  EQU   X'20'                       CLIENT TOTAL                         
BRCLCQ   EQU   BRCLTOQ+BRDTLQ              CLIENT CHANGE                        
*                                                                               
MSHDQ    EQU   X'80'     MEDIA SUMMARY   - HEADING                              
MSADDQ   EQU   X'40'                       ADD TO MEDIA TOTALS                  
MSMETOQ  EQU   X'20'                       MEDIA  TOTALS                        
MSRNTOQ  EQU   X'10'                       RUN TOTAL                            
MSMECQ   EQU   MSMETOQ+MSADDQ              MEDIA CHANGE                         
*                                                                               
IIHDQ    EQU   X'80'     INTERNAL INVOICE  HEADING                              
IIJOFIQ  EQU   X'40'                       FIRST FOR JOB                        
IIACFIQ  EQU   X'20'                       FIRST FOR ACCOUNT                    
IIDTLQ   EQU   X'10'                       PRINT DETAIL                         
IIJOTOQ  EQU   X'08'                       JOB TOTAL                            
IIRNTOQ  EQU   X'04'                       RUN TOTAL                            
IIJOCQ   EQU   IIJOTOQ+IIJOFIQ+IIACFIQ+IIDTLQ   CHANGE OF JOB                   
IIACCQ   EQU   IIACFIQ+IIDTLQ                   CHANGE OF ACCOUNT               
*                                                                               
TGHDQ    EQU   X'80'     TARGET ACCOUNTS - HEADING                              
TGDTLQ   EQU   X'40'                       PRINT DETAIL                         
*                                                                               
ICHDQ    EQU   X'80'     INTERNAL INCOME - HEADING                              
ICJOFIQ  EQU   X'40'                     - FIRST FOR JOB                        
ICDTLQ   EQU   X'20'                       PRINT DETAIL                         
ICJOTOQ  EQU   X'10'                     - JOB TOTAL                            
ICRNTOQ  EQU   X'08'                     - RUN TOTAL                            
ICJOCQ   EQU   ICJOTOQ+ICJOFIQ+ICDTLQ    - CHANGE OF JOB                        
*                                                                               
SPHDQ    EQU   X'80'     STUDIO INTERCOMP- HEADING                              
SPSTFIQ  EQU   X'40'                     - FIRST FOR STUDIO                     
SPDTLQ   EQU   X'20'                       PRINT DETAIL                         
SPSTTOQ  EQU   X'10'                     - STUDIO TOTAL                         
SPRNTOQ  EQU   X'08'                     - RUN TOTAL                            
*                                                                               
IXHDQ    EQU   X'80'     INTERCOMPANY EXCEPTION REPORT- HEADING                 
IXDTLQ   EQU   X'40'                                    PRINT DETAIL            
*                                                                               
PBHDQ    EQU   X'80'     PERCENT OF BILLING - HEADING                           
PBDTLQ   EQU   X'40'                          PRINT DETAIL                      
*                                                                               
ASHDQ    EQU   X'80'     ASP - HEADING                                          
ASADDQ   EQU   X'40'                                ADD AMOUNTS                 
ASBILQ   EQU   X'20'                                BILL TOTALS                 
ASLSTQ   EQU   X'10'                                LAST TIME                   
*                                                                               
GTHDQ    EQU   X'80'     GROUP BILL TRACE SUMMARY - HEADING                     
GTADDQ   EQU   X'40'                                ADD AMOUNTS                 
GTBILQ   EQU   X'20'                                BILL TOTALS                 
GTLSTQ   EQU   X'10'                                LAST TIME                   
GTDATQ   EQU   X'08'                                DATE CHANGE                 
*                                                                               
NBHDQ    EQU   X'80'     NON-BILLABLE REPORT- HEADING                           
NBDTLQ   EQU   X'40'                          PRINT DETAIL                      
*                                                                               
PAGEONE  DC    H'1'                                                             
*                                                                               
DTLCNT   DS    PL2                 COUNT DETAIL LINES                           
RPTNUM   DS    XL1                 REPORT NUMBER                                
LRPTNUM  DS    XL1                 LAST REPORT NUMBER                           
ADCBK    DS    F                   A(CONTROL BLOCK ENTRY)                       
ADLCBK   DS    F                   A(LAST CONTROL BLOCK ENTRY)                  
*                                                                               
NTOTROW  EQU   50                                                               
TOTROW   DS    (NTOTROW)PL(L'BK$)                                               
*                                                                               
NRECAP   DS    XL1                 NUMBER OF GROUP RECAP ENTRIES                
GRPDUE   DS    PL(L'BK$)                                                        
*                                                                               
         DS    0H                                                               
PRVD     DS    CL(L'CTXPRVD)       PROVINCE                                     
*                                                                               
ERRMSGS  DS    0XL71                                                            
         DC    AL1(ERRSTUD),CL70'STUDIO LINK ERROR'                             
         DC    AL1(ERR2MANY),CL70'TOO MANY TRANSACTIONS TO BILL'                
         DC    AL1(ERRACNF),CL70'ACCOUNT NOT FOUND'                             
         DC    AL1(ERRACNB),CL70'INVALID ACCOUNT FOR POSTING'                   
         DC    AL1(ERRTABO),CL70'TABLE OVERFLOW'                                
         DC    AL1(ERRMBGR),CL70'MUST BE A GROUP REQUEST'                       
         DC    AL1(EOT)                                                         
                                                                                
*                                                                               
ICXWTAB  DS    0CL60                                                            
ICXWMSG1 DC    CL60'NO INTERCOMPANY POSTING MADE - AGENCY JOB IS AN EXP*        
               ENSE JOB'                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
MXRECAP  EQU   25                  MAX NUMBER OF RECAP ENTRIES                  
         DS    0H                                                               
GBRTAB   DS    (MXRECAP)XL(GBRCLNQ)                                             
*                                                                               
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET UP A PRINT LINE FROM PRINT CONTROL TABLE             *         
*  PARM 1  BYTE 0     C'T'  TOTALS ONLY                               *         
*          BYTE 1-3   A(PRINT CONTROL TABLE)                          *         
*       2  BYTE 1-3   A(RECORD)                                       *         
***********************************************************************         
SETPL    NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)         R2=A(PRINT CONTROL TABLE)                    
         MVC   BKDSP,0(R2)         SAVE DISPLACEMENT TO BUCKETS                 
         LA    R2,2(R2)                                                         
         USING PLCD,R2                                                          
         MVC   TOTFLG,0(R1)        SET POSSIBLE 'TOTAL FLAG'                    
         LR    R6,R3                                                            
         CLI   TOTFLG,C'T'         FOR TOTALS R3=A(ACCUMS)                      
         BE    *+8                                                              
         LA    R3,SKVKEY-SORTD(R3) FOR DETAIL R3=A(VARIABLE KEY/DATA)           
*                                                                               
SETPL3   CLI   PLOPT,0             TEST PRINTING OPTIONS                        
         BE    SETPL4              NO, OK TO PRINT                              
         TM    PLOPT,PLNOTX        TEST, SKIP IF TAX                            
         BNO   SETPL3A             NO,                                          
         TM    PLOPT,PLTXNCD       TEST PRINTING TAX                            
         BO    SETPL23             YES, SKIP ENTRY                              
*                                                                               
SETPL3A  TM    PLOPT,PLTXNCD       TAX - NOT CD                                 
         BNO   *+12                                                             
         TM    TXOPT,PLTXNCD                                                    
         BNO   SETPL23                                                          
         TM    PLOPT,PLTX2ND       TAX ON SECOND                                
         BNO   *+12                                                             
         TM    TXOPT,PLTX2ND                                                    
         BNO   SETPL23                                                          
*                                                                               
SETPL4   CLI   TOTFLG,C'T'         TEST TOTALS ONLY                             
         BNE   SETPL5                                                           
         CLI   PLCROUT,AMTRQ       TEST AMOUNT FIELD                            
         BE    SETPL5                                                           
         CLI   PLCROUT,GSTRQ                                                    
         BE    SETPL5                                                           
         CLI   PLCROUT,PSTRQ                                                    
         BNE   SETPL23             NO, SKIP IT                                  
*                                                                               
SETPL5   LLC   R4,PLCLEN           R4=LENGTH                                    
         SR    R5,R5                                                            
         ICM   R5,3,PLCCOL         R5=COLUMN                                    
         LA    RF,P                                                             
         AR    R5,RF                                                            
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,PLCSRC                                                      
         AR    R6,R3               R6=SOURCE FIELD IN SORT RECORD               
         CLI   TOTFLG,C'T'         TEST TOTAL LINE                              
         BNE   SETPL6              NO,                                          
         XR    RF,RF                                                            
         ICM   RF,3,BKDSP          DISPLACEMENT TO BUCKETS                      
         SR    R6,RF               R6 NOW POINTS TO TOTAL BUCKET                
         B     SETPL9                                                           
*                                                                               
SETPL6   CLI   PLCROUT,0           TEST SPECIAL ROUTINE                         
         BNE   SETPL7                                                           
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R6)                                                    
         B     SETPL23                                                          
*                                                                               
SETPL7   CLI   PLCROUT,DATRQ       TEST DATE CONVERSION                         
         BNE   SETPL8                                                           
         GOTO1 DATCON,DMCB,(1,0(R6)),(6,0(R5))                                  
         B     SETPL23                                                          
*                                                                               
SETPL8   CLI   PLCROUT,DATR2Q      TEST DATE CONVERSION                         
         BNE   SETPL9                                                           
         GOTO1 DATCON,DMCB,(1,0(R6)),(8,0(R5))                                  
         B     SETPL23                                                          
*                                                                               
SETPL9   CLI   PLCROUT,GSTRQ       TEST GST                                     
         BE    SETPL17                                                          
*                                                                               
SETPL11  CLI   PLCROUT,PSTRQ       TEST PST                                     
         BE    SETPL17                                                          
*                                                                               
SETPL15  CLI   PLCROUT,AMTRQ       AMOUNT CONVERSION                            
         BE    *+6                                                              
         DC    H'0'                                                             
SETPL17  CURED (P8,0(R6)),((R4),0(R5)),2,MINUS=YES,ZERO=BLANK                   
         ORG   *-2                                                              
         TM    CURFLG,CURPZRO      NEED TO PRINT ZERO'S ?                       
         BNO   *+8                                                              
         NI    CURPEDT1-CURPARMD(R1),ALL-(CURPZERB) REMOVE ZERO=BLANK           
         TM    CURFLG,CURPAWC      PRINT AMOUNT WITH COMMAS                     
         BNO   SETPL18                                                          
         ICM   R4,15,0(R6)         FIRST FOUR BYTES OF AMOUNT                   
         SRL   R4,4                KEEP 7 HIGH ORDER POSITIONS                  
         LTR   R4,R4                                                            
         BNZ   SETPL18             AMOUNT MORE THAN 1 MILLION                   
         OI    CURPEDT1-CURPARMD(R1),CURPCOMY  OK,TO PRINT COMMAS               
*                                                                               
SETPL18  BASR  RE,RF               GO TO CURED                                  
         TM    TXOPT,PLTXNCD       TEST PRINTING TAX - NOT CD                   
         BO    SETPL22             YES, SKIP THE GST= PREFIX                    
         CLI   PLCROUT,GSTRQ       TEST GST                                     
         BNE   SETPL19                                                          
         CP    0(8,R6),PZERO                                                    
         BE    SETPL22                                                          
         L     RF,ADICO                                                         
         LA    RF,DD@VAT1-DICD(RF) GST                                          
         B     SETPL20                                                          
*                                                                               
SETPL19  CLI   PLCROUT,PSTRQ       TEST PST                                     
         BNE   SETPL22                                                          
         CP    0(8,R6),PZERO                                                    
         BE    SETPL22                                                          
         LARL  RF,PRVD             PROVINCE CODE                                
*                                                                               
SETPL20  CLI   0(R5),C' '          GST/PST=999.99                               
         BH    *+12                                                             
         LA    R5,1(R5)                                                         
         B     SETPL20                                                          
         SHI   R5,4                                                             
         MVC   0(3,R5),0(RF)                                                    
         MVI   3(R5),C'='                                                       
*                                                                               
SETPL22  DS    0H                                                               
SETPL23  LA    R2,PLCLNQ(R2)                                                    
         CLI   0(R2),EOT                                                        
         BNE   SETPL3                                                           
         J     XIT                                                              
         DROP  R2                                                               
*                                                                               
TOTFLG   DS    CL1                 TOTAL FLAG                                   
BKDSP    DS    H                   DISPLACEMENT TO BUCKETS                      
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE THE GROUP BILLING RECORD                                     *         
***********************************************************************         
UPGBR    NTR1  BASE=*,LABEL=*                                                   
         L     R9,AJXBLK                                                        
         USING JXBLKD,R9                                                        
         L     R4,AIO1             R3=A(GROUP BILL SORT RECORD)                 
         USING SORTD,R4                                                         
*                                                                               
         CLI   FORMCDE,C'0'        TEST GROUP FORMAT                            
         JE    XIT                 NO, SKIP IT                                  
*                                                                               
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         BZ    UPGBR9              NO,                                          
*                                                                               
         LA    R2,DKEY                                                          
         USING GRBRECD,R2                                                       
         XC    GRBKEY,GRBKEY                                                    
         MVI   GRBKTYP,GRBKTYPQ                                                 
         MVI   GRBKSUB,GRBKSUBQ                                                 
         MVC   GRBKCPY,CPY         COMPANY                                      
         MVC   GRBKCLI,CLI         CLIENT                                       
         MVC   GRBKBILN,GBCKOBN    ORIGINAL BILL NUMBER                         
         XR    RF,RF                                                            
         ICM   RF,7,GBCKOBD        ORIGINAL DATE(PWOS)                          
         LNR   RF,RF                                                            
         STCM  RF,7,GRBKBILD       DATE COMPLEMENT                              
         MVC   AIO,AIO3                                                         
*                                                                               
         GOTO1 ADMGR,ACCTHIQ                                                    
         L     R2,AIO                                                           
         CLC   GRBKEY,DKEY         TEST RECORD FOUND                            
         JNE   XIT                 NO,                                          
*                                                                               
         OI    GRBRSTA,GRBSUNBQ    INDICATE UNBILLED                            
*                                                                               
         XR    R0,R0                                                            
         LA    R3,ACCORFST(R2)                                                  
         USING BDAELD,R3                                                        
UPGBR3   CLI   0(R3),BDAELQ                                                     
         BE    UPGBR5                                                           
         CLI   0(R3),0                                                          
         JE    XIT                                                              
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     UPGBR3                                                           
*                                                                               
UPGBR5   MVC   BDAUNUM,GBCKBNUM   BILL NUMBER                                   
         MVC   BDAUNDT,GBCKBDTE   BILL DATE                                     
         GOTO1 ADMGR,ACCTWRTQ      WRITE DIRECTORY                              
         J     XIT                                                              
*                                                                               
         USING GRBRECD,R2                                                       
UPGBR9   L     R2,AIO3                                                          
         MVC   AIO,AIO3                                                         
         XC    GRBKEY(100),GRBKEY                                               
         MVI   GRBKTYP,GRBKTYPQ                                                 
         MVI   GRBKSUB,GRBKSUBQ                                                 
         MVC   GRBKCPY,CPY         COMPANY                                      
         MVC   GRBKCLI,CLI         CLIENT                                       
         MVC   GRBKBILN,GBCKBNUM   BILL NUMBER                                  
         XR    RF,RF                                                            
         ICM   RF,7,GBCKBDTE                                                    
         LNR   RF,RF                                                            
         STCM  RF,7,GRBKBILD       BILL DATE                                    
         OI    GRBRSTA,GRBSPTSQ                                                 
*                                                                               
         L     R3,AELMNT                                                        
         USING BDAELD,R3                                                        
         XC    BDAEL(BDALNQ),BDAEL                                              
         MVI   BDAEL,BDAELQ                                                     
         MVI   BDALN,BDALNQ                                                     
         MVC   BDAFORM,FORMCDE                                                  
         MVI   BDALVL,C' '                                                      
         TM    RQLV,LVA                                                         
         BNO   *+8                                                              
         MVI   BDALVL,C'C'                                                      
         TM    RQLV,LVB                                                         
         BNO   *+8                                                              
         MVI   BDALVL,C'P'                                                      
         MVC   BDAFDTE,TTLSDAY2                                                 
         MVC   BDATDTE,TTLEDAY2                                                 
         MVI   BDATYPE,C'C'                                                     
         MVC   BDARUN,TODAY1                                                    
         MVC   BDADUE,DUEDT1                                                    
         MVC   BDARCVA,RECVAC                                                   
         MVC   BDARCVO,OFC                                                      
*                                                                               
         ZAP   BDAPAY,GBCPAY                                                    
         ZAP   BDAREC,GBCREC                                                    
         ZAP   BDAINC,GBCINC                                                    
         ZAP   BDAGRS,GBCGRS                                                    
         ZAP   BDACSH,GBCCSD                                                    
         ZAP   BDAINT,GBCINT                                                    
         ZAP   BDATME,GBCTIM                                                    
         ZAP   BDAOOP,GBCOOP                                                    
         ZAP   BDAPRV,GBCPRV                                                    
         ZAP   BDAPRE,GBCPRE                                                    
         ZAP   BDAPRE+L'BDAPRE(L'BDAPRE),GBCRET                                 
         ZAP   BDAPRE+(L'BDAPRE*2)(L'BDAPRE),GBCPRT                             
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO,AELMNT,0                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 ADMGR,ACCADDQ                                                    
         J     XIT                                                              
         DROP  R2,R3,R4,R9                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* TABLE TO MANAGE REPORT CONTROL BREAKS                               *         
***********************************************************************         
         DS    0H                                                               
CBINDX   DS    XL3                 INDEX TO CONTROL BREAK TABLE                 
         ORG   CBINDX+L'CBINDX                                                  
         DC    AL1(PRDSUM),AL2(CBPS-RUNREP)                                     
         DC    AL1(CLISUM),AL2(CBCS-RUNREP)                                     
         DC    AL1(GRPSUM),AL2(CBGS-RUNREP)                                     
         DC    AL1(BILREG),AL2(CBBL-RUNREP)                                     
         DC    AL1(MEDSUM),AL2(CBMS-RUNREP)                                     
         DC    AL1(INTINV),AL2(CBII-RUNREP)                                     
         DC    AL1(TARGET),AL2(CBTG-RUNREP)                                     
         DC    AL1(INTINC),AL2(CBIC-RUNREP)                                     
         DC    AL1(STUINP),AL2(CBSP-RUNREP)                                     
         DC    AL1(INPEXC),AL2(CBIX-RUNREP)                                     
         DC    AL1(POBREG),AL2(CBPB-RUNREP)                                     
         DC    AL1(ASPREG),AL2(CBAS-RUNREP)                                     
         DC    AL1(GBTRAS),AL2(CBGT-RUNREP)                                     
         DC    AL1(NONBIL),AL2(CBNB-RUNREP)                                     
*                                                                               
*                                          PRODUCT SUMMARY                      
CBPS     DC    AL1(PRDSUM),AL1(CBPSX-CBPS)                                      
         DC    AL1(PSPRHDQ+PSMEHDQ+PSDTLQ)                                      
         DC    AL1(PSMETOQ+PSPRTOQ)                                             
         DC    AL1(PSDTLQ)                                                      
         DC    AL2(PRDREP-RUNREP)                                               
         DC    AL1((CBPSX-*)/L'CBKL)                                            
         DC    AL1(CPSKGRP-CPSK,L'CPSKGRP,PSALLQ)                               
         DC    AL1(CPSKPRME-CPSK,L'CPSKPRME,PSMECQ)                             
CBPSX    EQU   *                                                                
*                                                                               
*                                          CLIENT SUMMARY                       
CBCS     DC    AL1(CLISUM,CBCSX-CBCS)                                           
         DC    AL1(CSCLHDQ+CSMEHDQ+CSDTLQ)                                      
         DC    AL1(CSMETOQ+CSCLTOQ)                                             
         DC    AL1(CSDTLQ)                                                      
         DC    AL2(CLIREP-RUNREP)                                               
         DC    AL1((CBCSX-*)/L'CBKL)                                            
         DC    AL1(CPSKCLME-CPSK,L'CPSKCLME,CSMECQ)                             
CBCSX    EQU   *                                                                
*                                                                               
*                                          GROUP SUMMARY                        
CBGS     DC    AL1(GRPSUM,CBGSX-CBGS)                                           
         DC    AL1(GBPMHDQ+GBJBHDQ)                                             
         DC    AL1(GBJBTQ+GBMETQ+GBPOMQ+GBWKTQ)                                 
         DC    AL1(0)                                                           
         DC    AL2(GBSREP-RUNREP)                                               
         DC    AL1((CBGSX-*)/L'CBKL)                                            
         DC    AL1(GBSKPOM-BILK,L'GBSKPOM,GBPOMQ+GBMETQ+GBJOBCQ)                
         DC    AL1(GBSKMED-BILK,L'GBSKMED,GBMETQ+GBJOBCQ)                       
         DC    AL1(GBSKJOB-BILK,L'GBSKJOB,GBJOBCQ)                              
         DC    AL1(GBSKWKC-BILK,L'GBSKWKC,GBWKTQ)                               
CBGSX    EQU   *                                                                
*                                                                               
*                                            BILLING REGISTER                   
CBBL     DC    AL1(BILREG),AL1(CBBLX-CBBL)                                      
         DC    AL1(BRHDQ+BRDTLQ)                                                
         DC    AL1(BRCLTOQ)                                                     
         DC    AL1(BRDTLQ)                                                      
         DC    AL2(BLRREP-RUNREP)                                               
         DC    AL1((CBBLX-*)/L'CBKL)                                            
         DC    AL1(BLRKCLI-BLRK,L'BLRKCLI,BRCLCQ)                               
CBBLX    EQU   *                                                                
*                                                                               
*                                            MEDIA SUMMARY                      
CBMS     DC    AL1(MEDSUM),AL1(CBMSX-CBMS)                                      
         DC    AL1(MSHDQ+MSADDQ)                                                
         DC    AL1(MSMETOQ+MSRNTOQ)                                             
         DC    AL1(MSADDQ)                                                      
         DC    AL2(MSREP-RUNREP)                                                
         DC    AL1((CBMSX-*)/L'CBKL)                                            
         DC    AL1(MSRKCDE-MSRK,L'MSRKCDE,MSMECQ)                               
CBMSX    EQU   *                                                                
*                                                                               
*                                            INTERNAL INVOICE                   
CBII     DC    AL1(INTINV),AL1(CBIIX-CBII)                                      
         DC    AL1(IIHDQ+IIJOFIQ+IIACFIQ+IIDTLQ)                                
         DC    AL1(IIJOTOQ+IIRNTOQ)                                             
         DC    AL1(IIDTLQ)                                                      
         DC    AL2(IIREP-RUNREP)                                                
         DC    AL1((CBIIX-*)/L'CBKL)                                            
         DC    AL1(INIKJOB-INIK,L'INIKJOB,IIJOCQ)                               
         DC    AL1(INIACT-INIK,L'INIACT,IIACCQ)                                 
CBIIX    EQU   *                                                                
*                                                                               
*                                            TARGET ACCOUNTS                    
CBTG     DC    AL1(TARGET),AL1(CBTGX-CBTG)                                      
         DC    AL1(TGHDQ+TGDTLQ)                                                
         DC    AL1(0)                                                           
         DC    AL1(TGDTLQ)                                                      
         DC    AL2(TARREP-RUNREP)                                               
         DC    AL1((CBTGX-*)/L'CBKL)                                            
CBTGX    EQU   *                                                                
*                                                                               
*                                            INTERNAL INCOME REGISTER           
CBIC     DC    AL1(INTINC),AL1(CBICX-CBIC)                                      
         DC    AL1(ICHDQ+ICJOFIQ+ICDTLQ)                                        
         DC    AL1(ICJOTOQ+ICRNTOQ)                                             
         DC    AL1(ICDTLQ)                                                      
         DC    AL2(ICREP-RUNREP)                                                
         DC    AL1((CBICX-*)/L'CBKL)                                            
         DC    AL1(INCKCLI-INCK,INCKJLNQ,ICJOCQ)                                
CBICX    EQU   *                                                                
*                                                                               
*                                            STUDIO INTERCOMPANY                
CBSP     DC    AL1(STUINP),AL1(CBSPX-CBSP)                                      
         DC    AL1(SPHDQ+SPSTFIQ+SPDTLQ)                                        
         DC    AL1(SPSTTOQ+SPRNTOQ)                                             
         DC    AL1(SPDTLQ)                                                      
         DC    AL2(SPREP-RUNREP)                                                
         DC    AL1((CBSPX-*)/L'CBKL)                                            
CBSPX    EQU   *                                                                
*                                                                               
*                                       INTERCOMPANY POSTING EXCEPTION          
CBIX     DC    AL1(INPEXC),AL1(CBIXX-CBIX)                                      
         DC    AL1(IXHDQ+IXDTLQ)                                                
         DC    AL1(0)                                                           
         DC    AL1(IXDTLQ)                                                      
         DC    AL2(ICXREP-RUNREP)                                               
         DC    AL1((CBIXX-*)/L'CBKL)                                            
CBIXX    EQU   *                                                                
*                                                                               
*                                       PERCENT OF BILLING                      
CBPB     DC    AL1(POBREG),AL1(CBPBX-CBPB)                                      
         DC    AL1(PBHDQ+PBDTLQ)                                                
         DC    AL1(0)                                                           
         DC    AL1(PBDTLQ)                                                      
         DC    AL2(POBREP-RUNREP)                                               
         DC    AL1((CBPBX-*)/L'CBKL)                                            
CBPBX    EQU   *                                                                
*                                                                               
*                                       ASP                                     
CBAS     DC    AL1(ASPREG),AL1(CBASX-CBAS)                                      
         DC    AL1(ASHDQ)                                                       
         DC    AL1(ASBILQ+ASLSTQ)                                               
         DC    AL1(ASADDQ)                                                      
         DC    AL2(ASPREP-RUNREP)                                               
         DC    AL1((CBGTX-*)/L'CBKL)                                            
         DC    AL1(ASPRDATA-ASPRK,ASPRKLNQ,ASBILQ)                              
CBASX    EQU   *                                                                
*                                                                               
*                                          GROUP BILL TRACE SUMMARY             
CBGT     DC    AL1(GBTRAS,CBGTX-CBGT)                                           
         DC    AL1(GTHDQ)                                                       
         DC    AL1(GTBILQ+GTLSTQ)                                               
         DC    AL1(GTADDQ)                                                      
         DC    AL2(GBTREP-RUNREP)                                               
         DC    AL1((CBGTX-*)/L'CBKL)                                            
         DC    AL1(GBCKCLI-GBCK,GBCKBK1,GTDATQ+GTBILQ)                          
         DC    AL1(GBCKCLI-GBCK,GBCKBK2,GTBILQ)                                 
CBGTX    EQU   *                                                                
*                                                                               
*                                            NON-BILLABLE REPORT                
CBNB     DC    AL1(NONBIL),AL1(CBNBX-CBNB)                                      
         DC    AL1(NBHDQ+NBDTLQ)                                                
         DC    AL1(0)                                                           
         DC    AL1(NBDTLQ)                                                      
         DC    AL2(NBJREP-RUNREP)                                               
         DC    AL1((CBNBX-*)/L'CBKL)                                            
CBNBX    EQU   *                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TRACE ROUTINES                                                      *         
***********************************************************************         
TRCE     NMOD1 0,**TRCE,RA                                                      
         L     RC,BASERC           RESTORE RC                                   
         MVC   P,SPACES                                                         
         L     RF,ADMASTC                                                       
         MVC   TPARM6,MCVPRINT-MASTD(RF)                                        
         MVI   TPARM6,C'P'                                                      
*                                                                               
         SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     TRCTRN              TRACE BILLABLE TRANSACTIONS                  
         B     TRCPRO                    PRORATA BLOCK                          
         B     TRCFMT                    FORMAT KEYS & RECORDS                  
         B     TRCTSR              RECORDS TO SORT                              
         B     TRCFSR              RECORDS FROM SORT                            
         B     TRCBFM              BILL FORMAT RECORDS                          
*                                                                               
TRCTRN   L     R2,CBTRN            R2=A(CURRENT ITEM)                           
         LA    RF,BTRNLQ           RF=LENGTH                                    
         LA    R1,PBTRN            R1=A(EYE CATCHER)                            
         BAS   RE,TRACE                                                         
*                                                                               
         LA    R3,TRCTAB                                                        
         LA    R4,P+1                                                           
TRCTRN3  SR    RF,RF                                                            
         ICM   RF,3,7(R3)                                                       
         AR    RF,R2               RF=A(PACKED DATA FIELD)                      
         LLC   R1,9(R3)            R1=LENGTH                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         ZAP   DUB,0(0,RF)                                                      
         CP    DUB,PZERO           SKIP ZERO AMOUNTS                            
         BE    TRCTRN7                                                          
         IC    R1,6(R3)            LENGTH OF CONSTANT                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       CONSTANT TO PRINT LINE                       
         LA    R4,1(R1,R4)                                                      
         CLC   0(4,R3),=C'RATE'                                                 
         BE    TRCTRN5                                                          
         EDIT  (P8,DUB),(13,(R4)),2,MINUS=YES,ALIGN=LEFT                        
         B     TRCTRN6                                                          
*                                                                               
TRCTRN5  EDIT  (P8,DUB),(13,(R4)),4,MINUS=YES,ALIGN=LEFT                        
*                                                                               
TRCTRN6  CLI   0(R4),C' '                                                       
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         LA    R4,1(R4)                                                         
*                                                                               
TRCTRN7  LA    R3,L'TRCTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   TRCTRN3                                                          
         CLC   P,SPACES                                                         
         BE    TRCX                                                             
         GOTO1 ACREPORT                                                         
         B     TRCX                                                             
*                                                                               
*                                  DUMP PRORATA BLOCK                           
TRCPRO   L     R4,AMONACC                                                       
         L     R2,ACMAPROB-ACMD(R4)                                             
         LA    RF,PR$LNQ           RF=LENGTH                                    
         LA    R1,PPROR                                                         
         BAS   RE,TRACE                                                         
         B     TRCX                                                             
*                                                                               
TRCFMT   L     R2,DMCB             KEY                                          
         LA    RF,PDRLNQ                                                        
         LA    R1,PPKEY                                                         
         BAS   RE,TRACE                                                         
         L     R2,DMCB+4           RECORD                                       
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)                                                       
         LA    R1,PPREC                                                         
         BAS   RE,TRACE                                                         
         B     TRCX                                                             
*                                                                               
TRCTSR   LA    R1,PSTO             TRACE RECORDS TO SORT                        
         B     *+8                                                              
TRCFSR   LA    R1,PSFR             TRACE RECORDS FROM SORT                      
         L     R2,ASRTWK                                                        
         USING SORTD,R2                                                         
         SR    RF,RF                                                            
         ICM   RF,3,SRLEN          RECORD LENGTH                                
         CLI   SRDATA,BEDKTYPQ     EDIT & FORMAT RECORDS                        
         BE    TRCSR3                                                           
         BAS   RE,TRACE                                                         
         B     TRCX                                                             
*                                                                               
TRCSR3   LA    RF,SKLNQ           DUMP KEY                                      
         BAS   RE,TRACE                                                         
         GOTO1 ACDITTO,DMCB,SRDATA,ACRECTYP,HEXOUT,ADCPRINT,PRINTER             
         B     TRCX                                                             
*                                                                               
TRCBFM   GOTO1 ACDITTO,DMCB,AIO,ACRECTYP,HEXOUT,ADCPRINT,PRINTER                
         B     TRCX                                                             
*                                                                               
TRCX     J     XITE                                                             
*                                                                               
TRACE    LR    R0,RE               COMMON PRNTBL ROUTINE                        
         STCM  R2,15,TPARM2        SOURCE                                       
         STCM  RF,15,TPARM4        LENGTH                                       
         MVC   TPARM1,0(R1)        EYE CATCHER                                  
         GOTO1 PRNTBL,TPARM                                                     
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
BDSP     EQU   BTRNBK-BTRND                                                     
GDSP     EQU   BGSTBK-BTRND                                                     
PDSP     EQU   BPSTBK-BTRND                                                     
TRCTAB   DS    0XL10                                                            
         DC    CL6'NET=  ',AL1(3),AL2((JOBNET-JOBD)+BDSP),AL1(BKLQ)             
         DC    CL6'COMM= ',AL1(4),AL2((JOBCOM-JOBD)+BDSP),AL1(BKLQ)             
         DC    CL6'GROSS=',AL1(5),AL2((JOBGRS-JOBD)+BDSP),AL1(BKLQ)             
         DC    CL6'CD=   ',AL1(2),AL2((JOBCD-JOBD)+BDSP),AL1(BKLQ)              
         DC    CL6'HOURS=',AL1(5),AL2((JOBHRS-JOBD)+BDSP),AL1(BKLQ)             
         DC    CL6'RATE= ',AL1(4),AL2(BTRNCOMR-BTRND),AL1(L'BTRNCOMR)           
         DC    CL6'GST=  ',AL1(3),AL2((CTAXTAX-CTAXD)+GDSP),AL1(BKLQ)           
         DC    CL6'PST=  ',AL1(3),AL2((CTAXTAX-CTAXD)+PDSP),AL1(BKLQ)           
         DC    X'FF'                                                            
*                                                                               
TPARM    DS    0F                                                               
TPARM1   DC    AL4(0)              LENGTH/EYE CATCHER                           
TPARM2   DC    AL4(0)              RECORD                                       
TPARM3   DC    CL4'DUMP'                                                        
TPARM4   DC    AL4(0)              LENGTH                                       
TPARM5   DC    AL1(2),AL3(P2D)                                                  
TPARM6   DC    A(0)                                                             
*                                                                               
PBTRN    DC    AL1(4),AL3(*+3),C'BTRN'                                          
PPROR    DC    AL1(7),AL3(*+3),C'PRORATA'                                       
PPKEY    DC    AL1(4),AL3(*+3),C'PKEY'                                          
PPREC    DC    AL1(4),AL3(*+3),C'PREC'                                          
PSTO     DC    AL1(7),AL3(*+3),C'TO SORT'                                       
PSFR     DC    AL1(9),AL3(*+3),C'FROM SORT'                                     
P2D      DC    C'2D'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* IO COUNT DISPLAY                                                    *         
*  P1 =  0(ENTER) X(EXIT)                                                       
*        A(MODE)                                                                
***********************************************************************         
IOCNTR   NMOD1 0,**IOCN,RA                                                      
         L     RC,BASERC           RESTORE RC                                   
         MVC   P,SPACES                                                         
         LA    R6,P                                                             
         USING IOCNTD,R6                                                        
         MVC   IOENX,IOENTRQ       SET ENTER OR EXIT                            
         CLI   0(R1),C'X'                                                       
         BNE   *+10                                                             
         MVC   IOENX,IOEXITQ                                                    
         L     R2,0(R1)            R2=A(MODE)                                   
         LA    RF,MODETAB                                                       
         CLC   0(1,R2),0(RF)       MATCH MODE TO TABLE                          
         BE    *+16                                                             
         LA    RF,L'MODETAB(RF)                                                 
         CLI   0(RF),EOT                                                        
         BNE   *-18                                                             
         MVC   IOMODE,1(RF)        MODE TO PRINT                                
*                                                                               
         MVC   IOCNTC,IOCNTQ       DMCOUNT=00                                   
         L     RF,AMONACC                                                       
         L     RF,ACMDMCNT-ACMD(RF)                                             
         ICM   R0,15,0(RF)                                                      
         EDIT  (R0),IOCNT,ALIGN=LEFT                                            
         L     R2,12(R1)           R2=A(IO)                                     
         MVC   IOSRTC,IOSRTQ       START IO=                                    
         L     RF,ADMASTC                                                       
         ICM   R0,15,MCACTIOS-MASTD(RF)                                         
         EDIT  (R0),IOSRT,ALIGN=LEFT                                            
         GOTO1 ACREPORT                                                         
         J     XIT                                                              
*                                                                               
IOENTRQ  DC    C'ENTER'                                                         
IOEXITQ  DC    C'EXIT '                                                         
IOCNTQ   DC    C'DMCOUNT='                                                      
IOSRTQ   DC    C'START IO='                                                     
*                                                                               
MODETAB  DS    XL9                                                              
         DC    AL1(RUNFRST),C'RUNFRST '                                         
         DC    AL1(REQFRST),C'REQFRST '                                         
         DC    AL1(COMPFRST),C'COMPFRST'                                        
         DC    AL1(UNITFRST),C'UNITFRST'                                        
         DC    AL1(LEDGFRST),C'LEDGFRST'                                        
         DC    AL1(LEVAFRST),C'LEVAFRST'                                        
         DC    AL1(LEVBFRST),C'LEVBFRST'                                        
         DC    AL1(LEVCFRST),C'LEVCFRST'                                        
         DC    AL1(ACCFRST),C'ACCFRST '                                         
         DC    AL1(SBACFRST),C'SBACFRST'                                        
         DC    AL1(RUNLAST),C'RUNLAST '                                         
         DC    AL1(REQLAST),C'REQLAST '                                         
         DC    AL1(COMPLAST),C'COMPLAST'                                        
         DC    AL1(UNITLAST),C'UNITLAST'                                        
         DC    AL1(LEDGLAST),C'LEDGLAST'                                        
         DC    AL1(LEVALAST),C'LEVALAST'                                        
         DC    AL1(LEVBLAST),C'LEVBLAST'                                        
         DC    AL1(LEVCLAST),C'LEVCLAST'                                        
         DC    AL1(ACCLAST),C'ACCLAST '                                         
         DC    AL1(SBACLAST),C'SBACLAST'                                        
         DC    AL1(PROCACC),C'PROCACC '                                         
         DC    AL1(PROCHIST),C'PROCHIST'                                        
         DC    AL1(PROCTRNS),C'PROCTRNS'                                        
         DC    AL1(ANALFRST),C'ANALFRST'                                        
         DC    AL1(ANALLAST),C'ANALLAST'                                        
         DC    AL1(PROCLEVA),C'PROCLEVA'                                        
         DC    AL1(PROCLEVB),C'PROCLEVB'                                        
         DC    AL1(PROCLEVC),C'PROCLEVC'                                        
         DC    AL1(PROCEST),C'PROCEST '                                         
         DC    AL1(PROCLEVD),C'PROCLEVD'                                        
         DC    AL1(PROCSBAC),C'PROCSBAC'                                        
         DC    AL1(PROCORD),C'PROCORD '                                         
         DC    AL1(PROCACL),C'PROCACL '                                         
         DC    AL1(OFFLAST),C'OFFLAST '                                         
         DC    AL1(PROCOFA),C'PROCOFA '                                         
         DC    AL1(OFALAST),C'OFALAST '                                         
         DC    AL1(WRITOFA),C'WRITOFA '                                         
         DC    AL1(OFFIRST),C'OFFIRST '                                         
         DC    AL1(PROCCBUK),C'PROCCBUK'                                        
         DC    AL1(PROCOBUK),C'PROCOBUK'                                        
         DC    AL1(OFACFRST),C'OFACFRST'                                        
         DC    AL1(OFACLAST),C'OFACLAST'                                        
         DC    AL1(SCACFRST),C'SCACFRST'                                        
*                                                                               
         DC    AL1(JOBBERQ),C'JOBBER  '                                         
         DC    AL1(AJAXQ),C'AJAX  '                                             
         DC    AL1(GETOPTQ),C'GETOPT  '                                         
*                                                                               
         DC    AL1(EOT),C'OTHER   '                                             
         DROP  RA,RB,R6                                                         
         LTORG                                                                  
         EJECT                                                                  
         GETEL R3,DATADISP,ELCODE                                               
GETELN   AH    R3,NEWDISP                                                       
         J     FIRSTEL                                                          
*                                                                               
XITE     LR    RE,RB                                                            
XIT      CR    RE,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE PRODUCT SUMMARY  DETAIL LINE                   *           
**********************************************************************          
PSPLD    DSECT                                                                  
         DS    CL1                                                              
PSPLJOB  DS    CL6                 JOB                                          
         DS    CL4                                                              
PSPLOTH  DS    CL9                 OTHER                                        
         DS    CL8                                                              
PSPLBIL  DS    CL7                 BILL NUMBER                                  
         DS    CL4                                                              
PSPLNET  DS    CL13                NET                                          
         DS    CL4                                                              
PSPLCOM  DS    CL12                COM                                          
         DS    CL3                                                              
PSPLGRS  DS    CL13                GROSS                                        
         DS    CL1                                                              
PSPLGST  DS    CL13                GST                                          
         ORG   PSPLD+((PSECOND+89)-P)                                           
PSPLPST  DS    CL12                PST ON PSECOND                               
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE CLIENT SUMMARY  DETAIL LINE                     *          
**********************************************************************          
CSPLD    DSECT                                                                  
         DS    CL1                                                              
CSPLPRD  DS    CL3                 PRODUCT                                      
CSPLJOB  DS    CL6                 JOB                                          
         DS    CL1                                                              
CSPLOTH  DS    CL9                 OTHER                                        
         DS    CL8                                                              
CSPLBIL  DS    CL7                 BILL NUMBER                                  
         DS    CL4                                                              
CSPLNET  DS    CL13                NET                                          
         DS    CL4                                                              
CSPLCOM  DS    CL12                COM                                          
         DS    CL3                                                              
CSPLGRS  DS    CL13                GROSS                                        
         DS    CL1                                                              
CSPLGST  DS    CL13                TAX                                          
         ORG   CSPLD+((PSECOND+89)-P)                                           
CSPLPST  DS    CL12                PST ON PSECOND                               
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE GROUP BILL DETAIL LINE                          *          
**********************************************************************          
GBPLD    DSECT                                                                  
         DS    CL1                                                              
GBPLMED  DS    CL12                MEDIA NAME                                   
         DS    CL9                                                              
GBPLJBC  DS    CL6                 JOB CODE                                     
         DS    CL12                                                             
GBPLJBN  DS    CL36                JOB NAME                                     
         ORG   GBPLJBC+1                                                        
GBPLWKC  DS    CL2                 WORKCODE                                     
         DS    CL1                                                              
GBPLWKN  DS    CL36                WORKCODE NAME                                
         DS    CL9                                                              
GBPLAMT  DS    CL13                AMOUNT                                       
         ORG   GBPLD+L'P                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE BILLING REGISTER PRINT LINE                     *          
**********************************************************************          
BRPLD    DSECT                                                                  
         DS    CL1                                                              
BRPLMED  DS    CL1                 MEDIA                                        
         DS    CL1                                                              
BRPLCLI  DS    CL3                 CLIENT                                       
         DS    CL1                                                              
BRPLPRD  DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
BRPLJOB  DS    CL6                 JOB                                          
         DS    CL5                                                              
BRPLDTE  DS    CL6                 DATE                                         
         DS    CL1                                                              
BRPLBIL  DS    CL7                 BILL NUMBER                                  
         DS    CL4                                                              
BRPLACT  DS    CL12                ACTUAL                                       
         DS    CL4                                                              
BRPLNET  DS    CL12                NET                                          
         DS    CL3                                                              
BRPLCOM  DS    CL12                COM                                          
         DS    CL1                                                              
BRPLGRS  DS    CL12                GROSS                                        
         DS    CL2                                                              
BRPLCSD  DS    CL12                CASH DISCOUNT                                
         ORG   BRPLCSD             OR                                           
BRPLGST  DS    CL12                GST (DON'T SHOW CD)                          
         ORG   BRPLD+((PSECOND+100)-P)                                          
BRPLPST  DS    CL12                PST ON PSECOND                               
*                                                                               
         ORG   BRPLD+((PSECOND+71)-P)   OR IF THEY WANT CD                      
BRPLGST2 DS    CL12                     GST ON PSECOND                          
         ORG   BRPLD+((PTHIRD+71)-P)                                            
BRPLPST2 DS    CL12                     PST ON THIRD                            
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE MEDIA SUMMARY PRINT LINE                        *          
**********************************************************************          
MSPLD    DSECT                                                                  
         DS    CL1                                                              
MSPLNME  DS    CL12                MEDIA NAME                                   
         DS    CL27                                                             
MSPLACT  DS    CL12                ACTUAL                                       
         DS    CL4                                                              
MSPLNET  DS    CL12                NET                                          
         DS    CL3                                                              
MSPLCOM  DS    CL12                COM                                          
         DS    CL1                                                              
MSPLGRS  DS    CL12                GROSS                                        
         DS    CL2                                                              
MSPLCSD  DS    CL12                CASH DISCOUNT                                
         ORG   MSPLCSD             OR                                           
MSPLGST  DS    CL12                GST (DON'T SHOW CD)                          
         ORG   MSPLD+((PSECOND+100)-P)                                          
MSPLPST  DS    CL12                PST ON PSECOND                               
*                                                                               
         ORG   MSPLD+((PSECOND+71)-P)   OR IF THEY WANT CD                      
MSPLGST2 DS    CL12                     GST ON PSECOND                          
         ORG   MSPLD+((PTHIRD+71)-P)                                            
MSPLPST2 DS    CL12                     PST ON THIRD                            
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE INTERNAL INCOME BILLING SUMMARY (SK REVERSAL)   *          
**********************************************************************          
IIPLD    DSECT                                                                  
         DS    CL1                                                              
IIPLJOB  DS    CL12                JOB                                          
         DS    CL1                                                              
IIPLACT  DS    CL12                ACCOUNT                                      
         DS    CL5                                                              
IIPLDTE  DS    CL8                 DATE                                         
         DS    CL3                                                              
IIPLNUM  DS    CL7                 BILL NUMBER                                  
         DS    CL3                                                              
         DS    CL3                                                              
IIPLREF  DS    CL6                 REFERENCE                                    
         DS    CL3                                                              
IIPLWC   DS    CL2                 WORKCODE                                     
         DS    CL1                                                              
IIPLAMT  DS    CL13                AMOUNT                                       
         ORG   IIPLD+L'P                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE REGISTER OF TARGET ACCOUNTS PRINT LINE          *          
**********************************************************************          
TGPLD    DSECT                                                                  
         DS    CL1                                                              
TGPLMED  DS    CL1                 MEDIA                                        
         DS    CL1                                                              
TGPLCLI  DS    CL3                 CLIENT                                       
         DS    CL1                                                              
TGPLPRD  DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
TGPLJOB  DS    CL6                 JOB                                          
         DS    CL5                                                              
TGPLDTE  DS    CL6                 DATE                                         
         DS    CL1                                                              
TGPLNUM  DS    CL7                 BILL NUMBER                                  
         DS    CL3                                                              
         DS    CL1                                                              
TGPLRCV  DS    CL12                RECEIVABLE                                   
         DS    CL3                                                              
TGPLCST  DS    CL12                COSTING                                      
         DS    CL4                                                              
TGPLSLS  DS    CL12                SALES                                        
         ORG   TGPLD+L'P                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE INTERNAL INCOME REGISTER PRINT LINE             *          
**********************************************************************          
ICPLD    DSECT                                                                  
         DS    CL1                                                              
ICPLCLI  DS    CL3                 CLIENT                                       
         DS    CL1                                                              
ICPLPRD  DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
ICPLJOB  DS    CL6                 JOB                                          
         DS    CL8                                                              
ICPLNUM  DS    CL7                 BILL NUMBER                                  
         DS    CL6                                                              
ICPLWC   DS    CL2                 WORKCODE                                     
         DS    CL3                                                              
ICPLINC  DS    CL13                INCOME AMOUNT                                
         DS    CL1                                                              
ICPLCRAC DS    CL14                CREDIT ACCOUNT                               
         DS    CL2                                                              
ICPLPOST DS    CL13                POSTING AMOUNT                               
         DS    CL2                                                              
ICPLDRAC DS    CL14                DEBIT ACCOUNT                                
         ORG   ICPLD+L'P                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE STUDIO INTERCOMPANY REPORT PRINT LINE           *          
**********************************************************************          
SPPLD    DSECT                                                                  
         DS    CL3                                                              
SPPLMED  DS    CL1                 MEDIA                                        
         DS    CL3                                                              
SPPLCLI  DS    CL3                 CLIENT                                       
         DS    CL1                                                              
SPPLPRD  DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
SPPLJOB  DS    CL6                 JOB                                          
         DS    CL1                                                              
SPPLDTE  DS    CL8                 DATE                                         
         DS    CL4                                                              
SPPLNUM  DS    CL7                 BILL NUMBER                                  
         DS    CL6                                                              
SPPLAMT  DS    CL13                AMOUNT                                       
         DS    CL3                                                              
SPPLAGJO DS    CL12                AGENCY JOB                                   
         DS    CL4                                                              
SPPLCRAC DS    CL14                CREDIT ACCOUNT                               
         ORG   SPPLD+L'P                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE INTERCOMPANY POST EXCEPTION PRINT LINE          *          
**********************************************************************          
IXPLD    DSECT                                                                  
         DS    CL1                                                              
IXPLCLI  DS    CL3                 CLIENT                                       
         DS    CL1                                                              
IXPLPRD  DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
IXPLJOB  DS    CL6                 JOB                                          
         DS    CL3                                                              
IXPLSTUD DS    CL4                 STUDIO                                       
         DS    CL3                                                              
IXPLAGYJ DS    CL12                AGENCY JOB                                   
         DS    CL2                                                              
IXPLDTE  DS    CL8                 DATE                                         
         DS    CL2                                                              
IXPLNUM  DS    CL7                 BILL NUMBER                                  
         DS    CL5                                                              
IXPLWMSG DS    CL60                MESSAGE                                      
         ORG   IXPLD+L'P                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE PERCENT OF BILL REGISTER                        *          
**********************************************************************          
PBPLD    DSECT                                                                  
         DS    CL1                                                              
PBPLCLI  DS    CL3                 CLIENT                                       
         DS    CL1                                                              
PBPLPRD  DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PBPLJOB  DS    CL6                 JOB                                          
         DS    CL3                                                              
PBPLACC  DS    CL14                VENDOR ACCOUNT                               
         DS    CL1                                                              
PBPLWRK  DS    CL2                 WORKCODE                                     
         DS    CL3                                                              
PBPLNUM  DS    CL7                 BILL NUMBER                                  
         DS    CL1                                                              
PBPLAMT  DS    CL13                AMOUNT                                       
         ORG   PBPLD+L'P                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER THE ASP REGISTER                                    *          
**********************************************************************          
ASPLD    DSECT                                                                  
         DS    CL1                                                              
ASPLCLI  DS    CL3                 CLIENT                                       
         DS    CL1                                                              
ASPLPRD  DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
ASPLJOB  DS    CL6                 JOB                                          
         DS    CL3                                                              
ASPLACC  DS    CL14                VENDOR ACCOUNT                               
         DS    CL1                                                              
ASPLWRK  DS    CL2                 WORKCODE                                     
         DS    CL3                                                              
ASPLNUM  DS    CL7                 BILL NUMBER                                  
         DS    CL1                                                              
ASPLAMT  DS    CL13                AMOUNT                                       
         ORG   ASPLD+L'P                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO CLIENT GROUP BILL REPORT (AC27)                           *          
**********************************************************************          
GBCPD    DSECT                                                                  
         DS    CL1                                                              
GBCPBNO  DS    CL6                 BILL NUMBER                                  
         DS    CL1                                                              
GBCPPAY  DS    CL11                SJ AMOUNT                                    
         DS    CL1                                                              
GBCPREC  DS    CL11                SR AMOUNT                                    
         DS    CL1                                                              
GBCPINC  DS    CL11                12 AMOUNT                                    
         DS    CL1                                                              
GBCPGRS  DS    CL11                11 AMOUNT                                    
         DS    CL1                                                              
GBCPCSD  DS    CL11                CD                                           
         DS    CL1                                                              
GBCPINT  DS    CL11                INT INCOME                                   
         DS    CL1                                                              
GBCPTIM  DS    CL11                TIME                                         
         DS    CL1                                                              
GBCPOOP  DS    CL11                OOP                                          
         DS    CL1                                                              
GBCPPRE  DS    CL11                PRE-BILL                                     
         DS    CL1                                                              
GBCPRET  DS    CL11                RETAINER                                     
         ORG   GBCPD+L'P                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER A NON-BILLABLE JOB PRINT LINE                       *          
**********************************************************************          
NBPLD    DSECT                                                                  
         DS    CL1                                                              
NBPLCLI  DS    CL3                 CLIENT                                       
         DS    CL1                                                              
NBPLPRD  DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
NBPLJOB  DS    CL6                 JOB                                          
         DS    CL2                                                              
NBPLERR  DS    XL60                                                             
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER PRINT LINE CONTROL TABLE ENTRY                      *          
**********************************************************************          
PLCD     DSECT                                                                  
PLOPT    DS    XL1                 OPTIONS                                      
PLNOTX   EQU   X'80'               .NOT IF PRINTING  TAX                        
PLTXNCD  EQU   X'40'               .PRINT TAX - NOT CD                          
PLTX2ND  EQU   X'20'               .PRINT TAX ON SECOND LINE                    
PLCLEN   DS    XL1                 DATA LENGTH                                  
PLCROUT  DS    XL1                 POINTER TO SPECIAL ROUTINE                   
DATRQ    EQU   1                   .DATE CONVERSION MMM/YY                      
DATR2Q   EQU   2                   .DATE CONVERSION MMM/DD/YY                   
AMTRQ    EQU   3                   .AMOUNT CONVERSION                           
GSTRQ    EQU   4                   .GST AMOUNT CONVERSION                       
PSTRQ    EQU   5                   .PST AMOUNT CONVERSION                       
PLCCOL   DS    AL2                 DISPLACEMENT INTO P                          
PLCSRC   DS    AL2                 SOURCE                                       
PLCLNQ   EQU   *-PLCD                                                           
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER A CONTROL BREAK TABLE ENTRY                         *          
**********************************************************************          
CBKD     DSECT ,                                                                
CBKREP   DS    AL1                 REPORT NUMBER                                
CBKLEN   DS    AL1                 LENGTH OF TABLE ENTRY                        
CBKFRST  DS    AL1                 FIRST ROUTINES                               
CBKLAST  DS    AL1                 LAST TIME ROUTINES                           
CBKSAME  DS    AL1                 ROUTINES IF SAME KEY                         
CBKROUT  DS    AL2                 REPORT ROUTINE                               
CBKNUM   DS    AL1                 NUMBER OF LEVEL ENTRIES                      
CBKL     DS    0XL3                BREAK LEVEL ENTRY                            
CBKLKDSP DS    AL1                 DISPLACEMENT COMPARE                         
CBKLKCMP DS    AL1                 LENGTH OF KEY COMPARE                        
CBKLVL   DS    AL1                 BREAK ROUTINES                               
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER AN IO DISPLAY LINE                                  *          
**********************************************************************          
IOCNTD   DSECT                                                                  
IOENX    DS    CL5                 ENTER/EXIT                                   
         DS    CL1                                                              
IOMODE   DS    CL8                 MODE(IE. PROCACC)                            
         DS    CL1                                                              
IOCNTC   DS    CL(L'IOCNTQ)        DMCOUNT=                                     
IOCNT    DS    CL7                  000                                         
         DS    CL1                                                              
IOSRTC   DS    CL(L'IOSRTQ)        START IO=                                    
IOSRT    DS    CL7                 000                                          
         EJECT                                                                  
GBRCD    DSECT                    DSECT FOR GROUP BILL RECAP RECORD             
GBRCMED  DS    CL1                 MEDIA CODE                                   
GBRCMEDN DS    CL12                MEDIA NAME                                   
GBRCDUE  DS    PL(L'BK$)           AMOUNT DUE                                   
GBRCLNQ  EQU   *-GBRCD                                                          
         EJECT                                                                  
NBILD    DSECT                                                                  
* ACREPNBC                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPNBC                                                       
         PRINT ON                                                               
* ACREPNBD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPNBD                                                       
         PRINT ON                                                               
JXBLKD   DSECT                                                                  
* ACJAXD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACJAXD                                                         
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPNB04 04/29/14'                                      
         END                                                                    
