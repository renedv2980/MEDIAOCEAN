*          DATA SET ACREPPM02  AT LEVEL 004 AS OF 08/17/00                      
*PHASE ACPM02A                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'ACPM - PM ESTIMATE TAPE '                                       
ACPM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PM02**,R9                                                    
         USING ACWORKD,RA                                                       
         L     RA,0(R1)                                                         
         USING ACPMD,RC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         XC    OPTN,OPTN           CLEAR OPTION FIELD                           
         USING ACCRECD,R3                                                       
         L     R3,AIO1                                                          
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  R3                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
         MVI   RCSUBPRG,0                                                       
         CLC   ALPHAID,=C'YN'      ARE WE RUNNING ON YNR?                       
         BNE   *+12                                                             
         MVI   RCSUBPRG,1          YES - SET UP PRINTOUT                        
         OI    OPTN,OPTYN          SET BITS FOR YNR RUN                         
*                                                                               
         CLI   QOPT1,C'Y'            TAPE REQUESTED?                            
         BNE   REQF10                                                           
         LH    R2,OUTCNT                                                        
         LA    R2,1(R2)                                                         
         STH   R2,OUTCNT                                                        
         MVC   DSPARM+13(2),ALPHAID FILL IN TAPE DATASET NAME                   
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),((R2),DSPARM)                           
         OPEN  (OUTP,(OUTPUT))                                                  
         ZAP   TAPECNT,=P'0'                                                    
*                                                                               
REQF10   XC    REQSTRT,REQSTRT                                                  
         CLC   QSTART,SPACES                                                    
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,REQSTRT)                               
REQF20   MVC   REQEND,=XL3'FFFFFF'                                              
         CLC   QEND,SPACES                                                      
         BE    REQF30                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,REQEND)                                  
*                                                                               
REQF30   MVC   SAVCUL(1),RCCOMPFL                                               
         MVC   SAVCUL+1(2),=CL2'SJ'                                             
         ZAP   REQEST,=P'0'        ESTIMATE AMOUNT FOR REQUEST                  
         ZAP   REQCOM,=P'0'        COMMISSIONABLE FOR REQUEST                   
         ZAP   REQNCOM,=P'0'       NON-COM FOR REQUEST                          
         ZAP   REQACOM,=P'0'       AGENCY COMMISSION FOR REQUEST                
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         XC    ESTSTAT,ESTSTAT                                                  
         XC    HIGHADD,HIGHADD                                                  
         XC    NROWS,NROWS         NUBER OF ROWS RETURNED BY JOBBER             
         XC    SVREV,SVREV         CLEAR REV# FIELD FOR JOBBER                  
         L     R4,ADACC                                                         
         USING EVERECD,R2                                                       
         LA    R2,SVKEY                                                         
         XC    SVKEY,SVKEY         CLEAR KEY TO READ COMMENT RECORD             
         MVI   EVEKTYP,EVEKTYPQ    X'2C' - ESTIMATE VERSION RECORDS             
         MVI   EVEKSUB,EVEKSUBQ    X'36' - SUB RECORD                           
         MVC   EVEKCPY(3),SAVCUL   C/U/L                                        
         MVC   EVEKCLI(L'EVEKCLI+L'EVEKPRO),SPACES     CLEAR CLI/PROD           
         MVC   EVEKCLI(3),3(R4)    CLIENT                                       
         MVC   EVEKPRO(3),6(R4)    PRODUCT                                      
         MVC   EVEKJOB,9(R4)       JOB                                          
         MVI   EVEKTYPE,EVEKTREV   'R' - LOOKING FOR REVISIONS ONLY             
         MVI   EVEKVER,1           VERSION 1 (THE FIRST ONE)                    
         L     R2,AIO1             R2 = A(IO AREA 1)                            
         L     R3,AIO2             R3 = A(IO AREA 2)                            
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         B     PACC15                                                           
*                                                                               
PACC10   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
PACC15   CLC   0(EVEKVER-EVEKEY,R2),SVKEY     STILL READING SAME JOB            
         BNE   PACC60                                                           
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         TM    OPTN,OPTYN          ARE WE RUNNING ON YNR?                       
         BNO   PACC20              IF YES - ONLY APPROVED ESTIMATES             
         USING EAPELD,R4                                                        
         LR    R4,R2               PREP FOR GETEL                               
         MVI   ELCODE,EAPELQ       X'AB' - ESTIMATE APPROVAL ELEMENT            
         BAS   RE,GETEL2                                                        
         BNE   PACC10              IF NO ELEMENT - NOT APPROVED SKIP            
         CLC   EAPDATE,REQSTRT     APPROVED WITHIN REQ RANGE                    
         BL    PACC10                                                           
         CLC   EAPDATE,REQEND                                                   
         BNH   PACC30              NO                                           
         OI    ESTSTAT,NOTCLOSE    THIS EST CAN'T BE CLOSED AS THERE            
         B     PACC10              ARE SUBSEQUENT REVISIONS ON JOB              
*                                                                               
         USING EUPELD,R4                                                        
PACC20   LR    R4,R2               PREP FOR GETEL                               
         MVI   ELCODE,EUPELQ       X'A9' - ESTIMATE UPDATE ESTIMATE             
         BAS   RE,GETEL2           I.E. DATE ESTIMATE OPENED                    
         BNE   PACC10                                                           
         CLC   EUPADD,REQSTRT      ADDED WITHIN REQ RANGE                       
         BL    PACC10                                                           
         CLC   EUPADD,REQEND                                                    
         BNH   PACC30              NO                                           
         OI    ESTSTAT,NOTCLOSE    THIS EST CAN'T BE CLOSED AS THERE            
         B     PACC10              ARE SUBSEQUENT REVISIONS ON JOB              
*                                                                               
PACC30   CLC   SVREV,EVEKVER       SAVE REVISION NUMBER AS BEFORE?              
         BE    PACC40              YES - NO NEED TO GO TO JOBBER AGAIN          
*                                                                               
         MVC   SVREV,EVEKVER                                                    
         OI    SVREV,X'F0'         MAKE IT EBCDIC                               
         MVC   LOOKFLD+1(1),SVREV  UPDATE REV NUMBER IN FIELD FOR JOBBR         
         L     R5,AMONACC                                                       
         USING ACMD,R5                                                          
         GOTO1 ACMAJOBL,DMCB,LOOKFLDH,ACMACOLL,ADCOMFAC                         
         CLI   DMCB+4,X'00'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
PACC40   BAS   RE,GETACC           FILL TOTAL AND DETAIL WITH DATA              
         TM    OPTN,OPTYN          ARE WE RUNNING ON YNR?                       
         BO    PACC50              YES - ONLY CURRENT ESTIMATES                 
         CLI   EVEKVER,1           IS THIS THE ORIGINAL ESTIMATE                
         BNE   PACC50                                                           
         MVC   HIGHADD,EUPADD      SAVE DATE ADDED                              
         BAS   RE,PUTSRT           PUT ESTIMATE RECORDS TO SORT                 
         MVC   SVREV,EVEKVER       UPDATE SAVED REV # FOR NEXT COMPARE          
         B     PACC10                                                           
*                                                                               
PACC50   CLC   EUPADD,HIGHADD      IS THIS A LATER REVISION                     
         BL    PACC10              NO (TIE GOES TO THE LATER R#)                
         MVC   HIGHADD,EUPADD                                                   
         OI    ESTSTAT,GOTAREV                                                  
         XR    R2,R3               SWAP POINTERS-DATAMGR READS INTO R2          
         XR    R3,R2                                                            
         XR    R2,R3                                                            
         MVC   SVREV,EVEKVER       UPDATE SAVED REV # FOR NEXT COMPARE          
         B     PACC10                                                           
*                                                                               
PACC60   OC    HIGHADD,HIGHADD     DID I FIND ANY REVISIONS                     
         BZ    PACCX               NO                                           
         LR    R2,R3               SAVED ESTIMATE POINTER TO R2                 
         TM    ESTSTAT,GOTAREV                                                  
         BNO   PACCX               THERE WERE NO SAVED REVISIONS                
         BAS   RE,PUTSRT                                                        
*                                                                               
PACCX    L     RE,LASTIO           RESTORE KEY FOR MONACC                       
         MVC   SVKEY,0(RE)         RE-READ THE LAST ACCOUNT                     
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
*         GET A RECORD FROM SORT, PUT TO TAPE AND REPORT             *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'Y'                                                       
         BNE   REQ60                                                            
REQL10   GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R2,15,4(R1)                                                      
         BZ    REQL50              EXIT LOOP IF NOTHING RETURNED                
         MVC   TOTAREA(PMLNQ),0(R2)                                             
         USING PMD,R2                                                           
         LA    R2,TOTAREA                                                       
         CLC   PMRECTYP,=C'40'     IS THIS A 40 TYPE REC                        
         BNE   REQL20                                                           
         LH    R3,=Y(PM40LNQ+4)                                                 
         STH   R3,LRECL                                                         
         B     REQL30                                                           
REQL20   LH    R3,=Y(PMTPLEN+4)    MUST BE A 43 TYPE RECODS                     
         STH   R3,LRECL                                                         
*                                                                               
REQL30   CLI   QOPT7,C'Y'          Q, PARTIAL OUTPUT TAPE DUMP REQUIRED         
         BNE   REQL40               N, SKIP TAPE DUMP                           
         LA    R4,=CL7'OUTPUT'                                                  
         GOTO1 =V(PRNTBL),DMCB,(7,(R4)),TPREC,C'DUMP',(R3),=C'2D',     X        
               (C'P',PRINT)                                                     
*                                                                               
REQL40   PUT   OUTP,TPREC           Y, WRITES OUTPUT TAPE FILE                  
         AP    TAPECNT,=P'1'       RUNNING COUNT OF TAPE RECORDS                
         B     REQL10              GET NEXT TAPE REC                            
*                                                                               
REQL50   GOTO1 ACREPORT                                                         
         MVC   P+1(23),=CL23'NUMBER OF TAPE RECORDS:'                           
         EDIT  (P4,TAPECNT),(13,P+25),COMMAS=YES,ZERO=NOBLANK,ALIGN=LEF0        
               T                                                                
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   TAPECNT,=P'0'                                                    
         CLOSE (OUTP)                                                           
*                                                                               
         USING PLINED,R2                                                        
REQ60    LA    R2,P                                                             
         MVC   P+1(24),=C'TOTALS FOR ALL ESTIMATES'                             
         TM    OPTN,OPTYN          ARE WE RUNNING ON YNR?                       
         BNO   REQL70              NO - DON'T PRINT ESTIMATE TOTALS             
         LA    R5,PESTAMT          ESTIMATE FIELD IN PRINT LINE                 
         ZAP   DOUBLE,REQEST       REQUEST TOTAL FOR ESTIMATE                   
         BAS   RE,EDITIT                                                        
REQL70   LA    R5,PCOMAMT                                                       
         ZAP   DOUBLE,REQCOM                                                    
         BAS   RE,EDITIT                                                        
         LA    R5,PNCAMT                                                        
         ZAP   DOUBLE,REQNCOM                                                   
         BAS   RE,EDITIT                                                        
         LA    R5,PAGYCOM                                                       
         ZAP   DOUBLE,REQACOM                                                   
         BAS   RE,EDITIT                                                        
         AP    DOUBLE,REQCOM                                                    
         AP    DOUBLE,REQNCOM                                                   
         LA    R5,PCURDATE                                                      
         BAS   RE,EDITIT                                                        
         GOTO1 ACREPORT                                                         
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*        PROCESS AN ACCOUNT RECORD                                   *          
*        GET USER FIELD INFO                                         *          
**********************************************************************          
         SPACE 1                                                                
GETACC   NTR1                                                                   
         TM    ESTSTAT,GOTACC      I ALREADY HAVE THIS ACCOUNT INFO             
         BO    GETACCX                                                          
         OI    ESTSTAT,GOTACC                                                   
*                                                                               
         L     R4,ADLVANAM                                                      
         LA    R5,CLINAME                                                       
         BAS   RE,GETNAME                                                       
         L     R4,ADLVBNAM                                                      
         LA    R5,PRONAME                                                       
         BAS   RE,GETNAME                                                       
*                                                                               
         USING ACTRECD,R4                                                       
         L     R4,ADACC                                                         
         USING PMD,R2                                                           
         LA    R2,TOTAREA                                                       
         BAS   RE,CLRAREA                                                       
         OI    PMERR,NOESTRT+NOEEND+NOBNNUM                                     
*                                                                               
         LA    R0,CONTABN          NUMBER OF ENTRIES IN CONSTANT TABLE          
         LA    R1,CONTAB           R1 = A(CONSTANT TABLE)                       
GETACC05 CLC   ALPHAID,0(R1)                                                    
         BE    *+14                                                             
         LA    R1,L'CONTAB(R1)                                                  
         BCT   R0,GETACC05                                                      
         DC    H'0'                DIE IF AGENCY NOT FOUND                      
*                                                                               
         MVC   PMCONST,2(R1)       CORRECT CONSTANTS                            
         MVC   PMRECTYP,=C'43'                                                  
         MVC   PMJOB(6),ACTKACT+6           SAVE JOB                            
         MVC   PMJOB+6(6),ACTKACT           SAVE CLI AND PROD IN UNUSED         
         USING NAMELD,R4                                                        
         L     R4,ADACCNAM                                                      
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   PMJNAME(0),NAMEREC                                               
*                                                                               
         USING PPRELD,R4                                                        
         L     R4,ADACCJOB                                                      
         MVC   PMPOB,PPRBILLP                                                   
*                                                                               
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
*                                                                               
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,AIO1                                                      
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NROWS,JBNROWS       SAVE NUMBER OF ROWS                          
*                                                                               
         USING UFSELD,R4                                                        
         L     R4,ADACC                                                         
         AH    R4,DATADISP                                                      
         MVI   ELCODE,UFSELQ       X'A2' - USER FIELD SELECT ELEMENT            
GETACC10 BAS   RE,NEXTEL                                                        
         BNE   GETACC50                                                         
         CLC   UFSCODE,=CL2'PE'    ESTIMATE NUMBER?                             
         BNE   GETACC20                                                         
         NI    PMERR,TURNOFF-NOBNNUM                                            
         SR    R1,R1                                                            
         IC    R1,UFSLN                                                         
         SH    R1,=Y(UFSLN1Q+1)                                                 
         BM    GETACC20                                                         
         EX    R1,*+4                                                           
         MVC   PMESTNO(0),UFSDATA                                               
         B     GETACC10                                                         
*                                                                               
GETACC20 CLC   UFSCODE,=CL2'EE'    CLOSE DATE                                   
         BNE   GETACC30                                                         
         CLI   UFSEDIT,C'D'        MUST BE A DATE FIELD                         
         BNE   GETACC10                                                         
         CLI   UFSLN,UFSLN1Q       ANY DATA HERE?                               
         BNH   GETACC30            NO                                           
         GOTO1 DATVAL,DMCB,(0,UFSDATA),(X'80',PMCDATE)                          
         NI    PMERR,TURNOFF-NOEEND                                             
         B     GETACC10                                                         
GETACC30 CLC   UFSCODE,=CL2'ES'    START DATE                                   
         BNE   GETACC40                                                         
         CLI   UFSEDIT,C'D'        MUST BE A DATE FIELD                         
         BNE   GETACC10                                                         
         CLI   UFSLN,UFSLN1Q       ANY DATA HERE?                               
         BNH   GETACC10            NO                                           
         GOTO1 DATVAL,DMCB,(0,UFSDATA),(X'80',PMSDATE)                          
         NI    PMERR,TURNOFF-NOESTRT                                            
         B     GETACC10                                                         
*                                                                               
GETACC40 CLC   UFSCODE,=CL2'FI'                                                 
         BNE   GETACC10                                                         
         CLI   UFSLN,UFSLN1Q       ANY DATA HERE?                               
         BNH   GETACC10            NO                                           
         CLI   UFSDATA,C'F'                                                     
         BNE   GETACC10                                                         
         OI    ESTSTAT,FINAL                                                    
         B     GETACC10                                                         
*                                                                               
GETACC50 LA    R2,DETAREA                                                       
         BAS   RE,CLRAREA                                                       
         MVC   DETAREA(PMCOM-PMREC),TOTAREA   MOVE INTO THE DETAIL REC          
         MVC   PMRECTYP,=C'40'                                                  
         LA    R3,TOTAREA                                                       
         MVC   PMJOB+6(6),SPACES                                                
*                                                                               
GETACCX  B     EXIT                           THE FIELDS IT SHARES              
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
**********************************************************************          
* PUT AN ESTIMATE RECORD TO SORT                                     *          
*        RECORD IS AT 0(R2)                                          *          
**********************************************************************          
         SPACE 1                                                                
PUTSRT   NTR1                                                                   
         MVI   ESTCNT,0          INIT TABLE FOR W/C DETAIL                      
*                                                                               
         USING PMD,R3                                                           
         LA    R3,DETAREA                                                       
         USING EVERECD,R2                                                       
         SR    R1,R1                                                            
         IC    R1,EVEKVER                                                       
         BCTR  R1,0                PM REVISIONS START AT ZERO                   
         EDIT  (R1),(2,PMREVNO),FILL=0,ZERO=NOBLANK                             
         LA    R5,TOTAREA                                                       
         MVC   PMREVNO-PMREC(2,R5),PMREVNO                                      
         ZAP   TOTEST,=P'0'                                                     
         ZAP   PMCOM-PMD(6,R5),=P'0'                                            
         ZAP   PMNONCOM-PMD(6,R5),=P'0'                                         
         ZAP   PMAGYCOM-PMD(6,R5),=P'0'                                         
*                                                                               
         USING EPRELD,R4                                                        
         LR    R4,R2                                                            
         MVI   ELCODE,EPRELQ        X'B1' - ESTIMATE PREPARER ELEMENT           
         BAS   RE,GETEL2                    GET DATE PREPARED FOR TAPE          
         BNE   PUTSRT10                                                         
         GOTO1 DATCON,DMCB,(1,EPRDATE),(X'20',PMPDATE-PMREC(R5))                
         B     PUTSRT20                                                         
*                                                                               
PUTSRT10 TM    OPTN,OPTYN          ARE WE RUNNING ON YNR?                       
         BNO   PUTSRT20            NO - DON'T BOTHER WITH DATE ADDED            
         USING EUPELD,R4                                                        
         LR    R4,R2                                                            
         MVI   ELCODE,EUPELQ        X'A9' - ESTIMATE UPDATE ELEMENT             
         BAS   RE,GETEL2                    GET DATE ADDED FOR TAPE             
         BNE   PUTSRT20                                                         
         GOTO1 DATCON,DMCB,(1,EUPADD),(X'20',PMPDATE-PMREC(R5))                 
*                                                                               
PUTSRT20 LR    R4,R2                                                            
         AH    R4,DISP2                                                         
         USING EDAELD,R4                                                        
PUTSRT30 MVI   ELCODE,EDAELQ       X'AC' - ESTIMATE DATE ELEMENT                
         BAS   RE,NEXTEL2                                                       
         BNE   PUTSRT70                                                         
*                                                                               
         L     R5,ADGOBLOC         GET WORK CODE COMMISH RATE                   
         USING GOBLOCKD,R5                                                      
         MVC   GOSELWC,EDAWORK                                                  
         GOTO1 GETOPT,DMCB,GOBLOCKD                                             
         XC    GOSELWC,GOSELWC                                                  
         ZAP   COMRATE,GOAGYCOM                                                 
         DROP  R5                                                               
*                                                                               
         MVI   PM0,C'0'                                                         
         MVC   PMWC,EDAWORK                                                     
*                                                                               
         ZAP   PMNONCOM,=P'0'                                                   
         ZAP   PMCOM,=P'0'                                                      
         MP    COMRATE,EDACOMM     RATE TIMES COMMISIONABLE IS COMMISH          
         SRP   COMRATE,64-6,5                                                   
         LA    R5,PMCOM            CONSIDER AMOUNT COMISSIONABLE                
         ZAP   PMAGYCOM,COMRATE                                                 
         BNZ   *+8                 ZERO COMMISION, SO ITS NONCOM                
         LA    R5,PMNONCOM                                                      
         ZAP   0(6,R5),EDACOMM                                                  
         CLI   EDALN,EDALNQ2       IS THERE A NON COM AMOUNT ON THIS            
         BNE   *+10                NO                                           
         AP    PMNONCOM,EDANCOM    ADD IT TO THE NON-COMM-ABLE                  
*                                                                               
         LR    R5,R4               SAVE R4                                      
         USING WCOELD,R4                                                        
         L     R4,ADLEDGER         GET WORK CODE NAME                           
         AH    R4,DATADISP                                                      
         MVI   ELCODE,WCOELQ           X'12' - WORK CODE ELEMENT                
PUTSRT40 BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WCOCODE,PMWC                                                     
         BNE   PUTSRT40                                                         
         MVC   PMWCNAME(15),WCODESC    WORK CODE DESCRIPTION                    
*                                                                               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         USING JBCOLD,R4                                                        
         L     R4,ACMACOL          ADDRESS OF COLUMNS RETURNED                  
         DROP  RF                                                               
*                                                                               
         ZAP   DETEST,=P'0'                                                     
         SR    R0,R0                                                            
         ICM   R0,3,NROWS                                                       
         BZ    PUTSRT60                                                         
PUTSRT50 CLC   PMWC,JBCOLWC        MATCH ON WORK CODE                           
         BNE   *+14                NOT FOUND - KEEP LOOPING                     
         ZAP   DETEST,JBCOLVAL     FOUND ADD TOTAL                              
         B     PUTSRT60            EXIT LOOP                                    
         LA    R4,18(R4)           BUMP TO NEXT ROW                             
         BCT   R0,PUTSRT50                                                      
         DROP  R4                                                               
*                                                                               
PUTSRT60 LR    R4,R5                   RESTORE R4                               
         LA    R5,TOTAREA              UPDATE TOTAL RECORD DATA                 
         AP    TOTEST,DETEST           ADD ESTIMATE TO TOTAL                    
         AP    PMCOM-PMD(6,R5),PMCOM                                            
         AP    PMNONCOM-PMD(6,R5),PMNONCOM                                      
         AP    PMAGYCOM-PMD(6,R5),PMAGYCOM                                      
         BAS   RE,SRTR                                                          
         B     PUTSRT30                                                         
*                                                                               
PUTSRT70 LA    R3,TOTAREA          PRINT TOTAL REC AND WC DETAIL RECS           
         BAS   RE,SRTR             PUT TOTAL RECORD TO SORT                     
         AP    REQEST,TOTEST       UPDATE REQUEST TOTALS                        
         AP    REQCOM,PMCOM        UPDATE REQUEST TOTALS                        
         AP    REQACOM,PMAGYCOM                                                 
         AP    REQNCOM,PMNONCOM                                                 
         BAS   RE,PRTRTE                                                        
         SR    R1,R1                                                            
         ICM   R1,1,ESTCNT         NUMBER OF DETAIL RECS IN TABLE               
         BZ    PUTSRTX             NOTHING IN TABLE                             
         L     R3,AESTTBL          R3 = A(ESTIMATE TABLE)                       
         BAS   RE,PRTRTE           PRINT THE RECORD AT 0(R3)                    
         LA    R3,ESTLNQ(R3)       NEXT RECORD                                  
         BCT   R1,*-8                                                           
         GOTO1 ACREPORT            DOUBLE SPACE                                 
*                                                                               
PUTSRTX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* SORT ROUTINE - R3 HAS ADRESS OF RECORD TO PUT TO SORT              *          
**********************************************************************          
         SPACE 1                                                                
SRTR     NTR1                                                                   
         USING PMD,R3                                                           
         CLC   PMRECTYP(2),=C'43'                                               
         BE    *+8                                                              
         BAS   RE,BLDTBL                                                        
         GOTO1 ADSORTER,DMCB,=C'PUT',(R3)                                       
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* CLEAR TAPE RECORD AREA                                             *          
**********************************************************************          
         SPACE 1                                                                
         USING PMD,R2                                                           
CLRAREA  NTR1                                                                   
         MVI   PMREC,C' '                                                       
         MVC   PMREC+1(PMLNQ-1),PMREC                                           
         ZAP   PMCOM,=P'0'                                                      
         ZAP   PMNONCOM,=P'0'                                                   
         ZAP   PMAGYCOM,=P'0'                                                   
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD TABLE ENTRY ROUTINE                                          *          
*    SAVE W/C DETAIL IN TABLE SO I CAN PRINT IT AFTER I HAVE PRINTED *          
*    WORK CODE TOTALS - 0(R3) IS A(WORK CODE DETAIL RECORD)          *          
**********************************************************************          
         SPACE 1                                                                
         USING ESTTABD,R2                                                       
BLDTBL   NTR1                                                                   
         L     R2,AESTTBL           R2 = A(ESTIMATE TABLE)                      
         CLI   ESTCNT,ESTMAX        COMPARE CURRENT COUNT TO MAX                
         BNH   *+6                                                              
         DC    H'0'                 TOO MANY W/C'S ON THIS ESTIMATE             
         SR    R5,R5                                                            
         IC    R5,ESTCNT            NUMBER OF WC'S IN TABLE                     
         LR    R1,R5                SAVE R5 FOR LATER                           
         LA    R1,1(R1)             BUMP  UP ONE                                
         STC   R1,ESTCNT            UPDATE COUNT                                
         LA    R1,ESTLNQ            LENGTH OF WC ENTRIES                        
         MR    R4,R1                TIMES NUMBER IN TABLE IS OFFSET             
         AR    R2,R5                ADD RESULT A(TABLE) FOR ADDRESS             
         MVC   ESTREC,0(R3)         SAVE W/C DETAIL RECORD                      
         ZAP   ESTAMT,DETEST       ESTIMATE AMOUNT                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
*       R3 = A(RECORD TO BE PRINTED)                                 *          
*       EITHER TOTAL RECORD (43) WHICH IS CURRENT RECORD AT (R3)     *          
*       EITHER W/C DETAIL (40) FROM ESTIMATE TABLE AESTTBL           *          
**********************************************************************          
         SPACE 1                                                                
         USING PMD,R3                                                           
PRTRTE   NTR1                                                                   
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         MVC   HEAD4+12(3),ACTKACT         CLIENT                               
         MVC   HEAD4+16(36),CLINAME                                             
         MVC   HEAD5+12(3),ACTKACT+3       PRODUCT                              
         MVC   HEAD5+16(36),PRONAME                                             
         USING PLINED,R2                                                        
         LA    R2,P                                                             
         MVC   PWC,PMWC                                                         
*                                                                               
         TM    OPTN,OPTYN          ARE WE RUNNING ON YNR?                       
         BNO   PRTRTE05            DONT PRINT ESTIMATE AMOUNTS                  
         ZAP   DOUBLE,TOTEST       ASSUME ESTIMATE AMOUNT TOTAL                 
         CLC   PMRECTYP,=C'43'     IS THIS A WC TOTAL RECORD                    
         BE    *+10                YES - CONTINUE                               
         USING ESTTABD,R3                                                       
         ZAP   DOUBLE,ESTAMT       ELSE - DETAIL RECORD FROM EST TABLE          
         LA    R5,PESTAMT                                                       
         BAS   RE,EDITIT                                                        
*                                                                               
         USING PMD,R3                                                           
PRTRTE05 ZAP   DOUBLE,PMCOM                                                     
         LA    R5,PCOMAMT                                                       
         BAS   RE,EDITIT                                                        
         ZAP   DOUBLE,PMNONCOM                                                  
         LA    R5,PNCAMT                                                        
         BAS   RE,EDITIT                                                        
         ZAP   DOUBLE,PMAGYCOM                                                  
         LA    R5,PAGYCOM                                                       
         BAS   RE,EDITIT                                                        
         CLC   PMRECTYP,=C'40'     IS THIS A WC DETIL RECORD                    
         BE    PRTRTEX             YES                                          
*                                                                               
         MVC   PJOB,PMJOB          NO, FILL IN DATA NEEDED FOR SUMMARY          
         TM    PMERR,NOBNNUM                                                    
         BO    *+10                                                             
         MVC   PBN,PMESTNO                                                      
         MVC   PREVNUM,PMREVNO                                                  
         MVC   PWC,SPACES                                                       
         TM    PMERR,NOESTRT       IS THERE AN ESTIMATE START DATE              
         BO    PRTRTE10            NO                                           
         LA    R4,PMSDATE          Y, PRINT IT                                  
         LA    R5,PESDATE                                                       
         BAS   RE,GETDTE                                                        
PRTRTE10 TM    PMERR,NOEEND        IS THERE AN ESTIMATE END DATE                
         BO    PRTRTE20            NO                                           
         LA    R4,PMCDATE          YES                                          
         LA    R5,PECDATE                                                       
         BAS   RE,GETDTE                                                        
PRTRTE20 TM    ESTSTAT,FINAL       IS THERE AN FINAL INDICATOR?                 
         BNO   PRTRTE30            NO                                           
         MVI   PFINAL,C'F'                                                      
         TM    PMERR,NOEEND        WAS THERE AN ENDDATE                         
         BNO   PRTRTE30            YES                                          
         MVC   PMCDATE(7),=C'MISSING' NO, THERE SHOULD BE IF FINAL              
PRTRTE30 LA    R4,PMPDATE          PRINT PREP DATE                              
         LA    R5,PCURDATE                                                      
         BAS   RE,GETDTE                                                        
*                                                                               
PRTRTEX  GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* GETEL #1 - EMULATED                                                *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* GETEL #2 - NON-EMULATED                                            *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R4,DISP2,ELCODE,2                                               
         EJECT                                                                  
**********************************************************************          
* GET NAME ROUTINE                                                   *          
*      R4 = A(NAME ELEMENT)                                          *          
*      R5 = A(RECEIVING FIELD)                                       *          
**********************************************************************          
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         MVC   0(36,R5),SPACES                                                  
         CLI   0(R4),X'20'         MAKE SURE I WAS PASSED THE NAME EL           
         BNE   EXIT                                                             
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   0(0,R5),NAMEREC                                                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* UTILITY ROUTINES                                                   *          
*         - EDIT ROUTINE     (EDITIT)                                *          
*         - GET DATE ROUTINE (GETDTE)                                *          
**********************************************************************          
         SPACE 1                                                                
EDITIT   NTR1                      EDIT ROUTINE                                 
         EDIT  (P8,DOUBLE),(11,(R5)),2,MINUS=YES                                
         B     EXIT                                                             
*                                                                               
GETDTE   NTR1                      GET DATE ROUTINE                             
         GOTO1 DATCON,DMCB,(0,0(R4)),(5,0(R5))                                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
AESTTBL  DC    A(ESTTBL)           ESTIMATE TABLE                               
AIO1     DC    A(IO1)              IO AREA 1                                    
AIO2     DC    A(IO2)              IO AREA 2                                    
DATVAL   DC    V(DATVAL)           DATE VALIDATION ROUTINE                      
*                                                                               
DDPARM   DC    CL8'OUTP'                                                        
DSPARM   DC    CL20'ACCTAPE.AC0PMXX1' THE XX WILL BECOME THE ALPHAID            
OUTCNT   DC    H'0'                   RELATIVE GENERATION FOR DYNALLOC          
*                                                                               
LOOKFLDH DC    AL1(8+L'LOOKFLD),4X'00',AL1(L'LOOKFLD),AL2(0)                    
LOOKFLD  DC    C'R1G'              REV NUMBER - CHANGED AT TIME OF CALL         
*                                                                               
OUTP     DCB   DDNAME=OUTP,DSORG=PS,RECFM=VB,LRECL=408,MACRF=PM,       X        
               BUFNO=2,BLKSIZE=4084                                             
*                                                                               
*              SORT IS - BN#, DDS ACCOUNT, RECTYPE, WORKCODE                    
SORTCARD DC    CL80'SORT FIELDS=(1,26,A,217,12,A,47,2,A,45,2,D,34,2,A),X        
               FORMAT=BI'                                                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(230)'                                 
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
CONTAB   DS    0CL15                   CONSTANTS TABLE                          
         DC    C'BS',C'310774PDA10PM'  BATES CONSTANTS                          
         DC    C'YN',C'331543PDA10PM'  YNR   CONSTANTS                          
CONTABN  EQU   (*-CONTAB)/L'CONTAB                                              
*                                                                               
ESTCNT   DS    XL1                 NUMBER OF W/C ERNTRIES FOR THIS EST          
ESTTBL   DS    (ESTMAX*ESTLNQ)C                                                 
ESTMAX   EQU   200                 MAX NUMBER OF WC'S                           
         EJECT                                                                  
**********************************************************************          
* IO BUFFERS                                                         *          
**********************************************************************          
         SPACE 1                                                                
         DC    C'**I01**'                                                       
IO1      DS    CL2000                                                           
*                                                                               
         DC    C'**I02**'                                                       
IO2      DS    CL2000              SAVE A RECORD AREA                           
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,(R2),0                     
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,(R2),0                     
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,(R2),0                     
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LR    R3,R2                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,(R2),DMWORK                
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* STORAGE DSECT                                                      *          
**********************************************************************          
         SPACE 1                                                                
ACPMD    DSECT                                                                  
TPVOLS   DS    D                                                                
ESTPOINT DS    F                   POINTER TO NEXT AVAILABLE SPACE              
NROWS    DS    H                   NUMBER OF ROWS RETURNED BY JOBBER            
DISP2    DS    H                   DATADISP FOR NON-EMULATION CALLS             
*                                                                               
TAPECNT  DS    PL4                                                              
DETEST   DS    PL8                 ESTIMATE AMOUNT - DETAIL                     
TOTEST   DS    PL8                 ESTIMATE AMOUNT - TOTAL                      
REQEST   DS    PL8                                                              
REQCOM   DS    PL8                                                              
REQNCOM  DS    PL8                                                              
REQACOM  DS    PL8                                                              
TPDSN    DS    CL20                                                             
*                                                                               
OPTN     DS    XL1                 OPTION FIELD                                 
OPTYN    EQU   X'80'               YNR RUN                                      
*                                                                               
NAMFLD   DS    0CL36               NAME FIELD                                   
CLINAME  DS    CL36                                                             
PRONAME  DS    CL36                                                             
*                                                                               
ESTSTAT  DS    CL1                 STATUS BYTE                                  
NOTCLOSE EQU   X'01'                                                            
GOTACC   EQU   X'02'                                                            
FINAL    EQU   X'04'                                                            
GOTAREV  EQU   X'08'                                                            
*                                                                               
SVREV    DS    XL1                 SAVED AREA FOR REVISION NUMBER               
ELCODE   DS    CL1                                                              
COMMAND  DS    CL8                                                              
REQSTRT  DS    CL3                 PACKED REQEST START DATE                     
REQEND   DS    CL3                 & END DATE                                   
SAVCUL   DS    CL3                 COMP, UNIT, LEDG I'M READING                 
SVKEY    DS    CL42                BUILD ESTIMATE KEY                           
MYKEYSV  DS    CL56                SAVE KEY FOR MONACC                          
TPREC    DS    0F                  LRECL+TOTAREA)                               
LRECL    DS    F                                                                
TOTAREA  DS    (PMLNQ)C                                                         
DETAREA  DS    (PMLNQ)C                                                         
HIGHADD  DS    CL3                 THE HIGHEST PREP DATA ENCOUNTERED            
COMRATE  DS    PL16                                                             
*                                                                               
EOF      EQU   X'FF'               END OF FILE                                  
TURNOFF  EQU   X'FF'               TURNOFF SWITCH                               
ACPMDLN  EQU   *-ACPMD             MUST BE LAST                                 
         EJECT                                                                  
**********************************************************************          
* TAPE/SORT RECORD DSECT                                             *          
**********************************************************************          
         SPACE 1                                                                
PMD      DSECT                                                                  
PMREC    DS    0C                                                               
PMCONST  DS    0CL13               CONTANT '310774PDA10PM'                      
PMVENNO  DS    CL6                 VENDOR NUMBER                                
PMMEDIA  DS    CL2                 MEDIA CODE                                   
PMDATA   DS    CL1                 DATATYPE                                     
PMCMPCLI DS    CL4                 COMPANY CLIENT                               
PMESTNO  DS    CL12                ESTIMATE NUMBER                              
PMJOB    DS    CL6                 AGENCY JOB NUMBER                            
PM0      DS    CL1                                                              
PMWC     DS    CL2                 WORK CATEGORY                                
PMCLI    DS    CL3                 CLIENT CODE                                  
PMPROD   DS    CL3                 PRODUCT CODE                                 
         DS    CL3                 FILLER                                       
PMRECTYP DS    CL2                 RECORD TYPE (43-TOTAL, 40-WCDETAIL)          
PMREVNO  DS    CL2                 REVISION NUMBER                              
PMCOM    DS    PL6                 COMMISSIONABLE AMOUNT                        
PMNONCOM DS    PL6                 NON-COMMISSIONABLE AMOUNT                    
PMAGYCOM DS    PL6                 AGENCY COMMISSIONABLE AMOUNT                 
PMWCNAME DS    CL20                WORK CODE NAME ON REC TYPE 40                
PM40LNQ  EQU   *-PMD               DETAIL RECORD LENGTH                         
         ORG   PMWCNAME            '43' RECORD - NO WORK CODE NAME              
PMSDATE  DS    CL6                 YYMMDD                                       
PMCDATE  DS    CL6                 YYMMDD                                       
         DS    CL6                 PREVIOUS ESTIMATE DATE (SPACES)              
PMPDATE  DS    CL6                 PREPARED DATE (B1 EL)                        
         DS    CL10                AD NUM (DO NOT USE                           
         DS    CL25                SIZE COLOR (DO NOT USE)                      
         DS    CL25                MEDIA/DATE ("   "   " )                      
PMJNAME  DS    CL35                JOB NAME                                     
PMPOB    DS    CL15                PRINT ON BILLS                               
         DS    CL16                MEDIA EXP (DON'T USE)                        
PMTPLEN  EQU   *-PMD                                                            
PMERR    DS    CL1                 ERRORS IN THIS RECORD                        
PMSRTACT DS    CL12                DDS ACCOUNT FOR SORT                         
PMLNQ    EQU   *-PMD                                                            
NOBNNUM  EQU   X'01'               BN=USER FIELD MISSING                        
NOESTRT  EQU   X'02'               ESTIMATE START DATE MISSING                  
NOEEND   EQU   X'04'               ESTIMATE END DATE MISSOING                   
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
PREC     DS    0CL132                                                           
         DS    CL1                                                              
PJOB     DS    CL6                                                              
         DS    CL1                                                              
PBN      DS    CL12                                                             
         DS    CL1                                                              
PREVNUM  DS    CL2                                                              
         DS    CL8                                                              
PWC      DS    CL2                 WORK-CODE                                    
         DS    CL9                                                              
PESTAMT  DS    CL11                ESTIMATE AMOUNT TOTAL                        
         DS    CL2                                                              
PCOMAMT  DS    CL11                COMMISSIONABLE AMOUNT                        
         DS    CL5                                                              
PNCAMT   DS    CL11                NON-COMMISSIONABLE AMOUNT                    
         DS    CL1                                                              
PAGYCOM  DS    CL11                AGENCY COMMISSION                            
         DS    CL2                                                              
PCURDATE DS    CL8                 ESTIMATE PREPARER DATE                       
         DS    CL1                                                              
PESDATE  DS    CL8                 ESTIMATE START DATE                          
         DS    CL1                                                              
PECDATE  DS    CL8                 ESTIMATE CLOSE DATE                          
         DS    CL2                                                              
PFINAL   DS    CL1                 FINAL INDICATOR                              
PLINLNQ  EQU   *-PLINED                                                         
         EJECT                                                                  
**********************************************************************          
* ESTIMATE TABLE DSECT                                               *          
**********************************************************************          
         SPACE 1                                                                
ESTTABD  DSECT                                                                  
ESTREC   DS    CL(PMLNQ)           TAPE/SORT RECORD                             
ESTAMT   DS    PL6                 ESTIMATE AMOUNT                              
ESTLNQ   EQU   *-ESTREC                                                         
         EJECT                                                                  
**********************************************************************          
* ++INCLUDES                                                         *          
**********************************************************************          
         SPACE 1                                                                
GOBLOCKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
*                                                                               
* ACGENMODES                                                                    
* ACGENFILE                                                                     
* ACREPWORKD                                                                    
* ACMASTD                                                                       
* ACJOBBERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
*                                                                               
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPPM02 08/17/00'                                      
         END                                                                    
