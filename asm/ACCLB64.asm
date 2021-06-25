*          DATA SET ACCLB64    AT LEVEL 009 AS OF 08/16/00                      
*PHASE T62164A                                                                  
*&&      SET   NOP=N                                                            
CLB64    TITLE '- PC BILLING - DRAFT/UPDATE BILLS'                              
CLB64    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB64**,R8,CLEAR=YES                                          
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         USING PBLKD,R7                                                         
         L     RC,AOVERWRK                                                      
         USING UBWORKD,RC                                                       
*                                                                               
         GOTO1 GETREC              GET BILL RECORD                              
         BNE   EXIT                                                             
         GOTO1 VALLIV              VALIDATE LIVE BILL NUMBER                    
         BNE   EXIT                                                             
         GOTO1 VALACC              VALIDATE ACCOUNT CODES                       
         BNE   EXIT                                                             
         GOTO1 VALBAT              VALIDATE BATCH DETAILS                       
         BNE   EXIT                                                             
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* GET BILL HEADER RECORD AND EXTRACT DETAILS                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GETREC   NTR1  ,                                                                
         LA    R2,IOKEY                                                         
         USING BEDRECD,R2                                                       
         XC    BEDPAS,BEDPAS                                                    
         MVI   BEDPTYP,BEDPTYPQ                                                 
         MVC   BEDPCPY,CUABIN                                                   
         MVI   BEDPSUB,BEDPSUBQ                                                 
         MVC   BEDPBLNO,PBDRAFT#                                                
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BNE   EXIT                                                             
         CLC   BEDPAS(BEDPIND-BEDPAS),IOKEYSAV                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
*                                                                               
         LA    R3,BEDRFST                                                       
         USING BLHELD,R3                                                        
         XR    RF,RF                                                            
GREC02   CLI   BLHEL,0             COPY BLHELD                                  
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
         IC    RF,BLHLN                                                         
         CLI   BLHEL,BLHELQ                                                     
         BE    *+8                                                              
         BXH   R3,RF,GREC02                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   UBBLHEL(0),BLHELD                                                
         DROP  R3                                                               
*                                                                               
         USING BLHELD,UBBLHEL                                                   
GREC04   DS    0H                                                               
         TM    BLHINDS1,BLHIRTOT   TEST TOTALS DEF. NEED UPDATING               
         BO    GREC08                                                           
         L     R3,AIOA                                                          
         LA    R3,ACCORFST(R3)                                                  
         USING JCBELD,R3                                                        
         XR    RF,RF                                                            
GREC06   CLI   JCBEL,0                                                          
         BE    GREC08                                                           
         CLI   JCBELD,JCBELQ                                                    
         BE    *+12                                                             
         IC    RF,JCBLN                                                         
         BXH   R3,RF,GREC06                                                     
         CLC   BLHTSEQ,JCBSEQ      TEST SEQUENCE NUMBER THE SAME                
         BE    GREC10              NO - TOTALS MAY NEED UPDATING                
GREC08   OI    PBINDS1,PBITUPD                                                  
         DROP  R3                                                               
*                                                                               
GREC10   OC    BLHSCH,BLHSCH                                                    
         BNZ   *+10                                                             
         ZAP   BLHSCH,BCPZERO                                                   
         OC    BLHDSC,BLHDSC                                                    
         BNZ   *+10                                                             
         ZAP   BLHDSC,BCPZERO                                                   
         ZAP   PBSRCPC,BLHSCH      SET SURCHARGE/DISCOUNT %                     
         ZAP   PBDSCPC,BLHDSC                                                   
         MVC   CSFMLANG,BLHLANG    SET BILL FORMAT LANGUAGE                     
*                                                                               
GREC12   CLC   BLHCUR,CSBILCUR     TEST BILLING CURRENCY THE SAME               
         BE    GREC14                                                           
*        TEST ANUTHING PENDING ??                                               
*        B YES                                                                  
         MVC   FVXTRA(L'BLHCUR),BLHCUR                                          
         MVC   FVMSGNO,=AL2(AE$BCDMA)                                           
         B     EXITN                                                            
*                                                                               
GREC14   CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY                        
         BE    GREC16                                                           
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    GREC16                                                           
         CLC   BLHRATE,CSEXCRAT    TEST EXCHANGE RATE THE SAME                  
         BE    GREC16                                                           
         GOTO1 AEDTRAT,BOPARM,(L'FVXTRA,FVXTRA),BLHRATE,0                       
         MVC   FVMSGNO,=AL2(AE$BRDMA)                                           
         B     EXITN                                                            
*                                                                               
GREC16   DS    0H                                                               
         TM    BLHINDS1,BLHIAGBR   TEST AUTO-GEN BATCH REF                      
         BO    *+10                                                             
         MVC   PBBILREF,BLHBREF                                                 
         MVC   PBBILMP,BLHBMOS                                                  
         MVC   PBDATC,BLHTRND                                                   
         GOTO1 VDATCON,BOPARM,(2,PBDATC),(1,PBDATP)                             
         MVC   PBDUE,BLHDUED                                                    
         OC    BLHSCH,BLHSCH                                                    
         BNZ   *+10                                                             
         ZAP   BLHSCH,BCPZERO                                                   
         ZAP   PBSRCPC,BLHSCH                                                   
         OC    BLHDSC,BLHDSC                                                    
         BNZ   *+10                                                             
         ZAP   BLHDSC,BCPZERO                                                   
         ZAP   PBDSCPC,BLHDSC                                                   
         TM    BLHINDS1,BLHIAGBN   TEST AUTO-GEN BILL NUMBER                    
         BO    *+10                                                             
         MVC   PBLIVE#,BLHLVNO                                                  
*                                                                               
         LA    R3,BEDRFST                                                       
         USING SPAELD,R3           EXTRACT ACCOUNT CODES                        
         XR    RF,RF                                                            
GREC22   CLI   SPAEL,0                                                          
         BE    GREC30                                                           
         CLI   SPAEL,SPAELQ                                                     
         BNE   GREC28                                                           
         CLI   SPATYPE,SPATDEBT    DEBTORS ACCOUNT                              
         BNE   *+14                                                             
         MVC   PBDEBULA,SPAAULA                                                 
         B     GREC28                                                           
         CLI   SPATYPE,SPATSRCH    SURCHARGE ACCOUNT                            
         BNE   *+14                                                             
         MVC   PBSRCULA,SPAAULA                                                 
         B     GREC28                                                           
         CLI   SPATYPE,SPATDISC    DISCOUNT ACCOUNT                             
         BNE   *+14                                                             
         MVC   PBDSCULA,SPAAULA                                                 
         B     GREC28                                                           
         CLI   SPATYPE,SPATINCA    INCOME ACCRUAL ACCOUNT                       
         BNE   *+14                                                             
         MVC   PBACCULA,SPAAULA                                                 
         B     GREC28                                                           
*                                                                               
GREC28   IC    RF,SPALN                                                         
         BXH   R3,RF,GREC22                                                     
         DROP  R3                                                               
*                                                                               
GREC30   DS    0H                                                               
         LA    R3,BEDRFST                                                       
         USING SCIELD,R3                                                        
         XR    RF,RF                                                            
GREC32   CLI   SCIEL,0                                                          
         BE    GREC40                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   GREC38                                                           
         CLI   SCITYPE,SCITCBAP    SAVE BILL NET/COMMISSION                     
         BNE   GREC34                                                           
         ZAP   PBBHAPN,SCIAMNT                                                  
         ZAP   PBBHAPC,SCIADMN                                                  
         B     GREC38                                                           
GREC34   CLI   SCITYPE,SCITTADV    SAVE ADVANCES NET/COMMISSION                 
         BNE   GREC38                                                           
         ZAP   PBTADVN,SCIAMNT                                                  
         ZAP   PBTADVC,SCIADMN                                                  
*                                                                               
GREC38   IC    RF,SCILN                                                         
         BXH   R3,RF,GREC32                                                     
         DROP  R3                                                               
*                                                                               
GREC40   DS    0H                                                               
         CLI   PBMODE,PBTOTQ       TEST ONLY REFRESHING TOTALS                  
         BE    GREC50                                                           
         ZAP   BODUB1,PBCBAPN      BODUB1 = PENDING NET + ADVANCES NET          
         AP    BODUB1,PBTADVN                                                   
         ZAP   BODUB2,PBCBAPC      BODUB2 = PENDING COM + ADVANCES COM          
         AP    BODUB2,PBTADVC                                                   
         CP    BODUB1,PBBHAPN      MUST EQUAL BILL AMOUNTS                      
         BNE   *+14                                                             
         CP    BODUB2,PBBHAPC                                                   
         BE    GREC50                                                           
         MVC   FVMSGNO,=AL2(AE$DBNEJ)                                           
         B     EXITN                                                            
*                                                                               
GREC50   DS    0H                                                               
         LA    R3,BEDRFST                                                       
         USING BMAELD,R3                                                        
GREC52   CLI   BMAEL,0             PROCESS ACCRUALS                             
         BE    GREC60                                                           
         CLI   BMAEL,BMAELQ                                                     
         BNE   GREC58                                                           
         XR    R4,R4                                                            
         ICM   R4,1,PBMNTHSN                                                    
         BNZ   GREC54                                                           
         CLC   BMAMOA,BLHBMOS      TEST FIRST ENTRY IS BILL MOA                 
         BE    GREC54              NO - ADD ZERO ENTRY                          
         PUSH  USING                                                            
         USING PBMNTHSD,PBMNTHS                                                 
         MVC   PBMMP,BLHBMOS                                                    
         ZAP   PBMPCT,BCPZERO                                                   
         ZAP   PBMAMT,BCPZERO                                                   
         LA    R4,1                                                             
         POP   USING                                                            
GREC54   LA    RE,1(R4)                                                         
         STC   RE,PBMNTHSN                                                      
         MHI   R4,PBMNTHSL                                                      
         LA    R4,PBMNTHS(R4)                                                   
         USING PBMNTHSD,R4                                                      
         MVC   PBMMP,BMAMOA                                                     
         MVC   PBMPCT,BMAPC                                                     
         MVC   PBMAMT,BMAAMT                                                    
         DROP  R4                                                               
*                                                                               
GREC58   DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,BMALN                                                         
         BXH   R3,RF,GREC52                                                     
         DROP  R3                                                               
*                                                                               
GREC60   DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCOUNT CODES                                              *         
***********************************************************************         
         SPACE 1                                                                
VALACC   NTR1  ,                                                                
         CLI   PBMODE,PBTOTQ       NO NEED IF REFRESHING TOTALS                 
         BE    EXITY                                                            
*                                  TEST DEBTORS (RECEIVABLE) ACCOUNT            
         GOTO1 VALACT,BOPARM,PBDEBULA,0                                         
         BNE   EXIT                                                             
*                                                                               
         CP    PBSRCPC,BCPZERO     TEST FOR SURCHARGE                           
         BE    VACC02                                                           
         GOTO1 (RF),(R1),PBSRCULA,LEDGSRC                                       
         BNE   EXIT                                                             
*                                                                               
VACC02   CP    PBDSCPC,BCPZERO     TEST FOR DISCOUNT                            
         BE    VACC04                                                           
         GOTO1 (RF),(R1),PBDSCULA,LEDGDSC                                       
         BNE   EXIT                                                             
*                                                                               
VACC04   CLI   PBMNTHSN,0          TEST FOR ACCRUALS                            
         BE    VACC06                                                           
         GOTO1 (RF),(R1),PBACCULA,LEDGACC                                       
         BNE   EXIT                                                             
*                                                                               
VACC06   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
         PUSH  USING                                                            
VALACT   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,0(R2)                                                    
         CLI   ACTKULA,C' '                                                     
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITN                                                            
         CLI   ACTKACT,C' '                                                     
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITN                                                            
         GOTO1 AGETACT,(R3)                                                     
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH DETAILS                                              *         
***********************************************************************         
         SPACE 1                                                                
VALBAT   NTR1  ,                                                                
         CLC   PBBILREF,BCSPACES   TEST HAVE BILL REF#                          
         BH    VBAT10                                                           
*                                                                               
         CLI   PBMODE,PBLIVEQ      TEST UPDATING                                
         BE    VBAT02                                                           
         MVC   PBBILREF,PBDRAFT#                                                
         CLC   PBBILREF,BCSPACES   NO - USE PART OF DRAFT NUMBER                
         BH    VBAT10                                                           
VBAT02   XR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,BCJOBCOD(RE)                                                  
         MVC   PBBILREF(1),0(RE)                                                
         MVC   PBBILREF+1(3),PBLIVE#+3                                          
*                                                                               
VBAT10   DS    0H                                                               
         MVC   BOFULL1(L'PBBILMP),PBBILMP                                       
         MVI   BOFULL1+L'PBBILMP,X'01'                                          
         MVC   FVIFLD,BCSPACES                                                  
         GOTO1 VDATCON,BOPARM,(1,BOFULL1),(9,FVIFLD)                            
         MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         PUSH  USING                                                            
         USING BMONVALD,BOWORK1                                                 
         GOTO1 VBMONVAL,BOPARM,(10,FVIFLD),('POSTBILL',ACOM),          *        
               (CULANG,BMONVALD),(CUABIN,0)                                     
         MVC   BOBYTE1,0(R1)       BATCH SECURITY LEVEL                         
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    *+14                                                             
         MVC   FVMSGNO,BMOMSG                                                   
         B     EXITN                                                            
*                                                                               
         MVC   PBBILMP,BMOMOSP                                                  
         MVC   PBBILMC,BMOMOSC                                                  
         MVC   CSBSECL,BOBYTE1     SET BATCH SECURITY                           
         POP   USING                                                            
*                                                                               
         LA    RF,X'80'            BATCH REF MAY BE INCREMENTED                 
*        TEST ANY INPUT TO THE REF#                                             
*        BNE   VBAT12                                                           
*        XR    RF,RF                                                            
VBAT12   GOTO1 AADDOBH,BOPARM,('POSTBILL',PBBILREF),((RF),BASACTH)              
         BE    VBAT14                                                           
         CLI   PBMODE,PBLIVEQ      ERROR ONLY IMPORTANT IF LIVE                 
         BE    EXITN                                                            
*                                                                               
VBAT14   DS    0H                                                               
         XR    R0,R0               VALIDAT BATCH HEADERS FOR ACCRUALS           
         ICM   R0,1,PBMNTHSN                                                    
         BZ    VBAT20                                                           
         LA    R4,PBMNTHS                                                       
         USING PBMNTHSD,R4                                                      
VBAT16   MVC   PBMREF,PBBILREF                                                  
         GOTO1 AADDOBH,BOPARM,('POSTBILL',PBMREF),(X'80',BASACTH)               
         BE    VBAT18                                                           
         CLI   PBMODE,PBLIVEQ      ERROR ONLY IMPORTANT IF LIVE                 
         BE    EXITN                                                            
*                                                                               
VBAT18   DS    0H                                                               
         MVC   PBMMC,PBMMP         SET MONTH CHARACTERS                         
         OI    PBMMC,X'F0'                                                      
         TR    PBMMC+1(1),MONTR                                                 
         LA    R4,PBMNTHSL(R4)                                                  
         BCT   R0,VBAT16                                                        
         DROP  R4                                                               
*                                                                               
VBAT20   DS    0H                                                               
*                                                                               
VALBATY  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
MONTR    DC    C'.123456789......ABC'                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIVE BILL NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALLIV   NTR1  ,                                                                
         CLI   PBMODE,PBLIVEQ      ONLY NEED IF UPDATING                        
         BNE   EXITY                                                            
*                                                                               
         CLC   PBLIVE#,BCSPACES                                                 
         BH    VLIV10                                                           
         BAS   RE,GETBNO           GET AUTO NUMBER                              
         BNE   EXIT                                                             
         B     VALLIVY                                                          
*                                                                               
VLIV10   CLI   BCP103,C'N'         TEST MANUAL BILL NUMBERS ALLOWED             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITN                                                            
*                                                                               
         LA    R2,IOKEY            TEST OLD STYLE =CBILL RECORDS                
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS       TEST BILL NUMBER ALREADY USED                
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,PBLIVE#                                                 
         MVI   PBRPIND,BEDPILVE                                                 
*                                                                               
         CLI   P#DUPEOK,C'Y'       TEST DUPLICATE INVOICES ALLOWED              
         BNE   *+16                                                             
         MVC   PBRPUSER,CUUSER                                                  
         MVC   PBRPJOB,BCJOBCOD                                                 
*                                                                               
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,PBRPCRED-PBRPAS-1                                             
         CLI   P#DUPEOK,C'Y'                                                    
         BE    *+8                                                              
         LA    RE,PBRPUSER-PBRPAS-1                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PBRPAS(0),IOKEYSAV                                               
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECAE)                                           
         B     EXITN                                                            
         DROP  R2                                                               
*                                                                               
VLIV12   DS    0H                                                               
         LA    R2,IOKEY                                                         
         USING BEDPAS,R2           TEST PC BILLING RECORDS                      
         XC    BEDPAS,BEDPAS       TEST BILL NUMBER ALREADY USED                
         MVI   BEDPTYP,BEDPTYPQ                                                 
         MVC   BEDPCPY,CUABIN                                                   
         MVI   BEDPSUB,BEDPSUBQ                                                 
         MVC   BEDPBLNO,PBLIVE#                                                 
         MVI   BEDPIND,BEDPILVE                                                 
*                                                                               
         CLI   P#DUPEOK,C'Y'       TEST DUPLICATE INVOICES ALLOWED              
         BNE   *+16                                                             
         MVC   BEDPUSER,CUUSER                                                  
         MVC   BEDPJOB,BCJOBCOD                                                 
*                                                                               
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,BEDPCRED-BEDPAS-1                                             
         CLI   P#DUPEOK,C'Y'                                                    
         BE    *+8                                                              
         LA    RE,BEDPUSER-BEDPAS-1                                             
         EX    RE,*+8                                                           
         B    *+10                                                              
         CLC   BEDPAS(0),IOKEYSAV                                               
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECAE)                                           
         B     EXITN                                                            
*                                                                               
VALLIVY  MVC   CSBILNUM,PBLIVE#                                                 
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NEXT AUTOMATIC BILL NUMBER                           *         
*                                                                     *         
* EXIT: PBLIVE# = BILL NUMBER                                         *         
*       RECORD TO BE UPDATED IN IO5                                   *         
***********************************************************************         
         SPACE 1                                                                
GETBNO   NTR1  ,                                                                
         TM    BCJOBSTA,BCJOBBNA   TEST 'AUTO' IS ALLOWED                       
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITN                                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BCTMONP+1        GET PWOS MONTH                               
         SRDL  R0,4                AND CONVERT TO EBCDIC                        
         STC   R0,UBBMNTH                                                       
         SRL   R1,28                                                            
         STC   R1,UBBMNTH+1                                                     
         OC    UBBMNTH,=X'F0F0'                                                 
*                                                                               
GETBNO02 LA    R2,IOKEY                                                         
         TM    BCJOBSTA,BCJOBBNO   TEST OFFICE LEVEL BILL NUMBERS               
         BZ    GETBNO10                                                         
         USING OGRRECD,R2          READ PRODUCTION OFFICE RECORD                
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,CUABIN                                                   
         MVC   OGRKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   OGRKOFC,CSOFFICE                                                 
         L     R1,AIO5                                                          
         CLC   OGRKEY,0(R1)                                                     
         BE    GETBNO04                                                         
         L     R1,=A(IOREAD+IOACCDIR+IO5)                                       
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IOGETRUP+IOACCMST+IO5)                                     
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO5                                                          
*                                                                               
GETBNO04 LA    R1,OGRRFST-OGRRECD(R1)                                           
         USING BNCELD,R1                                                        
         SR    R0,R0                                                            
GETBNO06 CLI   BNCEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BNCEL,BNCELQ                                                     
         BE    *+14                                                             
         IC    R0,BNCLN                                                         
         AR    R1,R0                                                            
         B     GETBNO06                                                         
         MVC   PBLIVE#,BNCLBIL                                                  
         CLI   BCP102,C'N'         TEST RESET BILL NUMBER                       
         BE    GETBNO08                                                         
         CLC   UBBMNTH,PBLIVE#                                                  
         BE    GETBNO08                                                         
         MVC   PBLIVE#(L'UBBMNTH),UBBMNTH                                       
         MVC   PBLIVE#+L'UBBMNTH(L'BNCRSET),BNCRSET                             
GETBNO08 PACK  BODUB1,PBLIVE#                                                   
         AP    BODUB1,=P'1'                                                     
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  BNCLBIL,BODUB1                                                   
         B     GETBNO22                                                         
*                                                                               
GETBNO10 TM    BCJOBSTA,BCJOBBNL   TEST LEDGER LEVEL BILL NUMBERS               
         BZ    GETBNO12                                                         
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'BCCPYPRD),BCCPYPRD                                     
         B     GETBNO14                                                         
*                                                                               
         USING PMDRECD,R2                                                       
GETBNO12 TM    BCJOBSTA,BCJOBBNM   TEST MEDIA LEVEL BILL NUMBERS                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   PMDKEY,BCSPACES                                                  
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUABIN                                                   
         SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,BCJOBCOD(RE)                                                  
         MVC   PMDKMED,0(RE)                                                    
*                                                                               
GETBNO14 L     R1,AIO5                                                          
         CLC   PMDKEY,0(R1)                                                     
         BE    GETBNO16                                                         
         L     R1,=A(IOREAD+IOACCDIR+IO5)                                       
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IOGETRUP+IOACCMST+IO5)                                     
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO5                                                          
*                                                                               
GETBNO16 LA    R1,PMDRFST-PMDRECD(R1)                                           
         USING PMDELD,R1                                                        
         SR    R0,R0                                                            
GETBNO18 CLI   PMDEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PMDEL,PMDELQ                                                     
         BE    *+14                                                             
         IC    R0,PMDLN                                                         
         AR    R1,R0                                                            
         B     GETBNO18                                                         
         MVC   PBLIVE#,PMDLBILL                                                 
         CLI   BCP102,C'N'         TEST RESET BILL NUMBER                       
         BE    GETBNO20                                                         
         CLC   UBBMNTH,PBLIVE#                                                  
         BE    GETBNO20                                                         
         MVC   PBLIVE#(L'UBBMNTH),UBBMNTH                                       
         MVC   PBLIVE#+L'UBBMNTH(L'PMDRBILL),PMDRBILL                           
GETBNO20 PACK  BODUB1,PBLIVE#                                                   
         AP    BODUB1,=P'1'                                                     
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  PMDLBILL,BODUB1                                                  
*                                                                               
         USING BEDRECD,R2                                                       
GETBNO22 XC    BEDPAS,BEDPAS       TEST BILL NUMBER ALREADY USED                
         MVI   BEDPTYP,BEDPTYPQ    (PC BILLING BILLS)                           
         MVC   BEDPCPY,CUABIN                                                   
         MVI   BEDPSUB,BEDPSUBQ                                                 
         MVC   BEDPBLNO,PBLIVE#                                                 
         MVI   BEDPIND,BEDPILVE                                                 
         GOTO1 AIO,IOHIUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BEDPAS(BEDPUSER-BEDPAS),IOKEYSAV                                 
         BE    GETBNO02                                                         
*                                                                               
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS       TEST BILL NUMBER ALREADY USED                
         MVI   PBRPTYP,PBRPTYPQ    (MIANFRAME =CBILL BILLS)                     
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,PBLIVE#                                                 
         MVI   PBRPIND,PBRPILVE                                                 
         GOTO1 AIO,IOHIUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BE    GETBNO02                                                         
*                                                                               
GETBNOX  OI    PBINDS1,PBIAUTO                                                  
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
LEDGSRC  DC    C'SI',C'SE',C'SQ',X'00'                                          
LEDGDSC  DC    C'SI',C'SE',X'00'                                                
LEDGACC  DC    C'SQ',X'00'                                                      
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
ADDEND   DC    C'ADD=END'                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
* ACCLBFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBFILE                                                      
         PRINT ON                                                               
* ACCLBLINK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBLINK                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
UBWORKD  DSECT                                                                  
UBBLHEL  DS   XL255                                                             
UBBMNTH  DS   CL2                 EBCIDIC MONTH NUMBER FOR BILL RESET           
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACCLB64   08/16/00'                                      
         END                                                                    
