*          DATA SET ACCLB08    AT LEVEL 036 AS OF 08/16/00                      
*PHASE T62108A                                                                  
CLB08    TITLE '- BILL PROGRAM - DISPLAY/CHANGE (LISTPARA)'                     
CLB08    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB8**,R8,R7,R6,CLEAR=YES,RR=RE                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRK,RC                                                       
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
         ST    RB,ABASE1           SAVE BASE REGISTERS                          
         ST    R8,ABASE2                                                        
         ST    R7,ABASE3                                                        
         ST    R6,ABASE4                                                        
         LA    R5,OSVALS           R5=A(OVERLAY SAVED AREA IN TWA)              
         USING OSVALSD,R5                                                       
*                                                                               
         LA    R0,AROUTN           R0=(NUMBER OF NON ADDR ROUTINES)             
         LA    RE,AROUT            RE=A(NON ADDRESS ROUT ADDRESSES)             
         LA    RF,ARELROUT         RF=A(RELOCATED ADDRESSES)                    
MAIN10   L     R1,0(RE)            GET NEXT ADDRESS                             
         A     R1,RELO             RELOCATE IT                                  
         ST    R1,0(RF)            PUT IN RELOACTED LIST                        
         LA    RE,4(RE)            BUMP UP ADDRESS LISTS                        
         LA    RF,4(RF)                                                         
         BCT   R0,MAIN10                                                        
*                                                                               
         LA    R3,LEDTAB           R3=A(LINE EDIT TABE)                         
         LA    RF,LEHLNQ(R3)                                                    
         ST    RF,ALENTRY          DEFAULT TO UK IF NO ENTRY FOUND              
         USING LEDTABD,R3                                                       
MAIN20   CLI   LEHLANG,EOT         END OF TABLE?                                
         BE    MAIN35                                                           
         CLC   CULANG,LEHLANG      MATCH ON LANGUAGE                            
         BE    MAIN30                                                           
         SR    RF,RF                                                            
         IC    RF,LEHLEN           RF=L'(LANGUAGE ENTRY)                        
         AR    R3,RF                                                            
         B     MAIN20                                                           
MAIN30   LA    R3,LEHLNQ(R3)                                                    
         ST    R3,ALENTRY          R3=A(LINE EDIT COMMAND ENTRIES)              
         DROP  R3                                                               
*                                                                               
MAIN35   L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVC   OSVCURD,TIOBCURD    SAVE DISPLACEMENT TO CURSOR POSITION         
         XC    TIOBCURD,TIOBCURD                                                
         DROP  RF                                                               
*                                                                               
         TM    OSVENFLG,OSVENCLQ   RETURNING FROM NTRSES CALL?                  
         BO    MAIN40                                                           
         CLI   TWASCRN,S#BILDIS    DISPLAY SCREEN LOADED?                       
         BE    MAIN60                                                           
         MVCDD OSVMX@OF(L'OSVMX@OF),AC#OF                                       
         GOTO1 VDICTAT,BCDMCB,C'SM  ',OSVMX@OF                                  
         GOTO1 AOVRSCR,BCDMCB,('S#BILDIS',BASOLAYH) LOAD EDIT SCREEN            
         BE    *+6                                                              
         DC    H'0'                                                             
MAIN40   MVI   OSVENFLG,0                                                       
         MVI   BCPFKEY,0           INITITIALISE PFKEY                           
         BAS   RE,GBILL            GET BILL REC                                 
         BNE   ERREXIT                                                          
         BAS   RE,GVATTAB          BUILD ACTUAL AMTS TABLE BY VAT TYPE          
*                                                                               
         BAS   RE,GJOB             GET JOB RECORD                               
         BAS   RE,PROTSCRN         PROTECT SCREEN IF LIVE BILL                  
         B     MAIN95                                                           
*                                                                               
MAIN60   BAS   RE,CMNDLIN          VALIDATE COMMAND LINE                        
         BNE   ERREXIT                                                          
         BAS   RE,EDTSCRN          EDIT SCREEN                                  
         BNE   ERREXIT                                                          
         BAS   RE,LINCMND          DEAL WITH LINE COMMANDS                      
         BNE   ERREXIT                                                          
*                                                                               
MAIN80   DS    0H                                                               
         BAS   RE,SCROLL           DEAL WITH SCROLLING                          
         CLI   OSVPACTV,0          NO ACTIVE PARAGRAPH RECORDS?                 
         BNE   MAIN95                                                           
         MVI   PRMODE,PRADDQ       ADD AN EMPTY PARAGRAPH                       
         MVI   PRPARA,1                                                         
         ZAP   PRNET,=P'0'                                                      
         ZAP   PRCOM,=P'0'                                                      
         MVI   PRTAX,TAXNDQ        TAX NOT DEFINED                              
         MVC   PRDESC,BCSPACES                                                  
         XC    PRALVALS,PRALVALS   SET NULL SO LINE INDEX NOT CHANGED           
         BAS   RE,PARAREC                                                       
         BAS   RE,BILLREC          UPDATE PARA RECORD LINE SEQ CHANGED          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN95   BAS   RE,DISHEAD          DISPLAY HEADER LINE                          
         BAS   RE,DISSCRN          DISPLAY SCREEN                               
         BAS   RE,DISLCMND         DISPLAY LINE COMMANDS                        
         BAS   RE,DEFCURS          DEAL WITH DEFAULT CURSOR POSITION            
MAINX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET A BILL RECORD. BILL NUMBER ASSUMED TO BE IN 'CSBILNUM'          *         
***********************************************************************         
         SPACE 1                                                                
GBILL    NTR1                                                                   
         MVC   DISBILN,CSBILNUM    GET BILL NUMBER                              
         OI    DISBILNH+FLDOIND-FLDHDRD,FOUTTRN                                 
         LA    R4,IOKEY            R4=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,R4                                                       
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ    RECORD TYPE                                  
         MVC   PBRPCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRPSUB,PBRPPASQ    PASSIVE RECORD                               
         MVC   PBRPBLNO,CSBILNUM   BILL NUMBER                                  
*        MVI   PBRPIND,PBRPIDFT    DRAFT BILL                                   
         GOTO1 AIO,IO1+IOACCDIR+IOHIGH GET BILL RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
*        CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV RECORD FOUND?                   
         CLC   PBRPAS(PBRPIND-PBRPAS),IOKEYSAV RECORD FOUND?                    
         BNE   GBILERRX                                                         
         GOTO1 VDATCON,BCDMCB,(2,PBRPCRED),(1,OSVPBCRD) CREATED DATE            
*                                                                               
         GOTO1 AIO,IO1+IOACCMST+IOGET GET BILL RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1             R4=A(BILL RECORD)                            
         MVC   OSVBILD,PBRKBILD    SAVE BILLED DATE                             
         MVC   OSVJOB,PBRKJOB      SAVE JOB                                     
         MVC   OSVSEQ,PBRKSEQ      SAVE SEQUENCE NUMBER (COMPLEMENT)            
         MVI   SCHINDS,0                                                        
         MVI   DSCINDS,0                                                        
*                                                                               
         LA    R4,PBRRFST          R4=A(FIRST ELEMENT)                          
         USING BLHELD,R4                                                        
GBIL10   CLI   BLHEL,EOR           END OF RECORD?                               
         BE    GBIL60                                                           
         CLI   BLHEL,BLHELQ        BILL HEADER ELEMENT?                         
         BE    GBIL30                                                           
         CLI   BLHEL,NDXELQ        INDEX ELEMENT?                               
         BE    GBIL40                                                           
         CLI   BLHEL,SCIELQ        CASH BUCKET?                                 
         BE    GBIL50                                                           
*                                                                               
GBIL20   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,BLHLN                                                         
         AR    R4,R0                                                            
         B     GBIL10                                                           
*                                                                               
GBIL30   SR    RF,RF                                                            
         IC    RF,BLHFORM          BILLING FORMAT                               
         CURED (RF),(L'DISFORM,DISFORM),0,DMCB=BCDMCB,ALIGN=LEFT                
         OI    DISFORMH+FLDOIND-FLDHDRD,FOUTTRN                                 
         TM    SCHINDS,SCHIAMT     TEST ALREADY HAVE SUCHARGE AMOUNT            
         BO    GBIL32                                                           
         OC    BLHSCH,BLHSCH       NO - SAVE % VALUE                            
         BNZ   *+10                                                             
         ZAP   BLHSCH,=P'0'                                                     
         ZAP   SCHAMT,BLHSCH                                                    
GBIL32   TM    DSCINDS,DSCIAMT     TEST ALREADY HAVE DISCOUNT AMOUNT            
         BO    GBIL34                                                           
         OC    BLHDSC,BLHDSC       NO - SAVE % VALUE                            
         BNZ   *+10                                                             
         ZAP   BLHDSC,=P'0'                                                     
         ZAP   DSCAMT,BLHDSC                                                    
GBIL34   B     GBIL20                                                           
*                                                                               
         USING NDXELD,R4                                                        
GBIL40   MVC   OSVPHIGH,NDXHIGH    HIGHEST PARAGRAPH NUMBER                     
         MVC   OSVPACTV,NDXACTV    HIGHEST ACTIVE PARAGRAPH NUMBER              
         SR    RF,RF                                                            
         IC    RF,NDXLN            RF=L'(INDEX ELEMENT)                         
         SH    RF,=Y(NDXINDX+1-NDXELD)                                          
         EX    RF,*+4              RF=(X LENGTH OF INDEX LIST)                  
         MVC   OSVPLST(0),NDXINDX  SAVE PARAGRAPH INDEX LIST                    
         B     GBIL20                                                           
*                                                                               
         USING SCIELD,R4                                                        
GBIL50   CLI   SCITYPE,SCITCBSG    TEST SURCHARGE AMOUNT                        
         BNE   GBIL52                                                           
         OI    SCHINDS,SCHIAMT                                                  
         ZAP   SCHAMT,SCIAMNT                                                   
         B     GBIL20                                                           
GBIL52   CLI   SCITYPE,SCITCBDC    TEST DISCOUNT AMOUNT                         
         BNE   GBIL20                                                           
         OI    DSCINDS,DSCIAMT                                                  
         ZAP   DSCAMT,SCIAMNT                                                   
         B     GBIL20                                                           
         DROP  R4                                                               
*                                                                               
GBIL60   TM    SCHINDS,SCHIAMT     SURCHARGE AMOUNT OR PERCENT?                 
         BZ    GBIL62                                                           
         CURED SCHAMT,(L'DISSCH,DISSCH),CSCURBIL,DMCB=BCDMCB,ALIGN=LEFT         
         B     GBIL64                                                           
GBIL62   CURED SCHAMT,(L'DISSCH,DISSCH),2,DMCB=BCDMCB,ALIGN=LEFT                
         LA    RF,L'DISSCH-2                                                    
         LA    RE,DISSCH(RF)                                                    
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-12                                                          
         MVI   1(RE),C'%'                                                       
GBIL64   OI    DISSCHH+FLDOIND-FLDHDRD,FOUTTRN                                  
*                                                                               
         TM    DSCINDS,DSCIAMT     DISCOUNT AMOUNT OR PERCENT?                  
         BZ    GBIL66                                                           
         CURED DSCAMT,(L'DISDSC,DISDSC),CSCURBIL,DMCB=BCDMCB,ALIGN=LEFT         
         B     GBIL68                                                           
GBIL66   CURED DSCAMT,(L'DISDSC,DISDSC),2,DMCB=BCDMCB,ALIGN=LEFT                
         LA    RF,L'DISDSC-2                                                    
         LA    RE,DISDSC(RF)                                                    
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-12                                                          
         MVI   1(RE),C'%'                                                       
GBIL68   OI    DISDSCH+FLDOIND-FLDHDRD,FOUTTRN                                  
*                                                                               
         LA    R2,DISJOBH          R2=A(JOB FIELD HEADER)                       
         USING FLDHDRD,R2                                                       
         MVC   DISJOB,OSVJOB       DISPLAY JOB CODE                             
         OI    FLDOIND,FOUTTRN                                                  
         LA    R2,DISBILNH                                                      
         MVC   DISBILN,CSBILNUM    DISPLAY BILL NUMBER                          
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED BIT ON              
         OI    FLDOIND,FOUTTRN                                                  
         MVI   OSVSTCOL,1          SET FIRST DISPLAY COLUMN TO START            
         MVI   OSVSTLIN,1          SET FIRST DISPLAY LINE TO START              
         LA    R2,DISLCM1H         R2=A(FIRST TEXT LINE FIELD HEADER)           
         SR    R2,RA                                                            
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVI   TIOBCURI,0          SET CURSOR INDEX TO START OF FIELD           
         STCM  R2,3,TIOBCURD       OVERRIDE DEFAULT CURSOR POSITION             
         DROP  RF                                                               
*                                                                               
GBILX    CR    RB,RB               CC EQUAL FOR OK EXIT                         
         B     EXIT                                                             
GBILERRX LTR   RB,RB               CC UNEQUAL FOR ERROR EXIT                    
         B     ERREXIT                                                          
         EJECT                                                                  
***********************************************************************         
* GET ALLOCATION DETAILS OFF THE JOB RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
GJOB     NTR1                                                                   
         LA    R4,IOKEY            R4=A(KEY FOR PRODUCTION BILL RECORD)         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKULA(L'BCCPYPRD),BCCPYPRD PRODUCTION UNIT/LEDGER              
         MVC   ACTKACT,OSVJOB      JOB ACCOUNT                                  
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET JOB RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO1             R4=A(JOB RECORD)                             
         LA    R4,ACTRFST          R4=A(FIRST ELEMENT)                          
         USING SCIELD,R4                                                        
GJOB10   CLI   SCIEL,EOR           END OF RECORD?                               
         BE    GJOBX                                                            
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT?                     
         BE    GJOB30                                                           
*                                                                               
GJOB20   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,SCILN                                                         
         AR    R4,R0                                                            
         B     GJOB10                                                           
*                                                                               
GJOB30   CLI   SCITYPE,SCITCBAP    CLIENT BILLING - PENDING ALLOC TYPE?         
         BNE   GJOB20                                                           
         LA    R2,DISALLCH                                                      
         USING TLINED,R2                                                        
         CURED SCIAMNT,(L'TLNET,TLNET),CSCURBIL,MINUS=YES,DMCB=BCDMCB, X        
               ALIGN=LEFT                                                       
         CURED SCIADMN,(L'TLCOM,TLCOM),CSCURBIL,MINUS=YES,DMCB=BCDMCB, X        
               ALIGN=LEFT                                                       
         ZAP   BCDUB,SCIAMNT       GET TOTAL AMOUNT                             
         AP    BCDUB,SCIADMN                                                    
         CURED BCDUB,(L'TLTOT,TLTOT),CSCURBIL,MINUS=YES,DMCB=BCDMCB             
         OI    FLDOIND-FLDHDRD(R2),FOUTTRN                                      
         DROP  R2,R4                                                            
*                                                                               
GJOBX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD ACTUAL AMOUNTS BY VAT TYPE TABLE BY READING ALL PARA RECORDS  *         
***********************************************************************         
         SPACE 1                                                                
GVATTAB  NTR1                                                                   
         MVI   OSVVATTB,0          INIT VAT TABLE                               
         ZAP   OSVTBNET,=P'0'      INIT TOTAL NET FOR BILL                      
         ZAP   OSVTBCOM,=P'0'      INIT TOTAL COMMISSION FOR BILL               
         ZAP   OSVTBVAT,=P'0'      INIT TOTAL VAT FOR BILL                      
         ZAP   OSVTBTOT,=P'0'      INIT TOTAL BILL AMOUNT                       
         SR    R0,R0                                                            
         IC    R0,OSVPACTV         R0=(NUMBER OPF ACTIVE PARA RECORDS)          
         LA    R2,OSVPLST          R2=A(PARAGRAPH INDEX LIST)                   
GVAT10   LA    R3,IOKEY            R3=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,R3                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,0(R2)      PARAGRAPH#                                   
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET PARAGRAPH RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1             R3=A(LINE NUMBER RECORD)                     
         LA    R3,PBRRFST          R3=A(FIRST ELEMENT)                          
         USING PGHELD,R3                                                        
GVAT20   CLI   PGHEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PGHEL,PGHELQ        PARAGRAPH HEADER ELEMENT?                    
         BE    GVAT30                                                           
*                                                                               
         SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,PGHLN                                                         
         AR    R3,RE                                                            
         B     GVAT20                                                           
*                                  PARAGRAPH HEADER ELEMENT                     
GVAT30   LA    RE,VATMAXQ          RE=(MAX NUMBER OF VAT TYPES)                 
         LA    R4,OSVVATTB         R4=A(VAT TABLE)                              
         USING VATTABD,R4                                                       
GVAT40   CLI   VATTYPE,EOT         END OF TABLE?                                
         BE    GVAT60                                                           
         B     GVAT50                                                           
*        CLI   PGHTAX,0            NO TAX ENTERED?                              
*        BNE   *+12                                                             
*        CLI   VATTYPE,TAXNDQ      PREVIOUS ENTRY FOR NO TAX DEFINED?           
*        BE    GVAT50                                                           
*        CLC   VATTYPE,PGHTAX      MATCH ON PREVIOUS ENTRY?                     
*        BE    GVAT50                                                           
*        LA    R4,VATLNQ(R4)                                                    
*        BCT   RE,GVAT40                                                        
*        DC    H'0'                                                             
*                                                                               
GVAT50   AP    VATNET,PGHNET       INCREMENT NET AMOUNT                         
         AP    VATCOM,PGHCOM       INCREMENT COMMISSION AMOUNT                  
         B     GVAT80                                                           
*                                                                               
GVAT60   CLI   PGHTAX,0            TAX TYPE NOT DEFINED?                        
         BNE   *+12                                                             
         MVI   VATTYPE,TAXNDQ      SET TAX NOT DEFINED                          
         B     *+10                                                             
         MVC   VATTYPE,PGHTAX      VAT TYPE                                     
         ZAP   VATRATE,=P'0'       INIT VAT RATE                                
         ZAP   VATNET,PGHNET       NET AMOUNT                                   
         ZAP   VATCOM,PGHCOM       COMMISSION AMOUNT                            
         B     GVAT70                                                           
*        CLI   VATTYPE,TAXNDQ      ANY TAX TYPE?                                
*        BE    GVAT70                                                           
*        LA    R1,WORKLST                                                       
*        USING VTCD,R1                                                          
*        XC    VTCD(VTCLNQ),VTCD                                                
*        MVI   VTCACTN,VTCALOOK    LOOK UP VAT TYPE ATRRIBUTES                  
*        MVC   VTCCPY,CUABIN       COMPANY CODE                                 
*        MVC   VTCOFFC,CSOFFICE    OFFICE                                       
*        MVC   VTCCPYS1,BCCPYST1   COMPANY STATUS BYTE 1                        
*        MVC   VTCTYPE,PGHTAX      VAT TYPE                                     
*        MVC   VTCCOMF,ACOM        A(COMFACS)                                   
*        MVC   VTCINVD,OSVPBCRD    CREATED DATE                                 
*        GOTO1 VVATICAN                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        SR    RF,RF                                                            
*        ICM   RF,3,VTCRATE                                                     
*        CVD   RF,BCDUB                                                         
*        ZAP   VATRATE,BCDUB       VAT RATE                                     
GVAT70   MVI   VATLNQ(R4),EOT      SET NEW END OF TABLE                         
*                                                                               
GVAT80   LA    R2,1(R2)            BUMP TO NEXT PARAGRAPH INDEX                 
         BCT   R0,GVAT10                                                        
*                                  GET TOTALS FOR BILL                          
         LA    R4,OSVVATTB         R4=A(VAT TABLE)                              
         USING VATTABD,R4                                                       
GVAT90   CLI   VATTYPE,EOT         END OF TABLE?                                
         BE    GVATX                                                            
         AP    OSVTBNET,VATNET     INCREMENT TOTAL NET FOR BILL                 
         AP    OSVTBCOM,VATCOM     INCREMENT TOTAL COMM FOR BILL                
         ZAP   BCDUB,VATNET        GET BILL NET                                 
         AP    BCDUB,VATCOM        PLUS BILL COMMISSION                         
         AP    OSVTBTOT,BCDUB                                                   
         ZAP   PL16,VATRATE                                                     
         MP    PL16,BCDUB          (VAT RATE)*(NET+COMMISSION)                  
         BZ    GVAT100                                                          
         SRP   PL16,64-4,5         DIVIDE BY 100.00                             
         AP    OSVTBVAT,PL16       INCREMENT TOTAL VAT FOR BILL                 
         AP    OSVTBTOT,OSVTBVAT   BILL TOTAL (NET+COMMISION+VAT)               
GVAT100  LA    R4,VATLNQ(R4)                                                    
         B     GVAT90                                                           
GVATX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROTECT SCREEN IF LIVE BILL                                         *         
***********************************************************************         
         SPACE 1                                                                
PROTSCRN LR    R0,RE                                                            
         OC    OSVBILD,OSVBILD     DRAFT BILL?                                  
         BZ    PROTSX                                                           
*                                                                               
         LA    R2,DISLCM1H         R2=A(FIRST TEXT LINE FIELD HEADER)           
         USING FLDHDRD,R2                                                       
PROTS10  CLI   FLDLEN,0                                                         
         BE    PROTSX                                                           
         OI    FLDATB,FATBPROT                                                  
         OI    FLDOIND,FOUTTRN                                                  
         SR    RF,RF                                                            
         IC    RF,FLDLEN                                                        
         AR    R2,RF                                                            
         B     PROTS10                                                          
*                                                                               
PROTSX   LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT NET/COMMISSION AMOUNTS                                         *         
***********************************************************************         
         SPACE 1                                                                
EDTSCRN  NTR1                                                                   
         MVC   NXTLOFF,OSVSTLIN    SET NEXT LINE NUMBER TO START LINE           
         MVI   PRMODE,0            CLEAR PARAGRAPH MODE                         
         LA    R3,MAXSLINQ         R3=(NUMBER OF TEXT LINES)                    
         LA    R2,DISLCM1H         R2=A(1ST LINE COMMAND FIELD HEADER)          
         USING SLINED,R2                                                        
*                                                                               
         MVC   FVMSGNO,=AL2(AE$INVAM) INVALID AMOUNT                            
EDTS10   XC    PRNET(L'PRNET+L'PRCOM),PRNET INIT AMOUNTS                        
         TM    SLNETH+FLDIIND-FLDHDRD,FINPVAL NET CHANGED?                      
         BO    EDTS20                                                           
         LA    RF,SLNETH                                                        
         ST    RF,FVADDR                                                        
         OI    BCAPINDS,BCAPIBAU      SET AMT CHNGED FLAG FOR CALLER            
         SR    RF,RF                                                            
         IC    RF,SLNETH+FLDILEN-FLDHDRD RF=L'(INPUT)                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BCDMCB,(BOBYTE1,SLNET),(X'40',(RF))                     
         CLI   0(R1),0                                                          
         BNE   EDTSERRX                                                         
         ZAP   PRNET,4(8,R1)       GET NEW AMOUNT                               
         CURED PRNET,(L'SLNET,SLNET),CSCURBIL,MINUS=YES,DMCB=BCDMCB,   X        
               ALIGN=LEFT                                                       
EDTS20   TM    SLCOMH+FLDIIND-FLDHDRD,FINPVAL COMMISSION CHANGED?               
         BO    EDTS30                                                           
         LA    RF,SLCOMH                                                        
         ST    RF,FVADDR                                                        
         OI    BCAPINDS,BCAPIBAU      SET AMT CHNGED FLAG FOR CALLER            
         SR    RF,RF                                                            
         IC    RF,SLCOMH+FLDILEN-FLDHDRD RF=L'(INPUT)                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BCDMCB,(BOBYTE1,SLCOM),(X'40',(RF))                     
         CLI   0(R1),0                                                          
         BNE   EDTSERRX                                                         
         ZAP   PRCOM,4(8,R1)       GET NEW AMOUNT                               
         CURED PRCOM,(L'SLCOM,SLCOM),CSCURBIL,MINUS=YES,DMCB=BCDMCB,   X        
               ALIGN=LEFT                                                       
*                                                                               
EDTS30   OC    PRNET(L'PRNET+L'PRCOM),PRNET AMOUNT(S) CHANGED?                  
         BZ    EDTS70                                                           
EDTS40   MVI   PRMODE,PRCHANGQ     ACTION CHANGE                                
         MVC   PRPARA,NXTLOFF      PARAGRAPH OFFSET                             
         BAS   RE,PARAREC          CHANGE PARAGRAPH RECORD                      
*                                                                               
         LA    R4,OSVVATTB         R4=A(VAT TABLE)                              
         USING VATTABD,R4                                                       
EDTS50   CLI   VATTYPE,EOT         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EDTS60                                                           
*        CLI   SLTAXCOD,C' '       TAX NOT DEFINED?                             
*        BH    *+12                                                             
*        CLI   VATTYPE,TAXNDQ      MATCH ON NOT DEFINED ENTRY?                  
*        BE    EDTS60                                                           
*        CLC   VATTYPE,SLTAXCOD    MATCH ON VAT TYPE?                           
*        BE    EDTS60                                                           
*        LA    R4,VATLNQ(R4)                                                    
*        B     EDTS50                                                           
*                                                                               
EDTS60   OC    PRNET,PRNET         NET DIFFERENCE?                              
         BZ    *+10                                                             
         SP    VATNET,PRNET        ADJUST NET AMOUNT                            
         OC    PRCOM,PRCOM         COMMISSION DIFFERENCE?                       
         BZ    EDTS70                                                           
         SP    VATCOM,PRCOM        ADJUST COMMISSION AMOUNT                     
*                                                                               
EDTS70   SR    RF,RF               BUMP LINE NUMBER                             
         IC    RF,NXTLOFF                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NXTLOFF                                                       
         LA    R2,SLLNQ(R2)        BUMP TO NEXT LINE                            
         BCT   R3,EDTS10                                                        
*                                                                               
         CLI   PRMODE,0            ANY CHANGES?                                 
         BE    *+8                                                              
         BAS   RE,BTOTAL           CALCULATE TOTALS FOR BILL                    
*                                                                               
EDTSX    CR    RB,RB                                                            
         B     EXIT                                                             
EDTSERRX LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY HEADER LINES                                                *         
***********************************************************************         
         SPACE 1                                                                
DISHEAD  NTR1                                                                   
         LA    R2,DISPARAH         R2=A(PARAGRAPH FIELD HEADER)                 
         USING FLDHDRD,R2                                                       
         MVC   FLDDATA(L'DISPARA),BCSPACES                                      
         CLI   OSVPACTV,0          NO ACTIVE PARA RECORDS?                      
         BE    DISH20                                                           
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN         RF=(PARA START PARA NUMBER)                  
         LA    R3,FLDDATA          R3=A(PARA NUMBER FIELD)                      
         CURED (RF),(3,0(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AR    R3,R0               BUMP UP BY LENGTH OF PARA NUMBER             
         SR    R4,R4                                                            
         IC    R4,OSVPACTV         R4=(NUMBER OF ACTIVE PARA RECORDS)           
         CLC   OSVSTLIN,OSVPACTV   START OF DISPLAY IS LAST PARA?               
         BE    DISH10                                                           
         MVI   0(R3),C'-'                                                       
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN                                                      
         LA    RF,MAXSLINQ-1(RF)                                                
         CR    RF,R4                                                            
         BNH   *+6                                                              
         LR    RF,R4               RF=(END PARA NUMBER)                         
         LA    R3,1(R3)                                                         
         CURED (RF),(3,0(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AR    R3,R0                                                            
DISH10   MVC   1(L'OSVMX@OF,R3),OSVMX@OF                                        
         LA    R3,L'OSVMX@OF(R3)                                                
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         CURED (R4),(3,2(R3)),0,DMCB=BCDMCB,ALIGN=LEFT ACTIVE PARAS             
DISH20   OI    FLDOIND,FOUTTRN                                                  
*                                                                               
DISHX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY PARAGRAPH DETAIL LINES                                      *         
***********************************************************************         
         SPACE 1                                                                
DISSCRN  NTR1                                                                   
         OI    BASSCRH+(FVOIND-FVIHDR),FVOXMT XMIT FLD SO TIOB UPDATED          
         MVC   NXTLOFF,OSVSTLIN    SET NEXT LINE NUMBER TO START LINE           
         LA    R4,MAXSLINQ         R4=(NUMBER OF TEXT LINES)                    
         LA    R2,DISLCM1H         R2=A(1ST LINE COMMAND FIELD HEADER)          
         USING SLINED,R2                                                        
*                                                                               
DISS10   MVC   SLLCM,BCSPACES     CLEAR LINE COMMAND                            
         OI    SLLCMH+FLDOIND-FLDHDRD,FOUTTRN                                   
         MVC   SLPDV(SLPDVLNQ),BCSPACES                                         
         OI    SLPDVH+FLDOIND-FLDHDRD,FOUTTRN                                   
         MVC   SLNET,BCSPACES                                                   
         OI    SLNETH+FLDOIND-FLDHDRD,FOUTTRN                                   
         OI    SLNETH+FLDIIND-FLDHDRD,FINPVAL                                   
         MVC   SLCOM,BCSPACES                                                   
         OI    SLCOMH+FLDOIND-FLDHDRD,FOUTTRN                                   
         OI    SLCOMH+FLDIIND-FLDHDRD,FINPVAL                                   
         MVC   SLTT(SLTTLNQ),BCSPACES                                           
         OI    SLTTH+FLDOIND-FLDHDRD,FOUTTRN                                    
         CLC   NXTLOFF,OSVPACTV    NO MORE PARAGRAPHS TO DISPLAY?               
         BH    DISS80                                                           
*                                                                               
         OC    OSVBILD,OSVBILD     IF LIVE BILL FIELDS REMAIN PROTECTED         
         BNZ   DISS15                                                           
         NI    SLLCMH+FLDATB-FLDHDRD,X'FF'-FATBPROT                             
*        NI    SLNETH+FLDATB-FLDHDRD,X'FF'-FATBPROT                             
*        NI    SLCOMH+FLDATB-FLDHDRD,X'FF'-FATBPROT                             
*                                                                               
DISS15   SR    RF,RF                                                            
         IC    RF,NXTLOFF                                                       
         CVD   RF,BCDUB                                                         
         UNPK  SLPARA,BCDUB        DISPLAY PARAGRAPH NUMBER                     
         OI    SLPARA+L'SLPARA-1,X'F0'                                          
         LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         SR    RE,RE                                                            
         IC    RE,NXTLOFF          OFFSET TO LINE INDEX                         
         BCTR  RE,0                                                             
         LA    RE,OSVPLST(RE)      RE=A(CORRESPONDING INDEX FOR PARA)           
         MVC   PBRKPARA,0(RE)      PARAGRAPH#                                   
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET PARAGRAPH RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             RF=A(LINE NUMBER RECORD)                     
         LA    R3,PBRRFST          R3=A(FIRST ELEMENT)                          
         USING PGHELD,R3                                                        
DISS20   CLI   PGHEL,EOR           END OF RECORD?                               
         BE    DISS130                                                          
         CLI   PGHEL,PGHELQ        PARAGRAPH HEADER ELEMENT?                    
         BE    DISS40                                                           
         CLI   PGHEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    DISS60                                                           
*                                                                               
DISS30   SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,PGHLN                                                         
         AR    R3,RE                                                            
         B     DISS20                                                           
*                                  PARAGRAPH HEADER ELEMENT                     
DISS40   OI    SLCOMH+FLDOIND-FLDHDRD,FOUTTRN TRANSMIT FIELD                    
         CURED PGHNET,(L'SLNET,SLNET),CSCURBIL,MINUS=YES,DMCB=BCDMCB,  X        
               ALIGN=LEFT                                                       
         OI    SLNETH+FLDOIND-FLDHDRD,FOUTTRN TRANSMIT FIELD                    
         CURED PGHCOM,(L'SLCOM,SLCOM),CSCURBIL,MINUS=YES,DMCB=BCDMCB,  X        
               ALIGN=LEFT                                                       
*        MVI   SLTAXCOD,C' '       TAX CODE                                     
*        CLI   PGHTAX,0            TAX CODE NOT DEFINED?                        
*        BE    *+10                                                             
*        MVC   SLTAXCOD,PGHTAX     TAX CODE                                     
         ZAP   BCDUB,PGHNET         GET NET                                     
         AP    BCDUB,PGHCOM         PLUS COMMISSION                             
         BZ    DISS30                                                           
*        LA    RF,OSVVATTB         RF=A(VAT TABLE ATTRIBUTES)                   
*        USING VATTABD,RF                                                       
*ISS50   CLI   VATTYPE,EOT         VAT TYPE NOT FOUND?                          
*        BE    DISS55                                                           
*        CLC   VATTYPE,PGHTAX      MATCH ON VAT TYPE?                           
*        BE    *+12                                                             
*        LA    RF,VATLNQ(RF)                                                    
*        B     DISS50                                                           
*        ZAP   PL16,VATRATE                                                     
*        BZ    DISS55                                                           
*        DROP  RF                                                               
*        MP    PL16,BCDUB          (VAT RATE)*(NET+COMMISSION)                  
*        SRP   PL16,64-4,5         DIVIDE BY 100.00                             
*        ZAP   PL8,PL16                                                         
*        CURED PL8,(L'SLTAX,SLTAX),CSCURBIL,MINUS=YES,DMCB=BCDMCB               
*        AP    BCDUB,PL16                                                       
DISS55   CURED BCDUB,(L'SLTOTAL,SLTOTAL),CSCURBIL,MINUS=YES,DMCB=BCDMCB         
         B     DISS30                                                           
*                                                                               
         USING FFTELD,R3                                                        
DISS60   CLI   FFTTYPE,FFTTPGHC    PARAGRAPH HEADER COMMENT?                    
         BNE   DISS30                                                           
         SR    RF,RF                                                            
         ICM   RF,1,FFTDLEN        RF=L'(HEADER COMMENT)                        
         BZ    DISS30                                                           
         CH    RF,=Y(L'SLDESC)     TRUNCATE DESCRIPTION IF TOO LONG             
         BNH   *+8                                                              
         LA    RF,L'SLDESC                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SLDESC(0),FFTDATA  DISPLAY HEADER COMMENT                        
         B     DISS30                                                           
*                                  NO MORE PARAGRAPHS TO DISPLAY                
DISS80   OI    SLLCMH+FLDATB-FLDHDRD,FATBPROT                                   
*        OI    SLNETH+FLDATB-FLDHDRD,FATBPROT                                   
*        OI    SLCOMH+FLDATB-FLDHDRD,FATBPROT                                   
         SR    RF,RF                                                            
         IC    RF,OSVPACTV         RF=(NUMBER OF ACTIVE PARA INDEXES)           
         LA    RF,1(RF)                                                         
         CLM   RF,1,NXTLOFF        NEXT PARA AFTER END OF TEXT DISP?            
         BNE   DISS130                                                          
         LR    RE,R2               RE=A(SCREEN LINE AFTER END OF TEXT)          
         SR    RE,RA                                                            
         CLM   RE,3,OSVCURD        WILL CURSOR BE ON PROTECTED SCREEN?          
         BH    DISS90                                                           
         SH    RE,=Y(SLLNQ)        SET CURS TO LAST SCREEN LIN WITH TXT         
         L     RF,AINP                                                          
         MVI   TIOBCURI-TIOBD(RF),0                                             
         B     DISS110                                                          
DISS90   LA    RE,SLTTH                                                         
         SH    RE,=Y(SLLNQ)        RE=A(LINE NUM FLD ON LAST WITH TXT)          
         SR    RE,RA                                                            
         CLM   RE,3,OSVCURD        CURSOR ON LAST TOTAL FIELD?                  
         BNH   DISS100                                                          
         SH    RE,=Y(SLTTH-SLCOMH)                                              
         CLM   RE,3,OSVCURD        CURSOR ON LAST COMMISSION FLD?               
         BNH   DISS100                                                          
         SH    RE,=Y(SLCOMH-SLNETH)                                             
         CLM   RE,3,OSVCURD        CURSOR ON LAST NET FIELD?                    
         BNH   DISS100                                                          
         SH    RE,=Y(SLNETH-SLPDVH)                                             
         CLM   RE,3,OSVCURD        CURSOR ON PARA/DESC FIELD?                   
         BNH   DISS100                                                          
         SH    RE,=Y(SLPDVH-SLLCMH)                                             
         CLM   RE,3,OSVCURD        CURSOR ON LAST ACTION FIELD?                 
         BH    DISS130                                                          
DISS100  L     RF,AINP                                                          
DISS110  OC    TIOBCURD-TIOBD(L'TIOBCURD,RF),TIOBCURD-TIOBD(RF)                 
         BNZ   DISS130                                                          
         STCM  RE,3,TIOBCURD-TIOBD(RF) OVERRIDE DEFAULT CURSOR CONTROL          
*                                                                               
DISS130  SR    RF,RF               BUMP LINE NUMBER                             
         IC    RF,NXTLOFF                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NXTLOFF                                                       
DISS140  LA    R2,SLLNQ(R2)        BUMP TO NEXT SCREEN LINE                     
         BCT   R4,DISS10                                                        
         DROP  R2                                                               
*                                                                               
         LA    R2,DISBILH          R2=A(BILL TOTAL FIELDS)                      
         USING TLINED,R2                                                        
         OI    TLTOTLNH+(FLDOIND-FLDHDRD),FOUTTRN                               
         CURED OSVTBNET,(L'TLNET,TLNET),CSCURBIL,MINUS=YES,            X        
               DMCB=BCDMCB,ALIGN=LEFT                                           
         CURED OSVTBCOM,(L'TLCOM,TLCOM),CSCURBIL,MINUS=YES,            X        
               DMCB=BCDMCB,ALIGN=LEFT                                           
*        CURED OSVTBVAT,(L'TLTAX,TLTAX),CSCURBIL,MINUS=YES,DMCB=BCDMCB          
         CURED OSVTBTOT,(L'TLTOT,TLTOT),CSCURBIL,MINUS=YES,DMCB=BCDMCB          
         DROP  R2                                                               
*                                                                               
DISS210  MVC   FVMSGNO,=AL2(AI$EPARA) ENTER PARAGRAPH DETAILS                   
         TM    BCAPINDS,BCAPIBAU      SET AMT CHNGED FLAG FOR CALLER            
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(AI$DETCH) DETAILS CHANGED                           
         OC    OSVBILD,OSVBILD        LIVE BILL IS DISPLAY ONLY                 
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$RECDS) RECORD DISPLAYED                          
         TM    OSVEBFLG,BLKCOUTQ                                                
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AI$BLKIN) BLOCK COMMAND INCOMPLETE                  
         B     DISSX                                                            
         TM    OSVEBFLG,BLKPENDQ                                                
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(AI$MCPEN) MOVE/COPY IS PENDING                      
DISSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LINE COMMANDS                                               *         
***********************************************************************         
         SPACE 1                                                                
DISLCMND NTR1                                                                   
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN         RF=(LINE NO AT START OF CURR SCREEN)         
         LA    RE,MAXSLINQ-1(RF)   RE=(LINE NO AT END OF CURR SCREEN)           
         CLM   RE,1,OSVPACTV       BEYOND NUMBER OF PARAGRAPHS IN BILL?         
         BNH   *+8                                                              
         IC    RE,OSVPACTV                                                      
         STC   RE,BCBYTE2          PARA NUMBER AT END OF SCREEN                 
*                                                                               
         LA    R3,EBLTAB           R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
DISL10   CLI   EBLCTYPE,EOT        END OF TABLE?                                
         BE    DISLX                                                            
         SR    R2,R2                                                            
         IC    R2,EBLNM            R2=(MAX NUMBER OF ENTRIES IN LIST)           
         SR    R1,R1                                                            
         ICM   R1,3,EBLDIS                                                      
         LA    R1,OSVALSD(R1)      R1=A(BLOCK VALUES LIST)                      
         USING BLKVALD,R1                                                       
         ICM   RE,15,ALENTRY       RE=A(LINE EDIT COMMAND ENTRIES)              
         USING LEDTABD,RE                                                       
DISL40   CLI   LENCTYPE,EOT        END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LENCTYPE,EBLCTYPE   MATCH ON COMMAND TYPE                        
         BE    *+12                                                             
         LA    RE,LENLNQ(RE)       BUMP TO NEXT ENTRY                           
         B     DISL40                                                           
         ST    RE,BCFULL           SAVE ENTRY FOR CURRENT BLOCK TYPE            
         DROP  RE                                                               
DISL50   OC    BLKSTRT(BLKLNQ),BLKSTRT NO MORE VALUES?                          
         BZ    DISL80                                                           
         CLC   BLKSTRT,OSVSTLIN    START CMND BEFORE START OF SCREEN?           
         BL    DISL60                                                           
         CLC   BLKSTRT,BCBYTE2                                                  
         BH    DISL60              START CMND AFTER END OF SCREEN?              
         MVC   BCBYTE1,BLKSTRT                                                  
         BAS   RE,FRMTCOM          FORMAT START COMMAND ON SCREEN               
DISL60   CLI   BLKLEN,0            END CMND NOT NEEDED FOR NUMERIC CMND         
         BNE   DISL70                                                           
         CLC   BLKEND,OSVSTLIN     END CMND BEFORE START OF SCREEN?             
         BL    DISL70                                                           
         CLC   BLKEND,BCBYTE2      END CMND AFTER END OF SCREEN?                
         BH    DISL70                                                           
         MVC   BCBYTE1,BLKEND                                                   
         BAS   RE,FRMTCOM          FORMAT END COMMAND ON SCREEN                 
DISL70   LA    R1,BLKLNQ(R1)       BUMP TO NEXT BLOCK LIST VALUE                
         BCT   R2,DISL50                                                        
DISL80   LA    R3,EBLLNQ(R3)       BUMPT TO NEXT EDIT TABLE ENTRY               
         B     DISL10                                                           
DISLX    B     EXIT                                                             
         DROP  R1,R3                                                            
         SPACE 1                                                                
***********************************************************************         
* FORMAT COMMAND LINE ON SCREEN                                       *         
* ON NTRY R1=A(ENTRY IN BLOCK VALUES LIST)                            *         
***********************************************************************         
         SPACE 1                                                                
FRMTCOM  NTR1                                                                   
         USING BLKVALD,R1                                                       
         SR    R2,R2                                                            
         IC    R2,BCBYTE1          R2=(LINE NUMBER OF COMAND)                   
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN         RF=(LINE NUMBER OF SCREEN START)             
         SR    R2,RF                                                            
         MH    R2,=Y(DISLCM2H-DISLCM1H)                                         
         LA    R2,DISLCM1H(R2)     R2=A(LINE CMND FIELD HEADER)                 
         USING FLDHDRD,R2                                                       
         MVI   FLDILEN,2           SET LENGTH                                   
         SR    RF,RF                                                            
         ICM   RF,15,BCFULL        RF=A(COMMAND ENTRY)                          
         MVC   FLDDATA(L'DISLCM1),LENCOMM-LEDTABD(RF)                           
         CLI   BLKLEN,1            NUMERIC COMMAND HIGHER THAN 1 LINE?          
         BH    FRMT10                                                           
         CLI   BLKLEN,0            BLOCK COMMAND?                               
         BE    FRMTCX                                                           
         MVI   FLDDATA+1,C' '      GET RID OF SECOND CHAR                       
         MVI   FLDILEN,1                                                        
         B     FRMTCX                                                           
FRMT10   SR    RF,RF               DISPLAY NUMERIC PART OF COMMAND              
         IC    RF,BLKLEN                                                        
         CURED (RF),(2,FLDDATA+1),0,DMCB=BCDMCB,ALIGN=LEFT                      
         AH    R0,=H'1'                                                         
         STC   R0,FLDILEN                                                       
FRMTCX   B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* DEAL WITH LINE COMMANDS                                             *         
***********************************************************************         
         SPACE 1                                                                
LINCMND  NTR1                                                                   
         CLI   OSVCMND,CMNRESQ     RESET LINE COMMANDS?                         
         BNE   *+14                                                             
         XC    OSVEVAL(OSVELNQ),OSVEVAL INIT ALL LINE EDIT VARIABLES            
         B     LINCX                                                            
*                                                                               
         NI    OSVEBFLG,X'FF'-BLKPENDQ SWITCH OFF COPY/MOVE IS PENDING          
         MVI   TOTINRP,0           INIT TOTAL OF INSERT AND REPEATS             
         BAS   RE,INBVAL           INIT BLOCK VALUES ON CURRENT SCREEN          
*                                                                               
         MVC   NXTLOFF,OSVSTLIN    SET NEXT LINE NUMBER TO START LINE           
         LA    R0,MAXSLINQ         R0=(NUMBER OF TEXT LINES)                    
         LA    R2,DISLCM1H         R2=A(FIRST LINE EDIT FIELD HEADER)           
         USING FLDHDRD,R2                                                       
*                                                                               
         MVC   FVMSGNO,=AL2(AE$INVIF) INVALID INPUT                             
LINC10   ST    R2,FVADDR                                                        
         CLC   FLDDATA(L'DISLCM1),BCSPACES ANY INPUT?                           
         BNH   LINC100                                                          
*                                                                               
         OC    FLDDATA(L'DISLCM1),BCSPACES PAD OUT FIELD WITH SPACES            
         MVI   BLEN,0              BLOCK LENGTH IF NUMERIC COMMAND              
         MVI   BFR1,0              START LINE NUMBER FOR COMMAND                
         MVI   BFR2,0              END LINE NUMBER FOR COMMAND                  
         ICM   R3,15,ALENTRY       R3=A(LINE EDIT COMMAND ENTRIES)              
         USING LEDTABD,R3                                                       
LINC40   CLI   LENCOMM,EOT         END OF TABLE?                                
         BE    LINCERRX                                                         
         CLC   LENCOMM,FLDDATA     MATCH ON COMMAND?                            
         BNE   LINC50                                                           
         CLI   FLDILEN,2           NON NUMERIC BLOCK COMMAND?                   
         BE    LINC80                                                           
         B     LINC60              ASSUME SINGLE CHAR IS NUMERIC CMND           
LINC50   CLC   LENCOMM(1),FLDDATA  MATCH ON FIRST CHAR?                         
         BNE   LINC70                                                           
         SR    RF,RF               MUST BE FOLLOWED BY BLANKS OR NUMBER         
         IC    RF,FLDILEN                                                       
         SH    RF,=H'1'                                                         
         BZ    LINC60                                                           
         GOTO1 VCASHVAL,BCDMCB,(C'N',FLDDATA+1),(RF)                            
         CLI   0(R1),0                                                          
         BNE   LINC70                                                           
         ICM   RF,15,4(R1)                                                      
         BNZ   *+8                                                              
LINC60   LA    RF,1                DEFAULT TO 1 IF NO NUMERIC INPUT             
         STC   RF,BLEN             SET BLOCK LENGTH                             
         B     LINC80                                                           
*                                                                               
LINC70   LA    R3,LENLNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
         B     LINC40                                                           
*                                                                               
LINC80   MVC   BFR1,NXTLOFF        SAVE LINE NUMBER OF COMMAND                  
         BAS   RE,CHKCONF          CHECK FOR CONFLICT ETC                       
         BNE   LINCERRX                                                         
         CLI   LENCTYPE,LENDELQ    DELETE COMMAND?                              
         BNE   LINC90                                                           
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         OC    TIOBCURD,TIOBCURD   DEFAULT CURSOR CONTROL OVERWRITTEN?          
         BNZ   LINC90                                                           
         LR    RE,R2                                                            
         SR    RE,RA                                                            
         MVI   TIOBCURI,0          SET CURSOR TO START OF FIELD                 
         STCM  RE,3,TIOBCURD       KEEP CURSOR ON SAME LINE COMMAND             
         DROP  RF                                                               
LINC90   BAS   RE,CHKBSIZE         CHECK SIZE OF BILL NOT EXCEEDED              
         BNE   LINCERRX                                                         
*                                                                               
LINC100  LA    R2,DISLCM2H-DISLCM1H(R2) BUMP TO NXT LINE CMND FIELD HDR         
         SR    RF,RF               BUMP SCREEN LINE NUMBER                      
         IC    RF,NXTLOFF                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NXTLOFF                                                       
         BCT   R0,LINC10                                                        
         DROP  R2,R3                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         MVC   FVADDR,OSVECONF                                                  
         TM    OSVEBFLG,BLKPCONQ                                                
         BNO   *+12                                                             
         TM    OSVEBFLG,BLKCOUTQ                                                
         BO    LINCERRX                                                         
         LA    R3,EBLTAB           R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
         MVI   OSVECTYP,0          INIT SAVED COMMAND TYPE                      
LINC110  CLI   EBLCTYPE,EOT        END OF TABLE                                 
         BE    LINC120                                                          
         SR    RF,RF                                                            
         ICM   RF,3,EBLDIS         RF=(DISBLOCK LIST FOR COMMAND)               
         LA    RF,OSVALSD(RF)      RF=A(BLOCK LIST FOR COMMAND)                 
         OC    0(BLKLNQ,RF),0(RF)  ANYTHING IN THE LIST?                        
         BZ    *+10                                                             
         OC    OSVECTYP,EBLCTYPE   SET COMMAND IS IN USE                        
         LA    R3,EBLLNQ(R3)       BUMP TO NEXT TABLE ENTRY                     
         B     LINC110                                                          
         DROP  R3                                                               
*                                                                               
LINC120  TM    OSVEBFLG,BLKCOUTQ   BLOCK COMMAND OUTSTANDING?                   
         BO    LINCX                                                            
         LA    R3,EBLTAB           R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
LINC160  CLI   EBLCTYPE,EOT        END OF TABLE?                                
         BE    LINC170                                                          
         SR    RF,RF                                                            
         ICM   RF,3,EBLFUNC                                                     
         BZ    *+10                                                             
         LA    RF,CLB08(RF)        RF=A(EDIT FUNCTION)                          
         BASR  RE,RF               EXECUTE EDIT COMMAND                         
         LA    R3,EBLLNQ(R3)       BUMP TO NEXT TABLE ENTRY                     
         B     LINC160                                                          
         DROP  R3                                                               
*                                                                               
LINC170  OC    OSVECTYP,OSVECTYP   ANY COMMANDS USED?                           
         BZ    LINCX                                                            
         BAS   RE,BILLREC          UPDATE BILL RECORD LINE SEQ CHANGED          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LINCX    CR    RB,RB                                                            
         B     EXIT                                                             
LINCERRX LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK FOR COMMAND CONFLICTS AND BUILD BLOCK VALUE LISTS             *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
CHKCONF  NTR1                                                                   
         USING LEDTABD,R3                                                       
         LA    RF,EBLTAB           RF=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,RF                                                       
CHKC10   CLI   EBLCTYPE,EOT        END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LENCTYPE,EBLCTYPE   MATCH ON COMMAND                             
         BE    *+12                                                             
         LA    RF,EBLLNQ(RF)       BUMP TO NEXT ENTRY                           
         B     CHKC10                                                           
*                                                                               
         CLI   BLEN,1                                                           
         BNH   *+12                                                             
         TM    EBLSTAT,EBLNONMQ    NUMERIC COMMAND NOT ALLOWED?                 
         BO    CHKCERRX                                                         
         MVC   PRECCMP,EBLPRECM    SAVE COMPATIBLE PRIOR CMNDS                  
         MVC   PROCCMP,EBLPROCM    SAVE COMPATIBLE PROCEEDING COMMANDS          
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NUMBER OF ENTRIES IN LIST)           
         SR    R2,R2                                                            
         ICM   R2,3,EBLDIS                                                      
         LA    R2,OSVALSD(R2)      R2=A(BLOCK VALUE LIST)                       
         USING BLKVALD,R2                                                       
         DROP  RF                                                               
*                                                                               
         TM    LENCTYPE,LENAFTQ+LENBEFQ AFTER/BEFORE?                           
         BZ    CHKC20                                                           
         MVC   OSVETOTY,LENCTYPE   SAVE 'TO' COMMAND TYPE                       
*                                  CHECK CONFLICT WITH PRIOR COMMAND            
CHKC20   SR    RF,RF                                                            
         ICM   RF,1,BLEN           BLOCK COMMAND?                               
         BZ    CHKC30                                                           
         SR    RE,RE                                                            
         IC    RE,BFR1             RE=(START LINE NUM OF NUMERIC CMND)          
         AR    RF,RE                                                            
         BCTR  RF,0                                                             
         STC   RF,BFR2             RF=(END LINE NUM OF NUMERIC CMND)            
CHKC30   LA    RF,EBLTAB           R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,RF                                                       
CHKC40   CLI   EBLCTYPE,EOT        END OF TABLE?                                
         BE    CHKC120                                                          
         SR    RE,RE                                                            
         IC    RE,EBLNM            RE=(MAX NUMBER OF ENTRIES IN LIST)           
         SR    R1,R1                                                            
         ICM   R1,3,EBLDIS                                                      
         LA    R1,OSVALSD(R1)      R1=A(BLOCK LIST OF COMMANDS)                 
         MVC   BCBYTE1,PRECCMP     COMPATIBLE PRECEEDING COMMANDS               
         NC    BCBYTE1,EBLCTYPE                                                 
         MVC   BCBYTE2,PROCCMP     COMPATIBLE PROCEEDING COMMANDS               
         NC    BCBYTE2,EBLCTYPE                                                 
*                                                                               
CHKC50   OC    0(BLKLNQ,R1),0(R1)  END OF BLOCK LIST?                           
         BZ    CHKC110                                                          
*                                  CHECK PRECEEDING COMMANDS                    
         CLC   BFR1,BLKSTRT-BLKVALD(R1) PRECEEDING COMMAND IN LIST?             
         BL    CHKC80                                                           
         CLI   BCBYTE1,0           LIST COMMAND TOTALLY COMPATIBLE?             
         BNE   CHKC100                                                          
CHKC60   CLI   BLKEND-BLKVALD(R1),0 INCOMPLETE BLOCK CMND PRECEEDING?           
         BNE   CHKC70                                                           
         CLI   BLEN,0              IF NUMERIC COMMAND THEN CONFLICT             
         BNE   CHKCERRX                                                         
         B     CHKC100                                                          
CHKC70   CLC   BFR1,BLKEND-BLKVALD(R1) PRECEEDING CMND OVERLAPS CMND?           
         BNH   CHKCERRX                                                         
         B     CHKC100                                                          
*                                  CHECK PROCEEDING OFF SCREEN CMNDS            
CHKC80   CLI   BCBYTE2,0           LIST COMMAND TOTALLY COMPATIBLE?             
         BNE   CHKC100                                                          
         CLI   BLEN,0              BLOCK COMMAND?                               
         BNE   CHKC90                                                           
         TM    OSVEBFLG,BLKCOUTQ   BLOCK COMMAND OUTSTANDING?                   
         BO    CHKC100                                                          
         OI    OSVEBFLG,BLKPCONQ   SET POTENTIAL CONFLICT SITUATION             
         MVC   OSVECONF,FVADDR     SAVE ADDRESS OF LINE CMND FIELD HDR          
         B     CHKC100                                                          
CHKC90   CLC   BFR2,BLKSTRT-BLKVALD(R1) CMND OVERLAP?                           
         BNL   CHKCERRX            COMMAND CONFLICT                             
CHKC100 LA     R1,BLKLNQ(R1)       BUMP TO NEXT BLOCK LIST VALUE                
         BCT   RE,CHKC50                                                        
CHKC110 LA     RF,EBLLNQ(RF)       BUMP TO NEXT LINE COMMAND ENTRY              
         B     CHKC40                                                           
*                                                                               
CHKC120  CLI   BLEN,0              NUMERIC OR BLOCK COMMAND?                    
         BNE   CHKC160                                                          
*                                  DEAL WITH BLOCK COMMAND                      
         OC    BLKSTRT(BLKLNQ),BLKSTRT TOTALLY FREE ENTRY?                      
         BNZ   CHKC130                                                          
         MVC   BLKSTRT,BFR1        SET START OF BLOCK                           
         TM    OSVEBFLG,BLKCOUTQ   BLOCK COMMAND OUTSTANDING?                   
         BO    CHKCERRX                                                         
         OI    OSVEBFLG,BLKCOUTQ   SET BLOCK COMMAND OUTSTANDING                
         B     CHKC180                                                          
*                                                                               
CHKC130  CLI   BLKEND,0            ONLY ONE ENTRY? (HALF A BLOCK CMND)          
         BNE   CHKC140                                                          
         NI    OSVEBFLG,X'FF'-BLKCOUTQ SWITCH OFF BLOCK COMMAND OUTSTND         
         CLC   BLKSTRT,BFR1        NEW COMMAND FALLS BEFORE OR AFTER?           
         BH    *+14                                                             
         MVC   BLKEND,BFR1         AFTER                                        
         B     CHKC180                                                          
         MVC   BLKEND,BLKSTRT      IF BEFORE SWAP ROUND                         
         MVC   BLKSTRT,BFR1                                                     
         B     CHKC180                                                          
*                                  FULL BLOCK CMND ENTRY                        
CHKC140  CLC   BLKEND,BFR1         OLD END BEFORE NEW CMND?                     
         BL    CHKC170                                                          
         CLC   BLKSTRT,BFR1        OLD START AFTER NEW CMND?                    
         BH    CHKC150                                                          
         XC    BFR1,BLKEND         SWAP NEW CMND WITH OLD END                   
         XC    BLKEND,BFR1                                                      
         XC    BFR1,BLKEND                                                      
         B     CHKC170                                                          
*                                                                               
CHKC150  MVC   BCBYTE1,BFR1        SAVE NEW CMND                                
         MVC   BFR1,BLKEND         SET NEW CMND WITH OLD END                    
         MVC   BLKEND,BLKSTRT      SWAP OLD START WITH OLD END                  
         MVC   BLKSTRT,BCBYTE1     SWAP OLD START WITH NEW CMND                 
         B     CHKC170                                                          
*                                  DEAL WITH NUMERIC COMMAND                    
CHKC160  OC    BLKSTRT(BLKLNQ),BLKSTRT TOTALLY FREE ENTRY?                      
         BNZ   CHKC170                                                          
         MVC   0(BLKLNQ,R2),BFR1                                                
         B     CHKC180                                                          
*                                                                               
CHKC170  LA    R2,BLKLNQ(R2)       BUMP TO NEXT BLOCK LIST VALUE                
         BCT   R0,CHKC120                                                       
         B     CHKCERRX            BLOCK LIST FULL                              
         DROP  R2                                                               
*                                                                               
CHKC180  OC    OSVECPYB,OSVECPYB                                                
         BZ    CHKC190                                                          
         OC    OSVEMVEB,OSVEMVEB                                                
         BNZ   CHKCERRX                                                         
CHKC190  OC    OSVEBFLN,OSVEBFLN                                                
         BZ    *+14                                                             
         OC    OSVEAFLN(L'OSVEAFLN),OSVEAFLN                                    
         BNZ   CHKCERRX                                                         
*                                                                               
CHKCX    CR    RB,RB                                                            
         B     EXIT                                                             
CHKCERRX MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  RF,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK SIZE/END OF BILL NOT EXCEEEDED                                *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
CHKBSIZE NTR1                                                                   
         USING LEDTABD,R3                                                       
         TM    LENCTYPE,LENAFTQ+LENBEFQ BEFORE/AFTER CMND?                      
         BZ    *+14                                                             
         MVC   OSVETOAC,OSVPACTV   SAVE NUM OF ACTV LINES IN 'TO' PARA          
         B     CHKB10                                                           
         TM    LENCTYPE,LENCOPYQ+LENMOVEQ MOVE/COPY COMMAND?                    
         BZ    CHKBX                                                            
         TM    OSVETOTY,LENAFTQ+LENBEFQ HAD AFTER/BEFORE/OVER?                  
         BZ    CHKBX                                                            
CHKB10   TM    OSVEBFLG,BLKCOUTQ   ANY OUTSTANDING BLOCK COMMANDS?              
         BO    CHKBX                                                            
         USING BLKVALD,R1                                                       
         LA    R1,OSVECPYB         R1=A(COPY BLOCK LIST)                        
         CLI   BLKSTRT,0           NO START VALUE FOUND?                        
         BNE   CHKB20                                                           
         LA    R1,OSVEMVEB         R1=A(MOVE BLOCK LIST)                        
         CLI   BLKSTRT,0           NO START VALUE FOUND?                        
         BE    CHKBX                                                            
CHKB20   SR    RF,RF                                                            
         IC    RF,BLKEND           RF=(MOVE/COPY END LINE NUMBER)               
         SR    RE,RE                                                            
         IC    RE,BLKSTRT          RE=(MOVE/COPY START LINE NUMBER)             
         SR    RF,RE                                                            
         LA    RF,1(RF)            RF=L'(MOVE/COPY BLOCK)                       
*                                                                               
         LA    R1,OSVETOLN         R1=A(FIRST 'TO' TYPE COMMAND VALUES)         
         LA    R0,L'OSVETOLN/BLKLNQ R0(NUMBER OF 'TO' TYPE COMMANDS)            
         CLI   0(R1),EOT           COMMAND TYPE UNUSED?                         
         BNE   *+14                                                             
         LA    R1,BLKLNQ(R1)       BUMP TO NEXT COMMAND VALUE LIST              
         BCT   R0,*-12                                                          
         DC    H'0'                NO 'TO' COMMAND FOUND                        
         CLI   OSVEMVEB,0          MOVE COMMAND?                                
         BNE   CHKBX                                                            
CHKB30   SR    RE,RE                                                            
         IC    RE,BLKLEN           RE=(NUMBER OF REPEATS)                       
         STCM  RE,3,BCHALF                                                      
         MH    RF,BCHALF           RF=(NUMBER OF NEW LINES)                     
         SR    RE,RE                                                            
         IC    RE,OSVETOAC         RE=(NUM OF ACTVE LINES IN 'TO' PARA)         
         B     CHKB50                                                           
CHKB50   SR    R0,R0                                                            
         IC    R0,TOTINRP          R0=(NUM OF NEW LINS FROM PREV CMNDS)         
         AR    RF,R0                                                            
         STC   RF,TOTINRP                                                       
         AR    RF,RE               RF=(NUM OF LINES IN RESULTING PARA)          
         MVC   FVMSGNO,=AL2(AE$MPARA) MAXIMUM PARA SIZE EXCEEDED                
         CH    RF,=Y(MAXPLINQ)                                                  
         BH    CHKBERRX                                                         
         B     CHKBX                                                            
*                                                                               
CHKB60   MVC   FVMSGNO,=AL2(AE$EPEX) END OF PARAGRAPH EXCEEDED                  
         SR    R0,R0                                                            
         IC    R0,BLKSTRT          R0=('TO' LINE FOR OVER CMND)                 
         BCTR  R0,0                RF=(LAST LINE NUMBER OF BLOCK)               
         AR    RF,R0                                                            
         CR    RF,RE               BEYOND NUMBER OF ACTIVE LINES?               
         BH    CHKBERRX                                                         
*                                                                               
CHKBX    CR    RB,RB                                                            
         B     EXIT                                                             
CHKBERRX LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE PARAGRAPHS FROM A BILL                                       *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
DEL      NTR1                                                                   
         USING EBLTABD,R3                                                       
         CLI   OSVPACTV,0                                                       
         BE    DELX                                                             
         SR    R4,R4                                                            
         ICM   R4,3,EBLDIS                                                      
         LA    R4,OSVALSD(R4)      R4=A(DELETE LIST VALUES)                     
         USING BLKVALD,R4                                                       
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NO OF ENTRIES IN LIST)               
         DROP  R3                                                               
DEL10    OC    BLKSTRT(BLKLNQ),BLKSTRT END OF LIST?                             
         BZ    DELX                                                             
         CLI   BLKLEN,0            NUMERICAL COMMAND?                           
         BE    DEL15                                                            
         CLC   BLKEND,OSVPACTV     NUMERICAL DELETE PAST PARAGRAPH END?         
         BNH   DEL20                                                            
         MVC   BLKEND,OSVPACTV     SET DELETE END TO PARA END                   
DEL15    SR    RF,RF                                                            
         IC    RF,BLKEND           RECALCULATE BLOCK LENGTH                     
         SR    RE,RE                                                            
         IC    RE,BLKSTRT                                                       
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,BLKLEN                                                        
DEL20    SR    R3,R3                                                            
         IC    R3,BLKLEN                                                        
         SR    R2,R2                                                            
         IC    R2,BLKSTRT          R2=(BLOCK START NUMBER)                      
         LA    R2,OSVPLST-1(R2)                                                 
DEL22    LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION PARA RECORD)         
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,0(R2)      PARAGRAPH#                                   
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET PARAGRAPH RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             RF=A(LINE NUMBER RECORD)                     
         LA    RF,PBRRFST          RF=A(FIRST ELEMENT)                          
         USING PGHELD,RF                                                        
DEL23    CLI   PGHEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PGHEL,PGHELQ        PARAGRAPH HEADER ELEMENT?                    
         BE    DEL30                                                            
*                                                                               
         SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,PGHLN                                                         
         AR    RF,RE                                                            
         B     DEL23                                                            
*                                  PARAGRAPH HEADER ELEMENT                     
DEL30    LA    RE,OSVVATTB         R4=A(VAT TABLE)                              
         USING VATTABD,RE                                                       
DEL40    CLI   VATTYPE,EOT         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
*        CLC   VATTYPE,PGHTAX      MATCH ON VAT TYPE?                           
*        BE    *+12                                                             
*        LA    RE,VATLNQ(RE)                                                    
*        B     DEL40                                                            
*                                                                               
         SP    VATNET,PGHNET       ADJUST NET AMOUNT                            
         SP    VATCOM,PGHCOM       ADJUST COMMISSION AMOUNT                     
         LA    R2,1(R2)                                                         
         BCT   R3,DEL22                                                         
         DROP  RE,RF                                                            
*                                                                               
         MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         MVC   CMPTO,OSVPHIGH      MOVE TO OFFSET                               
         MVC   CMPFROM1,BLKSTRT    MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,BLKEND     MOVE FROM OFFSET 2 (END)                     
         LA    RF,OSVPLST          RF=A(LINE INDEX LIST)                        
         ST    RF,CMPAILST                                                      
         MVI   CMPILLN,L'OSVPLST   L'(LINE INDEX LIST)                          
         BAS   RE,CMI                                                           
         SR    RF,RF                                                            
         ICM   RF,1,BLKLEN         NUMERIC DELETE COMMAND?                      
         SR    RE,RE                                                            
         IC    RE,OSVPACTV                                                      
         SR    RE,RF                                                            
         STC   RE,OSVPACTV         RESET NUMBER OF LINES IN PARA                
         STC   RF,ALTBDVAL                                                      
         MVC   ALTBSTRT,BLKEND                                                  
         MVI   ALTBEND,X'FF'                                                    
         MVI   ALTBSTAT,ALTBSUBQ                                                
         MVI   ALTBCTYP,0                                                       
         BAS   RE,ALTBLKV          ADJUST ANY OTHER LINE COMMANDS               
         OI    LINEFLAG,LCHANGEQ   SET LINE INDEX LIST CHANGED                  
         LA    R4,BLKLNQ(R4)       BUMP TO NEXT LIST VALUE                      
         BCT   R0,DEL10                                                         
         DROP  R4                                                               
*                                                                               
DELX     XC    OSVEDELB,OSVEDELB   CLEAR OUT DELETE LIST                        
         BAS   RE,BTOTAL           CALCULATE TOTALS FOR BILL                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MOVE PARAGRAPHS WITHIN THE BILL                                     *         
* NTRY R3=A(LINE EDIT TABLE ENTRY FOR MOVE)                           *         
***********************************************************************         
         SPACE 1                                                                
MOVEP    NTR1                                                                   
         USING EBLTABD,R3                                                       
         SR    R2,R2                                                            
         ICM   R2,3,EBLDIS                                                      
         LA    R2,OSVALSD(R2)      R2=A(COPY/MOVE VALUES LIST)                  
         USING BLKVALD,R2                                                       
         OC    BLKSTRT(BLKLNQ),BLKSTRT ANY COPY VALUES?                         
         BNZ   MVE10                                                            
         OC    OSVETOLN,OSVETOLN       ANY 'TO' VALUES?                         
         BZ    MVEX                                                             
         B     *+14                                                             
*                                                                               
MVE10    OC    OSVETOLN,OSVETOLN                                                
         BNZ   *+12                                                             
         OI    OSVEBFLG,BLKPENDQ   MOVE IS PENDING                              
         B     MVEX                                                             
*                                                                               
         NI    OSVEBFLG,X'FF'-BLKPENDQ SWITCH OFF MOVE IS PENDING               
         CLI   BLKLEN,0            NUMERICAL COMMAND?                           
         BE    MVE20                                                            
         CLC   BLKEND,OSVPACTV     IF NUMERICAL COMMAND PAST PARA END           
         BNH   MVE30                                                            
         MVC   BLKEND,OSVPACTV     THEN RESET WITH PARA END                     
MVE20    SR    RE,RE               CALCULATE BLOCK LENGTH                       
         IC    RE,BLKEND                                                        
         SR    RF,RF                                                            
         IC    RF,BLKSTRT                                                       
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BLKLEN                                                        
MVE30    MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         CLI   OSVETOTY,LENBEFQ    BEFORE COMMAND?                              
         BNE   *+8                                                              
         OI    CMPMODE,CMPBEFRQ    MOVE MODE                                    
         LA    RF,OSVETOLN         RF=A('TO' VALUES LISTS)                      
         LA    R0,L'OSVETOLN/BLKLNQ R0=(NUMBER OF 'TO' LISTS)                   
         CLI   0(RF),0             EMPTY LIST?                                  
         BNE   *+14                                                             
         LA    RF,BLKLNQ(RF)       BUMP TO NEXT ONE                             
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
         MVC   CMPTO,BLKSTRT-BLKVALD(RF) SAVE 'TO' START LINE                   
         MVC   TOLINE,CMPTO        MOVE TO OFFSET                               
         MVC   CMPFROM1,BLKSTRT    MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,BLKEND     MOVE FROM OFFSET 2 (END)                     
         LA    RF,OSVPLST                                                       
         ST    RF,CMPAILST                                                      
         MVI   CMPILLN,L'OSVPLST                                                
         BAS   RE,CMI                                                           
         MVC   ALTBDVAL,BLKLEN                                                  
         MVC   ALTBSTRT,BLKSTRT                                                 
         MVI   ALTBEND,X'FF'                                                    
         MVI   ALTBCTYP,0                                                       
         MVI   ALTBSTAT,ALTBSUBQ                                                
         BAS   RE,ALTBLKV          ADJUST ANY OTHER LINE COMMANDS               
*                                                                               
         SR    RE,RE                                                            
         IC    RE,TOLINE           RE=('TO' LINE)                               
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN         RF=(SCREEN START LINE)                       
         CR    RE,RF               IS 'TO' LINE ON CURRENT SCREEN?              
         BL    MVE120                                                           
         LA    R1,MAXSLINQ-1(RF)   R1=(SCREEN END LINE)                         
         CR    RE,R1                                                            
         BH    MVE120                                                           
*                                  'TO' LINE IS ON CURRENT SCREEN               
         SR    RE,RF                                                            
         MH    RE,=Y(SLLNQ)                                                     
         LA    RE,DISLCM1H(RE)     RE=A(LINE COMMAND FIELD HEADER)              
         B     *+12                                                             
*                                                                               
MVE120   STC   RE,OSVSTLIN                                                      
         LA    RE,DISLCM1H         RE=A(FIRST COMMAND FIELD HEADER)             
         L     RF,AINP                                                          
         USING TIOBD,RF                                                         
         SR    RE,RA                                                            
         STCM  RE,3,TIOBCURD       OVERRIDE DEFAULT CURSOR CONTROL              
         MVI   TIOBCURI,0          SET CURSOR TO BEGINING OF FIELD              
         DROP  RF                                                               
*                                                                               
MVE180   XC    BLKSTRT(BLKLNQ),BLKSTRT CLEAR COPY/MOVE BLOCK LISTS              
         XC    OSVETOLN,OSVETOLN   CLEAR 'TO' BLOCK LISTS                       
MVEX     B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* COPY A PARAGRAPH                                                    *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
COPYP    NTR1                                                                   
         USING EBLTABD,R3                                                       
         LA    R2,OSVECPYB         R2=A(COPY/MOVE VALUES LIST)                  
         USING BLKVALD,R2                                                       
         XC    WORKTTXT,WORKTTXT                                                
         OC    BLKSTRT(BLKLNQ),BLKSTRT ANY COPY VALUES?                         
         BNZ   CPYP01                                                           
         OC    OSVETOLN,OSVETOLN       ANY 'TO' VALUES?                         
         BZ    CPYPX                                                            
         B     *+14                                                             
*                                                                               
CPYP01   OC    OSVETOLN,OSVETOLN                                                
         BNZ   *+12                                                             
         OI    OSVEBFLG,BLKPENDQ COPY/MOVE IS PENDING                           
         B     CPYPX                                                            
*                                                                               
         NI    OSVEBFLG,X'FF'-BLKPENDQ SWITCH OFF COPY/MOVE IS PENDING          
         CLI   BLKLEN,0            NUMERICAL COMMAND?                           
         BE    CPYP02                                                           
         CLC   BLKEND,OSVPACTV     IF NUMERICAL COMMAND PAST PARA END           
         BNH   CPYP03                                                           
         MVC   BLKEND,OSVPACTV     THEN RESET WITH PARA END                     
CPYP02   SR    RE,RE               CALCULATE BLOCK LENGTH                       
         IC    RE,BLKEND                                                        
         SR    RF,RF                                                            
         IC    RF,BLKSTRT                                                       
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BLKLEN                                                        
CPYP03   SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NO OF ENTRIES IN BLOCK LIST)         
         OC    BLKSTRT(BLKLNQ),BLKSTRT ANY COPY VALUES?                         
         BZ    CPYPX                                                            
         SR    RF,RF                                                            
         LA    RF,IOKEY            RF=A(LINE RECORD KEY)                        
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         SR    RE,RE                                                            
         IC    RE,BLKSTRT          OFFSET TO LINE INDEX                         
         BCTR  RE,0                                                             
         LA    RE,OSVPLST(RE)      RE=A(CORRESPONDING INDEX FOR LINE)           
         MVC   PBRKPARA,0(RE)      LINE#                                        
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE NUMBER RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             RF=A(TEXT LINE RECORD)                       
         MVC   FROMPARA,PBRKPARA   FROM PARA INDEX                              
         LA    RF,PBRRFST          RF=A(FIRST ELEMENT)                          
         USING PGHELD,RF                                                        
CPYP10   CLI   PGHEL,EOR           END OF RECORD?                               
         BE    CPYP60                                                           
         CLI   PGHEL,PGHELQ        PARAGRAPH HEADER ELEMENT?                    
         BE    CPYP30                                                           
         CLI   PGHEL,NDXELQ        INDEX ELEMENT?                               
         BE    CPYP40                                                           
         CLI   PGHEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    CPYP50                                                           
*                                                                               
CPYP20   SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,PGHLN                                                         
         AR    RF,RE                                                            
         B     CPYP10                                                           
*                                                                               
CPYP30   ZAP   PRNET,PGHNET        GET 'FROM' PARA NET AMOUNT                   
         ZAP   PRCOM,PGHCOM        GET 'FROM' PARA COMMISSION AMOUNT            
         MVC   PRTAX,PGHTAX        GET TAX CODE                                 
         LA    RE,OSVVATTB         R4=A(VAT TABLE)                              
         USING VATTABD,RE                                                       
CPYP35   CLI   VATTYPE,EOT         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   VATTYPE,PGHTAX      MATCH ON VAT TYPE?                           
         BE    *+12                                                             
         LA    RE,VATLNQ(RE)                                                    
         B     CPYP35                                                           
*                                                                               
         AP    VATNET,PGHNET       ADJUST NET AMOUNT                            
         AP    VATCOM,PGHCOM       ADJUST COMMISSION AMOUNT                     
         B     CPYP20                                                           
         DROP  RE,RF                                                            
*                                                                               
         USING NDXELD,RF           INDEX ELEMENT                                
CPYP40   MVC   FROMIVAL(FROMLNQ),NDXHIGH GET 'FROM' LINE INDEX VALUES           
         B     CPYP20                                                           
         DROP  RF                                                               
*                                                                               
         USING FFTELD,RF                                                        
CPYP50   CLI   FFTTYPE,FFTTPGHC    PARAGRAPH HEADER COMMENT?                    
         BNE   CPYP20                                                           
         MVC   PRDESC,BCSPACES                                                  
         SR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         LA    R1,L'PRDESC                                                      
         CR    RE,R1                                                            
         BNH   *+6                                                              
         LR    RE,R1                                                            
         SH    RE,=H'1'                                                         
         BM    CPYP20                                                           
         EX    RE,*+4                                                           
         MVC   PRDESC(0),FFTDATA                                                
         B     CPYP20                                                           
         DROP  RF                                                               
*                                                                               
CPYP60   LA    RF,OSVETOLN         RF=A('TO' VALUES LISTS)                      
         LA    R0,L'OSVETOLN/BLKLNQ R0=(NUMBER OF 'TO' LISTS)                   
         CLI   0(RF),EOT           EMPTY LIST?                                  
         BNE   *+14                                                             
         LA    RF,BLKLNQ(RF)       BUMP TO NEXT ONE                             
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,BLKSTRT-BLKVALD(RF) SAVE 'TO' START LINE                      
         CLI   OSVETOTY,LENAFTQ    AFTER COMMAND?                               
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,TOLINE                                                        
         MVI   PRMODE,PRADDQ       ADD AN EMPTY PARAGRAPH                       
         MVC   PRPARA,TOLINE                                                    
         XC    PRALVALS,PRALVALS   SET NULL SO LINE INDEX NOT CHANGED           
         BAS   RE,PARAREC                                                       
         SR    R4,R4                                                            
         ICM   R4,1,FROMACTV       R4=(NUM OF ACTIVE RECS ON FROM PARA)         
         BZ    CPYP200                                                          
         STC   R4,OSVLACTV                                                      
         MVC   OSVLHIGH,PRLHIGH                                                 
         CLM   R4,1,PRLHIGH                                                     
         BNH   *+8                                                              
         STC   R4,OSVLHIGH                                                      
         SR    RF,RF                                                            
         IC    RF,OSVLHIGH                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   OSVLLST(0),INDEX                                                 
         LA    R2,OSVLLST                                                       
         LA    R3,FROMLST                                                       
         L     RF,AIO1             ASSUME NEW PARA REC IN IOAREA 1              
         USING PBRRECD,RF                                                       
         MVC   TOPARA,PBRKPARA GET NEW PARAGRAPH INDEX                          
         LA    RF,IOKEY                                                         
CPYP70   MVC   PBRKPARA,FROMPARA                                                
         MVC   PBRKLINE,0(R3)                                                   
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE RECORD FOR FROM PARA            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,IOKEY                                                         
         CLC   PBRKLINE,PRLHIGH                                                 
         BH    CPYP90                                                           
         MVC   PBRKPARA,TOPARA                                                  
         MVC   PBRKLINE,0(R2)                                                   
         GOTO1 AIO,IO2+IOACCMST+IORDUP GET TEXT LINE RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R0,AIO2                                                          
         LH    R1,=Y(IOAREALN)                                                  
         LR    RF,R1                                                            
         L     RE,AIO1                                                          
         MVCL  R0,RE                                                            
         L     RF,AIO2             RF=A(TEXT LINE RECORD)                       
         MVC   PBRKPARA,TOPARA                                                  
         MVC   PBRKLINE,0(R2)                                                   
         GOTO1 AIO,IO2+IOACCMST+IOPUTREC PUT BACK A RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CPYP100                                                          
CPYP90   L     RF,AIO1             RF=A(TEXT LINE RECORD)                       
         MVC   PBRKPARA,TOPARA                                                  
         MVC   PBRKLINE,0(R2)                                                   
*                                                                               
         GOTO1 AIO,IO1+IOACCMST+IOADDREC ADD RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CPYP100  LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         LA    RF,IOKEY                                                         
         BCT   R4,CPYP70                                                        
         DROP  RF                                                               
*                                                                               
         MVI   PRMODE,PRCHANGQ     ADD AN EMPTY PARAGRAPH                       
         MVC   PRPARA,TOLINE                                                    
         XC    PRNET,PRNET                                                      
         XC    PRCOM,PRCOM                                                      
         MVI   PRTAX,0                                                          
         LA    RF,OSVLIVAL                                                      
         ST    RF,PRALVALS                                                      
         BAS   RE,PARAREC                                                       
*                                                                               
CPYP200  XC    OSVECPYB,OSVECPYB                                                
         XC    OSVETOLN,OSVETOLN   CLEAR 'TO' BLOCK LISTS                       
         BAS   RE,BTOTAL           CALCULATE TOTALS FOR BILL                    
CPYPX    B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE LINES FROM A PARAGRAPH                                       *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
EDTP     NTR1                                                                   
         OC    OSVEEDTB,OSVEEDTB                                                
         BZ    EDTPX                                                            
         MVC   CSPAROFF,OSVEEDTB   GET PARAGRAPH OFFSET                         
         OI    OSVENFLG,OSVENCLQ   SET NTRSES CALLED                            
         MVC   OSVEEDTB(L'OSVEEDTB-BLKLNQ),OSVEEDTB+BLKLNQ                      
         LA    RF,OSVEEDTB+L'OSVEEDTB-BLKLNQ                                    
         XC    0(BLKLNQ,RF),0(RF)                                               
         LA    R4,MAXSLINQ         R4=(NUMBER OF TEXT LINES)                    
         LA    R2,DISLCM1H         R2=A(1ST LINE COMMAND FIELD HEADER)          
         USING SLINED,R2                                                        
EDTP10   MVC   SLLCM,BCSPACES     CLEAR LINE COMMAND                            
         OI    SLLCMH+FLDOIND-FLDHDRD,FOUTTRN                                   
         LA    R2,SLLNQ(R2)        BUMP TO NEXT SCREEN LINE                     
         BCT   R4,EDTP10                                                        
         DROP  R2                                                               
         BAS   RE,DISLCMND                                                      
         GOTO1 ANTRSES,SUBTPARM    CALL ACTION SUBROUTINE                       
         DC    H'0'                                                             
EDTPX    B     EXIT                                                             
SUBTPARM DC    AL1(RECBIL,ACTEDT),AL1(1,0,0,0)                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE BLOCK VALUES FOR CURRENT SCREEN                          *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* XIT- BLOCK LINE VALUES CURRENTLY ON SCREEN ARE INITIALISED          *         
***********************************************************************         
INBVAL   NTR1                                                                   
         LA    R3,EBLTAB           R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
INBV10   CLI   EBLCTYPE,EOT        END OF TABLE?                                
         BE    INBV120                                                          
         MVC   INBLSTNM,EBLNM      SET MAX NUMBER OF ENTRIES IN LIST            
         SR    RF,RF                                                            
         ICM   RF,3,EBLDIS                                                      
         LA    RF,OSVALSD(RF)      RF=A(BLOCK VALUES LIST)                      
         ST    RF,INABLST                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN         RF=(LINE NO AT START OF CURR SCREEN)         
         LA    RE,MAXSLINQ(RF)                                                  
         BCTR  RE,0                RE=(LINE NO AT END OF CURR SCREEN)           
         STC   RF,BCBYTE1                                                       
         STC   RE,BCBYTE2                                                       
         SR    R2,R2                                                            
         ICM   R2,1,INBLSTNM       R2=(MAX NUM OF ENTRIES IN LIST)              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R4,15,INABLST       R4=A(BLOCK VALUES LIST)                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING BLKVALD,R4                                                       
INBV40   OC    BLKSTRT(BLKLNQ),BLKSTRT END OF LIST?                             
         BZ    INBV100                                                          
         CLC   BCBYTE1,BLKSTRT     BLOCK LINE BEFORE START OF SCREEN?           
         BH    INBV50                                                           
         CLC   BCBYTE2,BLKSTRT     BLOCK LINE AFTER END OF SCREEN?              
         BL    INBV50                                                           
         CLI   BLKLEN,0            NUMERIC COMMAND?                             
         BE    *+14                                                             
         XC    BLKSTRT(BLKLNQ),BLKSTRT CLEAR ALL VALS FOR NUMERIC CMND          
         B     INBV80                                                           
         MVI   BLKSTRT,0           CLEAR BLOCK START                            
         CLI   BLKEND,0            EMPTY SLOT?                                  
         BNE   INBV50                                                           
         NI    OSVEBFLG,X'FF'-BLKCOUTQ SWITCH OFF BLOCK COMMAND OUTSTND         
         B     INBV80                                                           
INBV50   CLI   BLKLEN,0            NUMERIC COMMAND?                             
         BNE   INBV60                                                           
         CLC   BCBYTE1,BLKEND      BLOCK VALUE BEFORE START OF SCREEN?          
         BH    INBV60                                                           
         CLC   BCBYTE2,BLKEND      BLOCK VALUE AFTER END OF SCREEN?             
         BL    INBV60                                                           
         MVI   BLKEND,0            CLEAR OUT END VALUE                          
         B     INBV60                                                           
*                                                                               
INBV60   OC    BLKSTRT(BLKLNQ),BLKSTRT EMPTY SLOT?                              
         BZ    INBV80                                                           
*                                                                               
         CLI   BLKEND,0            END VALUE?                                   
         BE    INBV70                                                           
         CLI   BLKSTRT,0           START VALUE?                                 
         BNE   INBV90                                                           
         MVC   BLKSTRT,BLKEND      PUT END VALUE IN START SLOT                  
         MVI   BLKEND,0                                                         
INBV70   OI    OSVEBFLG,BLKCOUTQ   SWITCH ON BLOCK COMMAND OUTSTND              
         B     INBV90                                                           
*                                                                               
INBV80   LR    R0,R4               R0=R4=A(BLOCK VALUE LIST ENTRY)              
         LA    RE,BLKLNQ(R4)       RE=A(NEXT BLOCK VALUE LIST ENTRY)            
         SR    RF,RF                                                            
         IC    RF,INBLSTNM         RF=(MAX NUM OF ENTRIES IN LIST)              
         MH    RF,=Y(BLKLNQ)                                                    
         A     RF,INABLST          RF=A(END OF LIST)                            
         LR    R1,RF                                                            
         SR    R1,R0               R1=L'(REMAINING LIST VALUES)                 
         SR    RF,RE               RE=L'(REMAINING LIST AFTER NXT NTRY)         
         MVCL  R0,RE               OVERWRITE EMPTY SLOT                         
         B     *+8                                                              
*                                                                               
INBV90   LA    R4,BLKLNQ(R4)                                                    
         BCT   R2,INBV40           BUMP TO NEXT BLOCK VALUE                     
         DROP  R4                                                               
INBV100  L     RF,INABLST          RF=A(BLOCK VALUE LIST)                       
         CLI   0(RF),EOT           END OF TABLE                                 
         BNE   INBV110                                                          
         MVC   BCHALF(1),OSVECTYP  SWITCH OFF SAVED COMMAND TYPE                
         NC    BCHALF(1),EBLCTYPE                                               
         BZ    *+10                                                             
         XC    OSVECTYP,EBLCTYPE                                                
         MVC   BCHALF(1),OSVETOTY  SWITCH OF 'TO' COMMAND TYPE                  
         NC    BCHALF(1),EBLCTYPE                                               
         BZ    INBV110                                                          
         XC    OSVETOTY,EBLCTYPE                                                
INBV110  LA    R3,EBLLNQ(R3)       BUMP TO NEXT LINE COMMAND ENTRY              
         B     INBV10                                                           
         DROP  R3                                                               
INBV120  CLI   OSVETOTY,0          NO 'TO' TYPE COMMAND?                        
         BNE   *+10                                                             
         XC    OSVETOLN(OSVETOLQ),OSVETOLN INIT 'TO' VALUES                     
INBVX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DEAL WITH COMMAND LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
CMNDLIN  NTR1                                                                   
         MVC   FVMSGNO,=AL2(AE$INVIF) INVALID INPUT FIELD                       
         MVI   CMNDFLAG,0          INITIALISE COMMAND FLAG                      
         MVI   OSVCMND,0           INITIALISE COMMAND TYPE                      
         LA    R2,BASOPTH          R2=A(OPTION LINE FIELD HEADER)               
         USING FLDHDRD,R2                                                       
         TM    FLDIIND,FINPVAL     PREVIOUSLY VALIDATED?                        
         BO    CMNDX                                                            
         ST    R2,FVADDR                                                        
         CLI   FLDILEN,0           ANY INPUT?                                   
         BE    CMND310                                                          
         LA    R0,WORKLST          CLEAR AREA USED FOR SCANNING                 
         LH    R1,=Y(L'WORKLST)                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   CMNDLNH,BASOPTH                                                  
         XC    CMNDLN,CMNDLN       INIT COMMAND LINE                            
         SR    RF,RF                                                            
         IC    RF,FLDILEN          R0=L'(COMMAND LINE INPUT)                    
         LR    R0,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   CMNDLN(0),BASOPT    GET COMMAND LINE                             
*                                                                               
CMND90   SR    R0,R0                                                            
         IC    R0,FLDILEN          R0=L'(COMMAND LINE INPUT)                    
         SR    RF,RF                                                            
CMND100  LA    RE,CMNDLN(RF)       RE=(NEXT INPUT CHAR)                         
         CLI   0(RE),C' '          BLANK                                        
         BH    CMND120                                                          
         CLI   1(RE),C' '          FOLLOWED BY NON BLANK?                       
         BNH   CMND120                                                          
CMND110  MVI   0(RE),C','          REPLACE BLANK WITH COMMA                     
CMND120  LA    RF,1(RF)            BUMP TO NEXT CHAR                            
         BCT   R0,CMND100                                                       
         DROP  R2                                                               
*                                                                               
         MVC   FVMSGNO,=AL2(AE$FLDTL) INPUT FIELD TOO LONG                      
         LA    RF,CMNDLNH                                                       
         GOTO1 VSCANNER,BCDMCB,(0,(RF)),(0,WORKLST),C',=  '                     
         CLI   4(R1),0                                                          
         BE    CMNDERRX            INPUT IS TOO LONG                            
*                                                                               
         LA    R3,WORKLST          R3=A(SCAN BLOCK)                             
         USING SCANBLKD,R3                                                      
         CLI   4(R1),1             IF MORE THAN ONE ENTRY                       
         BE    *+8                                                              
         MVI   FVINDX,1            SET FIELD INDEX                              
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         STC   R0,CMNSTNO          R0=(NUMBER OF ENTRIES IN SCAN BLOCK)         
*                                                                               
         L     R1,ATRUP            GET TRANSLATE TABLE FOR LANG                 
         SR    RE,RE                                                            
         ICM   RE,1,SC1STLEN       RE=L'(INPUT COMMAND)                         
         BZ    CMNDERRX                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    SC1STFLD(0),0(R1)   CONVERT TO UPPER CASE                        
         LA    R2,CMNDTAB          R2=A(COMMAND TABLE)                          
         USING CMNTABD,R2                                                       
         MVC   FVMSGNO,=AL2(AE$CMDNR) COMMAND NOT RECOGNISED                    
CMND130  CLI   CMNMTYPE,EOT        END OF TABLE?                                
         BE    CMNDERRX                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CMNWORD                                                     
         LA    RF,TWAD(RF)         RF=A(COMMAND WORD)                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),0(RF)   MATCH ON COMMAND?                            
         BE    *+12                                                             
         LA    R2,CMNLNQ(R2)       BUMP TO NEXT COMMAND ENTRY                   
         B     CMND130                                                          
*                                                                               
         MVC   OSVCMND,CMNMTYPE    SAVE COMMAND TYPE                            
         TM    CMNSTAT,CMNSPREQ    COMMAND REQUIRES PARAMETETERS?               
         BNO   CMND140                                                          
         MVC   FVMSGNO,=AL2(AE$TFPRM) TOO FEW PARAMETERS                        
         CLC   CMNSTNO,CMNMIN      ENSURE THAT MINIMUM NUMBER ENTERED           
         BL    CMNDERRX                                                         
         MVC   FVMSGNO,=AL2(AE$PARNR) PARAMETER NOT RECOGNISED                  
         B     CMND150                                                          
CMND140  MVC   FVMSGNO,=AL2(AE$PARNR) PARAMETER NOT RECOGNISED                  
         OC    CMNPTAB,CMNPTAB     PARAMETERS ALLOWED WITH COMMAND?             
         BNZ   CMND150                                                          
         CLI   CMNSTNO,1           ENSURE NO PARAMETERS ENTERED                 
         BH    CMNDERRX                                                         
         CLI   SC2NDLEN,0                                                       
         BNE   CMNDERRX                                                         
         B     CMND310                                                          
CMND150  SR    RF,RF                                                            
         ICM   RF,3,CMNPFLAG                                                    
         LA    RF,OSVALSD(RF)      RF=A(PARAMETER FLAG FOR COMMAND)             
         MVI   0(RF),0             INITIALISE FLAG                              
         CLI   SC2NDLEN,0          IF SECOND FIELD ENTERED VIA '='              
         BE    CMND300             TREAT IT AS SEPERATE INPUT                   
         SR    RE,RE                                                            
         IC    RE,FVINDX                                                        
         LA    RE,1(RE)            BUMP FIELD INDEX                             
         STC   RE,FVINDX                                                        
         MVC   SC1STLEN,SC2NDLEN   OVERWRITE FIRST INPUT                        
         MVI   SC2NDLEN,0                                                       
         MVC   SC1STFLD,SC2NDFLD                                                
CMND160  MVC   FVMSGNO,=AL2(AE$PARNR) PARAMETER NOT RECOGNISED                  
         CLI   SC2NDLEN,0                                                       
         BNE   CMNDERRX            PARAMETER NOT RECOGNISED                     
         L     R1,ATRUP            GET TRANSLATE TABLE FOR LANG                 
         SR    RE,RE                                                            
         IC    RE,SC1STLEN         RE=L'(PARAMETER)                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK(0),SC1STFLD  SAVE PARAMETER                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    SC1STFLD(0),0(R1)   CONVERT PARAMETER TO UPPER CASE              
         SR    R4,R4                                                            
         ICM   R4,3,CMNPTAB                                                     
         LA    R4,CLB08(R4)        R4=A(PARAMETER TABLE FOR COMMAND)            
         USING CPARTABD,R4                                                      
CMND170  CLI   CPARTYPE,EOT        END OF PARAMETER TABLE?                      
         BNE   CMND180             PARAMETER NOT RECOGNISED                     
         TM    CMNDFLAG,CMNDDUPQ   DUPLICATE PARAMETER?                         
         BNO   CMNDERRX                                                         
         MVC   FVMSGNO,=AL2(AE$DUPRM) DUPLICATE PARAMETER                       
         MVC   FVINDX,SVFVINDX                                                  
         B     CMNDERRX                                                         
CMND180  SR    RF,RF                                                            
         ICM   RF,3,CPARWORD                                                    
         LA    RF,TWAD(RF)         RF=A(PARAMETER WORD)                         
CMND240  SR    RE,RE                                                            
         ICM   RE,1,SC1STLEN       RE=L'(INPUT PARAMETER)                       
         BZ    CMNDERRX                                                         
         BCTR  RE,0                                                             
CMND250  EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),0(RF)   MATCH FOUND ON PARAMETER?                    
         BE    *+12                                                             
CMND260  LA    R4,CPARLNQ(R4)      BUMP TO NEXT PARAMETER ENTRY                 
         B     CMND170                                                          
*                                                                               
CMND290  OC    0(L'CMNPFLAG,RF),CPARTYPE SET PARAMETER TYPE                     
*                                                                               
CMND300  SR    RF,RF               BUMP UP FIELD INDEX                          
         IC    RF,FVINDX                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FVINDX                                                        
         LA    R3,SCBLKLQ(R3)      BUMP TO NEXT ENTRY IN SCAN BLOCK             
         BCT   R0,CMND160                                                       
         DROP  R2,R3,R4                                                         
*                                                                               
CMND310  LA    R2,BASOPTH          R2=A(COMMAND LINE FIELD HEADER)              
         USING FLDHDRD,R2                                                       
         MVC   FLDDATA(L'BASOPT),BCSPACES                                       
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         OI    FLDIIND,FINPVAL     SET VALIDATED THIS TIME ON                   
         MVI   FVINDX,0                                                         
         DROP  R2                                                               
*                                                                               
CMNDX    CR    RB,RB                                                            
         B     EXIT                                                             
CMNDERRX LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DEAL WITH SCROLLING                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCROLL   NTR1                                                                   
         TM    BCINDS1,BCIANYPF    HAS A PFKEY BEEN PRESSED?                    
         BO    SCRL20                                                           
         SR    R3,R3                                                            
         IC    R3,OSVSTLIN         R3=(NUMBER OF START LINE ON SCREEN)          
         LR    RF,R3                                                            
         LA    RF,MAXSLINQ-1(RF)   RF=(NUMBER OF LAST LINE ON SCREEN)           
         CLM   RF,1,OSVPACTV       END OF PARA BEFORE LAST SCREEN LINE?         
         BH    SCRLX                                                            
         L     RE,AINP             RE=A(TIOB)                                   
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS-TIOBD(RE) R1=(ABSOLUTE CURSOR ADDRESS)             
         CH    R1,=Y(80*20)        CURSOR ON LAST TEXT LINE?                    
         BL    SCRLX                                                            
         CH    R1,=Y(80*21)                                                     
         BNL   SCRLX                                                            
*                                                                               
SCRL10   LA    R3,1(R3)            SCROLL DOWN BY ONE LINE                      
         STCM  R3,1,OSVSTLIN                                                    
         B     SCRLX                                                            
*                                                                               
SCRL20   CLI   BCPFKEY,PFKBKWDQ    BACKWARDS?                                   
         BE    SCRL50                                                           
         CLI   BCPFKEY,PFKFRWDQ    FORWARDS?                                    
         BNE   SCRLX                                                            
*                                                                               
SCRL50   LA    RF,MAXSLINQ         RF=(MAX NUM OF TXT LINES ON SCREEN)          
         TM    BCSCRNUM,PFKIPAGE   PAGE?                                        
         BO    SCRL80                                                           
         SRL   RF,1                                                             
         TM    BCSCRNUM,PFKIHALF   HALF?                                        
         BO    SCRL80                                                           
         TM    BCSCRNUM,PFKIMAXN   MAX?                                         
         BNO   SCRL70                                                           
         LA    R3,1                R3=(FIRST LINE/PARAGRAPH)                    
         TM    BCSCROLL,PFKIUPDN   UP TO TOP OF TEXT?                           
         BO    SCRL160                                                          
         B     SCRL140                                                          
*                                                                               
SCRL70   SR    RF,RF                                                            
         ICM   RF,1,BCSCRNUM       SCROLL AMOUNT?                               
         BZ    SCRLX               PFKEY NOT A SCROLL PFKEY                     
*                                                                               
SCRL80   SR    R3,R3                                                            
         IC    R3,OSVSTLIN         R3=(START LINE NUM IN CURRENT DISP)          
         TM    BCSCROLL,PFKIUPDN   UP?                                          
         BO    SCRL110                                                          
         AR    R3,RF               R3=(NEXT START LINE NUMBER)                  
         LA    RF,MAXSLINQ-1(R3)                                                
         CLM   RF,1,OSVPACTV                                                    
         BH    SCRL100                                                          
         B     SCRL160                                                          
*                                                                               
SCRL100  LR    R1,R3                                                            
         LA    R1,MAXSLINQ(R1)     R1=(NEXT LAST LINE NUMBER)                   
         CLM   R1,1,OSVPACTV       BEYOND THE LAST ACTIVE LINE?                 
         BH    SCRL140                                                          
         B     SCRL160                                                          
*                                                                               
SCRL110  CH    R3,=H'1'                                                         
         BE    *+14                                                             
         SR    R3,RF               R3=(NEXT START LINE NUMBER)                  
         BP    SCRL160                                                          
         B     SCRL150                                                          
*                                                                               
         CLI   OSVPOFST,1          FIRST PARAGRAPH?                             
         BE    SCRL150                                                          
*                                                                               
SCRL140  SR    R3,R3                                                            
         IC    R3,OSVPACTV         NUMBER OF LINES IN PARAGRAPH                 
         SH    R3,=Y(MAXSLINQ-2)                                                
         LTR   R3,R3               BEYOND START OF DISPLAY?                     
         BP    *+8                                                              
SCRL150  LA    R3,1                                                             
SCRL160  STC   R3,OSVSTLIN         SAVE NEXT START LINE NUMBER                  
         B     SCRL200                                                          
*                                                                               
SCRL200  LA    R2,DISLCM1H         R2=A(FIRST TXT LINE FIELD HEADER)            
         LR    RE,R2                                                            
         SR    RE,RA                                                            
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVI   TIOBCURI,0          OVERRIDE DEFAULT CURSOR CONTROL              
         STCM  RE,3,TIOBCURD                                                    
         DROP  RF                                                               
SCRLX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHANGE/ADD A PARAGRAPH RECORD                                       *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* PRECPARM DS    0X                COPY/MOVE ROUTINE PARMS            *         
* PRMODE   DS    X                                                    *         
* PRADDQ   EQU   X'80'             ADD A RECORD (OR CHANGE UNACTIVE)  *         
* PRCHANGQ EQU   X'40'             CHANGE AN EXISTING RECORD (ACTIVE) *         
* PRNET    DS    PL6               NET AMOUNT (BIN ZEROS IF NO CHANGE)*         
* PRCOM    DS    PL6               COM AMOUNT (BIN ZEROS IF NO CHANGE)*         
*                                                                     *         
* XIT IF NET CHANGED THEN PRNET CONTAINS OLD-NEW AMOUNT               *         
*     IF COM CHANGED THEN PRCOM CONTAINS OLD-NEW AMOUNT               *         
***********************************************************************         
         SPACE 1                                                                
PARAREC  NTR1                                                                   
         MVI   PARAFLAG,0          INIT PARAGRAPH FLAG                          
         MVI   RECMODE,RECACTQ     SET ACTIVE RECORD MODE                       
         CLI   PRMODE,PRCHANGQ     ACTIVE LINE RECORD TO BE CHANGED?            
         BE    PARAR20                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(AE$MBILL) MAX BILL SIZE IS 200 PARAGRAPHS           
         CLI   OSVPACTV,MAXPLINQ   MAXIMUM NUMBER OF PARAGRAPHS?                
         BE    PARARERX                                                         
         SR    RF,RF                                                            
         IC    RF,OSVPACTV         RF=(NUMBER OF ACTIVE PARA RECORDS)           
         LA    RF,1(RF)            INCREMENT COUNT                              
         STC   RF,OSVPOFST         UPDATE CURRENT PARA NUMBER/OFFSET            
         CLC   OSVPHIGH,OSVPACTV   HIGHEST PARA INDEX IS ACTIVE?                
         STC   RF,OSVPACTV         UPDATE ACTIVE PARA COUNT                     
         BE    *+12                                                             
         MVI   RECMODE,RECUACTQ    UNACTIVE PARA RECORD TO BE USED              
         B     PARAR05                                                          
         STC   RF,OSVPHIGH         UPDATE NUMBER OF PARA RECORDS COUNT          
         LA    RE,OSVPLST(RF)                                                   
         BCTR  RE,0                RE=A(CORRESPONDING INDEX FOR PARA)           
         STC   RF,0(RE)            ATTATCH AT END OF LIST                       
         MVI   RECMODE,RECADDQ     PARA RECORD REQUIRES ADDING                  
*                                                                               
PARAR05  MVI   CMPMODE,CMPMOVEQ    MOVE COMMAND                                 
         OI    CMPMODE,CMPBEFRQ    BEFORE OFFSET                                
         MVC   CMPTO,PRPARA        OFFSET FOR LINE NUMBER                       
         MVC   CMPFROM1,OSVPACTV   START OF BLOCK                               
         MVC   CMPFROM2,OSVPACTV   END OF BLOCK                                 
         LA    RF,OSVPLST                                                       
         STCM  RF,15,CMPAILST      RF=A(LINE INDEX LIST)                        
         LA    RF,L'OSVPLST                                                     
         STCM  RF,1,CMPILLN        RF=L'(LINE INDEX LIST)                       
         BAS   RE,CMI              MOVE NEW LINE TO CORRECT POSITION            
*                                  IS CHANGE NECCESSARY?                        
PARAR20  LA    R3,IOKEY            R3=A(KEY FOR PRODUCTION PARA RECORD)         
         USING PBRRECD,R3                                                       
         CLI   RECMODE,RECADDQ     ADDING A NEW RECORD?                         
         BNE   *+8                                                              
         L     R3,AIO1             R3=A(IOAREA FOR NEW RECORD)                  
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         SR    RF,RF                                                            
         IC    RF,PRPARA                                                        
         LA    RF,OSVPLST-1(RF)                                                 
         MVC   PBRKPARA,0(RF)      PARAGRAPH#                                   
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM INIT ELEMENT STORAGE                 
         CLI   RECMODE,RECADDQ     ADDING A RECORD?                             
         BE    PARAR190                                                         
         GOTO1 AIO,IO1+IOACCMST+IORDUP GET PARAGRAPH RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1             R3=A(TEXT LINE RECORD)                       
         LA    R3,PBRRFST          R3=A(FIRST ELEMENT)                          
         USING PGHELD,R3                                                        
PARAR30  CLI   PGHEL,EOR           END OF RECORD?                               
         BE    PARAR200                                                         
         CLI   PGHEL,PGHELQ        PARAGRAPH HEADER ELEMENT?                    
         BE    PARAR50                                                          
         CLI   PGHEL,NDXELQ        INDEX ELEMENT?                               
         BE    PARAR120                                                         
         CLI   PGHEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    PARAR150                                                         
*                                                                               
PARAR40  SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,PGHLN                                                         
         AR    R3,RE                                                            
         B     PARAR30                                                          
*                                                                               
*ARAR50  CLI   PRMODE,PRADDQ       ACTIVE LINE RECORD TO BE RE-USED?            
*        BNE   *+14                                                             
*        ZAP   PGHNET,=P'0'        CLEAR NET AMOUNT                             
*        B     PARAR60                                                          
PARAR50  OC    PRNET,PRNET                                                      
         BZ    PARAR60                                                          
         ZAP   BCDUB,PGHNET        SAVE OLD NET AMOUNT                          
         SP    BCDUB,PRNET         GET DIFFERENCE BETWEEN OLD AND NEW           
         ZAP   PGHNET,PRNET        SET NEW COMMISSION AMOUNT                    
         ZAP   PRNET,BCDUB         RETURN DIFFERENCE                            
*ARAR60  CLI   PRMODE,PRADDQ       ACTIVE LINE RECORD TO BE RE-USED?            
*        BNE   *+14                                                             
*        ZAP   PGHCOM,=P'0'        CLEAR OUT COMMISSION AMOUNT                  
*        B     PARAR80                                                          
PARAR60  OC    PRCOM,PRCOM         PREVIOUSLY VALIDATED?                        
         BZ    PARAR80                                                          
         ZAP   BCDUB,PGHCOM        SAVE OLD COMMISION AMOUNT                    
         SP    BCDUB,PRCOM         GET DIFFERENCE BETWEEN OLD AND NEW           
         ZAP   PGHCOM,PRCOM        SET NEW COMMISSION AMOUNT                    
         ZAP   PRCOM,BCDUB         RETURN DIFFERENCE                            
PARAR80  CLI   PRMODE,PRADDQ       ACTIVE LINE RECORD TO BE RE-USED?            
         BNE   PARAR40                                                          
         MVC   PGHTAX,PRTAX        SET NEW TAX TYPE                             
         B     PARAR40                                                          
*                                                                               
         USING NDXELD,R3           INDEX ELEMENT                                
PARAR120 CLI   PRMODE,PRADDQ       ACTIVE PARA RECORD TO BE RE-USED?            
         BNE   *+12                                                             
         MVI   NDXACTV,0                                                        
         B     PARAR130                                                         
         ICM   RF,15,PRALVALS      RF=A(NEW LINE INDEX)                         
         BZ    PARAR130                                                         
         MVC   NDXHIGH(OSVLLNQ),0(RF)                                           
PARAR130 MVC   PRLHIGH,NDXHIGH     RETURN THE NUMBER OF LINE RECORDS            
         B     PARAR40                                                          
*                                                                               
         USING FFTELD,R3                                                        
PARAR150 CLI   FFTTYPE,FFTTPGHC    PARAGRAPH HEADER COMMENT?                    
         BNE   PARAR40                                                          
         CLI   PRMODE,PRADDQ       ACTIVE LINE RECORD TO BE RE-USED?            
         BNE   PARAR40                                                          
         CLI   FFTDLEN,36          LENGTH OF DATA = LENGTH OF FIELD?            
         BNE   PARAR170                                                         
         MVC   FFTDATA(L'PRDESC),PRDESC                                         
         B     PARAR40                                                          
*                                                                               
PARAR170 MVI   FFTEL,X'FF'                                                      
         GOTO1 VHELLO,BCDMCB,(C'D',ACCMST),(X'FF',AIO1),0                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM INIT ELEMENT STORAGE                 
         LA    R3,BOELEM                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTPGHC                                                 
         MVI   FFTSEQ,0                                                         
         LA    RF,L'PRDESC                                                      
         MVC   FFTDATA(0),PRDESC                                                
         LA    R1,L'PRDESC                                                      
         STC   R1,FFTDLEN                                                       
         LA    R1,FFTLN1Q+1(R1)                                                 
         STC   R1,FFTLN                                                         
*                                                                               
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PARAR40                                                          
*                                  ADDING A NEW RECORD                          
PARAR190 LA    RE,PBRRFST-PBRRECD(R3) RE=A(FIRST ELEMENT)                       
         MVI   0(RE),EOR           SET END OF RECORD                            
         LA    RE,1(RE)                                                         
         SR    RE,R3               RE=L'(NEW RECORD)                            
         STCM  RE,3,PBRRLEN-PBRRECD(R3)                                         
*                                                                               
         LA    R3,BOELEM           R3=A(ELEMENT WORK AREA)                      
         USING PGHELD,R3                                                        
         MVI   PGHEL,PGHELQ                                                     
         MVI   PGHLN,PGHLNQ                                                     
         ZAP   PGHNET,PRNET        SET NET AMOUNT                               
         ZAP   PGHCOM,PRCOM        SET COMMISSION AMOUNT                        
         MVC   PGHTAX,PRTAX        SET TAX CODE                                 
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3) ADD PARA HDR ELE           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         XC    BOELEM,BOELEM       INIT ELEMENT STORAGE                         
         LA    R3,BOELEM                                                        
         USING NDXELD,R3                                                        
         MVI   NDXEL,NDXELQ        BUILD EMPTY INDEX ELEMENT                    
         MVI   NDXLN,NDXLNQ                                                     
         MVC   PRLHIGH,NDXHIGH     RETURN THE NUMBER OF LINE RECORDS            
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         XC    BOELEM,BOELEM       INIT ELEMENT STORAGE                         
         LA    R3,BOELEM                                                        
         USING FFTELD,R3                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTPGHC                                                 
         MVI   FFTSEQ,0                                                         
         MVC   FFTDATA(L'PRDESC),PRDESC                                         
         LA    R1,L'PRDESC                                                      
         STC   R1,FFTDLEN                                                       
         LA    R1,FFTLN1Q+1(R1)                                                 
         STC   R1,FFTLN                                                         
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         GOTO1 AIO,IO1+IOACCMST+IOADDREC ADD RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PARAR220                                                         
*                                                                               
PARAR200 GOTO1 AIO,IO1+IOACCMST+IOPUTREC PUT BACK A RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PARAR220 TM    PARAFLAG,PERRORQ    ERROR FLAG ON?                               
         BO    PARARERX                                                         
*                                                                               
PARARX   CR    RB,RB                                                            
         B     EXIT                                                             
PARARERX LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHANGE A BILL RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
BILLREC  NTR1                                                                   
         LA    R3,IOKEY            R3=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,R3                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         GOTO1 AIO,IO1+IOACCMST+IORDUP GET BILL RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1             R3=A(BILL RECORD)                            
         LA    R3,PBRRFST          R3=A(FIRST ELEMENT)                          
         USING NDXELD,R3                                                        
BILLR10  CLI   NDXEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NDXEL,NDXELQ        INDEX ELEMENT?                               
         BE    BILLR20                                                          
*                                                                               
         SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,NDXLN                                                         
         AR    R3,RE                                                            
         B     BILLR10                                                          
*                                                                               
         USING NDXELD,R3           INDEX ELEMENT                                
BILLR20  SR    RF,RF                                                            
         IC    RF,NDXHIGH          RF=(NUM OF LINE RECS ON OLD PARA)            
         SR    RE,RE                                                            
         IC    RE,OSVPHIGH         RE=(NUM OF LINE RECS ON NEW PARA)            
         CR    RF,RE                                                            
         BH    *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                RF=(X LENGTH OF LONGEST INDX LIST)           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   NDXINDX(0),OSVPLST  ANY CHANGE                                   
         BNE   *+14                                                             
         CLC   NDXHIGH(L'NDXHIGH+L'NDXACTV),OSVPHIGH CHANGED?                   
         BE    BILLR40                                                          
         CLI   NDXLN,NDXLNQ                                                     
         BNE   BILLR30                                                          
         MVC   NDXINDX(L'OSVPLST),OSVPLST     SAVE LINE INDEX LIST              
         MVC   NDXHIGH(L'NDXHIGH+L'NDXACTV),OSVPHIGH SET HIGH AND ACTV          
         CLI   NDXHIGH,0           TRAP EMPTY ELEMENT BUG                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     BILLR40                                                          
*                                                                               
BILLR30  MVI   NDXEL,X'FF'         MARK FOR DELETION                            
         GOTO1 VHELLO,BCDMCB,(C'D',ACCMST),(X'FF',AIO1),0                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,BOELEM           R3=A(ELEMENT WORK AREA)                      
         XC    BOELEM,BOELEM                                                    
         MVI   NDXEL,NDXELQ        INDEX ELEMENT                                
         MVI   NDXLN,NDXLNQ        SET ELEMENT LENGTH                           
         MVC   NDXINDX(L'OSVPLST),OSVPLST SAVED PARA INDEX LIST                 
         MVC   NDXHIGH(L'NDXHIGH+L'NDXACTV),OSVPHIGH SET HIGH AND ACTV          
         CLI   NDXHIGH,0           TRAP EMPTY ELEMENT BUG                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
BILLR40  GOTO1 AIO,IO1+IOACCMST+IOPUTREC PUT BACK A RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BILLRX   CR    RB,RB                                                            
         B     EXIT                                                             
BILLRERX LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET DEFAULT CURSOR POSTION IF NO OVERRIDE HAS TAKEN PLACE           *         
***********************************************************************         
         SPACE 1                                                                
DEFCURS  LR    R0,RE                                                            
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         OC    OSVBILD,OSVBILD     IF LIVE BILL CURSOR GOES ON ACTION           
         BZ    DEFC05                                                           
         LA    R2,BASACTH                                                       
         SR    R2,RA               R2=(DISP TO ACTION FIELD HEADER)             
         MVI   TIOBCURI,0          SET CURSOR INDEX TO START OF FIELD           
         STCM  R2,3,TIOBCURD       SET CURSOR POSITION                          
         B     DEFCX                                                            
*                                                                               
DEFC05   SR    RE,RE                                                            
         ICM   RE,3,TIOBCURS       RE=ABSOLUTE A(CURSOR ON SCREEN)              
         CH    RE,=Y(TXTSTLIN*PSCRNLN) CURSOR BEFORE 1ST TEXT LINE?             
         BL    DEFC10                                                           
         OC    TIOBCURD,TIOBCURD   CURSOR POSITION OVERRIDEN?                   
         BNZ   DEFCX                                                            
*        CH    RE,=Y(TXTENLIN*PSCRNLN) CURSOR AFTER LAST TEXT LINE?             
*        BNL   *+8                                                              
*        LA    RE,PSCRNLN(RE)      RE=ABSOLUTE A(NEXT ROW, SAME COLUMN)         
*        SR    R2,R2                                                            
*        LR    R3,RE               R3=RE                                        
*        LA    R1,PSCRNLN          R1=L(SCREEN LINE)                            
*        STCM  R1,15,BCFULL                                                     
*                                                                               
*        D     R2,BCFULL                                                        
*        CH    R2,=Y(1+L'DISLCM1)  CURSOR IN LINE COMMAND COL?                  
*        BH    *+8                                                              
*        BCTR  R2,0                                                             
*        SR    RE,R2               SET TO FIRST COLUMN ON COMMAND LINE          
*        STCM  RE,3,TIOBCURS       SAVE ABSOLUTE CURSOR ADDRESS                 
         LA    RE,DISLCM1H         RE=A(OPTION FIELD HEADER)                    
         SR    RE,RA               RE=(DISPLACEMENT TO OPTION FIELD HD)         
         STCM  RE,3,TIOBCURD                                                    
         MVI   TIOBCURI,0          SET TO START OF FIELD                        
         OI    TIOBINDS,TIOBSETC   APPLICATION SET CURSOR POSITION              
         B     DEFCX                                                            
*                                                                               
DEFC10   LA    RE,BASOPTH          RE=A(OPTION FIELD HEADER)                    
         SR    RE,RA               RE=(DISPLACEMENT TO OPTION FIELD HD)         
         CLM   RE,3,OSVCURD        IS CURSOR CURRENTLY ON THIS LINE?            
         BNE   DEFCX                                                            
         STCM  RE,3,TIOBCURD                                                    
         MVI   TIOBCURI,0          SET TO START OF FIELD                        
DEFCX    OI    TIOBINDS,TIOBSETC   APPLICATION SET CURSOR POSITION              
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* ALTER BLOCK VALUES IF AFFECTED BY PREVIOUS LINE COMMAND             *         
* ON ENTRY FOLLOWING PARAMETERS ARE REQUIRED-                         *         
* ALTPARMS DS    0X                                                   *         
* ALTBSTRT DS    X                   DISP TO 1ST AFFECTED BLOCK VALUE *         
* ALTBEND  DS    X                   DISP TO LST AFFECTED BLOCK VALUE *         
* ALTBDVAL DS    X                   DISPLACEMENT VALUE               *         
* ALTBCTYP DS    X                   EXCEPT CMND TYPE LIST SPECIFIED  *         
* ALTBSTAT DS    X                   STATUS BYTE                      *         
* ALTBSUBQ EQU   X'80'               SUBTRACT DISP VAL (DEFAULT ADD)  *         
***********************************************************************         
         SPACE 1                                                                
ALTBLKV  NTR1                                                                   
         LA    R3,EBLTAB           R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
ALTB10   CLI   EBLCTYPE,EOT        END OF TABLE?                                
         BE    ALTBX                                                            
         CLC   EBLCTYPE,ALTBCTYP   EXCEPTION COMMAND SPECIFIED?                 
         BE    ALTB70                                                           
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NUMBER OF ENTRIES IN LIST)           
         SR    R1,R1                                                            
         ICM   R1,3,EBLDIS                                                      
         LA    R1,OSVALSD(R1)      R1=A(BLOCK LIST)                             
         USING BLKVALD,R1                                                       
         SR    RF,RF                                                            
         IC    RF,ALTBDVAL         RF=(DISPLACEMENT VALUE)                      
         TM    ALTBSTAT,ALTBSUBQ   SUBTRACT DISPLACEMENT?                       
         BNO   *+6                                                              
         LNR   RF,RF               SET TO NEGATIVE VALUE                        
ALTB40   OC    BLKSTRT(BLKLNQ),BLKSTRT NO MORE VALUES?                          
         BZ    ALTB70                                                           
         CLC   BLKSTRT,ALTBSTRT    BLOCK START IN AFFECTED AREA?                
         BL    ALTB50                                                           
         CLC   BLKSTRT,ALTBEND                                                  
         BH    ALTB50                                                           
         SR    RE,RE                                                            
         IC    RE,BLKSTRT                                                       
         AR    RE,RF                                                            
         BP    *+8                                                              
         LA    RE,1                                                             
         STC   RE,BLKSTRT                                                       
ALTB50   CLC   BLKEND,ALTBSTRT     BLOCK END IN AFFECTED AREA?                  
         BL    ALTB60                                                           
         CLC   BLKEND,ALTBEND                                                   
         BH    ALTB60                                                           
         SR    RE,RE                                                            
         IC    RE,BLKEND                                                        
         AR    RE,RF                                                            
         BP    *+8                                                              
         LA    RE,1                                                             
         STC   RE,BLKEND                                                        
ALTB60   LA    R1,BLKLNQ(R1)       BUMP TO NEXT BLOCK LIST VALUE                
         BCT   R0,ALTB40                                                        
ALTB70   LA    R3,EBLLNQ(R3)       BUMP TO NEXT EDIT TABLE ENTRY                
         B     ALTB10                                                           
ALTBX    B     EXIT                                                             
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET TOTALS FOR BILL*                                                          
***********************************************************************         
         SPACE 1                                                                
BTOTAL   NTR1                                                                   
         ZAP   OSVTBNET,=P'0'      INIT TOTAL NET FOR BILL                      
         ZAP   OSVTBCOM,=P'0'      INIT TOTAL COMM FOR BILL                     
         ZAP   OSVTBVAT,=P'0'      INIT TOTAL VAT FOR BILL                      
         ZAP   OSVTBTOT,=P'0'      INIT TOTAL GROSS FOR BILL                    
         LA    R4,OSVVATTB         R4=A(VAT TABLE)                              
         USING VATTABD,R4                                                       
BTOT10   CLI   VATTYPE,EOT         END OF TABLE?                                
         BE    BTOTX                                                            
         AP    OSVTBNET,VATNET     INCREMENT TOTAL NET FOR BILL                 
         AP    OSVTBCOM,VATCOM     INCREMENT TOTAL COMM FOR BILL                
         ZAP   BCDUB,VATNET        GET BILL NET                                 
         AP    BCDUB,VATCOM        PLUS BILL COMMISSION                         
         AP    OSVTBTOT,BCDUB                                                   
*        ZAP   PL16,VATRATE                                                     
*        MP    PL16,BCDUB          (VAT RATE)*(NET+COMMISSION)                  
*        BZ    BTOT20                                                           
*        SRP   PL16,64-4,5         DIVIDE BY 100.00                             
*        AP    OSVTBVAT,PL16       INCREMENT TOTAL VAT FOR BILL                 
*        AP    OSVTBTOT,OSVTBVAT   BILL TOTAL (NET+COMMISION+VAT)               
BTOT20   LA    R4,VATLNQ(R4)                                                    
         B     BTOT10                                                           
*                                                                               
BTOTX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* COPY/MOVE INDEXES                                                   *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* CMPARM   DS    0X                COPY/MOVE ROUTINE PARMS            *         
* CMPMODE  DS    X                 MODE BYTE                          *         
* CMPCOPYQ EQU   X'80'             COPY MODE                          *         
* CMPMOVEQ EQU   X'40'             MOVE MODE                          *         
* CMPBEFRQ EQU   X'20'             BEFORE 'TO' POS (DEFAULT AFTER)    *         
* CMPTO    DS    X                 COPY/MOVE TO LINE                  *         
* CMPFROM1 DS    X                 COPY/MOVE FROM LINE 1 (START)      *         
* CMPFROM2 DS    X                 COPY/MOVE FROM LINE 2 (END)        *         
* CMPAILST DS    A                 A(PARAGRAPH/LINE INDEX LIST)       *         
* CMLNQ    EQU   *-CMPARM                                             *         
* XIT-     ORIGINAL INDEX STRING OVERWRITTEN BY MANIPULATED STRING    *         
***********************************************************************         
         SPACE 1                                                                
CMI      NTR1                                                                   
         LA    R0,WORKLST          INIT WORKER INDEX LIST                       
         LH    R1,=Y(L'WORKLST)                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ICM   R2,15,CMPAILST      R2=A(PARAGRAPH/LINE INDEX LIST)              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,CMPTO            RF=(LINE NUMBER OF 'TO' POSITION)            
         TM    CMPMODE,CMPBEFRQ    COPY/MOVE BEFORE 'TO' POSITION?              
         BNO   CMI10                                                            
         SH    RF,=H'1'            REDUCE 'TO' LINE NUMBER                      
         STCM  RF,1,CMPTO                                                       
         BZ    CMI20                                                            
CMI10    LR    RE,RF               RE=RF                                        
         BCTR  RE,0                RE=X L'(STRING UP TO 'TO' POSITION)          
         EX    RE,*+4                                                           
         MVC   WORKLST(0),0(R2)    GET STRING UP TO 'TO' POSITION               
*                                                                               
CMI20    LA    R3,WORKLST(RF)      R3=A('TO' POSITION IN NEW LIST)              
         SR    RF,RF                                                            
         IC    RF,CMPFROM1         RF=(COPY/MOVE BLOCK START LINE)              
         SR    RE,RE                                                            
         IC    RE,CMPFROM2         RE=A(COPY/MOVE BLOCK END LINE)               
         SR    RE,RF               RF=(X LENGTH OF BLOCK)                       
         BNM   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         LA    R2,0(RF,R2)         R2=A(FIRST INDEX IN COPY/MOVE BLOCK)         
         EX    RE,*+4                                                           
         MVC   0(0,R3),0(R2)       GET COPY/MOVE BLOCK                          
*                                                                               
         LA    R3,1(RE,R3)         R3=A(NEXT FREE INDEX IN NEW LIST)            
         ICM   R2,15,CMPAILST                                                   
         SR    RF,RF                                                            
         IC    RF,CMPTO            RF=(TO LINE NUMBER)                          
         SR    RE,RE                                                            
         IC    RE,CMPILLN          RE=L'(INDEX LIST)                            
         SR    RE,RF                                                            
         BZ    CMI30                                                            
         BCTR  RE,0                RE=(X LEN OF REMAINING INDEX STRING)         
         LA    R2,0(RF,R2)         R2=A(1ST REMAING INDEX STRING)               
         EX    RE,*+4                                                           
         MVC   0(0,R3),0(R2)       GET REMAINING STRING                         
*                                                                               
CMI30    TM    CMPMODE,CMPCOPYQ    IF NOT COPY MODE REMOVE ORIG BLOCK           
         BO    CMI40                                                            
         SR    RF,RF                                                            
         IC    RF,CMPFROM1         RF=(START OF BLOCK LINE NUMBER)              
         SR    R2,R2                                                            
         IC    R2,CMPFROM2         R2=(END OF BLOCK LINE NUMBER)                
         SR    R2,RF                                                            
         LA    R2,1(R2)            R2=L'(MOVE BLOCK)                            
         CLC   CMPFROM1,CMPTO      ORIG MOVE BLOCK BEFORE 'TO' LINE NO?         
         BNH   *+6                                                              
         AR    RF,R2               ADJUST LINE NUMBER FOR NEW BLOCK             
         LA    R0,WORKLST(RF)      R0=A(1ST INDX IN ORIG MOVE BLOCK)            
         BCTR  R0,0                                                             
         LR    RE,R0               RE=A(1ST INDX CHAR AFTER MOVE BLOCK)         
         AR    RE,R2                                                            
         LH    R1,=Y(L'WORKLST)                                                 
         LA    R1,WORKLST(R1)                                                   
         LR    RF,R1                                                            
         SR    R1,R0               R1=L'(MOVE TO)                               
         SR    RF,RE               RF=L'(MOVE FROM)                             
         MVCL  R0,RE               OVER WRITE ORIGINAL MOVE BLOCK               
*                                                                               
CMI40    ICM   R2,15,CMPAILST                                                   
         SR    RE,RE                                                            
         IC    RE,CMPILLN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),WORKLST     OVER WRITE OLD STRING                        
*                                                                               
CMIX     B     EXIT                                                             
         EJECT                                                                  
ERREXIT  MVI   FVOMTYP,GTMERR                                                   
         B     *+8                                                              
EXIT     MVI   FVOMTYP,GTMINF                                                   
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST'                                                        
INDEX    DC    200AL1((*-INDEX)+1)                                              
         SPACE 1                                                                
AROUT    DS    0F                  NON ADDRESSABLE ROUTINES                     
         DC    A(DISSCRN)          CHANGE A LINE RECORD                         
AROUTN   EQU   (*-AROUT)/L'AROUT   NUMBER OF NON ADDRESSABLE ROUTINES           
         SPACE 1                                                                
*                                                                               
EBLTAB   DS    0X                  TABLE OF EDIT BLOCK LISTS                    
         DC    AL1(LENEDTQ)                                                     
         DC    AL1(L'OSVEEDTB/BLKLNQ),AL2(OSVEEDTB-OSVALSD,EDTP-CLB08)          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(EBLNONMQ)                                                    
         DC    AL1(LENDELQ)                                                     
         DC    AL1(L'OSVEDELB/BLKLNQ),AL2(OSVEDELB-OSVALSD,DEL-CLB08)           
         DC    AL1(LENAFTQ+LENBEFQ+LENREPQ)                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LENREPQ)                                                     
         DC    AL1(L'OSVEREPB/BLKLNQ),AL2(OSVEREPB-OSVALSD,0)                   
         DC    AL1(LENAFTQ+LENBEFQ+LENREPQ)                                     
         DC    X'FF'                                                            
         DC    AL1(0)                                                           
         DC    AL1(LENMOVEQ)                                                    
         DC    AL1(L'OSVEMVEB/BLKLNQ),AL2(OSVEMVEB-OSVALSD,MOVEP-CLB08)         
         DC    AL1(LENAFTQ+LENBEFQ+LENREPQ)                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LENCOPYQ)                                                    
         DC    AL1(L'OSVECPYB/BLKLNQ),AL2(OSVECPYB-OSVALSD,COPYP-CLB08)         
         DC    AL1(LENAFTQ+LENBEFQ+LENREPQ)                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(LENAFTQ)                                                     
         DC    AL1(L'OSVEAFLN/BLKLNQ),AL2(OSVEAFLN-OSVALSD,0)                   
         DC    AL1(LENREPQ)                                                     
         DC    AL1(LENDELQ+LENMOVEQ+LENCOPYQ+LENREPQ)                           
         DC    AL1(EBLNONMQ)                                                    
         DC    AL1(LENBEFQ)                                                     
         DC    AL1(L'OSVEBFLN/BLKLNQ),AL2(OSVEBFLN-OSVALSD,0)                   
         DC    AL1(LENREPQ)                                                     
         DC    AL1(LENDELQ+LENMOVEQ+LENCOPYQ+LENREPQ)                           
         DC    AL1(EBLNONMQ)                                                    
EBLTABX  DC    AL1(EOT)                                                         
*                                                                               
LEDTAB   DS    0X    LINE EDIT COMMAND TABLE                                    
*                                                                               
LEGBR    DC    AL1(LANGEUK,LEGBRX+1-LEGBR)                                      
         DC    C'DD ',AL1(LENDELQ)                                              
         DC    C'MM ',AL1(LENMOVEQ)                                             
         DC    C'C  ',AL1(LENCOPYQ)                                             
         DC    C'A  ',AL1(LENAFTQ)                                              
         DC    C'B  ',AL1(LENBEFQ)                                              
         DC    C'E  ',AL1(LENEDTQ)                                              
         DC    C'R  ',AL1(LENREPQ)                                              
LEGBRX   DC    AL1(EOT)                                                         
         SPACE 1                                                                
         DC    AL1(EOT)                                                         
*                                                                               
CMNDTAB  DS    0C                                                               
         DC    AL1(CMNRESQ),AL2(UC@RESET-TWAD),AL2(0),AL1(1)                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* GENERAL EQUATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
MAXSLINQ EQU   ((DISLCML-DISLCM1)/(DISLCM2-DISLCM1))+1 MAX LINE ON SCRN         
MAXPLINQ EQU   200                 MAX LINES IN PARAGRAPH                       
TXTSTLIN EQU   7                   SCREEN LINE NUMBER FOR 1ST TEXT LINE         
TXTENLIN EQU   20                  SCREEN LINE NUMBER FOR LST TEXT LINE         
PSCRNLN  EQU   80                  WIDTH OF PHYSICAL SCREEN LINE                
TAXNDQ   EQU   X'FF'               TAX CODE NOT DEFINED                         
EOR      EQU   0                                                                
         EJECT                                                                  
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORK                                                      
         EJECT                                                                  
TWAD     DSECT                     OVERLAY SAVED STORAGE                        
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF4D                                                       
         EJECT                                                                  
SLINED   DSECT                     DSECT FOR SCREEN LINE                        
SLLCMH   DS    CL(L'DISLCM1H)      LINE COMMAND HEADER                          
SLLCM    DS    CL(L'DISLCM1)       LINE COMMAND FIELD                           
         ORG   SLINED+DISPAR1H-DISLCM1H                                         
SLPDVH   DS    CL(L'DISPAR1H)      HEADER FOR PARA,DESC AND VAT FIELDS          
SLPDV    DS    0C                                                               
SLPARA   DS    CL3                 PARAGRAPH NUMBER FIELD                       
         DS    CL1                                                              
SLDESC   DS    CL34                PARAGRAPH DESCRIPTION FIELD                  
*LTAXCOD DS    CL1                 VAT TYPE                                     
SLPDVLNQ EQU   *-SLPARA                                                         
         ORG   SLINED+DISNET1H-DISLCM1H                                         
SLNETH   DS    CL(L'DISNET1H)      NET AMOUNT HEADER                            
SLNET    DS    CL(L'DISNET1)       NET AMOUNT FIELD                             
         ORG   SLINED+DISCOM1H-DISLCM1H                                         
SLCOMH   DS    CL(L'DISCOM1H)      COMMISION AMOUNT HEADER                      
SLCOM    DS    CL(L'DISCOM1)       COMMISSION AMOUNT FIELD                      
         ORG   SLINED+DISTT1H-DISLCM1H                                          
SLTTH    DS    CL(L'DISTT1H)       HEADER FOR TOTAL FIELDS                      
SLTT     DS    0C                                                               
*LTAX    DS    CL11                VAT AMOUNT FIELD                             
*        DS    CL1                                                              
SLTOTAL  DS    CL11                TOTAL AMOUNT FIELD                           
SLTTLNQ  EQU   *-SLTT                                                           
SLLNQ    EQU   *-SLINED                                                         
         SPACE 1                                                                
TLINED   DSECT                     DSECT FOR TOTAL LINE                         
TLTOTLNH DS    CL(L'DISBILH)       TOTALS HEADER                                
TLTOTLN  DS    0CL(L'DISBIL)       TOTALS FIELD                                 
TLNET    DS    CL11                NET AMOUNT FIELD                             
         DS    CL1                                                              
TLCOM    DS    CL11                COMMISSION AMOUNT FIELD                      
         DS    CL1                                                              
TLTOT    DS    CL11                TOTAL AMOUNT FIELD                           
TLTTLNQ  EQU   *-TLTOTLN                                                        
TLLNQ    EQU   *-TLINED                                                         
         SPACE 1                                                                
BLKVALD  DSECT                     DSECT FOR BLOCK VALUE LIST                   
BLKSTRT  DS    X                   START OF BLOCK                               
BLKEND   DS    X                   END OF BLOCK                                 
BLKLEN   DS    X                   L'(BLK IF NUMERIC CMND ELSE ZERO)            
BLKLNQ   EQU   *-BLKVALD                                                        
         SPACE 1                                                                
EBLTABD  DSECT                     OVERLAY BLOCK/LINE INIT TABLE                
EBLCTYPE DS    X                   COMMAND TYPE                                 
EBLNM    DS    X                   MAX NUMBER OF ENTRIES IN BLOCK LIST          
EBLDIS   DS    AL2                 DISPLACEMENT TO LIST                         
EBLFUNC  DS    AL2                 DISCPLACEMENT TO EDIT FUNCTION               
EBLPRECM DS    X                   COMPATIBLE PRIOR CMNDS                       
EBLPROCM DS    X                   COMPATIBLE POST CMNDS                        
EBLSTAT  DS    X                   STATUS BYTE                                  
EBLNONMQ EQU   X'80'               NUMERIC CMD HIGHER > 1 NOT ALLOWED           
EBLLNQ   EQU   *-EBLTABD                                                        
         SPACE 1                                                                
LEDTABD  DSECT                     OVERLAY LINE EDIT COMMAND TABLE              
LEHEAD   DS    0X                  ** TABLE HEADER **                           
LEHLANG  DS    X                   LANGUAGE CODE                                
LEHLEN   DS    XL1                 LENGTH OF ENTRY                              
LEHLNQ   EQU   *-LEHEAD            LENGTH OF HEADER                             
         ORG   LEHEAD                                                           
LENTRY   DS    0X                  ** TABLE ENTRY **                            
LENCOMM  DS    CL3                 COMMAND                                      
LENCTYPE DS    X                   LINE EDIT COMMAND TYPE                       
LENEDTQ  EQU   X'80'               EDIT                                         
LENDELQ  EQU   X'40'               DELETE                                       
LENMOVEQ EQU   X'20'               MOVE                                         
LENCOPYQ EQU   X'10'               COPY                                         
LENREPQ  EQU   X'08'               REPEAT                                       
LENAFTQ  EQU   X'04'               AFTER                                        
LENBEFQ  EQU   X'02'               BEFORE                                       
LENLNQ   EQU   *-LENTRY            LENGTH OF ENTRY                              
         SPACE 1                                                                
CONTABD  DSECT                     CONFLICT TABLE                               
CONCTYPE DS    X                   COMMAND TYPE                                 
CONCOMP  DS    X                   COMPATIBLE PRIOR NUMERIC COMMANDS            
CONLNQ   EQU   *-CONTABD                                                        
         SPACE 1                                                                
CMNTABD  DSECT                     COMMAND TABLE                                
CMNMTYPE DS    X                   MAIN COMMAND TYPE                            
CMNRESQ  EQU   1                   RESET                                        
CMNDELQ  EQU   2                   DELETE                                       
CMNFNDQ  EQU   3                   FIND                                         
CMNCHGQ  EQU   4                   CHANGE                                       
CMNCOLQ  EQU   5                   COLUMNS                                      
CMNWORD  DS    AL2                 COMMAND WORD                                 
CMNPTAB  DS    AL2                 PARAMETER TABLE                              
CMNMIN   DS    X                   MINIMUM NUMBER OF PARAMS + 1                 
CMNSTAT  DS    X                   STATUS BYTE                                  
CMNSPREQ EQU   X'80'               MAIN COMMAND REQUIRES PARAMETERS             
CMNSFFTQ EQU   X'40'               PARAMETERS INCLUDE FREE FORM TEXT            
CMNPFLAG DS    AL2                 DISPLACEMENT TO PARAMETER FLAG               
CMNLNQ   EQU   *-CMNTABD           LENGTH OF COMMAND TABLE ENTRY                
         SPACE 1                                                                
CPARTABD DSECT                     COMMAND PARAMTERE TABLE                      
CPARTYPE DS    X                   PARAMETER TYPE                               
CPARFFUQ EQU   1                   FREE FORM TEXT PARAMETER UPPER CASE          
CPARFFLQ EQU   2                   FREE FORM TEXT PARAMETER LOWER CASE          
CPARNM1Q EQU   1                   NUMERIC PARAM 1                              
CPARWORD DS    AL2                 PARAM WRD/DISP TO FREE FRM TXT PARAM         
CPARSTAT DS    X                   STATUS BYTE                                  
CPARSFFQ EQU   X'80'               FREE FORM TEXT PARAMETER                     
CPARSNMQ EQU   X'40'               NUMERIC PARAMETER                            
CPARINCM DS    6AL1                INCOMPATIBLE PARAMETER LIST                  
CPARLNQ  EQU   *-CPARTABD          LENGTH OF COMMAND TABLE ENTRY                
         SPACE 1                                                                
VATTABD  DSECT                     VAT TABLE                                    
VATTYPE  DS    CL(L'VTCTYPE)       VAT TYPE                                     
VATRATE  DS    PL4                 VAT RATE                                     
VATNET   DS    PL8                 NET AMOUNT FOR VAT TYPE                      
VATCOM   DS    PL8                 COMMISSION AMOUNT FOR VAT TYPE               
VATLNQ   EQU   *-VATTABD           LENGTH OF VAT TABLE ENTRY                    
VATMAXQ  EQU   7                   MAXIMUM NUMBER OF ENTRIES IN TABLE           
         SPACE 1                                                                
OSVALSD  DSECT                     OVERLAY SAVED VALUES                         
OSVMX@OF DS    CL4                 OF                                           
OSVCURD  DS    XL(L'TIOBCURD)      DISPLACEMENT TO CURSOR                       
OSVBILD  DS    XL(L'PBRKBILD)      BILLED DATE (ZEROS IF DRAFT)                 
*                                                                               
OSVEVAL  DS    0X                  LINE EDIT VALUES                             
OSVENFLG DS    X                   NTRSES FLAG                                  
OSVENCLQ EQU   X'80'               NTRSES ROUTINE CALLED                        
OSVECPYB DS    XL(BLKLNQ)          COPY BLOCK LIST                              
OSVEMVEB DS    XL(BLKLNQ)          MOVE BLOCK LIST                              
OSVEDELB DS    XL(BLKLNQ)          DELETE BLOCK LIST                            
OSVEREPB DS    XL(BLKLNQ)          REPEAT BLOCK                                 
OSVEEDTB DS    XL(14*BLKLNQ)       EDIT BLOCK                                   
OSVETOLN DS    0XL(2*BLKLNQ)                                                    
OSVEBFLN DS    XL(BLKLNQ)          TO LINE (BEFORE)                             
OSVEAFLN DS    XL(BLKLNQ)          TO LINE (AFTER)                              
OSVETOAC DS    X                   NUMBER OF ACTIVE LINES IN TO PARA            
OSVETOTY DS    X                   TO TYPE AFTER/BEFORE OR OVER                 
OSVETOLQ EQU   *-OSVETOLN          LENGTH OF TO VALUES                          
OSVECTYP DS    X                   EDIT COMMANDS                                
OSVEBFLG DS    X                   BLOCK FLAG                                   
BLKCOUTQ EQU   X'80'               BLOCK COMMAND OUTSTANDING                    
BLKPENDQ EQU   X'40'               COPY/MOVE IS PENDING                         
BLKPCONQ EQU   X'20'               POTENTIAL CONFLICT                           
OSVECONF DS    A                   A(POTENTIAL CONFLICT LINE)                   
OSVELNQ  EQU   *-OSVEVAL                                                        
*                                                                               
OSVSTLIN DS    X                   LINE NUM AT START OF CURRENT SCREEN          
OSVSTCOL DS    X                   COLM NUM AT START OF CURRENT SCREEN          
*                                                                               
OSVCLVAL DS    0X                  COMMAND LINE VALUES                          
OSVCMND  DS    X                   MAIN COMMAND LINE TYPE                       
*                                                                               
OSVJOB   DS    CL(L'TRNKACT)       JOB                                          
OSVSEQ   DS    XL(L'PBRKSEQ)       SEQUENCE# (COMPLEMENT)                       
OSVPARA  DS    XL(L'PBRKPARA)      PARAGRAPH#                                   
OSVPOFST DS    XL(L'PBRKPARA)      PARAGRAPH# OFFSET                            
OSVPIVAL DS    0X                  PARAGRAPH INDEX VALUES                       
OSVPHIGH DS    XL(L'NDXHIGH)       HIGHEST PARAGRAPH INDEX                      
OSVPACTV DS    XL(L'NDXACTV)       HIGHEST ACTIVE PARAGRAPH LINE INDEX          
OSVPLST  DS    XL200               PARAGRAPH INDEX LIST                         
OSVPLNQ  EQU   *-OSVPIVAL          LENGTH OF PARAGRAPH INDEX VALUES             
OSVLIVAL DS    0X                  LINE INDEX VALUES                            
OSVLHIGH DS    XL(L'NDXHIGH)       HIGHEST LINE INDEX                           
OSVLACTV DS    XL(L'NDXACTV)       HIGHEST ACTIVE LINE LINE INDEX               
OSVLLST  DS    XL200               LINE INDEX LIST                              
OSVLLNQ  EQU   *-OSVLIVAL          LENGTH OF LINE INDEX VALUES                  
OSVTBNET DS    PL8                 TOTAL BILL NET                               
OSVTBCOM DS    PL8                 TOTAL BILL COMMISSION                        
OSVTBVAT DS    PL8                 TOTAL BILL VAT                               
OSVTBTOT DS    PL8                 TOTAL BILL (NET+COMMISSION+VAT)              
OSVPBCRD DS    PL3                 PACKED YYMMDD BILL CREATED DATE              
OSVVATTB DS    XL((VATLNQ*VATMAXQ)+1)                                           
OSVALSND DS    0XL(OSVALSL-(*-OSVALSD)) N/D                                     
         DS    0H                                                               
         SPACE 1                                                                
OSSAVED  DSECT                                                                  
OSSVDSLW DS    0C                                                               
OSSAVEND DS    0XL(256-(*-OSSAVED)) N/D                                         
         DS    0H                                                               
         EJECT                                                                  
WORKD    DSECT                     OVERLAY W/S                                  
         ORG   OVERWRK                                                          
RELO     DS    A                   RELOCATION FACTOR                            
ABASE1   DS    A                   BASE REGISTERS SAVE AREA                     
ABASE2   DS    A                                                                
ABASE3   DS    A                                                                
ABASE4   DS    A                                                                
ARELROUT DS    0A                                                               
AFCTXT   DS    A                   FIND/CHANGE TEXT MODULE                      
AGPARA   DS    A                   GET AND DISPLAY PARAGRAPH                    
ALINEREC DS    A                   CHANGE/ADD A LINE RECORD                     
PL16     DS    PL16                                                             
PL8      DS    PL8                                                              
TOPARA   DS    X                                                                
FROMPARA DS    X                                                                
SVFVINDX DS    X                   SAVED FVINDX                                 
PARAFLAG DS    X                   PARAGRAPH FLAG                               
PERRORQ  EQU   X'80'               ERROR FOUND IN PARAGRAPH INPUT               
CMNDFLAG DS    X                   COMMAND FLAG                                 
CMNDDUPQ EQU   X'80'               DUPLICATE PARAMETER                          
CMNDLNH  DS    CL(L'BASOPTH)       COMMAND LINE DUMMY HEADER                    
CMNDLN   DS    CL(L'BASOPT)        COMMAND LINE                                 
CMNSTNO  DS    X                   NUMBER OF TEXT STRINGS ON COMM LINE          
CMNFFNO  DS    X                   NUMBER OF FREE FORM TEXT PARAMS              
CMNNUMNO DS    X                   NUMBER OF NUMERIC PARAMS                     
PRECCMP  DS    X                   COMPATIBLE PRECEEDING COMMANDS               
PROCCMP  DS    X                   COMPATIBLE PROCEEDING COMMANDS               
FSVAL    DS    0XL6                DINK INDICATORS FOR FREE FORM STRING         
FSVALST1 DS    X                   START DINK 1                                 
FSVALEN1 DS    X                   END DINK 1                                   
FSVALTP1 DS    X                   DINK TYPE 1                                  
FSVALST2 DS    X                   START DINK 2                                 
FSVALEN2 DS    X                   END DINK 2                                   
FSVALTP2 DS    X                   DINK TYPE 2                                  
TOTINRP  DS    X                   TOTAL INSERTS AND REPEATS                    
TOLINE   DS    X                   TO POSITION                                  
TOREPNO  DS    X                   NUMBER OF TIMES TO BE MOVED/COPIED           
AUTOINS  DS    X                   AUTO INSERT LINE                             
CHGPARMS DS    0X                  CHANGE TEXT PARMS                            
CHGCOLOF DS    X                   OFFSET TO CHANGE COLUMN                      
CHGLINOF DS    X                   OFFSET TO CHANGE LINE NUMBER                 
CHGALINE DS    A                   ADDRESS OF LINE TO BE CHANGED                
CHGLNQ   EQU   *-CHGPARMS                                                       
LRPARMS  DS    0X                                                               
LRMODE   DS    X                                                                
LRADDQ   EQU   X'80'               ADD A RECORD OR (CHANGE UNACTIVE)            
LRCHANGE EQU   X'40'               CHANGE AN EXISTING RECORD (ACTIVE)           
LROVRWRT EQU   X'20'               OVERWITE EXISTING RECORD  (ACTIVE)           
LRCOLUMN DS    X                   COLUMN NUMBER                                
LRLINE   DS    X                   LOGICAL LINE NUMBER                          
LRPARA   DS    X                   PARAGRAPH OF LINE                            
LRLINELN DS    X                   L'(LINE TO BE ADDED)                         
LRALINE  DS    A                   A(LINE TO BE ADDED                           
LRLNQ    EQU   *-LRPARMS                                                        
PRPARMS  DS    0X                                                               
PRMODE   DS    X                                                                
PRADDQ   EQU   X'80'               ADD A RECORD OR (CHANGE UNACTIVE)            
PRCHANGQ EQU   X'40'               CHANGE AN EXISTING RECORD (ACTIVE)           
PRPARA   DS    X                   PARAGRAPH OFFSET                             
PRNET    DS    PL6                 NET                                          
PRCOM    DS    PL6                 COMMISSION                                   
PRTAX    DS    X                   TAX TYPE                                     
PRDESC   DS    CL36                PARAGRAPH DESCRIPTION                        
PRLHIGH  DS    X                   NUMBER OF LINE RECS ON PARA RETURNED         
PRALVALS DS    A                   0 OR A(LIN INDX LST) ACT CHANGE ONLY         
PRLNQ    EQU   *-PRPARMS                                                        
BRMODE   DS    X                                                                
BRADDQ   EQU   X'80'               ADD A RECORD OR (CHANGE UNACTIVE)            
BRCHANGE EQU   X'40'               CHANGE AN EXISTING RECORD (ACTIVE)           
BRLNQ    EQU   *-PRPARMS                                                        
ALTPARMS DS    0X                                                               
ALTBSTRT DS    X                   DISP TO FIRST AFFECTED BLOCK VALUE           
ALTBEND  DS    X                   DISP TO LAST AFFECTED BLOCK VALUE            
ALTBDVAL DS    X                   DISPLACEMENT VALUE                           
ALTBCTYP DS    X                   EXCEPT COMMAND TYPE LIST SPECIFIED           
ALTBSTAT DS    X                   STATUS BYTE                                  
ALTBSUBQ EQU   X'80'               SUBTRACT DISP VAL (DEFAULT ADD)              
ALTLNQ   EQU   *-ALTPARMS                                                       
ATRUP    DS    A                   A(TRANSLATE TO UPPER TABLE FOR LANG)         
ABLOCK   DS    A                   A(BLOCK VALUE LIST)                          
BLOCKLN  DS    X                   L'(BLOCK VALUE LIST)                         
BFR1     DS    X                   BLOCK FROM LINE NUMBER                       
BFR2     DS    X                   BLOCK FROM LINE NUMBER                       
BLEN     DS    X                   BLOCK FROM LINE NUMBER                       
ALENTRY  DS    A                   A(LINE EDIT COMMAND ENTRIES)                 
ACMNFFT  DS    A                   A(FREE FORM TEXT PARAM LIST)                 
WORKTLN  DS    XL1                 LENGTH OF TEXT IN WORKTTXT                   
WORKTTXT DS    CL(MAXLLNQ)         WORK TEXT AREA                               
WORKLST  DS    XL400               WORK AREA FOR INDEX LISTS                    
LINEFLAG DS    X                   LINE INEX INDICATOR STATUS FLAG              
LCHANGEQ EQU   X'80'               LINE INDEX LIST CHANGED                      
NXTLOFF  DS    X                   NEXT LINE INDEX OFFSET IN INDX LIST          
RECMODE  DS    X                   RECORD ACTION MODE                           
RECACTQ  EQU   X'80'               ACTIVE RECORD TO BE CHANGED                  
RECUACTQ EQU   X'40'               UNACTIVE RECORD TO BE USED                   
RECADDQ  EQU   X'20'               RECORD TO BE ADDED                           
CMPARM   DS    0X                  COPY/MOVE ROUTINE PARMS                      
CMPMODE  DS    X                   MODE BYTE                                    
CMPCOPYQ EQU   X'80'               COPY MODE                                    
CMPMOVEQ EQU   X'40'               MOVE MODE                                    
CMPBEFRQ EQU   X'20'               COPY/MOVE BEFORE TO POS (DEFLT AFTR)         
CMPTO    DS    X                   COPY/MOVE TO OFFSET                          
CMPFROM1 DS    X                   COPY/MOVE FROM OFFSET 1 (START)              
CMPFROM2 DS    X                   COPY/MOVE FROM OFFSET 2 (END)                
CMPAILST DS    A                   A(PARAGRAPH/LINE INDEX LIST)                 
CMPILLN  DS    X                   L'(INDEX LIST)                               
CMLNQ    EQU   *-CMPARM                                                         
INPARM   DS    0X                  INITIALISE BLOCK VALUE LIST PARMS            
INBLSTNM DS    X                   LENGTH OF BLOCK VALUE LIST                   
INABLST  DS    A                   A(BLOCK VALUE LIST)                          
FROMIVAL DS    0X                  LINE INDEX VALS FOR FROM PARA                
FROMHIGH DS    XL(L'NDXHIGH)       HIGHEST LINE INDEX                           
FROMACTV DS    XL(L'NDXACTV)       HIGHEST ACTIVE LINE LINE INDEX               
FROMLST  DS    XL200               LINE INDEX LIST                              
FROMLNQ  EQU   *-FROMIVAL          LENGTH OF LINE INDEX VALUES                  
SCHINDS  DS    XL1                 SURCHARGE INDICATORS                         
SCHIAMT  EQU   X'80'               SCHAMT IS AN AMOUNT, NOT A %                 
SCHAMT   DS    PL6                 SURCHARGE AMOUNT / % (2DP)                   
DSCINDS  DS    XL1                 DISCOUNT INDICATORS                          
DSCIAMT  EQU   X'80'               DSCAMT IS AN AMOUNT, NOT A %                 
DSCAMT   DS    PL6                 DISCOUNT AMOUNT / % (2DP)                    
         SPACE 1                                                                
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036ACCLB08   08/16/00'                                      
         END                                                                    
