*          DATA SET ACCLB0A    AT LEVEL 022 AS OF 08/16/00                      
*PHASE T6210AA                                                                  
CLB0A    TITLE '- BILL PROGRAM - EDIT SCREEN'                                   
CLB0A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLBA**,R8,R7,R6,CLEAR=YES,RR=RE                              
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
         LH    R1,=Y(LC@NRTV-TWAD)                                              
         LA    R1,TWAD(R1)                                                      
         MVC   MY@NRTV,0(R1)                                                    
*                                                                               
         PUSH  USING                                                            
         USING TRNRECD,BOWORK1                                                  
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         L     RE,AGOPBLK                                                       
         MVC   GOABEXT-GOBLOCKD(L'GOABEXT,RE),AJOBBLK                           
         GOTO1 AGETOPT,BOPARM,TRNKEY                                            
         POP   USING                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CULANG                                                        
         SLL   RF,2                                                             
         L     RE,ATRUPTAB                                                      
         AR    RE,RF                                                            
         ICM   RF,15,0(RE)                                                      
         LA    RF,CLB0A(RF)                                                     
         ST    RF,ATRUP            RF=A(TRANSLATE TO UPPER CASE TAB)            
         LA    R3,LEDTAB           R3=A(LINE EDIT TABE)                         
         USING LEDTABD,R3                                                       
         LA    RF,LEHLNQ(R3)                                                    
         ST    RF,ALENTRY          DEFAULT TO UK IF NO ENTRY FOUND              
MAIN20   CLI   LEHLANG,EOT         END OF TABLE?                                
         BE    MAIN32                                                           
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
MAIN32   LA    R2,BASOPTH          R2=A(COMMAND LINE)                           
         USING FLDHDRD,R2                                                       
         OI    FLDATB,FATBLC       SET TO LOWER CASE EACH TIME THROUGH          
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,EDTBILNH         R2=A(BILL NUMBER FIELD HEADER)               
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         CLI   TWASCRN,S#BILEDT    EDIT SCREEN LOADED?                          
         BE    MAIN40                                                           
         MVI   BCPFKEY,0           INITITIALISE PFKEY                           
         LA    RF,OSSAVE           RF=A(SECONDARY TWA AREA FOR OVERLAY)         
         USING OSSAVED,RF                                                       
         GOTO1 VDICTAT,BCDMCB,C'LL  ',DICLOW,OSSVDSLW RESOLVE DICT              
         DROP  RF                                                               
         MVCDD OSVMX@OF(L'OSVMX@OF),AC#OF                                       
         GOTO1 VDICTAT,BCDMCB,C'SM  ',OSVMX@OF                                  
         GOTO1 AOVRSCR,BCDMCB,('S#BILEDT',BASOLAYH) LOAD EDIT SCREEN            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   OSVPIND,PBRPIDFT    DEFAULT TO DRAFT BILL                        
         SR    RE,RE               TEST WHETHER WE CAME FROM LIST               
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         CLI   CSACT-CSRECACT(RE),ACTLST                                        
         BNE   MAIN34              NOT FROM LIST - MUST BE DRAFT                
         L     RF,ALSVALS                                                       
         USING LSVALSD,RF                                                       
         USING TLSTD,LSTLST                                                     
         USING PBRPAS,TLBPAS                                                    
         CLI   PBRPTYP,PBRPTYPQ                                                 
         BNE   MAIN34                                                           
         CLI   PBRPSUB,PBRPPASQ                                                 
         BNE   MAIN34                                                           
         CLI   PBRPIND,PBRPILVE                                                 
         BNE   MAIN34                                                           
         MVI   OSVPIND,PBRPILVE    BILL IS LIVE                                 
         DROP  RF                                                               
         OI    EDTNETH+(FLDATB-FLDHDRD),FATBPROT                                
         OI    EDTCOMNH+(FLDATB-FLDHDRD),FATBPROT                               
         OI    EDTTYPEH+(FLDATB-FLDHDRD),FATBPROT                               
MAIN34   CLI   CSACT,ACTFFRM       ACTION ADD?                                  
         BE    MAIN40                                                           
         CLI   CSACT,ACTAFRM       ACTION AUTO-FORMAT?                          
         BNE   MAIN36                                                           
         GOTO1 VCOLY,BODMCB,('O#BILFRM',0),0,0                                  
         L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
         BNE   EXIT                                                             
         MVI   CSACT,ACTEDT        SET ACTION TO EDIT                           
         LA    R2,BASACTH          R2=A(ACTION FIELD HEADER)                    
         USING FLDHDRD,R2                                                       
         MVC   FLDDATA(L'BASACT),BCSPACES                                       
         LH    RE,=AL2(UC@EDIT-TWAD)                                            
         LA    RE,TWAD(RE)                                                      
         MVC   FLDDATA(L'UC@EDIT),0(RE) SET SCREEN ACTION TO EDIT               
         OI    FLDOIND,FOUTTRN                                                  
MAIN36   MVC   FVMSGNO,=AL2(AI$EBILL) ENTER BILL NUMBER                         
         OC    CSBILNUM,CSBILNUM   BILL NUM PASSED BY PREVIOUS MODULE?          
         BZ    EXIT                                                             
         MVC   EDTBILN,CSBILNUM                                                 
         OI    FLDOIND,FOUTTRN                                                  
         MVC   FVMSGNO,=AL2(AI$EPARN) ENTER PARA NUMBER OR PRESS ENTER          
         LA    R2,EDTPARAH         R2=A(PARAGRAPH FIELD HEADER)                 
         ST    R2,FVADDR                                                        
         CLI   CSPAROFF,0                                                       
         BNE   MAIN50                                                           
         B     EXIT                                                             
*                                                                               
MAIN40   L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVC   OSVCURD,TIOBCURD    SAVE DISPLACEMENT TO CURSOR POSITION         
         XC    TIOBCURD,TIOBCURD                                                
         DROP  RF                                                               
         CLI   CSACT,ACTFFRM       ACTION ADD?                                  
         BNE   MAIN50                                                           
         BAS   RE,GBILL            ADD BILL RECORD                              
         BNE   ERREXIT                                                          
         LA    R2,EDTBILNH                                                      
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED BIT ON              
         OI    FLDOIND,FOUTTRN                                                  
         MVI   CSACT,ACTEDT        SET ACTION TO EDIT                           
         LA    R2,BASACTH          R2=A(ACTION FIELD HEADER)                    
         USING FLDHDRD,R2                                                       
         MVC   FLDDATA(L'BASACT),BCSPACES                                       
         LH    RE,=AL2(UC@EDIT-TWAD)                                            
         LA    RE,TWAD(RE)                                                      
         MVC   FLDDATA(L'UC@EDIT),0(RE) SET SCREEN ACTION TO EDIT               
         OI    FLDOIND,FOUTTRN                                                  
         MVI   PRMODE,PRADDQ                                                    
         BAS   RE,PARAREC          ADD A PARAGRAPH RECORD                       
*        BE    MAIN90                                                           
         BE    MAIN94                                                           
         DC    H'0'                                                             
MAIN50   TM    FLDIIND,FINPVAL     BILL NUMBER PREVIOUSLY VALIDATED?            
         BO    MAIN60                                                           
         BAS   RE,GBILL            GET BILL REC                                 
         BNE   ERREXIT                                                          
         SR    R1,R1                                                            
         CLI   CSPAROFF,0                                                       
         BE    *+8                                                              
MAIN55   LA    R1,CSPAROFF                                                      
         ICM   RF,15,AGPARA                                                     
         BASR  RE,RF               READ AND DISPLAY CURRENT PARAGRAPH           
         BNE   ERREXIT                                                          
         LA    R2,EDTBILNH                                                      
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED BIT ON              
         OI    FLDOIND,FOUTTRN                                                  
         B     MAIN80                                                           
*                                                                               
MAIN60   MVI   PRMODE,PRCHANGQ                                                  
         BAS   RE,PARAREC          CHANGE PARAGRAPH RECORD IF NECESSARY         
         BNE   ERREXIT                                                          
         BAS   RE,CMNDLIN          VALIDATE COMMAND LINE                        
         BNE   ERREXIT                                                          
         BAS   RE,EDTSCRN          EDIT SCREEN                                  
         BNE   ERREXIT                                                          
         BAS   RE,LINCMND          DEAL WITH LINE COMMANDS                      
         BNE   ERREXIT                                                          
         CLI   BCPFKEY,PFKADDP     ADD PARAGRAPH?                               
         BNE   MAIN70                                                           
         CLI   OSVPIND,PBRPIDFT    TEST DRAFT                                   
         BE    MAIN62                                                           
         MVC   FVMSGNO,=AL2(AE$CAPLB)                                           
         LA    R2,BASOPTH          R2=A(OPTIONS FIELD)                          
         ST    R2,FVADDR                                                        
         B     ERREXIT                                                          
MAIN62   MVI   PRMODE,PRADDQ                                                    
         BAS   RE,PARAREC                                                       
         BNE   ERREXIT                                                          
         BAS   RE,BILLREC          CHANGE BILL RECORD                           
*        B     MAIN80                                                           
         B     MAIN94                                                           
MAIN70   LA    R2,EDTPARAH         R2=A(PARAGRAPH FIELD)                        
         USING FLDHDRD,R2                                                       
         TM    FLDIIND,FINPVAL     PREVIOUSLY VALIDATED?                        
         BO    MAIN80                                                           
         TM    OSVEBFLG,BLKCOUTQ   CANNOT CHNG PARA IF BLOCK INCOMPLETE         
         BO    MAIN80                                                           
         SR    R1,R1                                                            
         ICM   RF,15,AGPARA                                                     
         BASR  RE,RF               READ AND DISPLAY CURRENT PARAGRAPH           
         BNE   ERREXIT                                                          
         MVI   OSVSTLIN,1          SET START LINE TO BEGINING OF PARA           
         MVI   OSVSTCOL,1          SET START COLUMN TO FAR LEFT                 
*                                                                               
MAIN80   CLI   OSVCMND,CMNDELQ     DELETE PARAGRAPH?                            
         BNE   *+8                                                              
         BAS   RE,DPARA            DELETE PARAGRAPH RECORD                      
         BAS   RE,SCROLL           DEAL WITH SCROLLING                          
         GOTO1 AFCTXT              FIND/CHANGE TEXT                             
         BNE   ERREXIT                                                          
MAIN90   CLI   OSVLACTV,0          NO ACTIVE LINE RECORDS?                      
         BNE   MAIN95                                                           
         OC    OSVEINSB,OSVEINSB   ANY INSERTS?                                 
         BNZ   MAIN95                                                           
MAIN94   MVI   LRMODE,LRADDQ       ADD A BLANK LINE RECORD                      
         MVI   LRLINE,1            FIRST LINE                                   
         MVC   LRPARA,OSVPARA      PARAGRAPH INDEX                              
         MVI   LRCOLUMN,1          COLUMN                                       
         MVC   WORKTTXT(MAXLLNQ),BCSPACES CLEAR LINE                            
         MVI   LRLINELN,MAXLLNQ                                                 
         LA    RE,WORKTTXT         RE=A(LINE)                                   
         STCM  RE,15,LRALINE                                                    
         ICM   RF,15,ALINEREC      ADD/REACTIVATE/CHANGE LINE                   
         BASR  RE,RF                                                            
         MVI   PRMODE,PRCHANGQ                                                  
         BAS   RE,PARAREC          CHANGE PARAGRAPH RECORD IF NECESSARY         
*                                                                               
MAIN95   BAS   RE,DISHEAD          DISPLAY HEADER LINE                          
         BAS   RE,DISSCRN          DISPLAY SCREEN                               
         BAS   RE,DISLCMND         DISPLAY LINE COMMANDS                        
         BAS   RE,DEFCURS          DEAL WITH DEFAULT CURSOR POSITION            
         TM    OSVFCFLG,OSVFCTLQ   LINE TOO LONG FOR AMEND?                     
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$LLONG)                                           
         B     ERREXIT                                                          
MAINX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ BILL RECORD (ACTION EDIT)                                      *         
* ADD BILL RECORD (ACTION ADD)                                        *         
***********************************************************************         
         SPACE 1                                                                
GBILL    NTR1                                                                   
         LA    R2,EDTBILNH         R2=A(BILL NUMBER FIELD HEADER)               
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         CLI   CSACT,ACTFFRM       ACTION ADD?                                  
         BE    GBIL10                                                           
         MVC   FVMSGNO,=AL2(AE$MISIF) MISSING INPUT FIELD                       
         CLC   FLDDATA(L'EDTBILN),BCSPACES BILL NUMBER IS REQUIRED              
         BNH   GBILERRX                                                         
         MVC   OSVBILNO,EDTBILN    SAVE BILL NUMBER                             
         B     GBIL70                                                           
*                                                                               
GBIL10   LA    R3,IOKEY            R3=A(KEY OF PRODUCTION LEDGER REC)           
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN      COMPANY                                      
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD PRODUCTION LEDGER                   
         GOTO1 AIO,IO2+IOACCMST+IORDUP                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO2             R3=A(PRODUCTION LEDGER RECORD)               
         LA    R3,ACTRFST          R3=A(FIRST ELEMENT ON RECORD)                
         USING SCIELD,R3                                                        
GBIL20   CLI   SCIEL,EOR           END OF RECORD                                
         BE    GBIL40                                                           
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT?                     
         BE    GBIL50                                                           
*                                                                               
GBIL30   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,SCILN                                                         
         AR    R3,R0                                                            
         B     GBIL20                                                           
*                                  ADD A DRAFT BILL NUMBER TO LEDGER            
GBIL40   LA    R3,BOELEM           R3=A(ELEMENT WORK AREA)                      
         USING SCIELD,R3                                                        
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT                      
         MVI   SCITYPE,SCITDBNO    DRAFT BILL NUMBER ELEMENT                    
         MVI   SCILN,SCILN1Q       SET LENGTH                                   
         ZAP   SCIAMNT,=P'0'       INITIALISE DRAFT BILL SEQUENCE               
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO2,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,16(R1)        R3=A(NEW ELEMENT)                            
         B     *+12                                                             
*                                                                               
GBIL50   CLI   SCITYPE,SCITDBNO    DRAFT BILL NUMER SEQ TYPE?                   
         BNE   GBIL30                                                           
GBIL60   AP    SCIAMNT,=P'1'       INCREMENT DRAFT BILL NUMBER SEQUENCE         
         UNPK  BCDUB,SCIAMNT                                                    
         MVC   OSVBILNO,BCDUB+(L'BCDUB-L'OSVBILNO)                              
         OI    OSVBILNO+L'OSVBILNO-1,X'F0' UPDATE SAVED BILL NUMBER             
*                                                                               
GBIL70   LA    R4,IOKEY            R4=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,R4                                                       
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ    RECORD TYPE                                  
         MVC   PBRPCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRPSUB,PBRPPASQ    PASSIVE RECORD                               
         MVC   PBRPBLNO,OSVBILNO   BILL NUMBER                                  
         MVC   PBRPIND,OSVPIND     INDICATOR (DRAFT/LIVE)                       
         GOTO1 AIO,IO1+IOACCDIR+IOHIGH GET BILL RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV RECORD FOUND?                   
         BNE   GBIL80                                                           
         CLI   CSACT,ACTFFRM       ACTION ADD?                                  
         BNE   GBIL90                                                           
         CP    SCIAMNT,=P'999999'  MAX BILL NUMBER REACHED?                     
         BNE   GBIL60                                                           
         ZAP   SCIAMNT,=P'0'       RESET BILL NUMBER                            
         B     GBIL60                                                           
         DROP  R3                                                               
*                                                                               
GBIL80   MVC   FVMSGNO,=AL2(AE$RECNF) RECORD NOT FOUND                          
         CLI   CSACT,ACTFFRM       ACTION ADD?                                  
         BNE   GBILERRX                                                         
         B     GBIL120                                                          
*                                                                               
GBIL90   GOTO1 AIO,IO1+IOACCMST+IOGET GET BILL RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1             R4=A(BILL RECORD)                            
         MVI   OSVSTCOL,1          SET FIRST DISPLAY COLUMN TO START            
         MVI   OSVSTLIN,1          SET FIRST DISPLAY LINE TO START              
         MVC   OSVJOB,PBRKJOB      SAVE JOB                                     
         MVC   OSVSEQ,PBRKSEQ      SAVE SEQUENCE NUMBER (COMPLEMENT)            
*                                                                               
         LA    R4,PBRRFST          R4=A(FIRST ELEMENT)                          
         USING BLHELD,R4                                                        
GBIL100  CLI   BLHEL,EOR           END OF RECORD?                               
         BE    GBIL140                                                          
         CLI   BLHEL,BLHELQ        BILL HEADER RECORD?                          
         BE    GBIL106                                                          
         CLI   BLHEL,NDXELQ        INDEX ELEMENT?                               
         BE    GBIL110                                                          
*                                                                               
GBIL105  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,BLHLN                                                         
         AR    R4,R0                                                            
         B     GBIL100                                                          
*                                                                               
         USING BLHELD,R4           BILL HEADER RECORD                           
GBIL106  MVC   OSVBFORM,BLHFORM    BILL FORMAT                                  
         MVC   OSVBLANG,BLHLANG    BILL FORMAT LANGUAGE                         
         B     GBIL105                                                          
         DROP  R4                                                               
*                                                                               
         USING NDXELD,R4           INDEX ELEMENT                                
GBIL110  MVC   OSVPHIGH,NDXHIGH    HIGHEST PARAGRAPH NUMBER                     
         MVC   OSVPACTV,NDXACTV    HIGHEST ACTIVE PARAGRAPH NUMBER              
         SR    RF,RF                                                            
         IC    RF,NDXLN            RF=L'(INDEX ELEMENT)                         
         SH    RF,=Y(NDXINDX+1-NDXELD)                                          
         EX    RF,*+4              RF=(X LENGTH OF INDEX LIST)                  
         MVC   OSVPLST(0),NDXINDX  SAVE PARAGRAPH INDEX LIST                    
         B     GBIL105                                                          
         DROP  R4                                                               
*                                                                               
GBIL120  MVC   OSVJOB,BCJOBCOD     DISPLAY JOB CODE                             
         GOTO1 AIO,IO2+IOACCMST+IOPUTREC PUT BACK LEDGER RECORD                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,IOKEY                                                         
         USING PBRRECD,R4                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ                                                 
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,BCJOBCOD    JOB CODE                                     
         GOTO1 AIO,IO1+IOACCDIR+IOHIGH GET BILL RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    RF,=X'FFFF'         COMP SEQ OF FIRST BILL RECORD                
         CLC   PBRKEY(PBRKSEQ-PBRKEY),IOKEYSAV RECORD FOUND?                    
         BNE   GBIL130                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PBRKSEQ        GET SEQUENCE OF LATEST BILL NUMBER           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                DECREMENT TO GET SEQ FOR NEW BILL            
GBIL130  L     R4,AIO1             R4=A(IOAREA FOR NEW BILL RECORD)             
         MVC   PBRKEY,IOKEYSAV     RESTORE KEY                                  
         STCM  RF,3,PBRKSEQ        OVERWRITE SEQUENCE                           
         STCM  RF,3,OSVSEQ         SAVE SEQUENCE                                
         LA    RF,PBRRFST          RF=A(FIRST ELEMENT)                          
         MVI   0(RF),0             SET END OF RECORD MARKER                     
         LA    RF,1(RF)                                                         
         SR    RF,R4               RF=L'(NEW RECORD)                            
         STCM  RF,3,PBRRLEN        SET LENGTH OF NEW RECORD                     
         LA    R4,BOELEM           R4=A(ELEMENT WORK AREA)                      
         USING BLHELD,R4                                                        
         XC    BLHEL(BLHLNQ),BLHEL                                              
         MVI   BLHEL,BLHELQ        BILL HEADER ELEMENT                          
         MVI   BLHLN,BLHLNQ                                                     
         MVC   BLHJOB,BCJOBCOD     SET JOB CODE                                 
         MVC   BLHBLNO,OSVBILNO    SET BILL NUMBER                              
         MVC   BLHUSER,CUUSER                                                   
         MVC   BLHPERS,CUPASS                                                   
         MVC   BLHCRED,ASCDAT      CREATED DATE                                 
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         USING GOBBLOCK,RF                                                      
         ZAP   BLHDSC,GODSCPCT     DISCOUNT                                     
         ZAP   BLHSCH,GOSRGPCT     SURCHARGE                                    
         DROP  RF                                                               
         MVC   BLHFORM,CSFORMAT    GET BILLING FORMAT                           
         MVC   OSVBFORM,BLHFORM                                                 
         MVC   BLHLANG,CSFMLANG    GET BILLING FORMAT LANGUAGE                  
         MVC   OSVBLANG,BLHLANG                                                 
         MVC   BLHCUR,CSBILCUR     SET CURRENCY CODE IN BILL                    
         CLC   CSBILCUR,CSCPYCUR   TEST FOREIGN CURRENCY                        
         BE    *+10                                                             
         MVC   BLHRVAL,CSEXCVAL                                                 
         GOTO1 VDATCON,BCDMCB,(2,BCTODAYC),(0,BOWORK1)                          
         XR    RF,RF               SET BILL EXPIRY DATE                         
         ICM   RF,1,P#RETDAY       GET NON-STANDARD RETENTION DAYS              
         BNZ   *+8                                                              
         LA    RF,7                DEFAULT IS 7                                 
         GOTO1 VADDAY,BCDMCB,BOWORK1,BOWORK1+6,(RF)                             
         GOTO1 VDATCON,BCDMCB,(0,BOWORK1+6),(2,BLHEXPD)                         
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R4),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NDXELD,R4                                                        
         XC    NDXEL(NDXLNQ),NDXEL                                              
         MVI   NDXEL,NDXELQ        INDEX ELEMENT                                
         MVI   NDXLN,NDXLNQ                                                     
         MVI   NDXHIGH,1           SET HIGHEST EXISTING PARA                    
         MVI   NDXACTV,1           SET HIGHEST ACTIVE PARA                      
         MVI   NDXINDX,1           SET FOR PARAGRAPH 1                          
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R4),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO1+IOACCMST+IOADDREC ADD NEW BILL RECORD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         LA    R4,IOKEY                                                         
         USING PBRRECD,R4                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   PBRPTYP,PBRPTYPQ    RECORD TYPE                                  
         MVC   PBRPCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRPSUB,PBRPPASQ    PASSIVE RECORD                               
         MVC   PBRPBLNO,OSVBILNO   BILL NUMBER                                  
         MVI   PBRPIND,PBRPIDFT    DRAFT BILL                                   
         MVC   PBRPUSER,CUUSER     USER-ID                                      
         MVC   PBRPJOB,OSVJOB      JOB                                          
         MVC   PBRPCRED,ASCDAT     CREATED DATE                                 
         MVC   PBRPFORM,CSFORMAT   GET BILLING FORMAT                           
         MVC   PBRPPERS,CUPASS     PERSON CODE                                  
         MVC   PBRKDA,IODA         DISK ADDRESS                                 
         GOTO1 AIO,IO1+IOACCDIR+IOADDREC ADD RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         XC    OSVPVALS(OSVPLNQ),OSVPVALS  INIT CURRENT PARA VALUES             
         MVI   OSVPOFST,0                                                       
         MVI   OSVPARA,0                                                        
*                                                                               
GBIL140  LA    R2,EDTJOBH          R2=A(JOB FIELD HEADER)                       
         USING FLDHDRD,R2                                                       
         MVC   EDTJOB,OSVJOB       DISPLAY JOB CODE                             
         OI    FLDOIND,FOUTTRN                                                  
         LA    R2,EDTBILNH                                                      
         MVC   EDTBILN,OSVBILNO    DISPLAY BILL NUMBER                          
         MVI   OSVSTCOL,1          SET FIRST DISPLAY COLUMN TO START            
         MVI   OSVSTLIN,1          SET FIRST DISPLAY LINE TO START              
         LA    R2,EDTTXT1H         R2=A(FIRST TEXT LINE FIELD HEADER)           
         SR    R2,RA               R2=(DISP TO TEXT LINE FIELD HEADER)          
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVI   TIOBCURI,0          SET CURSOR INDEX TO START OF FIELD           
         STCM  R2,3,TIOBCURD       OVERRIDE DEFAULT CURSOR POSITION             
         DROP  RF                                                               
*                                                                               
         USING FBLKD,FFMTBLK                                                    
         USING BOFELD,FBBOFEL                                                   
*                                                                               
         LA    R2,IOKEY                                                         
         USING PBCRECD,R2          R2=A(CONTROL RECORD)                         
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,OSVBFORM    FORMAT NUMBER                                
         MVC   PBCKLANG,OSVBLANG   FORMAT LANGUAGE                              
         GOTO1 AIO,IOACCMST+IOREAD+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFMTBLK,BOPARM,('FBGET',FBLKD),AIO2                              
         MVC   OSVREPWD,BOFMAXWD   MAX LINE WIDTH                               
         SR    RF,RF                                                            
         IC    RF,OSVREPWD         RF=(MAX LINE WIDTH)                          
         CH    RF,=Y(MAXLLNQ)      WIDER THAN TEXT MAX?                         
         BNH   *+12                                                             
         LA    RF,MAXLLNQ                                                       
         STC   RF,OSVREPWD                                                      
         DROP  R2                                                               
*                                                                               
GBILX    CR    RB,RB               CC EQUAL FOR OK EXIT                         
         B     EXIT                                                             
GBILERRX LTR   RB,RB               CC UNEQUAL FOR ERROR EXIT                    
         B     ERREXIT                                                          
         EJECT                                                                  
***********************************************************************         
* READ AND DISPLAY PARAGRAPH RECORD                                   *         
* NTRY- R1=A(BYTE CONTAINING PARAGRAPH NUMBER)                        *         
*       R1=(BINARY ZEROS) IF PARAGRAPH SCREEN FIELD TO BE USED        *         
***********************************************************************         
         SPACE 1                                                                
GPARA    NTR1                                                                   
         L     RB,ABASE1                                                        
         L     R8,ABASE2                                                        
         L     R7,ABASE3                                                        
         L     R6,ABASE4                                                        
         LA    R2,EDTPARAH         R2=A(PARAGRAPH FIELD HEADER)                 
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         LTR   R1,R1               ADDRESS OF PARA SUPPLIED BY CALLER?          
         BZ    GPAR10                                                           
         SR    R3,R3                                                            
         ICM   R3,1,0(R1)          R3=(PARAGRAPH NUMBER TO BE READ)             
         BNZ   GPAR50                                                           
         DC    H'0'                                                             
GPAR10   CLC   FLDDATA(L'EDTPARA),BCSPACES PARAGRAPH NUMBER INPUT?              
         BH    *+12                                                             
         LA    R3,1                DEFAULT TO PARAGRAPH 1 IF NO INPUT           
         B     GPAR40                                                           
         MVC   FVMSGNO,=AL2(AE$NONIF) FIELD NOT NUMERIC                         
         LA    RF,0                                                             
         LA    RE,FLDDATA          RE=A(PARA FIELD)                             
         LA    R0,L'EDTPARA        R0=L'(PARA FIELD)                            
GPAR20   CLI   0(RE),C'0'          CHARACTER NOT NUMERIC?                       
         BL    GPAR30                                                           
         LA    RF,1(RF)            RF=(NUMBER OF NUMERIC CHARACTERS)            
         LA    RE,1(RE)            BUMP TO NEXT CHARACTER                       
         BCT   R0,GPAR20                                                        
*                                                                               
GPAR30   LTR   RF,RF               ANY NUMERIC CHARACTERS FOUND?                
         BZ    GPARERRX                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8              RF=X L'(NUMERIC INPUT)                       
         B     *+10                                                             
         PACK  BCDUB,FLDDATA(0)                                                 
         CVB   R3,BCDUB            R3=(PARAGRAPH NUMBER)                        
GPAR40   CURED (R3),(L'EDTPARA,FLDDATA),0,DMCB=BCDMCB,ALIGN=LEFT                
GPAR50   MVC   FVMSGNO,=AL2(AE$PNFND) PARAGRAPH NOT FOUND                       
         CLM   R3,1,OSVPACTV       PARAGRAPH NOT ACTIVE?                        
         BH    GPARERRX                                                         
         STC   R3,OSVPOFST         SAVE PARAGRAPH OFFEST                        
*                                                                               
         LA    RE,OSVPLST-1(R3)    RE=A(PARAGRAPH INDEX)                        
         LA    R3,IOKEY            R3=A(KEY FOR PRODUCTION PARA RECORD)         
         USING PBRRECD,R3                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,0(RE)      PARAGRAPH#                                   
         GOTO1 AIO,IO1+IOACCMST+IOHIGH GET PARAGRAPH RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1             RF=A(PRODUCTION PARA RECORD)                 
         CLC   PBRKEY(PBRKLINE-PBRKEY),IOKEYSAV RECORD FOUND?                   
         BNE   GPARERRX                                                         
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED ON                  
         MVC   OSVPARA,PBRKPARA    SAVE PARAGRAPH NUMBER                        
*                                                                               
         LA    R3,PBRRFST          R3=A(FIRST ELEMENT)                          
         USING PGHELD,R3                                                        
GPAR60   CLI   PGHEL,EOR           END OF RECORD?                               
         BE    GPARX                                                            
         CLI   PGHEL,PGHELQ        PARAGRAPH HEADER ELEMENT?                    
         BE    GPAR80                                                           
         CLI   PGHEL,NDXELQ        INDEX ELEMENT?                               
         BE    GPAR90                                                           
         CLI   PGHEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    GPAR100                                                          
*                                                                               
GPAR70   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,PGHLN                                                         
         AR    R3,R0                                                            
         B     GPAR60                                                           
*                                  PARAGRAPH HEADER ELEMENT                     
GPAR80   LA    R2,EDTNETH          R2=A(NET FIELD HEADER)                       
         USING FLDHDRD,R2                                                       
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED ON                  
         CURED PGHNET,(L'EDTNET,FLDDATA),CSCURBIL,MINUS=YES,           X        
               DMCB=BCDMCB,ALIGN=LEFT                                           
         LA    R2,EDTCOMNH         R2=A(COMMISSION FIELD HEADER)                
         USING FLDHDRD,R2                                                       
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED ON                  
         CURED PGHCOM,(L'EDTCOMN,FLDDATA),CSCURBIL,MINUS=YES,          X        
               DMCB=BCDMCB,ALIGN=LEFT                                           
         LA    R2,EDTTYPEH         R2=A(TAX CODE FIELD HEADER)                  
         USING FLDHDRD,R2                                                       
         MVC   EDTTYPE,BCSPACES                                                 
         TM    PGHHTYP,PGHHHRSQ    HOURS TYPE PARAGRAPH?                        
         BO    *+12                                                             
         LH    RE,=AL2(UC@COST-TWAD) COST PARAGRAPH                             
         B     *+8                                                              
         LH    RE,=AL2(UC@TIME-TWAD) TIME PARAGRAPH                             
         LA    RE,TWAD(RE)                                                      
         MVC   EDTTYPE(L'UC@COST),0(RE)                                         
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED ON                  
         B     GPAR70                                                           
*                                                                               
         USING NDXELD,R3           INDEX ELEMENT                                
GPAR90   MVC   OSVLHIGH,NDXHIGH    HIGHEST LINE NUMBER                          
         MVC   OSVLACTV,NDXACTV    HIGHEST ACTIVE LINE NUMBER                   
         SR    RF,RF                                                            
         IC    RF,NDXLN            RF=L'(INDEX ELEMENT)                         
         SH    RF,=Y(NDXINDX+1-NDXELD)                                          
         EX    RF,*+4              RF=(X LENGTH OF INDEX LIST)                  
         MVC   OSVLLST(0),NDXINDX  SAVE LINE INDEX LIST                         
         B     GPAR70                                                           
*                                                                               
         USING FFTELD,R3                                                        
GPAR100  CLI   FFTTYPE,FFTTPGHC    PARAGRAPH HEADER COMMENT?                    
         BNE   GPAR70                                                           
         LA    R2,EDTDESCH         R2=A(PARAGRAPH DESCRIPTION FIELD)            
         USING FLDHDRD,R2                                                       
         MVC   FLDDATA(L'EDTDESC),BCSPACES DISPLAY HEADER COMMENT               
         SR    RF,RF                                                            
         ICM   RF,1,FFTDLEN        RF=L'(HEADER COMMENT)                        
         BZ    GPAR70                                                           
         CH    RF,=Y(L'EDTDESC)    TRUNCATE DESCRIPTION IF TOO LONG             
         BNH   *+8                                                              
         LA    RF,L'EDTDESC                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FLDDATA(0),FFTDATA  DISPLAY HEADER COMMENT                       
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED ON                  
         B     GPAR70                                                           
*                                                                               
GPARX    CR    RB,RB               CC EQUAL FOR OK EXIT                         
         B     EXIT                                                             
GPARERRX LTR   RB,RB               CC UNEQUAL FOR ERROR EXIT                    
         B     ERREXIT                                                          
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE A PARAGRAPH RECORD OR ALL LINES IN THE PARAGRAPH             *         
***********************************************************************         
         SPACE 1                                                                
DPARA    NTR1                                                                   
         TM    OSVDPAR,OSVDALLQ    DELETE ALL LINES?                            
         BNO   DPAR10                                                           
         MVI   OSVSTLIN,1                                                       
         MVI   OSVLACTV,0          SET NUMBER OF ACTIVE LINES TO ZERO           
         MVI   LRMODE,LRADDQ       ADD A BLANK LINE RECORD                      
         MVI   LRLINE,1            FIRST LINE                                   
         MVC   LRPARA,OSVPARA      PARAGRAPH INDEX                              
         MVI   LRCOLUMN,1          START COLUMN                                 
         MVC   WORKTTXT(MAXLLNQ),BCSPACES                                       
         MVI   LRLINELN,MAXLLNQ    LENGTH OF LINE                               
         LA    RE,WORKTTXT         RE=A(LINE)                                   
         STCM  RE,15,LRALINE                                                    
         ICM   RF,15,ALINEREC                                                   
         BASR  RE,RF                                                            
         OI    LINEFLAG,LCHANGEQ   SET LINE INDEX CHANGED                       
         MVI   PRMODE,PRCHANGQ                                                  
         BAS   RE,PARAREC          CHANGE PARAGRAPH RECORD                      
         BE    DPARX                                                            
         DC    H'0'                                                             
*                                  DELETE WHOLE PARAGRAPH                       
DPAR10   MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         MVC   CMPTO,OSVPHIGH      MOVE TO OFFSET                               
         MVC   CMPFROM1,OSVPOFST   MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,OSVPOFST   MOVE FROM OFFSET 2 (END)                     
         LA    RF,OSVPLST          RF=A(PARAGRAPH INDEX LIST)                   
         ST    RF,CMPAILST                                                      
         MVI   CMPILLN,L'OSVPLST   LENGTH OF INDEX LIST                         
         BAS   RE,CMI              MOVE PARA INDEX TO END OF LIST               
         SR    RF,RF                                                            
         IC    RF,OSVPACTV         REDUCE NUMBER OF ACTIVE PARAGRAPHS           
         SH    RF,=H'1'                                                         
         BNM   *+8                                                              
         LA    RF,0                                                             
         STC   RF,OSVPACTV                                                      
         CLI   OSVPACTV,0          LAST PARAGRAPH DELETED?                      
         BNE   DPAR20                                                           
         MVI   PRMODE,PRADDQ       ADD A BLANK PARAGRAPH                        
         BAS   RE,PARAREC                                                       
DPAR20   BAS   RE,BILLREC                                                       
*                                                                               
         CLC   OSVPOFST,OSVPACTV   FIRST PARAGRAPH DELETED?                     
         BNH   *+8                                                              
         MVI   OSVPOFST,1                                                       
*                                                                               
         LA    R1,OSVPOFST         R1=A(PARAGRAPH# REQUIRED)                    
         ICM   RF,15,AGPARA                                                     
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
DPARX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EDIT SCREEN LINES                                                   *         
***********************************************************************         
         SPACE 1                                                                
EDTSCRN  NTR1                                                                   
         XC    SCVALS(SCVALSL),SCVALS                                           
         MVI   AUTOINS,0           INIT AUTOMATIC INSERT LINE                   
         XC    UNUSEINS,UNUSEINS   INIT NUMBER OF UNUSED INSERTS                
         MVC   NXTLOFF,OSVSTLIN    SET NEXT LINE NUMBER TO START LINE           
         LA    R0,MAXSLINQ         R0=(NUMBER OF TEXT LINES)                    
         LA    R2,EDTLCM1H         R2=A(1ST LINE COMMAND FIELD HEADER)          
         USING SLINED,R2                                                        
*                                                                               
EDTS10   TM    SLTXT1H+FLDIIND-FLDHDRD,FINPTHIS FIELD INPUT THIS TIME?          
         BO    EDTS20                                                           
         CLI   SLLIN1,INSERTQ      INSERT LINE?                                 
         BNE   EDTS50                                                           
         LA    RE,UNUSEINS         RE=A(LIST OF UNUSED INSERTS)                 
         CLI   0(RE),EOT           END OF LIST?                                 
         BE    *+12                                                             
         LA    RE,1(RE)            BUMP TO NEXT UNUSED INSERT NUMBER            
         B     *-12                                                             
         MVC   0(L'NXTLOFF,RE),NXTLOFF SAVE CURRENT UNUSED INS LINE             
*        SR    RF,RF               BUMP LINE NUMBER                             
*        IC    RF,NXTLOFF                                                       
*        LA    RF,1(RF)                                                         
*        STC   RF,NXTLOFF                                                       
         B     EDTS60                                                           
*                                                                               
EDTS20   MVI   LRMODE,LRCHANGE     CHANGE LINE RECORD                           
         CLI   SLLIN1,INSERTQ      INSERT LINE?                                 
         BNE   *+8                                                              
         MVI   LRMODE,LRADDQ       ADD/REACTIVATE LINE RECORD                   
         MVC   LRLINE,NXTLOFF      LINE NUMBER TO BE ADDED/CHANGED              
         MVC   LRPARA,OSVPARA      PARAGRAPH INDEX                              
         MVC   LRCOLUMN,OSVSTCOL   START COLUMN OF TEXT                         
         MVC   WORKTTXT(MAXLLNQ),BCSPACES                                       
         SR    R1,R1                                                            
         IC    R1,SLTXT1H+FLDILEN-FLDHDRD                                       
         SH    R1,=H'1'                                                         
         BM    *+14                                                             
         EX    R1,*+4                                                           
         MVC   WORKTTXT(0),SLTXT1  GET SCREEN LINE                              
         MVI   LRLINELN,L'SLTXT1   LENGTH OF SCREEN LINE                        
         CLI   OSVSTCOL,1          IF COLUMN 1                                  
         BNE   EDTS40                                                           
         CLI   SLLIN1,INSERTQ      AND CURRENT LINE IS AN INSERT                
         BNE   EDTS40                                                           
         LA    R1,SLLNQ(R2)        R1=A(NEXT SCREEN LINE)                       
         CLI   SLLIN1-SLINED(R1),INSERTQ AND NEXT LINE IS AN INSERT             
         BNE   EDTS40                                                           
         CLI   SLLCM1-SLINED(R1),C'+' AND COMMAND LINE CONTAINS A PLUS          
         BNE   EDTS40                                                           
         LA    RE,WORKTTXT+L'SLTXT1 THEN APPEND NEXT LINE ON TO CURRENT         
         LA    RF,MAXLLNQ-L'SLTXT1 RF=MAX L'(NEXT LINE TO BE APPENDED)          
         SR    R3,R3                                                            
         ICM   R3,1,SLTXT1H-SLINED+FLDILEN-FLDHDRD(R1) R3=L'(NXT LINE)          
         BZ    EDTS40                                                           
         CR    R3,RF               LEN OF NXT LINE < AVAILABLE LEN?             
         BNL   *+6                                                              
         LR    RF,R3               USE ACTUAL LEN TO AVOID BIN ZEROS            
         BCTR  RF,0                                                             
         LA    RE,WORKTTXT+L'SLTXT1 RE=A(APPEND POSITION)                       
         EX    RF,*+4                                                           
         MVC   0(0,RE),SLTXT1-SLINED(R1) GET PART OF LINE FOR APPEND            
         MVI   LRLINELN,MAXLLNQ    SET LENGTH OF LINE                           
         MVC   SLLCM1-SLINED(L'SLLCM1,R1),BCSPACES CLEAR LINE CMND FLD          
         SH    R3,=Y(MAXLLNQ-L'SLTXT1+1) NOT ALL OF NEXT LINE USED?             
         BM    EDTS30                                                           
         LA    RF,SLTXT1+1-SLINED(R1,RF) RF=A(FIRST UNUSED CHAR)                
         EX    R3,*+4                                                           
         MVC   SLTXT1-SLINED(0,R1),0(RF) MOVE UNUSED PORTION TO 1ST COL         
         LA    RF,SLTXT1-SLINED(R1,R3)                                          
         LA    RE,L'SLTXT1-2                                                    
         SR    RE,R3                                                            
         EX    RE,*+4                                                           
         MVC   0(0,RF),BCSPACES     CLEAR REST OF LINE                          
         B     EDTS40                                                           
EDTS30   NI    SLTXT1H-SLINED+FLDIIND-FLDHDRD(R1),X'FF'-FINPTHIS                
*                                                                               
EDTS40   LA    RE,WORKTTXT         RE=A(ADDRESS OF LINE)                        
         STCM  RE,15,LRALINE                                                    
         ICM   RF,15,ALINEREC      ADD/REACTIVATE/CHANGE LINE                   
         BASR  RE,RF                                                            
*                                                                               
EDTS50   LA    RF,SLTXT1           CHECK FOR STANDARD COMMENT                   
         LA    RE,L'EDTTXT1-2                                                   
         MVC   BCDUB,BCSPACES                                                   
         MVC   BCDUB(1),MY@NRTV                                                 
         MVI   BCDUB+1,C'='        N=                                           
         MVI   BCDUB+2,C'+'                                                     
         MVI   BCDUB+3,C'='        +=                                           
         L     R1,ATRUP                                                         
EDTS52   MVC   BCHALF,0(RF)                                                     
         TR    BCHALF(1),0(R1)                                                  
         CLC   BCHALF,BCDUB                                                     
         BE    EDTS54                                                           
         CLC   BCHALF,BCDUB+2                                                   
         BE    EDTS54                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,EDTS52                                                        
         B     EDTS56                                                           
EDTS54   MVC   BCDUB,BCSPACES                                                   
         MVC   BCDUB(L'SCMKCODE),2(RF)                                          
         TR    BCDUB(L'SCMKCODE),0(R1)                                          
         LA    RE,SLTXT1                                                        
         SR    RF,RE                                                            
         STC   RF,SCDISP           SET STANDARD COMMENT DISPLACEMENT            
         MVI   SC1IND,SC1IBUIL     BUILD STANDARD COMMENT TABLE ENTRY           
         GOTO1 ASTDCOM,BCDUB                                                    
         BNE   EXIT                                                             
*                                                                               
EDTS56   SR    RF,RF               BUMP LINE NUMBER                             
         IC    RF,NXTLOFF                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NXTLOFF                                                       
         CLI   SLLIN1,INSERTQ      INSERT LINE?                                 
         BNE   EDTS60                                                           
         BCTR  RF,0                                                             
         STC   RF,AUTOINS          SET FOR AUTO INSERT                          
         MVC   SLLIN1,BCSPACES     CLEAR DOTS OFF LINE NUMBER                   
EDTS60   LA    R2,SLLNQ(R2)        BUMP TO NEXT LINE                            
         BCT   R0,EDTS10                                                        
         DROP  R2                                                               
*                                                                               
EDTS70   MVI   PRMODE,PRCHANGQ                                                  
         BAS   RE,PARAREC          CHANGE PARAGRAPH RECORD IF NECESSARY         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EDTSX    CR    RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY HEADER LINES                                                *         
***********************************************************************         
         SPACE 1                                                                
DISHEAD  NTR1                                                                   
         LA    R2,EDTPARAH         R2=A(PARAGRAPH FIELD HEADER)                 
         USING FLDHDRD,R2                                                       
         MVC   FLDDATA(L'EDTPARA),BCSPACES                                      
         CLI   OSVPACTV,0          NO ACTIVE PARAGRAPHS?                        
         BE    DISH10                                                           
         SR    RF,RF                                                            
         IC    RF,OSVPOFST         RF=(CURRENT PARAGRAPH NUMBER)                
         LA    R3,FLDDATA          R3=A(PARAGRAPH FIELD)                        
         CURED (RF),(3,0(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AR    R3,R0               BUMP R3 BY LENGTH OF PARA NUMBER             
         MVC   1(L'OSVMX@OF,R3),OSVMX@OF                                        
         LA    R3,L'OSVMX@OF(R3)                                                
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         SR    RF,RF                                                            
         IC    RF,OSVPACTV         RF=(NUMBER OF ACTIVE PARAGRAPHS)             
         CURED (RF),(3,2(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         OI    FLDIIND,FINPVAL                                                  
DISH10   OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,EDTLTXTH         R2=A(LINE TEXT FIELD HEADER)                 
         MVC   FLDDATA(L'EDTLTXT),BCSPACES                                      
         LH    RE,=AL2(LC@LINE-TWAD) LINE                                       
         LA    RE,TWAD(RE)                                                      
         MVC   FLDDATA(L'LC@LINE),0(RE)                                         
         CLC   OSVSTLIN,OSVLACTV   START LINE IS LAST LINE?                     
         BNL   DISH20                                                           
         LH    RE,=AL2(LC@LINES-TWAD) LINES                                     
         LA    RE,TWAD(RE)                                                      
         MVC   FLDDATA(L'LC@LINES),0(RE)                                        
DISH20   OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,EDTLNUMH         R2=A(LINE NUMBER FIELD HEADER)               
         MVC   FLDDATA(L'EDTLNUM),BCSPACES                                      
         CLI   OSVLACTV,0          NO ACTIVE LINE RECORDS?                      
         BE    DISH40                                                           
         CLC   OSVSTLIN,OSVLACTV   START LINE IS LAST LINE?                     
         BNH   *+10                                                             
         MVC   OSVSTLIN,OSVLACTV   START LINE IS LAST LINE?                     
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN         RF=(TEXT START LINE NUMBER)                  
         LA    R3,FLDDATA          R3=A(LINE NUMBER FIELD)                      
         CURED (RF),(3,0(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AR    R3,R0               BUMP UP BY LENGTH OF LINE NUMBER             
         SR    R4,R4                                                            
         IC    R4,OSVLACTV         R4=(NUMBER OF ACTIVE LINE RECORDS)           
         CLC   OSVSTLIN,OSVLACTV   START LINE IS LAST LINE?                     
         BE    DISH30                                                           
         MVI   0(R3),C'-'                                                       
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN                                                      
         LA    RF,MAXSLINQ-1(RF)                                                
         CR    RF,R4                                                            
         BNH   *+6                                                              
         LR    RF,R4               RF=(TEXT END LINE NUMBER)                    
         LA    R3,1(R3)                                                         
         CURED (RF),(3,0(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AR    R3,R0                                                            
DISH30   MVC   1(L'OSVMX@OF,R3),OSVMX@OF                                        
         LA    R3,L'OSVMX@OF(R3)                                                
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         CURED (R4),(3,2(R3)),0,DMCB=BCDMCB,ALIGN=LEFT ACTIVE LINES             
DISH40   OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         LA    R2,EDTBANH          R2=A(BANNER FIELD HEADER)                    
         USING FLDHDRD,R2                                                       
         OI    FLDOIND,FOUTTRN                                                  
         CLI   OSVCMND,CMNCOLQ     COLUMN COMMAND ENTERED?                      
         BNE   DISH50                                                           
         CLI   OSVCPAR,0           YES/NO PARAMETER SUPPLIED?                   
         BNE   *+12                                                             
         XI    OSVCLFLG,OSVCLRLQ   FLIP THE BITS IF NO PARAMETER                
         B     DISH50                                                           
         TM    OSVCPAR,OSVCYESQ    YES PARAMETER ENTERD?                        
         BO    *+12                                                             
         NI    OSVCLFLG,X'FF'-OSVCLRLQ                                          
         B     DISH90                                                           
         OI    OSVCLFLG,OSVCLRLQ                                                
         B     *+12                                                             
DISH50   TM    OSVCLFLG,OSVCLRLQ   ARE WE DISPLAYING RULE LINE?                 
         BNO   DISH90                                                           
         LA    R1,1                R1=(COLUMN COUNTER)                          
         LA    R3,FLDDATA          R3=A(BANNER FIELD)                           
         MVI   FLDDATA,C'-'                                                     
         MVC   FLDDATA+1(L'EDTBAN-1),FLDDATA FILL BANNER WITH HYPHENS           
         LA    R0,10               R0(KEEPS TRACK OF EVERY TENTH COL)           
DISH60   SR    RE,RE                                                            
         IC    RE,OSVSTCOL         RE=(START COLUMN FOR SCREEN)                 
         LA    RE,L'EDTBAN-1(RE)   RE=(LAST COLUMN FOR SCREEN)                  
         CR    R1,RE               SCANNED WHOLE DISPLAY PORTIION?              
         BH    DISHX                                                            
         BCT   R0,DISH70                                                        
         LA    R0,10               RESET TENTH COLUMN COUNTER                   
         CLM   R1,1,OSVSTCOL       COLUMN PART OF DISPLAY PORTION?              
         BL    DISH80                                                           
         CVD   R1,BCDUB                                                         
         UNPK  BCHALF,BCDUB+6(2)                                                
         MVC   0(1,R3),BCHALF      PUT COL NUMBER ON BANNER LINE                
         B     *+12                                                             
DISH70   CLM   R1,1,OSVSTCOL       COLUMN PART OF DISPLAY PORTION               
         BL    DISH80                                                           
         CH    R0,=H'5'            EVERY FIFTH COLUMN PUT OUT A PLUS            
         BNE   *+8                                                              
         MVI   0(R3),C'+'                                                       
         LA    R3,1(R3)            BUMP TO NEXT CHAR IN BANNER FIELD            
DISH80   LA    R1,1(R1)            BUMP UP COLUMN NUMBER                        
         B     DISH60                                                           
*                                                                               
DISH90   TM    OSVHPAR,OSVHTIMQ+OSVHCSTQ DISPLAY TIME HEADER                    
         BZ    DISH180                                                          
         MVC   BILLHED,BCSPACES                                                 
         LA    R4,IOKEY                                                         
         USING PBCRECD,R4          R4=A(CONTROL RECORD)                         
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,OSVBFORM                                                 
         CLI   PBCKFMT,0                                                        
         BNE   DISH95                                                           
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   PBCKFMT,GOBILFRM-GOBBLOCK(RF)                                    
DISH95   MVC   PBCKLANG,OSVBLANG                                                
         LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IOACCMST+IOREAD(R1)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO4                                                          
*                                                                               
         LA    R4,PBCRFST-PBCRECD(R4)                                           
         USING BLFELD,R4                                                        
*        LA    R4,ALSTBLF                                                       
DISH100  CLI   BLFEL,EOR                                                        
         BE    DISH170                                                          
         CLI   BLFEL,BLFELQ                                                     
         BE    DISH120                                                          
*                                                                               
DISH110  SR    RF,RF                                                            
         IC    RF,BLFLN                                                         
         AR    R4,RF                                                            
         B     DISH100                                                          
*                                                                               
DISH120  TM    OSVHPAR,OSVHTIMQ                                                 
         BNO   DISH125                                                          
         CLI   BLFTYPE,BLFTTIMQ                                                 
         BNE   DISH110                                                          
         B     *+12                                                             
DISH125  CLI   BLFTYPE,BLFTCSTQ                                                 
         BNE   DISH110                                                          
         GOTO1 AFMTHED,BOPARM,(X'02',BLFELD)                                    
         BNE   DISH130                                                          
         LM    RE,RF,0(R1)                                                      
         LA    R1,BILLHED(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),UNDERLIN                                                 
         CLI   BLFHED1,C'*'                                                     
         BNE   DISH130                                                          
         CLC   BLFHED1+1(L'BLFHED1-1),BCSPACES                                  
         BH    DISH130                                                          
         EX    RF,*+4                                                           
         MVC   0(0,R1),BOELEM                                                   
         B     DISH110                                                          
DISH130  GOTO1 AFMTHED,BOPARM,(X'01',BLFELD)                                    
         BNE   DISH110                                                          
         LM    RE,RF,0(R1)                                                      
         LA    R1,BILLHED(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),BOELEM                                                   
         B     DISH110                                                          
*                                                                               
DISH170  SR    RF,RF                                                            
         IC    RF,OSVSTCOL                                                      
         LA    RF,BILLHED-1(RF)                                                 
         MVC   FLDDATA(L'EDTBAN),0(RF)                                          
         B     DISH190                                                          
*                                                                               
DISH180  LA    RE,OSSAVE           RE=A(SECONDARY OVERLAY SAVE AREA)            
         MVC   FLDDATA(L'EDTBAN),LC@TEXT-OSSAVED(RE) DEFAULT BANNER             
DISH190  CLI   OSVSTCOL,1          LEFT MOST COLUMN DISPLAYED?                  
         BE    *+10                                                             
         MVC   FLDDATA(L'LHSMORE),LHSMORE INDICATE MORE ON THE LEFT             
         SR    RF,RF                                                            
         IC    RF,OSVSTCOL                                                      
         LA    RF,L'EDTBAN-1(RF)                                                
         SR    R1,R1                                                            
         IC    R1,OSVREPWD         R1=(MAX LINE WIDTH)                          
         CR    RF,R1               RIGHT MOST COLUMN DISPLAYED?                 
         BE    *+10                IF YES THEN IDICATE MORE ON RIGHT            
         MVC   FLDDATA+L'EDTBAN-L'RHSMORE(L'RHSMORE),RHSMORE                    
DISHX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY SCREEN LINES                                                *         
***********************************************************************         
         SPACE 1                                                                
DISSCRN  NTR1                                                                   
         OI    BASSCRH+(FVOIND-FVIHDR),FVOXMT XMIT FLD SO TIOB UPDATED          
         MVC   NXTLOFF,OSVSTLIN    SET NEXT LINE NUMBER TO START LINE           
         LA    R0,MAXSLINQ         R0=(NUMBER OF TEXT LINES)                    
         LA    R2,EDTLCM1H         R2=A(1ST LINE COMMAND FIELD HEADER)          
         USING SLINED,R2                                                        
         LA    RF,SLTXT1H          RF=A(SCREEN TEXT LINE FIELD HEADER)          
         ST    RF,FVADDR                                                        
         LA    R4,OSVEINSB         R4=A(LIST OF INSERT BLOCK VALUES)            
         USING BLKVALD,R4                                                       
*                                                                               
DISS10   OC    BLKSTRT(BLKLNQ),BLKSTRT ANY OUTSTANDING INSERT?                  
         BZ    DISS40                                                           
         SR    R3,R3               INSERT LINES REQUESTED HERE?                 
         IC    R3,BLKSTRT                                                       
         LA    R3,1(R3)            R3=(LINE NUMBER FOR INSERT START)            
         CLM   R3,1,NXTLOFF        MATCH ON CURRENT LINE NUMBER?                
         BNE   DISS40                                                           
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         LA    RE,OSVEINSB         RE=A(LIST OF INSERT BLOCK VALUES)            
         CR    RE,R4               FIRST INSERT IN LIST?                        
         BNE   DISS20                                                           
         LA    RE,SLTXT1H          RE=A(LINE FIELD HEADER FOR INSERT)           
         SR    RE,RA               RE=(DISPLACEMENT TO FIELD HEADER)            
         STCM  RE,3,TIOBCURD       OVERRIDE DEFAULT CURSOR CONTROL              
         MVI   TIOBCURI,0          SET CURSOR AT START OF FIELD                 
         DROP  RF                                                               
DISS20   SR    R1,R1                                                            
         IC    R1,BLKLEN           R1=(NUMBER OF LINES TO BE INSERTED)          
         CR    R0,R1               NUM TO BE INSERTED > LEFT ON SCREEN?         
         BNL   *+6                                                              
         LR    R1,R0               SET TO NUM OF LINES LEFT ON SCREEN           
         STC   R1,ALTBDVAL         NUMBER OF INSERT                             
         STC   R3,ALTBSTRT         START LINE                                   
         MVI   ALTBEND,X'FF'       END LINE                                     
         MVI   ALTBCTYP,LENINSQ    COMMAND TYPE                                 
         MVI   ALTBSTAT,0                                                       
         BAS   RE,ALTBLKV          ALTER ANY BLOCK COMMANDS AFFECTED            
         LA    RE,SLTXT1H          RE=A(TEXT LINE FIELD HEADER)                 
         LA    R4,BLKLNQ(R4)       BUMP TO NEXT INSERT BLOCK ENTRY              
DISS30   MVC   SLLIN1,INSDOTS      SET LINE NUMBER TO DOTS                      
         OI    SLLIN1H+FLDOIND-FLDHDRD,FOUTTRN                                  
         NI    SLLIN1H+FLDATB-FLDHDRD,X'FF'-FATBHIGH                            
         MVC   SLLCM1,INSDOTS      SET COMMAND LINE TO DOTS                     
         OI    SLLCM1H+FLDOIND-FLDHDRD,FOUTTRN                                  
         NI    SLLCM1H+FLDATB-FLDHDRD,X'FF'-FATBPROT                            
         MVC   SLTXT1,BCSPACES     CLEAR TEXT LINE                              
         OI    SLTXT1H+FLDOIND-FLDHDRD,FOUTTRN                                  
         NI    SLTXT1H+FLDIIND-FLDHDRD,X'FF'-FINPTHIS                           
         NI    SLTXT1H+FLDATB-FLDHDRD,X'FF'-(FATBPROT+FATBHIGH)                 
         LA    R2,SLLNQ(R2)        BUMP TO NEXT SCREEN LINE                     
         BCT   R0,*+8                                                           
         B     DISS150             SCREEN FULL?                                 
         BCT   R1,DISS30                                                        
*                                                                               
DISS40   MVC   SLLCM1,BCSPACES     CLEAR LINE COMMAND                           
         OI    SLLCM1H+FLDOIND-FLDHDRD,FOUTTRN                                  
         NI    SLLCM1H+FLDATB-FLDHDRD,X'FF'-FATBPROT                            
         MVC   SLTXT1,BCSPACES     CLEAR LINE                                   
         OI    SLTXT1H+FLDOIND-FLDHDRD,FOUTTRN                                  
         MVI   SLTXT1H+FLDOLEN-FLDHDRD,L'SLTXT1 SET OUPUT LENGTH                
         NI    SLTXT1H+FLDIIND-FLDHDRD,X'FF'-FINPTHIS                           
         NI    SLTXT1H+FLDATB-FLDHDRD,X'FF'-(FATBPROT+FATBHIGH)                 
         CLC   NXTLOFF,OSVLACTV    ALL TXT LINES DISPLAYED?                     
         BH    DISS80                                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,NXTLOFF                                                       
         CVD   RF,BCDUB                                                         
         UNPK  SLLIN1,BCDUB        DISPLAY LINE NUMBER                          
         OI    SLLIN1+L'SLLIN1-1,X'F0'                                          
         OI    SLLIN1H+FLDOIND-FLDHDRD,FOUTTRN                                  
         OI    SLLIN1H+FLDATB-FLDHDRD,FATBHIGH                                  
         LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,OSVPARA    PARAGRAPH#                                   
         SR    RE,RE                                                            
         IC    RE,NXTLOFF          OFFSET TO LINE INDEX                         
         BCTR  RE,0                                                             
         LA    RE,OSVLLST(RE)      RE=A(CORRESPONDING INDEX FOR LINE)           
         MVC   PBRKLINE,0(RE)      LINE#                                        
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE NUMBER RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             RF=A(LINE NUMBER RECORD)                     
         LA    RF,PBRRFST          RF=A(FIRST ELEMENT)                          
         USING FFTELD,RF                                                        
DISS50   CLI   FFTEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    DISS70                                                           
*                                                                               
DISS60   SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,FFTLN                                                         
         AR    RF,RE                                                            
         B     DISS50                                                           
*                                                                               
DISS70   CLI   FFTTYPE,FFTTBLIN    PRODUCTION BILL DETAIL LINE?                 
         BNE   DISS60                                                           
         SR    RE,RE                                                            
         IC    RE,FFTDLEN          RE=L'(TEXT STRING)                           
         SR    R1,R1                                                            
         IC    R1,OSVSTCOL         R1=(START COLUMN FOR DISPLAY)                
         SR    RE,R1               RE=(X LEN OF TXT STRNG-COLUMN OFFST)         
         BM    DISS130             NOTHING TO DISPLAY?                          
         BCTR  R1,0                                                             
         LA    R1,FFTDATA(R1)      R1=A(1ST CHAR IN TXT STRING FOR DIS)         
         CH    RE,=Y(L'EDTTXT1-1)  LENGTH OF STRING TO LONG?                    
         BNH   *+8                                                              
         LA    RE,L'EDTTXT1-1      SET TO LENGTH OF DISPLAY AREA                
         EX    RE,*+4                                                           
         MVC   SLTXT1(0),0(R1)     DISPLAY LINE                                 
         B     DISS130                                                          
         DROP  RF                                                               
*                                  NO MORE TEXT TO DISPLAY                      
DISS80   MVC   SLLIN1,BCSPACES     CLEAR LINE NUM                               
         OI    SLLIN1H+FLDOIND-FLDHDRD,FOUTTRN                                  
         MVC   SLTXT1,BCSPACES     CLEAR LINE NUM                               
         NI    SLTXT1H+FLDIIND-FLDHDRD,X'FF'-FINPTHIS                           
         OI    SLTXT1H+FLDATB-FLDHDRD,FATBPROT SET FIELD PROTECT ON             
         MVC   SLLCM1,BCSPACES     CLEAR LINE NUM                               
         OI    SLLCM1H+FLDOIND-FLDHDRD,FOUTTRN                                  
         OI    SLLCM1H+FLDATB-FLDHDRD,FATBPROT                                  
         SR    RF,RF                                                            
         IC    RF,OSVLACTV         RF=(NUMBER OF ACTIVE LINE INDEXES)           
         LA    RF,1(RF)                                                         
         CLM   RF,1,NXTLOFF        NEXT LINE AFTER END OF TEXT DISP?            
         BNE   DISS130                                                          
         TM    OSVFCFLG,OSVFCFNQ+OSVFCCHQ FIND/CHANGE MODE?                     
         BNZ   DISS120                                                          
         LR    RE,R2               RE=A(SCREEN LINE AFTER END OF TEXT)          
         SR    RE,RA                                                            
         CLM   RE,3,OSVCURD        WILL CURSOR BE ON PROTECTED SCREEN?          
         BH    DISS90                                                           
         SH    RE,=Y(SLLNQ)        SET CURS TO LAST SCREEN LIN WITH TXT         
         L     RF,AINP                                                          
         MVI   TIOBCURI-TIOBD(RF),0                                             
         B     DISS110                                                          
DISS90   LA    RE,SLLIN1H                                                       
         SH    RE,=Y(SLLNQ)        RE=A(LINE NUM FLD ON LAST WITH TXT)          
         SR    RE,RA                                                            
         CLM   RE,3,OSVCURD        CURSOR ON LAST LINE NUMBER FIELD?            
         BNH   DISS100                                                          
         SH    RE,=Y(SLLIN1H-SLTXT1H)                                           
         CLM   RE,3,OSVCURD        CURSOR ON LAST TEXT LINE FIELD?              
         BNH   DISS100                                                          
         SH    RE,=Y(SLTXT1H-SLLCM1H)                                           
         CLM   RE,3,OSVCURD        CURSOR ON LAST LINE COMMAND FIELD?           
         BH    *+12                                                             
DISS100  L     RF,AINP                                                          
DISS110  STCM  RE,3,TIOBCURD-TIOBD(RF) OVERRIDE DEFAULT CURSOR CONTROL          
DISS120  MVCDD SLTXT1,AC#ENPAR,FL  END OF PARAGRAPH                             
         OI    SLTXT1H+FLDATB-FLDHDRD,FATBHIGH SET HIGH INTENSITY ON            
*                                                                               
DISS130  SR    RF,RF               BUMP LINE NUMBER                             
         IC    RF,NXTLOFF                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NXTLOFF                                                       
DISS140  LA    R2,SLLNQ(R2)        BUMP TO NEXT SCREEN LINE                     
         BCT   R0,DISS10                                                        
         DROP  R2,R4                                                            
*                                                                               
DISS150  XC    OSVEINSB,OSVEINSB   CLEAR OUT INSERTS                            
         CLI   BCPFKEY,PFKFINDQ    FIND PFKEY?                                  
         BE    DISS160                                                          
         CLI   BCPFKEY,PFKCHGQ     CHANGE PFKEY?                                
         BE    DISS160                                                          
         CLI   OSVCMND,CMNFNDQ     FIND COMMAND?                                
         BE    DISS160                                                          
         CLI   OSVCMND,CMNCHGQ     CHANGE COMMAND                               
         BNE   DISS210                                                          
DISS160  TM    OSVFCFLG,OSVFCMTQ   MATCH FOUND?                                 
         BNO   DISS180                                                          
         TM    OSVFCPAR,OSVFCALQ   ALL PARAMETER USED?                          
         BO    DISS170                                                          
         MVC   FVMSGNO,=AL2(AI$CHFD) CHARS FOUND                                
         TM    OSVFCFLG,OSVFCFNQ                                                
         BO    DISS190                                                          
         MVC   FVMSGNO,=AL2(AI$CHCH) CHARS CHANGED                              
         B     DISS190                                                          
DISS170  MVC   FVMSGNO,=AL2(AI$ACHFD) N CHARS FOUND                             
         TM    OSVFCFLG,OSVFCFNQ   FIND COMMAND?                                
         BO    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$ACHCH) N CHARS CHANGED                           
         XC    FVPARMS,FVPARMS                                                  
         LA    R2,FVPARMS          R2=A(MSG PARAMETERS)                         
         SR    RF,RF                                                            
         ICM   RF,3,OSVFCCNT       NUMBER OF MATCHES                            
         CURED (RF),(5,1(R2)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         NI    OSVFCPAR,X'FF'-OSVFCALQ SWITCH OF ALL PARAMETER                  
         B     DISS200                                                          
*                                  MATCH NOT FOUND                              
DISS180  L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVC   TIOBCURD,OSVCURD    OVERRIDE DEFAULT CURSOR CONTROL              
         LA    RE,EDTLCM1H                                                      
         SR    RE,RA                                                            
         CH    RE,TIOBCURD         CURSOR ON COMMAND LINE?                      
         BNE   *+8                                                              
         MVI   TIOBCURI,0          SET TO BEGINING OF FIELD                     
         DROP  RF                                                               
         TM    OSVFCFLG,OSVFCBTQ                                                
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AI$BTRCH) BOTTOM OF TEXT REACHED                    
         B     DISSX                                                            
         TM    OSVFCFLG,OSVFCTPQ                                                
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AI$TPRCH) TOP OF TEXT REACHED                       
         B     DISSX                                                            
         MVC   FVMSGNO,=AL2(AI$NCHFD) NO CHARS FOUND                            
DISS190  XC    FVPARMS,FVPARMS                                                  
         LA    R2,FVPARMS          R2=A(MESSAGE PARAMETERS)                     
DISS200  SR    RF,RF                                                            
         IC    RF,OSVFDLN          RF=L'(FIND STRING)                           
         LA    RE,3(RF)                                                         
         STC   RE,0(R2)                                                         
         MVI   1(R2),C''''         PUT STRING IN QUOTES                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   2(0,R2),OSVFDTXT                                                 
         LA    R2,3(R2,RF)                                                      
         MVI   0(R2),C''''                                                      
         B     DISSX                                                            
*                                                                               
DISS210  MVC   FVMSGNO,=AL2(AI$EPARA) ENTER PARAGRAPH DETAILS                   
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
*        CLM   RE,1,OSVLACTV       BEYOND NUMBER OF LINES IN PARAGRAPH?         
*        BNH   *+8                                                              
*        IC    RE,OSVLACTV                                                      
         STC   RE,BCBYTE2          LINE NUMBER AT END OF SCREEN                 
*                                                                               
         LA    R3,EBLTAB           R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
DISL10   CLI   EBLCTYPE,EOT        END OF TABLE?                                
         BE    DISLX                                                            
         CLI   EBLCTYPE,LENINSQ    DONT RE-DISPLAY INSERTS                      
         BE    DISL80                                                           
         TM    EBLCTYPE,LENMOVEQ+LENCOPYQ MOVE/COPY CMD?                        
         BZ    DISL20                                                           
         CLC   OSVPARA,OSVECMPA    SAME PARA AS DISPLAYED?                      
         BNE   DISL80                                                           
         B     DISL30                                                           
DISL20   TM    EBLCTYPE,LENAFTQ+LENBEFQ+LENOVERQ AFTER/BEFORE/OVER CMD?         
         BZ    DISL30                                                           
         CLC   OSVPARA,OSVETOPA    SAME PARA AS DISPLAYED?                      
         BNE   DISL80                                                           
DISL30   SR    R2,R2                                                            
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
         MH    R2,=Y(EDTLCM2H-EDTLCM1H)                                         
         LA    R2,EDTLCM1H(R2)     R2=A(LINE CMND FIELD HEADER)                 
         USING FLDHDRD,R2                                                       
         MVI   FLDILEN,2           SET LENGTH                                   
         SR    RF,RF                                                            
         ICM   RF,15,BCFULL        RF=A(COMMAND ENTRY)                          
         MVC   FLDDATA(L'EDTLCM1),LENCOMM-LEDTABD(RF)                           
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
         LA    R2,EDTLCM1H         R2=A(FIRST LINE EDIT FIELD HEADER)           
         USING FLDHDRD,R2                                                       
*                                                                               
         MVC   FVMSGNO,=AL2(AE$INVIF) INVALID INPUT                             
LINC10   ST    R2,FVADDR                                                        
         LA    R1,L'EDTLCM1        R1=L'(LINE COMMAND FIELD)                    
LINC20   LA    RF,FLDDATA-1(R1)                                                 
         CLI   0(RF),INSERTQ       INSERT DOT?                                  
         BNE   LINC30                                                           
         MVI   0(RF),C' '          CLEAR OUT INSERT DOT                         
         SR    RE,RE                                                            
         ICM   RE,1,FLDILEN        REDUCE INPUT LENGTH OF FIELD                 
         BZ    LINC30                                                           
         BCTR  RE,0                                                             
         STCM  RE,1,FLDILEN                                                     
LINC30   BCT   R1,LINC20           GET PREVIOUS CHARACTER                       
         CLC   FLDDATA(L'EDTLCM1),BCSPACES ANY INPUT?                           
         BNH   LINC100                                                          
*                                                                               
         OC    FLDDATA(L'EDTLCM1),BCSPACES PAD OUT FIELD WITH SPACES            
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
         BM    LINCERRX                                                         
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
LINC90   CLI   LENCTYPE,LENOVERQ   IF OVER COMMAND                              
         BNE   LINC95                                                           
         MVC   FVMSGNO,=AL2(AE$NAWTT) NOT ALLOWED                               
         CLI   EDTLIN1-EDTLCM1H(R2),INSERTQ NOT ALLOWED ON INSERT               
         BE    LINCERRX                                                         
LINC95   BAS   RE,CHKPSIZE         CHECK SIZE OF PARA NOT EXCEEDED              
         BNE   LINCERRX                                                         
*                                                                               
LINC100  LA    R2,EDTLCM2H-EDTLCM1H(R2) BUMP TO NXT LINE CMND FIELD HDR         
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
LINC120  LA    RF,OSVEINSB         RF=A(INSERT BLOCK LIST)                      
         USING BLKVALD,RF                                                       
         OC    BLKSTRT(BLKLNQ),BLKSTRT NO INSERT COMMANDS FOUND?                
         BNZ   LINC130                                                          
         CLI   AUTOINS,0           IS AUTO INSERT OUTSTANDING?                  
         BE    LINC130                                                          
         CLI   OSVLACTV,MAXPLINQ   PARAGRAPH FULL?                              
         BE    LINC130                                                          
         MVC   BLKSTRT,AUTOINS     PUT AUTO INSET VALUES IN LIST                
         MVC   BLKEND,AUTOINS                                                   
         MVI   BLKLEN,1                                                         
         DROP  RF                                                               
LINC130  LA    R3,UNUSEINS         R3=A(UNUSED INSET LINES LIST)                
LINC140  CLI   0(R3),EOT           END OF LIST?                                 
         BE    LINC150                                                          
         MVI   ALTBDVAL,1          SET ALTER BLOCK VALUES BY ONE LINE           
         MVC   ALTBSTRT,0(R3)      START LINE                                   
         MVI   ALTBEND,X'FF'       END LINE                                     
         MVI   ALTBCTYP,0          NO COMMAND TYPE EXCEPTIONS                   
         MVI   ALTBSTAT,ALTBSUBQ   SUBTRACT AMOUNT                              
         BAS   RE,ALTBLKV          ALTER BLOCK LISTS                            
         LA    R3,1(R3)            BUMP TO NEXT UNUSED INSERT LINE NUM          
         B     LINC140                                                          
*                                                                               
LINC150  MVI   SC1IND,SC1IPROC                                                  
         GOTO1 ASTDCOM                                                          
         USING SCTABD,R3                                                        
         L     R3,BOSVALS2                                                      
LINC152  CLI   SCTABD,EOT                                                       
         BE    LINC156                                                          
         SR    RE,RE                                                            
         ICM   RE,1,SCTNUM                                                      
         BZ    LINC154                                                          
         BCTR  RE,0                DROP ONE LINE (ALREADY ADDED)                
         LTR   RE,RE                                                            
         BZ    LINC154                                                          
         STC   RE,ALTBDVAL         ALTER BLOCK VALUES BY #LINES                 
         MVC   ALTBSTRT,SCTLIN     START LINE                                   
         MVI   ALTBEND,X'FF'       END LINE                                     
         MVI   ALTBSTAT,0                                                       
         MVI   ALTBCTYP,0          NO COMMAND TYPE EXCEPTIONS                   
         BAS   RE,ALTBLKV          ALTER BLOCK LISTS                            
LINC154  XC    SCTABD(SCTABL),SCTABD                                            
         LA    R3,SCTABL(R3)                                                    
         B     LINC152                                                          
         DROP  R3                                                               
*                                                                               
LINC156  TM    OSVEBFLG,BLKCOUTQ   BLOCK COMMAND OUTSTANDING?                   
         BO    LINCX                                                            
         LA    R3,EBLTAB           R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
LINC160  CLI   EBLCTYPE,EOT        END OF TABLE?                                
         BE    LINC170                                                          
         SR    RF,RF                                                            
         ICM   RF,3,EBLFUNC                                                     
         BZ    *+10                                                             
         LA    RF,CLB0A(RF)        RF=A(EDIT FUNCTION)                          
         BASR  RE,RF               EXECUTE EDIT COMMAND                         
         LA    R3,EBLLNQ(R3)       BUMP TO NEXT TABLE ENTRY                     
         B     LINC160                                                          
         DROP  R3                                                               
*                                                                               
LINC170  MVI   PRMODE,PRCHANGQ                                                  
         BAS   RE,PARAREC          UPDATE PARA RECORD LINE SEQ CHANGED          
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
         TM    LENCTYPE,LENCOPYQ+LENMOVEQ                                       
         BZ    *+14                                                             
         MVC   OSVECMPA,OSVPARA    SAVE PARA NUMBER OF MOVE/COPY CMND           
         B     CHKC20                                                           
         TM    LENCTYPE,LENAFTQ+LENBEFQ+LENOVERQ AFTER/BEFORE/OVER?             
         BZ    CHKC20                                                           
         MVC   OSVETOPA,OSVPARA    SAVE PARA INDEX                              
         MVC   OSVETOPO,OSVPOFST   SAVE PARA OFFSET                             
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
         TM    EBLCTYPE,LENAFTQ+LENBEFQ+LENOVERQ AFTER/BEFORE/OVER              
         BZ    *+14                                                             
         CLC   OSVETOPA,OSVPARA    RELEVANT TO THIS PARAGRAPH?                  
         BNE   CHKC110                                                          
         TM    EBLCTYPE,LENMOVEQ+LENCOPYQ MOVE/COPY?                            
         BZ    *+14                                                             
         CLC   OSVECMPA,OSVPARA    RELEVANT TO THIS PARAGRAPH?                  
         BNE   CHKC110                                                          
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
         CLI   BLKLEN-BLKVALD(R1),0 LIST COMMAND IS BLOCK OR NUMERIC?           
         BNE   CHKC60                                                           
         TM    EBLCTYPE,LENMOVEQ+LENCOPYQ IF BLOCK MOVE OR COPY                 
         BZ    CHKC60                                                           
         TM    LENCTYPE,LENINSQ+LENREPQ   THEN INSERT/REPEAT EXCEPTED           
         BNZ   CHKC100                                                          
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
         OC    OSVEAFLN(L'OSVEAFLN+L'OSVEOVLN),OSVEAFLN                         
         BNZ   CHKCERRX                                                         
         OC    OSVEOVLN,OSVEOVLN                                                
         BZ    *+14                                                             
         OC    OSVEBFLN(L'OSVEBFLN+L'OSVEAFLN),OSVEBFLN                         
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
* CHECK SIZE/END OF PARAGRAPH NOT EXCEEEDED                           *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
CHKPSIZE NTR1                                                                   
         USING LEDTABD,R3                                                       
         TM    LENCTYPE,LENAFTQ+LENBEFQ+LENOVERQ BEFORE/AFTER CMND?             
         BZ    *+14                                                             
         MVC   OSVETOAC,OSVLACTV   SAVE NUM OF ACTV LINES IN 'TO' PARA          
         B     CHKP10                                                           
         TM    LENCTYPE,LENCOPYQ+LENMOVEQ MOVE/COPY COMMAND?                    
         BZ    CHKP40                                                           
         TM    OSVETOTY,LENAFTQ+LENBEFQ+LENOVERQ HAD AFTER/BEFORE/OVER?         
         BZ    CHKPX                                                            
CHKP10   TM    OSVEBFLG,BLKCOUTQ   ANY OUTSTANDING BLOCK COMMANDS?              
         BO    CHKPX                                                            
         USING BLKVALD,R1                                                       
         LA    R1,OSVECPYB         R1=A(COPY BLOCK LIST)                        
         CLI   BLKSTRT,0           NO START VALUE FOUND?                        
         BNE   CHKP20                                                           
         LA    R1,OSVEMVEB         R1=A(MOVE BLOCK LIST)                        
         CLI   BLKSTRT,0           NO START VALUE FOUND?                        
         BE    CHKPX                                                            
CHKP20   SR    RF,RF                                                            
         IC    RF,BLKEND           RF=(MOVE/COPY END LINE NUMBER)               
         SR    RE,RE                                                            
         USING SCTABD,R2                                                        
         L     R2,BOSVALS2         MAY NEED TO ADJUST BLOCK END                 
         LA    R0,SCTABN                                                        
CHKP22   CLI   SCTLIN,0                                                         
         BE    CHKP26                                                           
         CLC   SCTLIN,BLKSTRT      CHECK COMMENT IN THIS BLOCK                  
         BL    CHKP24                                                           
         CLC   SCTLIN,BLKEND                                                    
         BH    CHKP24                                                           
         IC    RE,SCTNUM                                                        
         AR    RF,RE               ADD STANDARD COMMENT LINES                   
         OI    LINEFLAG,LSCIBLKQ   SET STANDARD COMMENT IN BLOCK                
CHKP24   LA    R2,SCTABL(R2)                                                    
         BCT   R0,CHKP22                                                        
CHKP26   IC    RE,BLKSTRT          RE=(MOVE/COPY START LINE NUMBER)             
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
         TM    LINEFLAG,LSCIBLKQ   TEST STANDARD COMMENT IN BLOCK               
         BZ    *+12                                                             
         NI    LINEFLAG,X'FF'-LSCIBLKQ                                          
         B     CHKP30                                                           
         CLI   OSVEMVEB,0          MOVE COMMAND?                                
         BE    CHKP30                                                           
         TM    OSVETOTY,LENOVERQ   OVER CMND?                                   
         BO    CHKP30                                                           
         CLI   BLKLEN,1            NO REPEATS?                                  
         BH    CHKP30                                                           
         CLC   OSVETOPA,OSVECMPA   IF SAME PARAGRAPH THEN SIZE NOT CHAN         
         BE    CHKPX                                                            
CHKP30   SR    RE,RE                                                            
         IC    RE,BLKLEN           RE=(NUMBER OF REPEATS)                       
         STCM  RE,3,BCHALF                                                      
         MH    RF,BCHALF           RF=(NUMBER OF NEW LINES)                     
         SR    RE,RE                                                            
         IC    RE,OSVETOAC         RE=(NUM OF ACTVE LINES IN 'TO' PARA)         
         TM    OSVETOTY,LENOVERQ   OVER CMND?                                   
         BO    CHKP60                                                           
         B     CHKP50                                                           
CHKP40   TM    LENCTYPE,LENINSQ+LENREPQ INSERT OR REPEAT?                       
         BZ    CHKPX                                                            
         SR    RF,RF                                                            
         IC    RF,BLEN             RF=(NUMBER OF NEW LINES)                     
         USING SCTABD,R2                                                        
CHKP50   L     R2,BOSVALS2         MAY NEED TO ADJUST BLOCK END                 
         LA    R0,SCTABN                                                        
CHKP52   CLI   SCTLIN,0                                                         
         BE    CHKP56                                                           
         CLC   SCTLIN,BLKSTRT      CHECK COMMENT ON THIS LINE                   
         BE    CHKP54                                                           
         LA    R2,SCTABL(R2)                                                    
         BCT   R0,CHKP52                                                        
         B     CHKP56                                                           
CHKP54   SR    RE,RE                                                            
         IC    RE,SCTNUM           ADD STANDARD COMMENT LINES                   
         AR    RF,RE                                                            
         IC    RE,TOTINRP          R0=(NUM OF NEW LINS FROM PREV CMNDS)         
         AR    RF,RE                                                            
         STC   RF,TOTINRP                                                       
         IC    RE,OSVLACTV         RE=(NUM OF ACTIVE LINES IN PARA)             
         AR    RF,RE               RF=(NUM OF LINES IN RESULTING PARA)          
CHKP56   MVC   FVMSGNO,=AL2(AE$MPARA) MAXIMUM PARA SIZE EXCEEDED                
         CH    RF,=Y(MAXPLINQ)                                                  
         BH    CHKPERRX                                                         
         B     CHKPX                                                            
*                                                                               
CHKP60   MVC   FVMSGNO,=AL2(AE$EPEX) END OF PARAGRAPH EXCEEDED                  
         SR    R0,R0                                                            
         IC    R0,BLKSTRT          R0=('TO' LINE FOR OVER CMND)                 
         BCTR  R0,0                RF=(LAST LINE NUMBER OF BLOCK)               
         AR    RF,R0                                                            
         CR    RF,RE               BEYOND NUMBER OF ACTIVE LINES?               
         BH    CHKPERRX                                                         
*                                                                               
CHKPX    CR    RB,RB                                                            
         B     EXIT                                                             
CHKPERRX LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* DELETE LINES FROM A PARAGRAPH                                       *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
DEL      NTR1                                                                   
         USING EBLTABD,R3                                                       
         CLI   OSVLACTV,0                                                       
         BE    DELX                                                             
         SR    R1,R1                                                            
         ICM   R1,3,EBLDIS                                                      
         LA    R1,OSVALSD(R1)      R1=A(DELETE LIST VALUES)                     
         USING BLKVALD,R1                                                       
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NO OF ENTRIES IN LIST)               
DEL10    OC    BLKSTRT(BLKLNQ),BLKSTRT END OF LIST?                             
         BZ    DELX                                                             
         CLC   BLKEND,OSVLACTV     NUMERICAL DELETE PAST PARAGRAPH END?         
         BNH   DEL20                                                            
         MVC   BLKEND,OSVLACTV     SET DELETE END TO PARA END                   
         SR    RF,RF                                                            
         IC    RF,BLKEND           RECALCULATE BLOCK LENGTH                     
         SR    RE,RE                                                            
         IC    RE,BLKSTRT                                                       
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,BLKLEN                                                        
DEL20    MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         MVC   CMPTO,OSVLHIGH      MOVE TO OFFSET                               
         MVC   CMPFROM1,BLKSTRT    MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,BLKEND     MOVE FROM OFFSET 2 (END)                     
         LA    RF,OSVLLST          RF=A(LINE INDEX LIST)                        
         ST    RF,CMPAILST                                                      
         MVI   CMPILLN,L'OSVLLST   L'(LINE INDEX LIST)                          
         BAS   RE,CMI                                                           
         SR    RF,RF                                                            
         ICM   RF,1,BLKLEN         NUMERIC DELETE COMMAND?                      
         BNZ   DEL30                                                            
         SR    RE,RE                                                            
         IC    RE,BLKSTRT                                                       
         SR    RF,RF                                                            
         IC    RF,BLKEND                                                        
         SR    RF,RE                                                            
         LA    RF,1(RF)            RF=(NUMBER OF LINES TO BE DELETED)           
DEL30    SR    RE,RE                                                            
         IC    RE,OSVLACTV                                                      
         SR    RE,RF                                                            
         STC   RE,OSVLACTV         RESET NUMBER OF LINES IN PARA                
         STC   RF,ALTBDVAL                                                      
         MVC   ALTBSTRT,BLKEND                                                  
         MVI   ALTBEND,X'FF'                                                    
         MVI   ALTBSTAT,ALTBSUBQ                                                
         MVI   ALTBCTYP,0                                                       
         BAS   RE,ALTBLKV          ADJUST ANY OTHER LINE COMMANDS               
         OI    LINEFLAG,LCHANGEQ   SET LINE INDEX LIST CHANGED                  
         LA    R1,BLKLNQ(R1)       BUMP TO NEXT LIST VALUE                      
         BCT   R0,DEL10                                                         
         DROP  R1                                                               
*                                                                               
DELX     XC    OSVEDELB,OSVEDELB   CLEAR OUT DELETE LIST                        
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* COPY/MOVE LINES FROM ONE PARA TO ANOTHER OR WITHIN ONE PARAGRAPH    *         
* NTRY R3=A(LINE EDIT TABLE ENTRY FOR COPY/MOVE)                      *         
***********************************************************************         
         SPACE 1                                                                
CMLN     NTR1                                                                   
         USING EBLTABD,R3                                                       
         SR    R2,R2                                                            
         ICM   R2,3,EBLDIS                                                      
         LA    R2,OSVALSD(R2)      R2=A(COPY/MOVE VALUES LIST)                  
         USING BLKVALD,R2                                                       
         OC    BLKSTRT(BLKLNQ),BLKSTRT ANY COPY VALUES?                         
         BNZ   CML10                                                            
         OC    OSVETOLN,OSVETOLN       ANY 'TO' VALUES?                         
         BZ    CMLX                                                             
         B     *+14                                                             
*                                                                               
CML10    OC    OSVETOLN,OSVETOLN                                                
         BNZ   *+12                                                             
         OI    OSVEBFLG,BLKPENDQ COPY/MOVE IS PENDING                           
         B     CMLX                                                             
*                                                                               
         NI    OSVEBFLG,X'FF'-BLKPENDQ SWITCH OFF COPY/MOVE IS PENDING          
         XC    FRMLLST,FRMLLST     INIT 'FROM' LIST                             
         CLC   OSVPARA,OSVECMPA    CURRENT PARAGRAPH SAME AS FROM PARA?         
         BNE   CML60                                                            
         CLI   BLKLEN,0            NUMERICAL COMMAND?                           
         BE    CML20                                                            
         CLC   BLKEND,OSVLACTV     IF NUMERICAL COMMAND PAST PARA END           
         BNH   CML30                                                            
         MVC   BLKEND,OSVLACTV     THEN RESET WITH PARA END                     
CML20    SR    RE,RE               CALCULATE BLOCK LENGTH                       
         IC    RE,BLKEND                                                        
         SR    RF,RF                                                            
         IC    RF,BLKSTRT                                                       
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BLKLEN                                                        
CML30    SR    R1,R1                                                            
         IC    R1,BLKSTRT                                                       
         BCTR  R1,0                                                             
         LA    R1,OSVLLST(R1)      R1=A(LINE INDEX AT START OF BLOCK)           
         SR    RE,RE                                                            
         IC    RE,BLKLEN                                                        
         BCTR  RE,0                RE=X L'(MOVE/COPY BLOCK)                     
         EX    RE,*+4                                                           
         MVC   FRMLLST(0),0(R1)    SAVE 'FROM' LINE LIST                        
         TM    OSVECTYP,LENMOVEQ   MOVE COMMAND?                                
         BNO   CML40                                                            
         MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         MVC   CMPTO,OSVLHIGH      MOVE TO OFFSET                               
         MVC   CMPFROM1,BLKSTRT    MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,BLKEND     MOVE FROM OFFSET 2 (END)                     
         LA    RF,OSVLLST                                                       
         ST    RF,CMPAILST                                                      
         MVI   CMPILLN,L'OSVLLST                                                
         BAS   RE,CMI                                                           
         SR    R1,R1                                                            
         IC    R1,BLKLEN                                                        
         SR    RE,RE                                                            
         IC    RE,OSVLACTV                                                      
         SR    RE,R1                                                            
         STC   RE,OSVLACTV         ADJUST PARAGRAPH LENGTH                      
         OI    LINEFLAG,LCHANGEQ   SET LINE INDEX LIST CHANGED                  
         CLC   OSVPARA,OSVETOPA    CURRENT PARAGRAPH SAME AS 'TO' PARA?         
         BNE   CML50                                                            
         MVC   ALTBDVAL,BLKLEN                                                  
         MVC   ALTBSTRT,BLKSTRT                                                 
         MVI   ALTBEND,X'FF'                                                    
         MVI   ALTBCTYP,LENMOVEQ   COMMAND TYPE                                 
         MVI   ALTBSTAT,ALTBSUBQ                                                
         BAS   RE,ALTBLKV          ADJUST ANY OTHER LINE COMMANDS               
         B     CML100                                                           
CML40    CLC   OSVPARA,OSVETOPA    CURRENT PARAGRAPH SAME AS 'TO' PARA?         
         BE    CML100                                                           
CML50    MVI   PRMODE,PRCHANGQ                                                  
         BAS   RE,PARAREC          CHANGE PARAGRAPH RECORD IF NECESSARY         
         BE    CML90                                                            
         DC    H'0'                                                             
*                                  'FROM' PARA IS NOT THE CURRENT PARA          
CML60    LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,OSVECMPA   PARAGRAPH#                                   
         GOTO1 AIO,IO1+IOACCMST+IORDUP GET PARAGRAPH RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             RF=A('FROM' PARA RECORD)                     
*                                                                               
         LA    RF,PBRRFST          RF=A(FIRST ELEMENT)                          
         USING NDXELD,RF                                                        
CML70    CLI   NDXEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NDXEL,NDXELQ        INDEX ELEMENT?                               
         BE    CML80                                                            
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,NDXLN                                                         
         AR    RF,R0                                                            
         B     CML70                                                            
*                                                                               
CML80    CLI   BLKLEN,0            NUMERICAL COMMAND?                           
         BE    CML85                                                            
         CLC   BLKEND,NDXACTV      IF NUMERICAL COMMAND PAST PARA END           
         BNH   CML87                                                            
         MVC   BLKEND,NDXACTV      THEN RESET WITH PARA END                     
CML85    SR    RE,RE               CALCULATE BLOCK LENGTH                       
         IC    RE,BLKEND                                                        
         SR    R0,R0                                                            
         IC    R0,BLKSTRT                                                       
         SR    RE,R0                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BLKLEN                                                        
CML87    SR    R1,R1                                                            
         IC    R1,BLKSTRT                                                       
         BCTR  R1,0                                                             
         LA    R1,NDXINDX(R1)                                                   
         SR    RE,RE                                                            
         IC    RE,BLKLEN           RE=L'(INDEX ELEMENT)                         
         BCTR  RE,0                                                             
         EX    RE,*+4              RE=(X LENGTH OF INDEX LIST)                  
         MVC   FRMLLST(0),0(R1)    SAVE FROM LINE LIST                          
*                                                                               
         TM    OSVECTYP,LENMOVEQ                                                
         BNO   CML100                                                           
         MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         MVC   CMPTO,NDXHIGH       MOVE TO OFFSET                               
         MVC   CMPFROM1,BLKSTRT    MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,BLKEND     MOVE FROM OFFSET 2 (END)                     
         LA    RE,NDXINDX                                                       
         ST    RE,CMPAILST                                                      
         MVI   CMPILLN,(L'NDXINDX*200)                                          
         BAS   RE,CMI                                                           
         SR    R1,R1                                                            
         IC    R1,BLKLEN                                                        
         SR    RE,RE                                                            
         IC    RE,NDXACTV                                                       
         SR    RE,R1                                                            
         STC   RE,NDXACTV          ADJUST PARAGRAPH LENGTH                      
         DROP  RF                                                               
         GOTO1 AIO,IO1+IOACCMST+IOPUTREC PUT BACK A RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CML100                                                           
*                                                                               
CML90    CLC   OSVPARA,OSVETOPA    'TO' PARA IS CURRENT PARA?                   
         BE    CML100                                                           
         MVI   OSVSTLIN,X'FF'      SET HIGH VALUE SO START IS RE-SET            
         LA    R1,OSVETOPO         R1=(COPY/MOVE TO PARAGRAPH)                  
         ICM   RF,15,AGPARA                                                     
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
CML100   SR    RF,RF                                                            
         ICM   RF,1,BLKLEN                                                      
         STCM  RF,3,BCHALF         SAVE MOVE/COPY BLOCK LENGTH                  
*                                                                               
         LA    RF,OSVETOLN         RF=A('TO' VALUES LISTS)                      
         LA    R0,L'OSVETOLN/BLKLNQ R0=(NUMBER OF 'TO' LISTS)                   
         CLI   0(RF),0             EMPTY LIST?                                  
         BNE   *+14                                                             
         LA    RF,BLKLNQ(RF)       BUMP TO NEXT ONE                             
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
         MVC   TOLINE,BLKSTRT-BLKVALD(RF) SAVE 'TO' START LINE                  
         SR    RE,RE                                                            
         IC    RE,TOLINE                                                        
         CLI   OSVLACTV,0          NO LINES IN PARA?                            
         BE    CML110                                                           
         CLI   OSVETOTY,LENAFTQ    AFTER COMMAND?                               
         BNE   CML110                                                           
         LA    RE,1(RE)            BUMP UP START LINE                           
         STC   RE,TOLINE           RE=('TO' LINE NUMBER)                        
CML110   MVC   TOREPNO,BLKLEN-BLKVALD(RF) GET NUMBER OF REPEATS                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN         RF=(SCREEN START LINE)                       
         CR    RE,RF               IS 'TO' LINE ON CURRENT SCREEN?              
         BL    CML120                                                           
         LA    R1,MAXSLINQ-1(RF)   R1=(SCREEN END LINE)                         
         CR    RE,R1                                                            
         BH    CML120                                                           
*                                  'TO' LINE IS ON CURRENT SCREEN               
         SR    RE,RF                                                            
         MH    RE,=Y(SLLNQ)                                                     
         LA    RE,EDTLCM1H(RE)     RE=A(LINE COMMAND FIELD HEADER)              
         B     *+12                                                             
*                                                                               
CML120   STC   RE,OSVSTLIN                                                      
         LA    RE,EDTLCM1H         RE=A(FIRST COMMAND FIELD HEADER)             
         L     RF,AINP                                                          
         USING TIOBD,RF                                                         
         SR    RE,RA                                                            
         STCM  RE,3,TIOBCURD       OVERRIDE DEFAULT CURSOR CONTROL              
         MVI   TIOBCURI,0          SET CURSOR TO BEGINING OF FIELD              
         DROP  RF                                                               
*                                                                               
         LA    R3,FRMLLST          R3=A('FROM' LINE INDEX LIST)                 
         SR    R0,R0                                                            
         IC    R0,BLKLEN           R0=(LENGTH OF MOVE/COPY BLOCK)               
CML130   MVC   BCBYTE1,TOLINE      'TO' LINE NUMBER                             
         LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,OSVECMPA   PARAGRAPH#                                   
         MVC   PBRKLINE,0(R3)      LINE#                                        
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE NUMBER RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             RF=A(LINE RECORD)                            
         LA    RF,PBRRFST          RF=A(FIRST ELEMENT)                          
         USING FFTELD,RF                                                        
CML140   CLI   FFTEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    CML160                                                           
*                                                                               
CML150   SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,FFTLN                                                         
         AR    RF,RE                                                            
         B     CML140                                                           
*                                                                               
CML160   CLI   FFTTYPE,FFTTBLIN    PRODUCTION BILL DETAIL LINE?                 
         BNE   CML150                                                           
         XC    WORKTTXT,WORKTTXT   INIT WORK AREA                               
         SR    RE,RE                                                            
         ICM   RE,1,FFTDLEN        RE=L'(TEXT LINE)                             
         BZ    CMLX                                                             
         STCM  RE,1,WORKTLN                                                     
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   WORKTTXT(0),FFTDATA PUT LINE IN WORK AREA                        
         DROP  RF                                                               
         MVI   LRMODE,LRADDQ       ADD LINE RECORD                              
         CLI   OSVETOTY,LENOVERQ   OVER WRITE COMMAND?                          
         BNE   *+8                                                              
         MVI   LRMODE,LROVRWRT     OVER WRITE RECORD                            
         MVC   LRPARA,OSVPARA      PARA INDEX                                   
         MVC   LRLINELN,WORKTLN    LENGTH OF LINE                               
         MVI   LRCOLUMN,1          COLUMN START                                 
         LA    RE,WORKTTXT         RE=A(LINE)                                   
         STCM  RE,15,LRALINE                                                    
         SR    R1,R1                                                            
         IC    R1,TOREPNO           R1=(NUMBER OF REPEATS)                      
CML170   MVC   LRLINE,BCBYTE1       LINE NUMBER                                 
         ICM   RF,15,ALINEREC       CHANGE/ADD LINE RECORD                      
         BASR  RE,RF                                                            
         SR    RE,RE                                                            
         IC    RE,BCBYTE1           RE=(LINE NUMBER)                            
         SR    RF,RF                                                            
         IC    RF,BLKLEN            RF=(MOVE/COPY BLOCK LENGTH)                 
         CLI   OSVETOTY,LENOVERQ    OVER WRITE COMMAND?                         
         BE    *+10                                                             
         SR    RF,R0                                                            
         LA    RF,1(RF)                                                         
         AR    RE,RF                                                            
         STC   RE,BCBYTE1           RE=(NEXT LINE NUMBER FOR ADD/CHA)           
         BCT   R1,CML170            REPEAT LINE                                 
         LA    R3,1(R3)             R3=A(NEXT 'FROM' LINE INDEX)                
         SR    RE,RE                                                            
         IC    RE,TOLINE                                                        
         LA    RE,1(RE)                                                         
         STC   RE,TOLINE            BUMP 'TO' LINE NUMBER                       
         BCT   R0,CML130            REPEAT FOR NUMBER OF LINES IN BLOCK         
*                                                                               
         CLI   OSVETOTY,LENOVERQ   OVER WRITE?                                  
         BE    CML180                                                           
         SR    RF,RF                                                            
         IC    RF,BLKLEN                                                        
         STCM  RF,3,BCHALF                                                      
         SR    RE,RE                                                            
         IC    RE,TOREPNO                                                       
         MH    RE,BCHALF                                                        
         STC   RE,ALTBDVAL                                                      
         MVC   ALTBSTRT,TOLINE                                                  
         MVI   ALTBEND,X'FF'                                                    
         MVI   ALTBSTAT,0                                                       
         MVI   ALTBCTYP,0                                                       
         BAS   RE,ALTBLKV           ADJUST ALL OTHER LINE COMMANDS              
CML180   XC    BLKSTRT(BLKLNQ),BLKSTRT CLEAR COPY/MOVE BLOCK LISTS              
         XC    OSVETOLN,OSVETOLN   CLEAR 'TO' BLOCK LISTS                       
CMLX     B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* REPEAT LINES WITHIN A PARAGRAPH                                     *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
REPT     NTR1                                                                   
         USING EBLTABD,R3                                                       
         XC    WORKTTXT,WORKTTXT                                                
         SR    R2,R2                                                            
         ICM   R2,3,EBLDIS                                                      
         LA    R2,OSVALSD(R2)      R2=A(REPEAT VALUES LIST)                     
         USING BLKVALD,R2                                                       
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NO OF ENTRIES IN BLOCK LIST)         
REPT10   OC    BLKSTRT(BLKLNQ),BLKSTRT ANY REPEAT VALUES?                       
         BZ    REPTX                                                            
         LA    RF,IOKEY            RF=A(LINE RECORD KEY)                        
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,OSVPARA    PARAGRAPH#                                   
         SR    RE,RE                                                            
         IC    RE,BLKSTRT          OFFSET TO LINE INDEX                         
         BCTR  RE,0                                                             
         LA    RE,OSVLLST(RE)      RE=A(CORRESPONDING INDEX FOR LINE)           
         MVC   PBRKLINE,0(RE)      LINE#                                        
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE NUMBER RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             RF=A(LINE NUMBER RECORD)                     
         LA    RF,PBRRFST          RF=A(FIRST ELEMENT)                          
         USING FFTELD,RF                                                        
REPT20   CLI   FFTEL,EOR           END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    REPT40                                                           
*                                                                               
REPT30   SR    RE,RE               BUMP TO NEXT LINE NUMBER                     
         IC    RE,FFTLN                                                         
         AR    RF,RE                                                            
         B     REPT20                                                           
*                                                                               
REPT40   CLI   FFTTYPE,FFTTBLIN    PRODUCTION BILL DETAIL LINE?                 
         BNE   REPT30                                                           
         SR    RE,RE                                                            
         ICM   RE,1,FFTDLEN                                                     
         BZ    REPTX                                                            
         STCM  RE,1,WORKTLN                                                     
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   WORKTTXT(0),FFTDATA                                              
         DROP  RF                                                               
         MVI   LRMODE,LRADDQ       ADD LINE                                     
         MVC   LRLINE,BLKSTRT      LINE NUMBER                                  
         MVC   LRPARA,OSVPARA      PARA NUMBER                                  
         MVC   LRLINELN,WORKTLN    LINE LENGTH                                  
         MVI   LRCOLUMN,1          COLUMN                                       
         LA    RE,WORKTTXT         RE=A(LINE)                                   
         STCM  RE,15,LRALINE                                                    
         SR    R1,R1                                                            
         IC    R1,BLKLEN                                                        
         ICM   RF,15,ALINEREC                                                   
         BASR  RE,RF                                                            
         BCT   R1,*-2                                                           
*                                                                               
         MVC   ALTBDVAL,BLKLEN                                                  
         MVC   ALTBSTRT,BLKSTRT                                                 
         MVI   ALTBEND,X'FF'                                                    
         MVI   ALTBSTAT,0                                                       
         MVI   ALTBCTYP,0                                                       
         BAS   RE,ALTBLKV          ADJUST OTHER LINE COMMANDS                   
*                                                                               
         LA    R2,BLKLNQ(R2)       BUMP TO NEXT REPEAT VALUE                    
         BCT   R0,REPT10                                                        
*                                                                               
REPTX    XC    OSVEREPB,OSVEREPB   CLEAR REPEAT VALUE LIST                      
         B     EXIT                                                             
         DROP  R2,R3                                                            
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
         TM    EBLCTYPE,LENMOVEQ+LENCOPYQ MOVE/COPY COMMAND                     
         BZ    INBV20                                                           
         CLC   OSVPARA,OSVECMPA    SAME PARA AS DISPLAYED?                      
         BNE   INBV100                                                          
         B     INBV30                                                           
INBV20   TM    EBLCTYPE,LENAFTQ+LENBEFQ+LENOVERQ AFTER/BEFORE/OVER CMD?         
         BZ    INBV30                                                           
         CLC   OSVPARA,OSVETOPA    SAME PARA AS DISPLAYED?                      
         BNE   INBV100                                                          
*                                                                               
INBV30   SR    RF,RF                                                            
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
         TM    OSVECTYP,LENCOPYQ+LENMOVEQ NO MOVE/COPY COMMANDS?                
         BNZ   *+8                                                              
         MVI   OSVECMPA,0          INIT MOVE/COPY PARAGRAPH NUMBER              
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
         XC    FSVAL,FSVAL         INIT FREE FORM TEXT STRING VALS              
         MVC   CMNDLNH,BASOPTH     INIT COMMAND LINE                            
         XC    CMNDLN,CMNDLN       INIT COMMAND LINE                            
         SR    RF,RF                                                            
         IC    RF,FLDILEN          R0=L'(COMMAND LINE INPUT)                    
         LR    R0,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   CMNDLN(0),BASOPT    GET COMMAND LINE                             
         SR    RF,RF                                                            
CMND10   LA    RE,CMNDLN(RF)       RE=A(NEXT INPUT CHAR)                        
         CLI   0(RE),C''''         SINGLE DINK?                                 
         BE    CMND20                                                           
         CLI   0(RE),C'"'          DOUBLE DINK?                                 
         BNE   CMND80                                                           
CMND20   CLI   FSVALST1,0          START DINK 1 UNUSED?                         
         BNE   CMND30                                                           
         STC   RF,FSVALST1         SET START DINK 1                             
         MVC   FSVALTP1,0(RE)      SAVE DINK TYPE                               
         B     CMND80                                                           
CMND30   CLC   FSVALTP1,0(RE)      MATCH ON DINK TYPE?                          
         BE    CMND40                                                           
         CLI   FSVALEN1,0          END DINK 1 UNUSED?                           
         BE    CMND80                                                           
         B     CMND50                                                           
CMND40   CLI   FSVALEN1,0          END DINK 1 UNUSED?                           
         BNE   CMND50                                                           
         STC   RF,FSVALEN1         SET END DINK 1                               
         B     CMND80                                                           
CMND50   CLI   FSVALST2,0          START DINK 2 UNUSED?                         
         BNE   CMND60                                                           
         STC   RF,FSVALST2         SET DINK 2                                   
         MVC   FSVALTP2,0(RE)      SAVE DINK TYPE                               
         B     CMND80                                                           
CMND60   CLC   FSVALTP2,0(RE)      MATCH ON DINK 2?                             
         BNE   CMND80                                                           
         CLI   FSVALEN2,0          END DINK 2 UNUSED?                           
         BNE   CMND80              EXTRA DINK                                   
         STC   RF,FSVALEN2                                                      
*                                                                               
CMND80   LA    RF,1(RF)            BUMP TO NEXT INPUT CHARACTER                 
         BCT   R0,CMND10                                                        
*                                                                               
         CLI   FSVALEN1,0          ENSURE COMPLETE PAIR OF DINKS                
         BNE   *+14                                                             
         XC    FSVALST1(L'FSVALST1+L'FSVALEN1+L'FSVALTP1),FSVALST1              
         B     CMND90                                                           
         CLI   FSVALEN2,0          ENSURE COMPLETE PAIR OF DINKS                
         BNE   CMND90                                                           
         XC    FSVALST2(L'FSVALST2+L'FSVALEN2+L'FSVALTP2),FSVALST2              
*                                                                               
CMND90   SR    R0,R0                                                            
         IC    R0,FLDILEN          R0=L'(COMMAND LINE INPUT)                    
         SR    RF,RF                                                            
CMND100  LA    RE,CMNDLN(RF)       RE=(NEXT INPUT CHAR)                         
         CLI   0(RE),C' '          BLANK                                        
         BH    CMND120                                                          
         CLI   1(RE),C' '          FOLLOWED BY NON BLANK?                       
         BNH   CMND120                                                          
         OC    FSVALST1(L'FSVALST1+L'FSVALEN1),FSVALST1                         
         BZ    CMND110                                                          
         CLM   RF,1,FSVALST1       IF SURROUNDED BY DINKS IGNORE                
         BL    CMND110                                                          
         CLM   RF,1,FSVALEN1                                                    
         BNH   CMND120                                                          
         OC    FSVALST2(L'FSVALST2+L'FSVALEN2),FSVALST2                         
         BZ    CMND110                                                          
         CLM   RF,1,FSVALST2                                                    
         BL    CMND110                                                          
         CLM   RF,1,FSVALEN2                                                    
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
         MVI   CMNFFNO,0           INITIALISE FREE FORM TXT STRNG COUNT         
         MVI   CMNNUMNO,0          INITIALISE NUMERICAL INPUT COUNT             
         CLI   OSVCMND,CMNCHGQ     CHANGE COMMAND ENTERED?                      
         BE    *+12                                                             
         CLI   OSVCMND,CMNFNDQ     FIND COMMAND ENTERED?                        
         BNE   *+8                                                              
         MVI   OSVFCCOL,0          INITIALISE COLUMN PARAMETER                  
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
         LA    R4,CLB0A(R4)        R4=A(PARAMETER TABLE FOR COMMAND)            
         USING CPARTABD,R4                                                      
CMND170  CLI   CPARTYPE,EOT        END OF PARAMETER TABLE?                      
         BNE   CMND180             PARAMETER NOT RECOGNISED                     
         TM    CMNDFLAG,CMNDDUPQ   DUPLICATE PARAMETER?                         
         BNO   CMNDERRX                                                         
         MVC   FVMSGNO,=AL2(AE$DUPRM) DUPLICATE PARAMETER                       
         MVC   FVINDX,SVFVINDX                                                  
         B     CMNDERRX                                                         
CMND180  TM    CPARSTAT,CPARSDFQ   DRAFT BILL ONLY PARAMETER?                   
         BZ    *+12                                                             
         CLI   OSVPIND,PBRPIDFT    TEST BILL IS LIVE                            
         BNE   CMND260                                                          
         TM    CPARSTAT,CPARSFFQ   FREE FORM TEXT PARAMETER?                    
         BNO   CMND220                                                          
         CLC   CMNFFNO,CPARTYPE    ALREADY SET THIS PARAMETER?                  
         BNL   CMND260                                                          
         MVC   CMNFFNO,CPARTYPE    UPDATE FREE FORM TXT COUNT                   
         SR    RF,RF                                                            
         ICM   RF,3,CPARWORD                                                    
         LA    RF,OSVALS(RF)       RF=A(FREE FORM TEXT AREA)                    
         SR    RE,RE                                                            
         IC    RE,SC1STLEN                                                      
         SH    RE,=H'1'            RE=X L'(FREE FORM TEXT                       
         BNZ   CMND190                                                          
         CLI   SC1STFLD,C'*'       USE PREVIOUS TEXT STRING?                    
         BNE   CMND190                                                          
         MVC   FVMSGNO,=AL2(AE$ENTXT) ENTER TEXT STRING                         
         CLI   0(RF),0                                                          
         BE    CMNDERRX                                                         
         B     CMND300                                                          
CMND190  CLI   SC1STFLD,C''''      START WITH DINKS?                            
         BE    *+12                                                             
         CLI   SC1STFLD,C'"'                                                    
         BNE   CMND200                                                          
         MVC   BCBYTE1,SC1STFLD    SAVE DINK                                    
         LA    R1,SC1STFLD(RE)                                                  
         CLC   BCBYTE1,0(R1)       IS LAST CHAR OF PARAM MATCHING DINK?         
         BNE   CMND200                                                          
         LA    R1,SC1STFLD+1       R1=A(FREE FORM TEXT IN UPPER CASE)           
         CLI   CPARTYPE,CPARFFUQ   UPPER CASE TEXT TYPE?                        
         BE    *+8                                                              
         LA    R1,BCWORK+1         R1=A(FREE FORM TEXT IN ORIG CASE)            
         SH    RE,=H'2'            AND REDUCE LENGTH ACCORDINGLY                
         B     CMND210                                                          
CMND200  LA    R1,SC1STFLD         R1=A(FREE FORM TEXT)                         
         CLI   CPARTYPE,CPARFFUQ   UPPER CASE TEXT TYPE?                        
         BE    *+8                                                              
         LA    R1,BCWORK           R1=A(FREE FORM TEXT IN ORIG CASE)            
CMND210  EX    RE,*+4                                                           
         MVC   1(0,RF),0(R1)       SAVE FREE FORM TEXT                          
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)            SAVE LENGTH OF FREE FORM TEXT                
         B     CMND300                                                          
CMND220  TM    CPARSTAT,CPARSNMQ   NUMERIC PARAMETER?                           
         BNO   CMND230                                                          
         TM    SC1STVAL,SCNUMQ     TEST INPUT IS NUMERIC                        
         BNO   CMND260                                                          
         CLC   CMNNUMNO,CPARTYPE   ALREADY SET THIS PARAMETER?                  
         BNL   CMND260                                                          
         MVC   CMNNUMNO,CPARTYPE   UPDATE NUMERICAL INPUT COUNT                 
         SR    RF,RF                                                            
         ICM   RF,3,CPARWORD                                                    
         LA    RF,OSVALS(RF)       RF=A(NUMERICAL INPUT SAVE AREA)              
         SR    RE,RE                                                            
         IC    RE,SC1STLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  BCDUB,SC1STFLD(0)                                                
         CVB   RE,BCDUB                                                         
         STC   RE,0(RF)            SAVE NUMERICAL INPUT                         
         B     CMND300                                                          
CMND230  SR    RF,RF                                                            
         ICM   RF,3,CPARWORD                                                    
         LA    RF,TWAD(RF)         RF=A(PARAMETER WORD)                         
         CLI   OSVCMND,CMNCHGQ     CHANGE COMMAND?                              
         BE    *+12                                                             
         CLI   OSVCMND,CMNFNDQ     FIND COMMAND?                                
         BNE   CMND240                                                          
         LA    RE,7                SET RE TO FILL LENGTH OF PARAM WORD          
         LA    R1,0(RE,RF)                                                      
         CLI   0(RE),C' '                                                       
         BH    CMND250                                                          
         BCT   RE,*-12                                                          
         DC    H'0'                                                             
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
         SR    RF,RF                                                            
         ICM   RF,3,CMNPFLAG                                                    
         LA    RF,OSVALS(RF)       RF=A(PARAMETER FLAG)                         
         LA    RE,CPARINCM         RE=A(LIST OF INCOMPATIBLE PARAMS)            
CMND270  MVC   BCBYTE1,0(RF)                                                    
         CLI   0(RE),EOT           END OF LIST?                                 
         BE    CMND280                                                          
         MVC   FVMSGNO,=AL2(AE$INPAR) INCOMPATIBLE PARAMETERS                   
         NC    BCBYTE1,0(RE)                                                    
         BNZ   CMNDERRX                                                         
         LA    RE,1(RE)            BUMP TO NEXT ENTRY                           
         B     CMND270                                                          
CMND280  NC    BCBYTE1,CPARTYPE                                                 
         BZ    CMND290             TEST FOR DUPLICATE                           
         OI    CMNDFLAG,CMNDDUPQ   SET POSSIBLE DUPLICATE FLAG ON               
         MVC   SVFVINDX,FVINDX                                                  
         B     CMND260                                                          
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
* DEAL WITH BOTH VERTICAL AND HORIZONTAL SCROLLING                    *         
***********************************************************************         
         SPACE 1                                                                
SCROLL   NTR1                                                                   
         TM    BCINDS1,BCIANYPF    HAS A PFKEY BEEN PRESSED?                    
         BO    SCRL20                                                           
         SR    R3,R3                                                            
         IC    R3,OSVSTLIN         R3=(NUMBER OF START LINE ON SCREEN)          
         LR    RF,R3                                                            
         LA    RF,MAXSLINQ-1(RF)   RF=(NUMBER OF LAST LINE ON SCREEN)           
         CLM   RF,1,OSVEINSB       INSERT LINE?                                 
         BE    SCRL10                                                           
         CLM   RF,1,OSVLACTV       END OF PARA BEFORE LAST SCREEN LINE?         
         BH    SCRLX                                                            
         L     RE,AINP             RE=A(TIOB)                                   
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS-TIOBD(RE) R1=(ABSOLUTE CURSOR ADDRESS)             
         CH    R1,=Y(80*22)        CURSOR ON LAST TEXT LINE?                    
         BL    SCRLX                                                            
         CH    R1,=Y(80*23)                                                     
         BNL   SCRLX                                                            
*                                                                               
SCRL10   LA    R3,1(R3)            SCROLL DOWN BY ONE LINE                      
         STCM  R3,1,OSVSTLIN                                                    
         B     SCRLX                                                            
*                                                                               
SCRL20   CLI   BCPFKEY,PFKBKWDQ    BACKWARDS?                                   
         BE    SCRL50                                                           
         CLI   BCPFKEY,PFKFRWDQ    FORWARDS?                                    
         BE    SCRL50                                                           
         CLI   BCPFKEY,PFKLEFTQ    LEFT?                                        
         BE    SCRL50                                                           
         CLI   BCPFKEY,PFKRGHTQ    RIGHT?                                       
         BE    SCRL50                                                           
*                                                                               
         CLI   BCPFKEY,PFKNEXTP    NEXT PARAGRAPH?                              
         BNE   SCRL30                                                           
         TM    OSVEBFLG,BLKCOUTQ   NOT ALLOWED IF OUTSTANDING BLOCK CMD         
         BO    SCRLX                                                            
         CLC   OSVPOFST,OSVPACTV   LAST PARAGRAPH?                              
         BE    SCRLX                                                            
         SR    RF,RF                                                            
         IC    RF,OSVPOFST                                                      
         LA    RF,1(RF)            BUMP TO NEXT PARAGRAPH                       
         B     SCRL40                                                           
*                                                                               
SCRL30   CLI   BCPFKEY,PFKPREVP    PREVIOUS PARAGRAPH?                          
         BNE   SCRLX                                                            
         TM    OSVEBFLG,BLKCOUTQ   NOT ALLOWED IF OUTSTANDING BLOCK CMD         
         BO    SCRLX                                                            
         CLI   OSVPOFST,1          FIRST PARAGRAPH?                             
         BE    SCRLX                                                            
         SR    RF,RF                                                            
         IC    RF,OSVPOFST                                                      
         BCTR  RF,0                BUMP TO PREVIOUS PARAGRAPH                   
SCRL40   STC   RF,OSVPOFST                                                      
         BCTR  RF,0                                                             
         LA    RF,OSVPLST(RF)                                                   
         MVC   OSVPARA,0(RF)       GET PARAGRAPH INDEX                          
         LA    R3,1                                                             
         LA    R1,OSVPOFST         R1=A(PARAGRAPH# REQUIRED)                    
         ICM   RF,15,AGPARA                                                     
         BASR  RE,RF                                                            
         BE    SCRL160                                                          
         DC    H'0'                                                             
*                                                                               
SCRL50   LA    RF,MAXSLINQ         RF=(MAX NUM OF TXT LINES ON SCREEN)          
         TM    BCSCROLL,PFKIHORZ   HORIZONTAL SCROLLING?                        
         BNO   *+8                                                              
         LA    RF,L'EDTTXT1        RF=(NUMBER OF TEXT COLS ON SCREEN)           
         TM    BCSCRNUM,PFKIPAGE   PAGE?                                        
         BO    SCRL80                                                           
         SRL   RF,1                                                             
         TM    BCSCRNUM,PFKIHALF   HALF?                                        
         BO    SCRL80                                                           
         TM    BCSCRNUM,PFKIMAXN   MAX?                                         
         BNO   SCRL70                                                           
         LA    R3,1                R3=(FIRST LINE/PARAGRAPH)                    
         TM    BCSCROLL,PFKIHORZ   HORIZONTAL SCROLLING?                        
         BO    SCRL60                                                           
         TM    BCSCROLL,PFKIUPDN   UP TO TOP OF TEXT?                           
         BO    SCRL160                                                          
         B     SCRL140                                                          
SCRL60   TM    BCSCROLL,PFKIUPDN   UP TO LEFT MOST COLUMN?                      
         BO    SCRL190                                                          
         B     SCRL170                                                          
*                                                                               
SCRL70   SR    RF,RF                                                            
         ICM   RF,1,BCSCRNUM       SCROLL AMOUNT?                               
         BZ    SCRLX               PFKEY NOT A SCROLL PFKEY                     
*                                                                               
SCRL80   SR    R3,R3                                                            
         TM    BCSCROLL,PFKIHORZ   HORIZONTAL SCROLLING?                        
         BO    SCRL120                                                          
         IC    R3,OSVSTLIN         R3=(START LINE NUM IN CURRENT DISP)          
         TM    BCSCROLL,PFKIUPDN   UP?                                          
         BO    SCRL110                                                          
         AR    R3,RF               R3=(NEXT START LINE NUMBER)                  
         CLC   OSVPOFST,OSVPACTV   LAST PARAGRAPH?                              
         BNE   SCRL90                                                           
         LA    RF,MAXSLINQ-1(R3)                                                
         CLM   RF,1,OSVLACTV                                                    
         BH    SCRL100                                                          
         B     SCRL160                                                          
*                                                                               
SCRL90   CLM   R3,1,OSVLACTV       BEYOND THE LAST ACTIVE LINE?                 
         BNH   SCRL160                                                          
         TM    OSVEBFLG,BLKCOUTQ                                                
         BO    SCRLX                                                            
         SR    RF,RF                                                            
         IC    RF,OSVPOFST                                                      
         LA    RF,1(RF)            BUMP TO NEXT PARAGRAPH                       
         STC   RF,OSVPOFST                                                      
         BCTR  RF,0                                                             
         LA    RF,OSVPLST(RF)      RF=A(NEXT PARAGRAPH INDEX)                   
         MVC   OSVPARA,0(RF)                                                    
         LA    R3,1                                                             
         LA    R1,OSVPOFST         R1=A(PARAGRAPH# REQUIRED)                    
         ICM   RF,15,AGPARA                                                     
         BASR  RE,RF                                                            
         BE    SCRL160                                                          
         DC    H'0'                                                             
*                                                                               
SCRL100  LR    R1,R3                                                            
         LA    R1,MAXSLINQ(R1)     R1=(NEXT LAST LINE NUMBER)                   
         CLM   R1,1,OSVLACTV       BEYOND THE LAST ACTIVE LINE?                 
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
         TM    OSVEBFLG,BLKCOUTQ                                                
         BO    SCRL150                                                          
         SR    RF,RF                                                            
         IC    RF,OSVPOFST                                                      
         BCTR  RF,0                                                             
         STC   RF,OSVPOFST                                                      
         BCTR  RF,0                                                             
         LA    RF,OSVPLST(RF)                                                   
         MVC   OSVPARA,0(RF)                                                    
         LA    R1,OSVPOFST         R1=A(PARAGRAPH# REQUIRED)                    
         ICM   RF,15,AGPARA                                                     
         BASR  RE,RF                                                            
         SR    R3,R3                                                            
         IC    R3,OSVLACTV                                                      
         SH    R3,=Y(MAXSLINQ-2)                                                
         BNP   SCRL150                                                          
         B     SCRL160                                                          
*                                                                               
SCRL120  IC    R3,OSVSTCOL         R3=(1ST COLUMN NUM IN CURRENT DISP)          
         TM    BCSCROLL,PFKIUPDN   UP?                                          
         BO    SCRL130                                                          
         AR    R3,RF               R3=(NEXT LEFTMOST COLUMN)                    
         LR    R1,R3                                                            
         LA    R1,L'EDTTXT1(R1)    R1=(NEXT RIGHTMOST COLUMN)                   
         CLM   R1,1,OSVREPWD       BEYOND THE LAST POSSIBLE COLUMN?             
         BH    SCRL170                                                          
         B     SCRL190                                                          
*                                                                               
SCRL130  SR    R3,RF               R3=(NEXT LEFTMOST COLUMN)                    
         B     SCRL180                                                          
*                                                                               
SCRL140  SR    R3,R3                                                            
         IC    R3,OSVLACTV         NUMBER OF LINES IN PARAGRAPH                 
         SH    R3,=Y(MAXSLINQ-2)                                                
         LTR   R3,R3               BEYOND START OF DISPLAY?                     
         BP    *+8                                                              
SCRL150  LA    R3,1                                                             
SCRL160  STC   R3,OSVSTLIN         SAVE NEXT START LINE NUMBER                  
         B     SCRL200                                                          
*                                                                               
SCRL170  SR    R3,R3                                                            
         ICM   R3,1,OSVREPWD                                                    
         SH    R3,=Y(L'EDTTXT1-1)                                               
         BP    *+8                                                              
         LA    R3,1                                                             
         B     SCRL190                                                          
SCRL180  LTR   R3,R3               BEYOND START OF DISPLAY?                     
         BP    *+8                                                              
         LA    R3,1                                                             
SCRL190  STC   R3,OSVSTCOL         SAVE NEXT START COLUMN NUMBER                
*                                                                               
SCRL200  LA    R2,EDTTXT1H         R2=A(FIRST TXT LINE FIELD HEADER)            
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
* CHANGE/ADD A LINE RECORD                                            *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* LRECPARM DS    0X                COPY/MOVE ROUTINE PARMS            *         
* LRMODE   DS    X                                                    *         
* LRADDQ   EQU   X'80'             ADD A RECORD (OR CHANGE UNACTIVE)  *         
* LRCHANGE EQU   X'40'             CHANGE AN EXISTING RECORD (ACTIVE) *         
* LROVRWRT EQU   X'20'             OVER WRITE LINE (ACTIVE)           *         
* LRMERGE  EQU   X'10'             MERGE LINE, IN WHICH CASE          *         
*                                  R3=A(STANDARD COMMENT TABLE ENTRY) *         
* LRCOLUMN DS    X                 COLUMN OFFSET                      *         
* LRLINE   DS    X                 LOGICAL LINE NUMBER                *         
* LRPARA   DS    X                 PARAGRAPH OF LINE                  *         
* LRLINELN DS    X                 L'(LINE TO BE ADDED/CHANGED)       *         
* LRALINE  DS    A                 A(LINE TO BE ADDED/CHANGED)        *         
***********************************************************************         
         SPACE 1                                                                
LINEREC  NTR1                                                                   
         L     RB,ABASE1                                                        
         L     R8,ABASE2                                                        
         L     R7,ABASE3                                                        
         L     R6,ABASE4                                                        
         MVI   RECMODE,RECACTQ     SET ACTIVE RECORD MODE                       
         CLI   LRMODE,LRMERGE      ACTIVE LINE RECORD TO BE MERGED?             
         BE    LINER20                                                          
         CLI   LRMODE,LRCHANGE     ACTIVE LINE RECORD TO BE CHANGED?            
         BE    LINER20                                                          
         CLI   LRMODE,LROVRWRT     ACTIVE LINE REC TO BE OVER-WRITTEN?          
         BE    LINER20                                                          
*                                  ASSUME NEW LINE                              
         SR    RF,RF                                                            
         IC    RF,OSVLACTV         RF=(NUMBER OF ACTIVE LINES IN PARA)          
         LA    RF,1(RF)            INCREMENT                                    
         CLC   OSVLHIGH,OSVLACTV   ARE ALL EXISTING LINE RECS ACTIVE?           
         STC   RF,OSVLACTV         UPDATE ACTIVE LINE NUMBER                    
         BE    *+12                                                             
         MVI   RECMODE,RECUACTQ    UNACTIVE LINE RECORD TO BE USED              
         B     LINER10                                                          
         STC   RF,OSVLHIGH         UPDATE NUMBER OF LINE RECORDS                
         LA    RE,OSVLLST(RF)                                                   
         BCTR  RE,0                RE=A(CORRESPONDING INDEX FOR LINE)           
         STC   RF,0(RE)            ATTATCH AT END OF LIST                       
         MVI   RECMODE,RECADDQ     LINE RECORD REQUIRES ADDING                  
LINER10  MVI   CMPMODE,CMPMOVEQ    MOVE COMMAND                                 
         OI    CMPMODE,CMPBEFRQ    BEFORE OFFSET                                
         MVC   CMPTO,LRLINE        OFFSET FOR LINE NUMBER                       
         MVC   CMPFROM1,OSVLACTV   START OF BLOCK                               
         MVC   CMPFROM2,OSVLACTV   END OF BLOCK                                 
         LA    RF,OSVLLST                                                       
         STCM  RF,15,CMPAILST      RF=A(LINE INDEX LIST)                        
         LA    RF,L'OSVLLST                                                     
         STCM  RF,1,CMPILLN        RF=L'(LINE INDEX LIST)                       
         BAS   RE,CMI              MOVE NEW LINE TO CORRECT POSITION            
         OI    LINEFLAG,LCHANGEQ   INDICATE LINE INDEX LIST CHANGED             
*                                                                               
LINER20  LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION LINE RECORD)         
         USING PBRRECD,RF                                                       
         CLI   RECMODE,RECADDQ     ADDING A NEW RECORD                          
         BNE   *+8                                                              
         L     RF,AIO1             RF=A(IOAREA FOR NEW LINE RECORD)             
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,LRPARA     PARAGRAPH#                                   
         CLI   PBRKPARA,0          TRAP EMPTY ELEMENT BUG                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,LRLINE           OFFSET TO LINE INDEX                         
         BCTR  RE,0                                                             
         LA    RE,OSVLLST(RE)      RE=A(CORRESPONDING INDEX FOR LINE)           
         MVC   PBRKLINE,0(RE)      LINE#                                        
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM INIT ELEMENT STORAGE                 
         CLI   RECMODE,RECADDQ     ADDING A RECORD?                             
         BE    LINER80                                                          
         GOTO1 AIO,IO1+IOACCMST+IORDUP GET TEXT LINE RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         LA    RF,PBRRFST          RF=A(FIRST ELEMENT)                          
         USING FFTELD,RF                                                        
LINER30  CLI   FFTEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    LINER50                                                          
*                                                                               
LINER40  SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,FFTLN                                                         
         AR    RF,RE                                                            
         B     LINER30                                                          
*                                                                               
LINER50  CLI   FFTTYPE,FFTTBLIN    PRODUCTION BILL DETAIL LINE?                 
         BNE   LINER40                                                          
         CLI   FFTDLEN,MAXLLNQ     IF DIFFERENT LENGTH BUILD NEW ELE            
         BNE   LINER60                                                          
         CLI   LRMODE,LRMERGE      TEST MERGE ACTIVE LINE                       
         BNE   LINER54                                                          
         SR    R4,R4               R3=A(STANDARD COMMENT TABLE ENTRY)           
         IC    R4,SCTDSP-SCTABD(R3)                                             
         SR    RE,RE                                                            
         IC    RE,LRCOLUMN         RE=(COLUMN NUMBER AT START OF DISP)          
         BCTR  RE,0                                                             
         AR    R4,RE                                                            
         LA    RE,MAXLLNQ-1        CLEAR OUT TRAILING TEXT                      
         SR    RE,R4                                                            
         LA    R4,FFTDATA(R4)                                                   
         EX    RE,*+4                                                           
         MVC   0(0,R4),BCSPACES                                                 
LINER54  CLI   RECMODE,RECUACTQ    USING UNACTIVE RECORD?                       
         BNE   LINER100                                                         
         MVC   FFTDATA(MAXLLNQ),BCSPACES CLEAR OUT TEXT LINE                    
         B     LINER100                                                         
LINER60  MVI   FFTEL,X'FF'         DELETION MARKER                              
         CLI   RECMODE,RECUACTQ    USING UNACTIVE RECORD?                       
         BE    LINER70                                                          
         SR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         BCTR  RE,0                RE=X L'(TEXT LINE)                           
         EX    RE,*+4                                                           
         MVC   BOELEM+FFTDATA-FFTELD(0),FFTDATA  GET ORIGINAL LINE              
LINER70  GOTO1 VHELLO,BCDMCB,(C'D',ACCMST),(X'FF',AIO1),0                       
         CLI   12(R1),0                                                         
         BE    LINER90                                                          
         DC    H'0'                                                             
*                                                                               
LINER80  LA    RE,PBRRFST-PBRRECD(RF) RE=A(FIRST ELEMENT)                       
         MVI   0(RE),EOR              SET TO END OF RECORD                      
         LA    RE,1(RE)                                                         
         SR    RE,RF                  RE=L'(NEW LINE RECORD)                    
         STCM  RE,3,PBRRLEN-PBRRECD(RF)                                         
*                                                                               
LINER90  LA    RF,BOELEM              RF=A(ELEMENT WORK AREA)                   
         MVI   FFTEL,FFTELQ           BUILD NEW ELEMENT                         
         MVI   FFTTYPE,FFTTBLIN                                                 
         MVI   FFTSEQ,0                                                         
LINER100 SR    RE,RE                                                            
         IC    RE,LRCOLUMN         RE=(COLUMN NUMBER AT START OF DISP)          
         BCTR  RE,0                                                             
         LA    RE,FFTDATA(RE)      RE=A(FIRST CHAR DISPLAYED IN LINE)           
         SR    R1,R1                                                            
         IC    R1,LRLINELN         R1=L'(LINE TO BE CHANGED)                    
         CH    R1,=Y(MAXLLNQ)      ENSURE NOT TOO LONG                          
         BNH   *+8                                                              
         LA    R1,MAXLLNQ                                                       
         ICM   R2,15,LRALINE       R2=A(LINE)                                   
         CLI   LRMODE,LROVRWRT     ACTIVE LINE REC TO BE OVER WRITTEN?          
         BNE   LINER120                                                         
LINER110 CLI   0(RE),C' '          IF BLANK CHAR                                
         BNE   *+10                                                             
         MVC   0(1,RE),0(R2)       THEN OVERWRITE                               
         LA    RE,1(RE)            BUMP UP LINES                                
         LA    R2,1(R2)                                                         
         BCT   R1,LINER110                                                      
         B     LINER130                                                         
LINER120 CLI   LRMODE,LRMERGE      TEST MERGE ACTIVE LINE                       
         BNE   LINER124                                                         
         SR    R4,R4               R3=A(STANDARD COMMENT TABLE ENTRY)           
         IC    R4,SCTDSP-SCTABD(R3)                                             
         AR    RE,R4               DISPLACE BEYOND ANY LEADING TEXT             
         AR    R2,R4               AND DON'T TAKE LEADING SPACES                
         SR    R1,R4               ADJUST L'MOVE ACCORDINGLY                    
LINER124 BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),0(R2)       GET LINE                                     
LINER130 CLI   FFTDLEN,MAXLLNQ     OLD ELEMENT?                                 
         BE    LINER140                                                         
         LA    R1,MAXLLNQ                                                       
         STC   R1,FFTDLEN                                                       
         LA    R1,FFTLN1Q+1(R1)                                                 
         STC   R1,FFTLN            SET LENGTH OF NEW ELEMENT                    
         DROP  RF                                                               
*                                                                               
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(RF),0 ADD NEW ELEMENT          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RECMODE,RECADDQ     ADDING RECORD?                               
         BNE   LINER140                                                         
         GOTO1 AIO,IO1+IOACCMST+IOADDREC ADD RECORD                             
         BE    LINERX                                                           
         DC    H'0'                                                             
LINER140 GOTO1 AIO,IO1+IOACCMST+IOPUTREC PUT BACK RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LINERX   L     RF,AIO1             RF=A(IOAREA FOR NEW LINE RECORD)             
         USING PBRRECD,RF                                                       
         LA    RF,PBRRFST                                                       
         USING FFTELD,RF                                                        
         CLI   FFTEL,FFTELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,FFTLN                                                         
         AR    RE,RF                                                            
         CLI   0(RE),EOR                                                        
         BE    *+6                                                              
         DC    H'0'                TRAP DODGY RECORD                            
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE/ADD A PARAGRAPH RECORD                                       *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* PRECPARM DS    0X                COPY/MOVE ROUTINE PARMS            *         
* PRMODE   DS    X                                                    *         
* PRADDQ   EQU   X'80'             ADD A RECORD (OR CHANGE UNACTIVE)  *         
* PRCHANGQ EQU   X'40'             CHANGE AN EXISTING RECORD (ACTIVE) *         
***********************************************************************         
         SPACE 1                                                                
PARAREC  NTR1                                                                   
         MVI   PARAFLAG,0          INIT PARAGRAPH FLAG                          
         MVI   RECMODE,RECACTQ     SET ACTIVE RECORD MODE                       
         CLI   PRMODE,PRCHANGQ     ACTIVE LINE RECORD TO BE CHANGED?            
         BE    PARAR10                                                          
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
         BE    PARAR05                                                          
         MVI   RECMODE,RECUACTQ    UNACTIVE PARA RECORD TO BE USED              
         LA    RE,OSVPLST-1(RF)    RE=A(CORRESPONDING INDEX FOR PARA)           
         MVC   OSVPARA,0(RE)                                                    
         B     PARAR20                                                          
PARAR05  STC   RF,OSVPHIGH         UPDATE NUMBER OF PARA RECORDS COUNT          
         LA    RE,OSVPLST(RF)                                                   
         BCTR  RE,0                RE=A(CORRESPONDING INDEX FOR PARA)           
         STC   RF,0(RE)            ATTATCH AT END OF LIST                       
         MVI   RECMODE,RECADDQ     PARA RECORD REQUIRES ADDING                  
         STC   RF,OSVPARA                                                       
         B     PARAR20                                                          
*                                  IS CHANGE NECCESSARY?                        
PARAR10  TM    EDTNETH+FLDIIND-FLDHDRD,FINPVAL NET AMOUNT INPUT?                
         BNO   PARAR20                                                          
         TM    EDTCOMNH+FLDIIND-FLDHDRD,FINPVAL COMMISSION INPUT?               
         BNO   PARAR20                                                          
         TM    EDTDESCH+FLDIIND-FLDHDRD,FINPVAL DESCRIPTION INPUT?              
         BNO   PARAR20                                                          
         TM    EDTTYPEH+FLDIIND-FLDHDRD,FINPVAL VAT CODE INPUT?                 
         BNO   PARAR20                                                          
         TM    LINEFLAG,LCHANGEQ   LINE INDEX LIST CHANGED?                     
         BNO   PARARX                                                           
         NI    LINEFLAG,X'FF'-LCHANGEQ                                          
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
         MVC   PBRKPARA,OSVPARA    PARAGRAPH#                                   
*        CLI   PBRKPARA,0          TRAP                                         
*        BNE   *+6                                                              
*        DC    H'0'                                                             
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
PARAR50  MVC   FVMSGNO,=AL2(AE$INVAM) INVALID AMOUNT                            
         LA    R2,EDTNETH          R2=A(NET FIELD HEADER)                       
         USING FLDHDRD,R2                                                       
         ST    R2,FVADDR                                                        
         CLI   PRMODE,PRADDQ       ACTIVE LINE RECORD TO BE RE-USED?            
         BNE   *+14                                                             
         ZAP   PGHNET,=P'0'        CLEAR NET AMOUNT                             
         B     PARAR60                                                          
         TM    FLDIIND,FINPVAL     PREVIOUSLY VALIDATED?                        
         BO    PARAR70                                                          
         OI    BCAPINDS,BCAPIBAU   SET AMOUNT CHNGED TO CALLING OVERLAY         
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        RF=L'(INPUT)                                 
         BNZ   PARAR55                                                          
         ZAP   PGHNET,=P'0'                                                     
         B     PARAR60                                                          
PARAR55  MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BCDMCB,(BOBYTE1,FLDDATA),(X'40',(RF))                   
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         OI    PARAFLAG,PERRORQ    SET PARAGRAPH ERROR FLAG                     
         B     PARAR75                                                          
         ZAP   PGHNET,4(8,R1)      GET NEW AMOUNT                               
PARAR60  CURED PGHNET,(L'EDTNET,FLDDATA),CSCURBIL,MINUS=YES,           X        
               DMCB=BCDMCB,ALIGN=LEFT                                           
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED                     
PARAR70  LA    R2,EDTCOMNH         R2=A(COMMISION FIELD HEADER)                 
         ST    R2,FVADDR                                                        
PARAR75  CLI   PRMODE,PRADDQ       ACTIVE LINE RECORD TO BE RE-USED?            
         BNE   *+14                                                             
         ZAP   PGHCOM,=P'0'        CLEAR OUT COMMISSION AMOUNT                  
         B     PARAR80                                                          
         TM    FLDIIND,FINPVAL     PREVIOUSLY VALIDATED?                        
         BO    PARAR90                                                          
         OI    BCAPINDS,BCAPIBAU   SET AMOUNT CHNGED TO CALLING OVERLAY         
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        RF=L'(INPUT)                                 
         BNZ   PARAR76                                                          
         ZAP   PGHCOM,=P'0'                                                     
         B     PARAR80                                                          
PARAR76  MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         GOTO1 VCASHVAL,BCDMCB,(BOBYTE1,FLDDATA),(X'40',(RF))                   
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         OI    PARAFLAG,PERRORQ    SET PARAGRAPH ERROR FLAG                     
         ZAP   PGHCOM,4(8,R1)      GET COMMISSION AMOUNT                        
PARAR80  CURED PGHCOM,(L'EDTCOMN,FLDDATA),CSCURBIL,MINUS=YES,          X        
               DMCB=BCDMCB,ALIGN=LEFT                                           
         OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED                     
         OI    FLDOIND,FOUTTRN                                                  
PARAR90  LA    R2,EDTTYPEH         R2=A(TYPE FIELD HEADER)                      
         ST    R2,FVADDR                                                        
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        IF NO INPUT ASSUME COST PARA                 
         BZ    PARAR98                                                          
         BCTR  RF,0                                                             
         LH    RE,=AL2(UC@COST-TWAD) COST                                       
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FLDDATA(0),0(RE)                                                 
         BE    PARAR98                                                          
         LH    RE,=AL2(UC@TIME-TWAD) TIME                                       
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FLDDATA(0),0(RE)                                                 
         BE    PARAR99                                                          
         TM    PARAFLAG,PERRORQ    SET PARAGRAPH ERROR FLAG                     
         BO    PARAR40                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF) INVALID INPUT                             
         OI    PARAFLAG,PERRORQ    SET PARAGRAPH ERROR FLAG                     
         B     PARAR40                                                          
*                                                                               
PARAR98  OI    PGHHTYP,PGHHCSTQ                                                 
         NI    PGHHTYP,X'FF'-PGHHHRSQ                                           
         LH    RE,=AL2(UC@COST-TWAD) COST                                       
         B     PARAR100                                                         
PARAR99  OI    PGHHTYP,PGHHHRSQ                                                 
         NI    PGHHTYP,X'FF'-PGHHCSTQ                                           
         LH    RE,=AL2(UC@TIME-TWAD) TIME                                       
*                                                                               
PARAR100 LA    RE,TWAD(RE)                                                      
         MVC   EDTTYPE(L'UC@COST),0(RE)                                         
         OI    FLDIIND,FINPVAL                                                  
         OI    FLDOIND,FOUTTRN                                                  
         B     PARAR40                                                          
*                                                                               
         USING NDXELD,R3           INDEX ELEMENT                                
PARAR120 CLI   PRMODE,PRADDQ       ACTIVE PARA RECORD TO BE RE-USED?            
         BNE   PARAR130                                                         
         MVI   NDXACTV,0                                                        
*        MVI   NDXACTV,1                                                        
         MVC   OSVLHIGH,NDXHIGH    HIGHEST EXISTING PARA REC                    
         MVI   OSVLACTV,0                                                       
         MVC   OSVLLST(NDXLNQ-(NDXINDX-NDXELD)),NDXINDX  SAVE LIST              
         B     PARAR40                                                          
PARAR130 SR    RF,RF                                                            
         IC    RF,NDXHIGH          RF=(NUM OF PARA RECS ON OLD BILL)            
         SR    RE,RE                                                            
         IC    RE,OSVLHIGH         RE=(NUM OF PARA RECS ON NEW BILL)            
         CR    RF,RE                                                            
         BH    *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                RF=(X LENGTH OF LONGEST INDX LIST)           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   NDXINDX(0),OSVLLST  ANY CHANGE                                   
         BNE   *+14                                                             
         CLC   NDXHIGH(L'NDXHIGH+L'NDXACTV),OSVLHIGH CHANGED?                   
         BE    PARAR40                                                          
         CLI   NDXLN,NDXLNQ                                                     
         BNE   PARAR140                                                         
         MVC   NDXINDX(L'OSVLLST),OSVLLST     SAVE LINE INDEX LIST              
         MVC   NDXHIGH(L'NDXHIGH+L'NDXACTV),OSVLHIGH SET HIGH AND ACTV          
         CLI   NDXHIGH,0           TRAP EMPTY ELEMENT BUG                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     PARAR40                                                          
PARAR140 MVI   NDXEL,X'FF'         MARK FOR DELETION                            
         GOTO1 VHELLO,BCDMCB,(C'D',ACCMST),(X'FF',AIO1),0                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         LA    R3,BOELEM                                                        
         USING NDXELD,R3                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   NDXEL,NDXELQ        INDEX ELEMENT                                
         MVI   NDXLN,NDXLNQ        SET ELEMENT LENGTH                           
         MVC   NDXINDX(L'OSVLLST),OSVLLST     SAVE LINE INDEX LIST              
         MVC   NDXHIGH(L'NDXHIGH+L'NDXACTV),OSVLHIGH SET HIGH AND ACTV          
         CLI   NDXHIGH,0           TRAP EMPTY ELEMENT BUG                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PARAR40                                                          
*                                                                               
         USING FFTELD,R3                                                        
PARAR150 CLI   FFTTYPE,FFTTPGHC    PARAGRAPH HEADER COMMENT?                    
         BNE   PARAR40                                                          
         LA    R2,EDTDESCH         R2=A(DESCRIPTION FILED HEADER)               
         USING FLDHDRD,R2                                                       
         CLI   PRMODE,PRADDQ       ACTIVE LINE RECORD TO BE RE-USED?            
         BNE   PARAR160                                                         
         MVC   FLDDATA(L'EDTDESC),BCSPACES CLEAR OUT DESCRIPTION                
         MVC   FFTDATA(L'EDTDESC),BCSPACES                                      
         B     PARAR180                                                         
PARAR160 TM    FLDIIND,FINPVAL     PREVIOUSLY VALIDATED?                        
         BO    PARAR40                                                          
         CLI   FFTDLEN,L'EDTDESC   LENGTH OF DATA = LENGTH OF FIELD?            
         BNE   PARAR170                                                         
         MVC   FFTDATA(L'EDTDESC),BCSPACES                                      
         SR    RF,RF                                                            
         IC    RF,FLDILEN                                                       
         SH    RF,=H'1'                                                         
         BM    PARAR180                                                         
         EX    RF,*+4                                                           
         MVC   FFTDATA(0),FLDDATA  GET DESCRIPTION                              
         B     PARAR180                                                         
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
         SR    RF,RF                                                            
         IC    RF,FLDILEN                                                       
         SH    RF,=H'1'                                                         
         BM    *+14                                                             
         EX    RF,*+4                                                           
         MVC   FFTDATA(0),FLDDATA                                               
         LA    R1,L'EDTDESC                                                     
         STC   R1,FFTDLEN                                                       
         LA    R1,FFTLN1Q+1(R1)                                                 
         STC   R1,FFTLN                                                         
*                                                                               
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PARAR180 OI    FLDIIND,FINPVAL     SET PREVIOUSLY VALIDATED                     
         OI    FLDOIND,FOUTTRN                                                  
         B     PARAR40                                                          
*                                  ADDING A NEW RECORD                          
PARAR190 LA    RE,PBRRFST-PBRRECD(R3) RE=A(FIRST ELEMENT)                       
         MVI   0(RE),EOR           SET END OF RECORD                            
         LA    RE,1(RE)                                                         
         SR    RE,R3               RE=L'(NEW RECORD)                            
         STCM  RE,3,PBRRLEN-PBRRECD(R3)                                         
*                                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM           R3=A(ELEMENT WORK AREA)                      
         USING PGHELD,R3                                                        
         MVI   PGHEL,PGHELQ                                                     
         MVI   PGHLN,PGHLNQ                                                     
         LA    R2,EDTNETH          R2=A(NET FIELD HEADER)                       
         USING FLDHDRD,R2                                                       
         ZAP   PGHNET,=P'0'        INIT NET AMOUNT                              
         CURED PGHNET,(L'EDTNET,FLDDATA),CSCURBIL,MINUS=YES,           X        
               DMCB=BCDMCB,ALIGN=LEFT                                           
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPVAL                                                  
         LA    R2,EDTCOMNH         R2=A(COMMISION FIELD HEADER)                 
         ZAP   PGHCOM,=P'0'        INIT COMMISSION AMOUNT                       
         CURED PGHCOM,(L'EDTCOMN,FLDDATA),CSCURBIL,MINUS=YES,          X        
               DMCB=BCDMCB,ALIGN=LEFT                                           
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPVAL                                                  
         LA    R2,EDTTYPEH         R2=A(TAX CODE FIELD HEADER)                  
         MVC   EDTTYPE,BCSPACES    INIT TYPE                                    
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPVAL                                                  
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0 ADD PARA HDR ELE         
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
         MVI   NDXHIGH,0                                                        
         MVI   NDXACTV,0                                                        
         MVI   NDXINDX,0                                                        
         XC    OSVLVALS(OSVLLNQ),OSVLVALS                                       
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         XC    BOELEM,BOELEM       INIT ELEMENT STORAGE                         
         LA    R2,EDTDESCH         R2=A(DESC FIELD HEADER)                      
         USING FLDHDRD,R2                                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPVAL                                                  
         MVC   FLDDATA(L'EDTDESC),BCSPACES                                      
         LA    R3,BOELEM                                                        
         USING FFTELD,R3                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTPGHC                                                 
         MVI   FFTSEQ,0                                                         
         MVC   FFTDATA(L'EDTDESC),BCSPACES                                      
         LA    R1,L'EDTDESC                                                     
         STC   R1,FFTDLEN                                                       
         LA    R1,FFTLN1Q+1(R1)                                                 
         STC   R1,FFTLN                                                         
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         GOTO1 AIO,IO1+IOACCMST+IOADDREC ADD RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PARAR210                                                         
*                                                                               
PARAR200 GOTO1 AIO,IO1+IOACCMST+IOPUTREC PUT BACK A RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
PARAR210 CLI   PRMODE,PRADDQ       ACTIVE LINE RECORD TO BE RE-USED?            
         BNE   PARAR220                                                         
         LA    RF,OSVEINSB         RF=A(INSERT BLOCK VALUES LIST)               
         USING BLKVALD,RF                                                       
         MVI   BLKSTRT,0           SET TO INSERT WHOLE SCREEN                   
         MVI   BLKEND,MAXSLINQ                                                  
         MVI   BLKLEN,MAXSLINQ                                                  
         DROP  RF                                                               
         MVI   OSVSTLIN,1          INIT START LINE                              
         MVI   OSVSTCOL,1          INIT START COLUMN                            
         B     PARARX                                                           
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
BILLR20  XR    RF,RF                                                            
         IC    RF,NDXHIGH          RF=(NUM OF LINE RECS ON OLD PARA)            
         XR    RE,RE                                                            
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
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
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
         SR    RE,RE                                                            
         ICM   RE,3,TIOBCURS       RE=ABSOLUTE A(CURSOR ON SCREEN)              
         CH    RE,=Y(TXTSTLIN*PSCRNLN) CURSOR BEFORE 1ST TEXT LINE?             
         BL    DEFC10                                                           
         OC    TIOBCURD,TIOBCURD   CURSOR POSITION OVERRIDEN?                   
         BNZ   DEFCX                                                            
         CH    RE,=Y(TXTENLIN*PSCRNLN) CURSOR AFTER LAST TEXT LINE?             
         BNL   *+8                                                              
         LA    RE,PSCRNLN(RE)      RE=ABSOLUTE A(NEXT ROW, SAME COLUMN)         
         SR    R2,R2                                                            
         LR    R3,RE               R3=RE                                        
         LA    R1,PSCRNLN          R1=L(SCREEN LINE)                            
         STCM  R1,15,BCFULL                                                     
*                                                                               
         D     R2,BCFULL                                                        
         CH    R2,=Y(1+L'EDTLCM1)  CURSOR IN LINE COMMAND COL?                  
         BH    *+8                                                              
         BCTR  R2,0                                                             
         SR    RE,R2               SET TO FIRST COLUMN ON COMMAND LINE          
         STCM  RE,3,TIOBCURS       SAVE ABSOLUTE CURSOR ADDRESS                 
         B     DEFCX                                                            
*                                                                               
DEFC10   TM    OSVFCFLG,OSVFCMTQ   MATCH FOUND?                                 
         BO    DEFCX                                                            
         LA    RE,BASOPTH          RE=A(OPTION FIELD HEADER)                    
         SR    RE,RA               RE=(DISPLACEMENT TO OPTION FIELD HD)         
         CLM   RE,3,OSVCURD        IS CURSOR CURRENTLY ON THIS LINE?            
         BNE   *+12                                                             
         STCM  RE,3,TIOBCURD                                                    
         B     DEFC20                                                           
         LA    RE,EDTLCM1H         RE=A(LINE COMMAND HEADER)                    
         SR    RE,RA               RE=(DISPLACEMENT TO OPTION FIELD HD)         
         STCM  RE,3,TIOBCURD                                                    
DEFC20   MVI   TIOBCURI,0          SET TO START OF FIELD                        
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
         TM    EBLCTYPE,LENMOVEQ+LENCOPYQ MOVE/COPY CMD?                        
         BZ    ALTB20                                                           
         CLC   OSVPARA,OSVECMPA    SAME PARA AS DISPLAYED?                      
         BNE   ALTB70                                                           
         B     ALTB30                                                           
ALTB20   TM    EBLCTYPE,LENAFTQ+LENBEFQ+LENOVERQ AFTER/BEFORE/OVER CMD?         
         BZ    ALTB30                                                           
         CLC   OSVPARA,OSVETOPA    SAME PARA AS DISPLAYED?                      
         BNE   ALTB70                                                           
ALTB30   SR    R0,R0                                                            
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
*        B     ALTB55                                                           
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
ALTB55   CLI   BLKLEN,0            IF NUMERIC COMMAND RECALC LENGTH             
         BE    ALTB60                                                           
         SR    R2,R2                                                            
         IC    R2,BLKEND                                                        
         SR    R4,R4                                                            
         IC    R4,BLKSTRT                                                       
         SR    R2,R4                                                            
         LA    R2,1(R2)                                                         
         STC   R2,BLKLEN                                                        
ALTB60   LA    R1,BLKLNQ(R1)       BUMP TO NEXT BLOCK LIST VALUE                
         BCT   R0,ALTB40                                                        
ALTB70   LA    R3,EBLLNQ(R3)       BUMP TO NEXT EDIT TABLE ENTRY                
         B     ALTB10                                                           
ALTBX    B     EXIT                                                             
         DROP  R1,R3                                                            
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
INSDOTS  DC    C'...'                                                           
LHSMORE  DC    C'<<<'                                                           
RHSMORE  DC    C'>>>'                                                           
UNDERLIN DC    132C'-'                                                          
         SPACE 1                                                                
DICLOW   DS    0X                                                               
         DCDDL AC#TEXT,L'EDTBAN,F  TEXT                                         
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
AROUT    DS    0F                  NON ADDRESSABLE ROUTINES                     
         DC    A(FCTXT)            FIND/CHANGE TEXT MODULE                      
         DC    A(GPARA)            GET AND DISPLAY A PARAGRAPH RECORD           
         DC    A(LINEREC)          CHANGE A LINE RECORD                         
         DC    A(STDCOM)           BUILD/PROCESS STANDARD COMMENT TABLE         
         DC    A(TRUPTAB)          TRANSLATE TABLE                              
AROUTN   EQU   (*-AROUT)/L'AROUT   NUMBER OF NON ADDRESSABLE ROUTINES           
         SPACE 1                                                                
*                                                                               
EBLTAB   DS    0X                  TABLE OF EDIT BLOCK LISTS                    
         DC    AL1(LENINSQ)                                                     
         DC    AL1(L'OSVEINSB/BLKLNQ),AL2(OSVEINSB-OSVALSD,0)                   
         DC    AL1(LENAFTQ+LENBEFQ+LENINSQ+LENREPQ)                             
         DC    X'FF'                                                            
         DC    AL1(LENDELQ)                                                     
         DC    AL1(L'OSVEDELB/BLKLNQ),AL2(OSVEDELB-OSVALSD,DEL-CLB0A)           
         DC    AL1(LENAFTQ+LENBEFQ+LENINSQ+LENREPQ)                             
         DC    AL1(0)                                                           
         DC    AL1(LENREPQ)                                                     
         DC    AL1(L'OSVEREPB/BLKLNQ),AL2(OSVEREPB-OSVALSD,REPT-CLB0A)          
         DC    AL1(LENAFTQ+LENBEFQ+LENINSQ+LENREPQ)                             
         DC    X'FF'                                                            
         DC    AL1(LENMOVEQ)                                                    
         DC    AL1(L'OSVEMVEB/BLKLNQ),AL2(OSVEMVEB-OSVALSD,CMLN-CLB0A)          
         DC    AL1(LENAFTQ+LENBEFQ+LENINSQ+LENREPQ)                             
         DC    AL1(0)                                                           
         DC    AL1(LENCOPYQ)                                                    
         DC    AL1(L'OSVECPYB/BLKLNQ),AL2(OSVECPYB-OSVALSD,CMLN-CLB0A)          
         DC    AL1(LENAFTQ+LENBEFQ+LENINSQ+LENREPQ)                             
         DC    AL1(0)                                                           
         DC    AL1(LENAFTQ)                                                     
         DC    AL1(L'OSVEAFLN/BLKLNQ),AL2(OSVEAFLN-OSVALSD,0)                   
         DC    AL1(LENINSQ+LENREPQ)                                             
         DC    AL1(LENDELQ+LENMOVEQ+LENCOPYQ+LENINSQ+LENREPQ)                   
         DC    AL1(LENBEFQ)                                                     
         DC    AL1(L'OSVEBFLN/BLKLNQ),AL2(OSVEBFLN-OSVALSD,0)                   
         DC    AL1(LENINSQ+LENREPQ)                                             
         DC    AL1(LENDELQ+LENMOVEQ+LENCOPYQ+LENINSQ+LENREPQ)                   
         DC    AL1(LENOVERQ)                                                    
         DC    AL1(L'OSVEOVLN/BLKLNQ),AL2(OSVEOVLN-OSVALSD,0)                   
         DC    AL1(LENINSQ+LENREPQ)                                             
         DC    AL1(0)                                                           
EBLTABX  DC    AL1(EOT)                                                         
*                                                                               
LEDTAB   DS    0X    LINE EDIT COMMAND TABLE                                    
*                                                                               
LEGBR    DC    AL1(LANGEUK,LEGBRX+1-LEGBR)                                      
         DC    C'I  ',AL1(LENINSQ)                                              
         DC    C'DD ',AL1(LENDELQ)                                              
         DC    C'MM ',AL1(LENMOVEQ)                                             
         DC    C'CC ',AL1(LENCOPYQ)                                             
         DC    C'R  ',AL1(LENREPQ)                                              
         DC    C'A  ',AL1(LENAFTQ)                                              
         DC    C'B  ',AL1(LENBEFQ)                                              
         DC    C'O  ',AL1(LENOVERQ)                                             
LEGBRX   DC    AL1(EOT)                                                         
         SPACE 1                                                                
LEGER    DC    AL1(LANGGER,LEGERX+1-LEGER)                                      
         DC    C'I  ',AL1(LENINSQ)                                              
         DC    C'DD ',AL1(LENDELQ)                                              
         DC    C'MM ',AL1(LENMOVEQ)                                             
         DC    C'CC ',AL1(LENCOPYQ)                                             
         DC    C'R  ',AL1(LENREPQ)                                              
         DC    C'A  ',AL1(LENAFTQ)                                              
         DC    C'B  ',AL1(LENBEFQ)                                              
         DC    C'O  ',AL1(LENOVERQ)                                             
LEGERX   DC    AL1(EOT)                                                         
         SPACE 1                                                                
LEDUT    DC    AL1(LANGDUT,LEDUTX+1-LEDUT)                                      
         DC    C'I  ',AL1(LENINSQ)                                              
         DC    C'AA ',AL1(LENDELQ)                                              
         DC    C'VV ',AL1(LENMOVEQ)                                             
         DC    C'KK ',AL1(LENCOPYQ)                                             
         DC    C'H  ',AL1(LENREPQ)                                              
         DC    C'N  ',AL1(LENAFTQ)                                              
         DC    C'E  ',AL1(LENBEFQ)                                              
         DC    C'O  ',AL1(LENOVERQ)                                             
LEDUTX   DC    AL1(EOT)                                                         
         SPACE 1                                                                
LEUSA    DC    AL1(LANGEUS,LEUSAX+1-LEUSA)                                      
         DC    C'I  ',AL1(LENINSQ)                                              
         DC    C'DD ',AL1(LENDELQ)                                              
         DC    C'MM ',AL1(LENMOVEQ)                                             
         DC    C'CC ',AL1(LENCOPYQ)                                             
         DC    C'R  ',AL1(LENREPQ)                                              
         DC    C'A  ',AL1(LENAFTQ)                                              
         DC    C'B  ',AL1(LENBEFQ)                                              
         DC    C'O  ',AL1(LENOVERQ)                                             
LEUSAX   DC    AL1(EOT)                                                         
         SPACE 1                                                                
         DC    AL1(EOT)                                                         
*                                                                               
CMNDTAB  DS    0C                                                               
         DC    AL1(CMNRESQ),AL2(UC@RESET-TWAD),AL2(0),AL1(1)                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL1(CMNDELQ),AL2(UC@DEL-TWAD),AL2(CPARDEL-CLB0A),AL1(2)          
         DC    AL1(CMNSPREQ),AL2(OSVDPAR-OSVALSD)                               
         DC    AL1(CMNFNDQ),AL2(UC@FIND-TWAD),AL2(CPARFND-CLB0A),AL1(2)         
         DC    AL1(CMNSPREQ+CMNSFFTQ),AL2(OSVFCPAR-OSVALSD)                     
         DC    AL1(CMNCHGQ),AL2(UC@CHG-TWAD),AL2(CPARCHG-CLB0A),AL1(3)          
         DC    AL1(CMNSPREQ+CMNSFFTQ),AL2(OSVFCPAR-OSVALSD)                     
         DC    AL1(CMNCOLQ),AL2(UC@COLS-TWAD),AL2(CPARCOL-CLB0A),AL1(1)         
         DC    AL1(0),AL2(OSVCPAR-OSVALSD)                                      
         DC    AL1(CMNHEDQ),AL2(UC@HEADR-TWAD),AL2(CPARHED-CLB0A)               
         DC    AL1(1,0),AL2(OSVHPAR-OSVALSD)                                    
         DC    AL1(EOT)                                                         
*                                                                               
CPARDEL  DC    AL1(OSVDPARQ),AL2(UC@PRGRP-TWAD),AL1(CPARSDFQ)                   
         DC    AL1(OSVDALLQ,0,0,0,0,0)                                          
         DC    AL1(OSVDALLQ),AL2(UC@ALL-TWAD),AL1(0)                            
         DC    AL1(OSVDPARQ,0,0,0,0,0)                                          
         DC    AL1(EOT)                                                         
*                                                                               
CPARCOL  DC    AL1(OSVCYESQ),AL2(UC@YES-TWAD),AL1(0)                            
         DC    AL1(OSVCNOQ,0,0,0,0,0)                                           
         DC    AL1(OSVCNOQ),AL2(UC@NO-TWAD),AL1(0)                              
         DC    AL1(OSVCYESQ,0,0,0,0,0)                                          
         DC    AL1(EOT)                                                         
*                                                                               
CPARHED  DC    AL1(OSVHTIMQ),AL2(UC@TIME-TWAD),AL1(0)                           
         DC    AL1(OSVHCSTQ,OSVHNOQ,0,0,0,0)                                    
         DC    AL1(OSVHCSTQ),AL2(UC@COST-TWAD),AL1(0)                           
         DC    AL1(OSVHTIMQ,OSVHNOQ,0,0,0,0)                                    
         DC    AL1(OSVHNOQ),AL2(UC@NO-TWAD),AL1(0)                              
         DC    AL1(OSVHTIMQ,OSVHCSTQ,0,0,0,0)                                   
         DC    AL1(EOT)                                                         
*                                                                               
CPARFND  DC    AL1(OSVFCALQ),AL2(UC@ALL-TWAD),AL1(0)                            
         DC    AL1(OSVFCFSQ,OSVFCLSQ,OSVFCPVQ,0,0,0)                            
         DC    AL1(OSVFCPFQ),AL2(UC@PREFX-TWAD),AL1(0)                          
         DC    AL1(OSVFCSFQ,OSVFCWDQ,0,0,0,0)                                   
         DC    AL1(OSVFCSFQ),AL2(UC@SUFFX-TWAD),AL1(0)                          
         DC    AL1(OSVFCPFQ,OSVFCWDQ,0,0,0,0)                                   
         DC    AL1(OSVFCPVQ),AL2(UC@PRV-TWAD),AL1(0)                            
         DC    AL1(OSVFCALQ,OSVFCFSQ,OSVFCLSQ,0,0,0)                            
         DC    AL1(OSVFCWDQ),AL2(UC@WORD-TWAD),AL1(0)                           
         DC    AL1(OSVFCPFQ,OSVFCSFQ,0,0,0,0)                                   
         DC    AL1(OSVFCFSQ),AL2(UC@FIRST-TWAD),AL1(0)                          
         DC    AL1(OSVFCLSQ,OSVFCALQ,OSVFCPVQ,0,0,0)                            
         DC    AL1(OSVFCLSQ),AL2(UC@LAST-TWAD),AL1(0)                           
         DC    AL1(OSVFCFSQ,OSVFCALQ,OSVFCPVQ,0,0,0)                            
         DC    AL1(CPARFFUQ),AL2(OSVFDVL-OSVALSD),AL1(CPARSFFQ)                 
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(CPARNM1Q),AL2(OSVFCCOL-OSVALSD),AL1(CPARSNMQ)                
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(EOT)                                                         
*                                                                               
CPARCHG  DC    AL1(OSVFCALQ),AL2(UC@ALL-TWAD),AL1(0)                            
         DC    AL1(OSVFCFSQ,OSVFCLSQ,OSVFCPVQ,0,0,0)                            
         DC    AL1(OSVFCPFQ),AL2(UC@PREFX-TWAD),AL1(0)                          
         DC    AL1(OSVFCSFQ,OSVFCWDQ,0,0,0,0)                                   
         DC    AL1(OSVFCSFQ),AL2(UC@SUFFX-TWAD),AL1(0)                          
         DC    AL1(OSVFCPFQ,OSVFCWDQ,0,0,0,0)                                   
         DC    AL1(OSVFCPVQ),AL2(UC@PRV-TWAD),AL1(0)                            
         DC    AL1(OSVFCALQ,OSVFCFSQ,OSVFCLSQ,0,0,0)                            
         DC    AL1(OSVFCWDQ),AL2(UC@WORD-TWAD),AL1(0)                           
         DC    AL1(OSVFCPFQ,OSVFCSFQ,0,0,0,0)                                   
         DC    AL1(OSVFCFSQ),AL2(UC@FIRST-TWAD),AL1(0)                          
         DC    AL1(OSVFCLSQ,OSVFCALQ,OSVFCPVQ,0,0,0)                            
         DC    AL1(OSVFCLSQ),AL2(UC@LAST-TWAD),AL1(0)                           
         DC    AL1(OSVFCFSQ,OSVFCALQ,OSVFCPVQ,0,0,0)                            
         DC    AL1(CPARFFUQ),AL2(OSVFDVL-OSVALSD),AL1(CPARSFFQ)                 
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(CPARFFLQ),AL2(OSVCHVL-OSVALSD),AL1(CPARSFFQ)                 
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(CPARNM1Q),AL2(OSVFCCOL-OSVALSD),AL1(CPARSNMQ)                
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(EOT)                                                         
*                                  TRANSLATE TO UPPER TABLE LIST                
TRUPTAB  DC    A(TRUPUKUS-CLB0A,TRUPUKUS-CLB0A)                                 
         DC    A(TRUPUKUS-CLB0A,TRUPGER-CLB0A)                                  
         DC    A(TRUPFRA-CLB0A,TRUPUKUS-CLB0A)                                  
         DC    A(TRUPUKUS-CLB0A,TRUPDUT-CLB0A)                                  
         SPACE 1                                                                
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
TRUPUKUS DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040797A7B7C7D7E7F' 70-7F                     
         DC    XL16'40C1C2C3C4C5C6C7C8C940404040408F' 80-8F                     
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' 90-9F                     
         DC    XL16'40A1E2E3E4E5E6E7E8E940ABAC404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F940BBBC4040BF' B0-BF                     
         DC    XL16'C0C1C2C3C4C5C6C7C8C940CBCC404040' C0-CF                     
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                     
         DC    XL16'E040E2E3E4E5E6E7E8E940EBEC404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FA4040404040' F0-FF                     
         SPACE 1                                                                
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
TRUPGER  DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'60614040404040404040E06B6C6D6E6F' 60-6F O UMLAUT 6A         
         DC    XL16'404040404040404040797A7B7C7D7E7F' 70-7F                     
         DC    XL16'40C1C2C3C4C5C6C7C8C940404040408F' 80-8F                     
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' 90-9F                     
         DC    XL16'40A1E2E3E4E5E6E7E8E940ABAC404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F940BBBC4040BF' B0-BF                     
         DC    XL16'4AC1C2C3C4C5C6C7C8C940CBCC404040' C0-CF A UMLAUT C0         
         DC    XL16'5AD1D2D3D4D5D6D7D8D9404040404040' D0-DF U UMLAUT D0         
         DC    XL16'E040E2E3E4E5E6E7E8E940EBEC404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FA4040404040' F0-FF                     
         SPACE 1                                                                
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
TRUPFRA  DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'60614040404040404040E46B6C6D6E6F' 60-6F U CIRC  6A          
         DC    XL16'404040404040404040797A7BC17D7E7F' 70-7F A GRAVE 7C          
         DC    XL16'40C1C2C3C4C5C6C7C8C940404040408F' 80-8F                     
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' 90-9F                     
         DC    XL16'40A1E2E3E4E5E6E7E8E940ABAC404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F940BBBC4040BF' B0-BF                     
         DC    XL16'C5C1C2C3C4C5C6C7C8C940CBCC404040' C0-CF E ECUTE C0          
         DC    XL16'C5D1D2D3D4D5D6D7D8D9404040404040' D0-DF E GRAVE D0          
         DC    XL16'C340E2E3E4E5E6E7E8E940EBEC404040' E0-EF C CEDIL E0          
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FA4040404040' F0-FF                     
         SPACE 1                                                                
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
TRUPDUT  DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040797A7B7C7D7E7F' 70-7F                     
         DC    XL16'40C1C2C3C4C5C6C7C8C940404040408F' 80-8F                     
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' 90-9F                     
         DC    XL16'40A1E2E3E4E5E6E7E8E940ABAC404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F940BBBC4040BF' B0-BF                     
         DC    XL16'C0C1C2C3C4C5C6C7C8C940CBCC404040' C0-CF                     
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                     
         DC    XL16'E040E2E3E4E5E6E7E8E940EBEC404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FA4040404040' F0-FF                     
         EJECT                                                                  
***********************************************************************         
* FIND/CHANGE TEXT STRING                                             *         
***********************************************************************         
         SPACE 1                                                                
FCTXT    CSECT                                                                  
         NMODL 0,**FCTX**,R8,R7,R6,CLEAR=YES                                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRK,RC                                                       
         CLI   OSVCMND,CMNFNDQ     NEW FIND COMMAND ENTERED?                    
         BNE   *+12                                                             
         MVI   OSVFCFLG,OSVFCFNQ   SET FIND MODE FLAG ON                        
         B     FCT10                                                            
         CLI   OSVCMND,CMNCHGQ     NEW CHANGE COMMAND ENTERED                   
         BNE   FCT20                                                            
         MVI   OSVFCFLG,OSVFCCHQ   SET CHANGE MODE FLAG ON                      
FCT10    XC    OSVFCCNT,OSVFCCNT   INIT NUMBER OF MATCHES FOUND                 
         B     FCT60                                                            
FCT20    LA    R2,BASOPTH          R2=A(COMMAND LINE FIELD HEADER)              
         ST    R2,FVADDR                                                        
         NI    OSVFCFLG,X'FF'-(OSVFCMTQ+OSVFCTLQ) MATCH/TOO LONG OFF            
         CLI   BCPFKEY,PFKFINDQ    FIND PFKEY?                                  
         BNE   FCT30                                                            
         MVC   FVMSGNO,=AL2(AE$ENTFD) ENTER FIND COMMAND                        
         CLI   OSVFDLN,0           FIND STRING BEEN ENTERED?                    
         BE    FCTERRX                                                          
         OI    OSVFCFLG,OSVFCFNQ   SET FIND MODE FLAG ON                        
         NI    OSVFCFLG,X'FF'-OSVFCCHQ SWITCH OFF CHANGE MODE                   
         B     FCT40                                                            
FCT30    CLI   BCPFKEY,PFKCHGQ     CHANGE PFKEY?                                
         BE    *+12                                                             
         NI    OSVFCFLG,X'FF'-(OSVFCFNQ+OSVFCCHQ) SWITCH OFF FIND/CHNGE         
         B     FCTX                                                             
         MVC   FVMSGNO,=AL2(AE$ENTCH) ENTER CHANGE COMMAND                      
         CLI   OSVCHLN,0           CHANGE STRING BEEN ENTERED?                  
         BE    FCTERRX                                                          
         OI    OSVFCFLG,OSVFCCHQ   SET CHANGE MODE FLAG ON                      
         NI    OSVFCFLG,X'FF'-OSVFCFNQ SWITCH OFF FIND MODE                     
FCT40    CLC   OSVFCPOF,OSVPOFST   SAME PARA AS PREVIOUS TIME THROUGH?          
         BNE   FCT50                                                            
         CLC   OSVFCSTL,OSVSTLIN   SAME START LINE AS PREVIOUS TIME?            
         BNE   FCT50                                                            
         CLC   OSVFCSTC,OSVSTCOL   SAME START COLUMN AS PREVIOUS TIME?          
         BNE   FCT50                                                            
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         CLC   TIOBCURS,=Y(TXTSTLIN*PSCRNLN) CURSOR BEFORE TEXT AREA?           
         BL    FCT60                                                            
         CLC   OSVFCCRS,TIOBCURS-TIOBD(RF) CURSOR POS SAME AS PREVIOUS?         
         BE    FCT60                                                            
FCT50    NI    OSVFCFLG,X'FF'-(OSVFCTPQ+OSVFCBTQ) SWITCH OF TOP/BOT             
FCT60    MVC   OSVFCSTL,OSVSTLIN   SAVE CURRENT SCREEN START LINE               
         MVC   OSVFCSTC,OSVSTCOL   SAVE CURRENT SCREEN START COL                
         MVC   OSVFCPOF,OSVPOFST   SAVE CURRENT SCREEN PARA OFFSET              
         MVC   OSVFCPRA,OSVPARA    SAVE CURRENT SCREEN PARA INDEX               
         L     RF,AINP                                                          
         MVC   OSVFCCRS,TIOBCURS   SAVE CURRENT SCREEN ABSOLUTE A(CURS)         
         DROP  RF                                                               
*                                                                               
         TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER SET?                      
         BO    FCT70                                                            
         TM    OSVFCPAR,OSVFCLSQ   LAST PARAMETER SET?                          
         BO    FCT70                                                            
         TM    OSVFCFLG,OSVFCTPQ   TOP OF TEXT PREVIOUSLY REACHED?              
         BNO   FCT110                                                           
FCT70    LA    R1,OSVPACTV         R1=A(LAST ACTIVE PARA INDEX)                 
         L     RF,AGPARA                                                        
         BASR  RE,RF               GET LAST PARAGRAPH                           
         MVC   OSVFCLTL,OSVLACTV   SET LAST LINE NUMBER                         
         MVC   OSVFCSCP,OSVPOFST   SET SEARCH START TO LAST PARA                
         MVC   OSVFCSCL,OSVLACTV   SET SEARCH START LINE TO LAST LINE           
         MVI   OSVFCSCC,MAXLLNQ    SET SEARCH START COL TO LAST COL             
         TM    OSVFCPAR,OSVFCLSQ   LAST PARAMETER SET?                          
         BNO   FCT80                                                            
         NI    OSVFCPAR,X'FF'-OSVFCLSQ SWITCH OFF LAST PARAMETER                
         OI    OSVFCPAR,OSVFCPVQ   AND REPLACE WITH PREVIOUS PARAM              
         B     FCT100                                                           
FCT80    TM    OSVFCFLG,OSVFCTPQ   PREVIOUS PARAMETER SET?                      
         BO    FCT100                                                           
         CLI   OSVFCPOF,1          CURR SCREEN IS FIRST PARA?                   
         BNE   FCT90                                                            
         CLI   OSVFCSTL,1          CURR SCREEN STARTS AT 1ST TEXT LINE?         
         BNE   FCT90                                                            
         L     RF,AINP             RF=A(TIOB)                                   
         CLC   =Y(TXTSTLIN*PSCRNLN),TIOBCURS-TIOBD(RF) CURS BEFORE TXT?         
         BNL   FCT100                                                           
FCT90    LA    R1,OSVFCPOF         R1=A(CURRENT PARA OFFSET)                    
         L     RF,AGPARA                                                        
         BASR  RE,RF               GET PARAGRAPH                                
         B     FCT130                                                           
*                                                                               
FCT100   NI    OSVFCFLG,X'FF'-OSVFCTPQ SWITCH OF TOP FLAG                       
         SR    R2,R2                                                            
         IC    R2,OSVLACTV                                                      
         LA    R2,OSVLLST-1(R2)    R2=A(INDEX OF LAST LIN IN LAST PARA)         
         B     FCT170                                                           
*                                                                               
FCT110   TM    OSVFCPAR,OSVFCALQ   ALL PARAMETER SET?                           
         BO    FCT120                                                           
         TM    OSVFCPAR,OSVFCFSQ   FIRST PARAMETER SET?                         
         BO    FCT120                                                           
         TM    OSVFCFLG,OSVFCBTQ   BOTTOM OF TEXT PREVIOUSLY REACHED?           
         BNO   FCT130                                                           
FCT120   NI    OSVFCFLG,X'FF'-OSVFCBTQ SWITCH OFF BOTTOM                        
         NI    OSVFCPAR,X'FF'-OSVFCFSQ  SWITCH OFF FIRST PARAM                  
         MVI   OSVFCSCP,1          SET SEARCH START PARA TO FIRST PARA          
         MVI   OSVFCSCL,1          SET SEARCH START LINE TO FIRST LINE          
         MVI   OSVFCSCC,1          SET SEARCH START COL TO FIRST COL            
         LA    R1,OSVFCSCP                                                      
         L     RF,AGPARA                                                        
         BASR  RE,RF               GET FIRST PARAGRAPH                          
         LA    R2,OSVLLST          R2=A(INDEX OF 1ST LINE IN 1ST PARA)          
         B     FCT170                                                           
*                                                                               
FCT130   MVC   OSVFCSCP,OSVPOFST   SET SEARCH START PARA TO CURR PARA           
         MVC   OSVFCSCL,OSVSTLIN   SET SEARCH STRT LIN TO CURR STRT LIN         
         MVI   OSVFCSCC,1          SET SEARCH START COL TO 1ST COL              
         SR    RF,RF                                                            
         IC    RF,OSVSTLIN                                                      
         LA    R2,OSVLLST-1(RF)    R2=A(INDEX OF CURRENT START LINE)            
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         SR    RE,RE                                                            
         ICM   RE,3,TIOBCURS       RE=ABSOLUTE A(CURSOR ON SCREEN)              
         CH    RE,=Y(TXTSTLIN*PSCRNLN) CURSOR ABOVE TEXT AREA?                  
         BNL   FCT150                                                           
         TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER SET?                      
         BO    FCT410                                                           
         B     FCT170                                                           
FCT150   CH    RE,=Y(TXTENLIN*PSCRNLN) CURSOR BEYOND TEXT AREA?                 
         BL    *+12                                                             
         LA    R2,MAXSLINQ(R2)     R2=A(INDX OF LIN AFTER CURR END LIN)         
         B     FCT160                                                           
         OI    OSVFCFLG,OSVFCIGQ   SET IGNORE FIRST MATCH FLAG ON               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       R1=(DISPLACEMENT TO FIELD WITH CURS)         
         DROP  RF                                                               
         LH    RF,=Y(TXTSTLIN*PSCRNLN) RF=(DISPLACEMENT TO START LINE)          
         SR    R1,RF                                                            
         LA    RF,PSCRNLN          RF=L'(PHYSICAL SCREEN LINE)                  
         STCM  RF,15,BCFULL                                                     
*                                                                               
         D     R0,BCFULL                                                        
         STC   R1,OSVFCSCL         R1=(SEARCH START LINE NUMBER)                
         AR    R2,R1               R2=A(INDEX OF SEARCH START LINE)             
         SH    R0,=Y(1+L'EDTLCM1)  SUBTRACT LINE CMND FIELD FROM COL NO         
         BP    *+8                                                              
         LA    R0,1                                                             
         SR    RF,RF                                                            
         IC    RF,OSVFCSTC                                                      
         BCTR  RF,0                                                             
         AR    R0,RF               ADJUST SEARCH START COLUMN                   
         STC   R0,BCBYTE1                                                       
         STC   R0,OSVFCSCC                                                      
*                                  ENSURE SEACH START NOT BEYOND PARA           
FCT160   SR    RF,RF                                                            
         IC    RF,OSVLACTV         RF=(NUMBER OF ACTIVE LINE RECORDS)           
         LA    RF,OSVLLST-1(RF)                                                 
         CR    R2,RF               ENSURE WE HAVE ACTIVE RECORD                 
         BNH   FCT170                                                           
         TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER SET?                      
         BNO   FCT450                                                           
         LR    R2,RF               R2=A(INDEX OF SEARCH START LINE)             
         LR    RE,R2                                                            
         LA    R1,OSVLLST-1                                                     
         SR    RE,R1                                                            
         STC   RE,OSVFCSCL         SET START LINE/COL FOR PREV SEARCH           
         MVI   OSVFCSCC,MAXLLNQ                                                 
*                                                                               
FCT170   LA    RF,IOKEY            RF=A(KEY FOR LINE RECORD)                    
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,OSVJOB      JOB                                          
         MVC   PBRKSEQ,OSVSEQ      SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,OSVPARA    PARAGRAPH#                                   
         MVC   PBRKLINE,0(R2)      LINE#                                        
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             RF=A(LINE RECORD)                            
         LA    R4,PBRRFST          R4=A(FIRST ELEMENT)                          
         USING FFTELD,R4                                                        
FCT180   CLI   FFTEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FFTEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    FCT200                                                           
*                                                                               
FCT190   SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,FFTLN                                                         
         AR    RF,RE                                                            
         B     FCT180                                                           
*                                                                               
FCT200   CLI   FFTTYPE,FFTTBLIN    PRODUCTION BILL DETAIL LINE?                 
         BNE   FCT190                                                           
         CLC   OSVFDLN,FFTDLEN     FIND STRING LONGER THAN TEXT LINE?           
         BH    FCT410                                                           
         L     R1,ATRUP            GET TRANSLATE TABLE FOR LANG                 
         SR    RF,RF                                                            
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                RF= X L'(TEXT)                               
         MVI   WORKTTXT,C' '       SET TWO BLANK LINES IN WORK AREA             
         MVC   WORKTTXT+1((MAXLLNQ*2)-1),WORKTTXT                               
         EX    RF,*+4                                                           
         MVC   WORKTTXT(0),FFTDATA  SAVE LINE FOR CONVERSION TO UPPER           
         MVC   WORKTTXT+MAXLLNQ(MAXLLNQ),WORKTTXT SAVE ORIGINAL LINE            
         EX    RF,*+8                                                           
         B     *+10                                                             
         TR    WORKTTXT(0),0(R1)   CONVERT 1ST SAVED LINE TO UPPER CASE         
         TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER SET?                      
         BNO   FCT210                                                           
         CLC   OSVPOFST,OSVPACTV   LAST PARAGRAPH?                              
         BNE   FCT210                                                           
         CLC   OSVFCLTL,0(R2)      LAST LINE OF PARAGRAPH?                      
         BNE   FCT210                                                           
         MVC   OSVFCLTC,FFTDLEN    GET LAST COLUMN POSITION OF TEXT             
*                                                                               
FCT210   TM    OSVFCFLG,OSVFCIGQ   IGNORE FIRST MATCH TEST FLAG ON?             
         BNO   FCT240                                                           
         NI    OSVFCFLG,X'FF'-OSVFCIGQ                                          
         CLI   OSVFCCOL,0          COLUMN PARAMETER SET?                        
         BE    FCT230                                                           
         TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER SET?                      
         BNO   FCT220                                                           
         CLC   OSVFCCOL,BCBYTE1    IF COL PARAMETER >= CURSOR COL               
         BNL   FCT410              THEN IGNORE LINE                             
         B     FCT250                                                           
FCT220   CLC   OSVFCCOL,BCBYTE1    IF COL PARAMETER <= CURSOR COL               
         BNH   FCT430              THEN IGNORE LINE                             
         B     FCT250                                                           
FCT230   LA    RF,WORKTTXT         RF=A(FIRST CHAR IN UPPER CASE LINE)          
         SR    RE,RE                                                            
         IC    RE,BCBYTE1          RE=(COL NUM OF CURSOR)                       
         BCTR  RE,0                                                             
         AR    RF,RE               RF=A(CHAR AT CURSOR POSITION)                
         TM    OSVFCFLG,OSVFCCHQ   IF CHANGE MODE DONT IGNORE                   
         BNO   FCT390                                                           
         B     FCT280                                                           
FCT240   CLI   OSVFCCOL,0          COLUMN PARAMETER SET?                        
         BE    FCT260                                                           
FCT250   SR    RF,RF                                                            
         IC    RF,OSVFCCOL                                                      
         LA    RF,WORKTTXT-1(RF)   RF=A(CHAR AT COLUMN PARAMETER)               
         B     FCT280                                                           
FCT260   TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER IN USE?                   
         BNO   FCT270                                                           
         SR    RE,RE                                                            
         IC    RE,FFTDLEN          RE=L'(TEXT LINE)                             
         LA    RF,WORKTTXT(RE)     RF=A(LAST CHAR IN LINE + 1)                  
         SR    R1,R1                                                            
         IC    R1,OSVFDLN          R1=L'(FIND STRING)                           
         SR    RF,R1               RF=A(LAST POSS MATCH IN TXT LINE)            
         B     *+8                                                              
FCT270   LA    RF,WORKTTXT         RF=A(FIRST POSS MATCH IN TXT LINE)           
*                                                                               
FCT280   SR    R1,R1                                                            
         IC    R1,OSVFDLN          R1=L'(FIND STRING)                           
         TM    OSVFCPAR,OSVFCWDQ   WORD PARAMETER SET?                          
         BO    FCT290                                                           
         TM    OSVFCPAR,OSVFCPFQ   PREFIX PARAMETER SET?                        
         BO    FCT290                                                           
         TM    OSVFCPAR,OSVFCSFQ   SUFFIX PARAMETER SET?                        
         BNO   FCT340                                                           
FCT290   LR    RE,RF                                                            
         BCTR  RE,0                RE=A(CHAR BEFORE MATCH STRING)               
         LA    R0,WORKTTXT         R0=A(FIRST CHAR)                             
         CR    R0,RF               IF START OF LINE THE START OF WORD           
         BE    FCT300                                                           
         CLI   0(RE),C'A'          IF CHAR NOT ALPHA NUMERIC                    
         BL    FCT300                                                           
         CLI   0(RE),C'9'          THEN EITHER WORD OR PREFIX                   
         BNH   FCT310                                                           
FCT300   TM    OSVFCPAR,OSVFCSFQ   SUFFIX PARAMETER?                            
         BO    FCT390                                                           
         B     *+12                                                             
FCT310   TM    OSVFCPAR,OSVFCSFQ                                                
         BNO   FCT390                                                           
         LA    RE,0(RF,R1)         RE=A(LAST CHAR IN MATCH TEST)                
         AH    R0,=Y(MAXLLNQ)                                                   
         CR    RE,R0               IF LAST CHAR                                 
         BNL   FCT320                                                           
         CLI   0(RE),C'A'          IF CHAR NOT ALPHA NUMERIC                    
         BL    FCT320                                                           
         CLI   0(RE),C'9'          THEN EITHER WORD OR SUFFIX                   
         BNH   FCT330                                                           
FCT320   TM    OSVFCPAR,OSVFCPFQ   PREFIX PARAMETER?                            
         BO    FCT390                                                           
         B     *+12                                                             
FCT330   TM    OSVFCPAR,OSVFCPFQ                                                
         BNO   FCT390                                                           
FCT340   BCTR  R1,0                R1=X L'(FIND STRING)                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),OSVFDTXT    MATCH ON TEXT LINE?                          
         BNE   FCT390                                                           
*                                                                               
         TM    OSVFCFLG,OSVFCCHQ   CHANGE MODE FLAG ON?                         
         BNO   FCT350                                                           
         LA    RE,OSVLLST          RE=A(SAVED LINE INDEX LIST)                  
         LR    R1,R2                                                            
         SR    R1,RE                                                            
         LA    R1,1(R1)                                                         
         STC   R1,CHGLINOF         R1=(LINE NUMBER OF MATCH)                    
         LA    RE,WORKTTXT         RE=A(UPPER CASE LINE)                        
         LR    R1,RF               R1=A(FIRST CHAR OF MATCH)                    
         SR    R1,RE               R1=(COL NUMBER OF MATCH)                     
         LA    R1,1(R1)                                                         
         STC   R1,CHGCOLOF                                                      
         LA    RE,WORKTTXT+MAXLLNQ RE=A(ORIGINAL LINE)                          
         ST    RE,CHGALINE                                                      
         BAS   RE,CHGTXT           CHANGE THE TEXT                              
         BE    *+12                                                             
         OI    OSVFCFLG,OSVFCTLQ   SET LINE AMENDED                             
         B     FCT355                                                           
         OI    OSVFCFLG,OSVFCAMQ   SET LINE AMENDED                             
         TM    OSVFCPAR,OSVFCALQ   ALL PARAMETER IN USE?                        
         BNO   FCT360                                                           
         MVC   WORKTTXT(MAXLLNQ),WORKTTXT+MAXLLNQ UPDATE UPPERCASE LINE         
         L     R1,ATRUP            GET TRANSLATE TABLE FOR LANG                 
         TR    WORKTTXT(MAXLLNQ),0(R1) CONVERT TO UPPER CASE                    
         B     *+12                                                             
*                                                                               
FCT350   TM    OSVFCPAR,OSVFCALQ   ALL PARAMETER SET?                           
         BNO   FCT360                                                           
         SR    RE,RE                                                            
         ICM   RE,3,OSVFCCNT                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,OSVFCCNT       INCREMENT MATCH COUNT                        
         TM    OSVFCFLG,OSVFCMTQ   PREVIOUS MATCH?                              
         BO    FCT390                                                           
FCT355   MVC   OSVFCSTL,OSVSTLIN   SAVE CURRENT SCREEN START LINE               
         MVC   OSVFCSTC,OSVSTCOL   SAVE CURRENT SCREEN START COL                
         MVC   OSVFCPOF,OSVPOFST   SAVE CURRENT SCREEN PARA OFFSET              
         MVC   OSVFCPRA,OSVPARA    SAVE CURRENT SCREEN PARA INDEX               
FCT360   BAS   RE,SETLC            SET NEXT START LINE AND COLUMN               
         TM    OSVFCFLG,OSVFCTLQ   LINE TOO LONG FOR AMEND?                     
         BO    FCT510                                                           
         OI    OSVFCFLG,OSVFCMTQ   SET MATCH FOUND FLAG ON                      
*                                                                               
FCT370   TM    OSVFCPAR,OSVFCALQ                                                
         BNO   FCTX                                                             
*                                                                               
FCT390   CLI   OSVFCCOL,0          COLUMN PARAMETER SET?                        
         BNE   FCT410                                                           
         SR    R1,R1                                                            
         IC    R1,OSVFDLN          R1=L'(FIND STRING)                           
         TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER SET?                      
         BNO   FCT400                                                           
         BCTR  RF,0                BUMP TO PREVIOUS CHAR IN LINE                
         LA    R0,WORKTTXT         R0=(START OF LINE)                           
         CR    RF,R0               IF START OF LINE REACHED                     
         BL    FCT410              THEN GET PREVIOUS LINE                       
         B     FCT280                                                           
*                                                                               
FCT400   SR    R1,R1                                                            
         IC    R1,OSVFDLN          R1=L'(FIND STRING)                           
         LA    RF,1(RF)            BUMP TO NEXT CHAR IN LINE                    
         LA    RE,0(R1,RF)                                                      
         LA    R0,WORKTTXT                                                      
         SR    RE,R0                                                            
         CLM   RE,1,FFTDLEN        END OF LINE REACHED?                         
         BNH   FCT280                                                           
         DROP  RF,R4                                                            
*                                                                               
FCT410   TM    OSVFCFLG,OSVFCAMQ   LINE AMENDED?                                
         BNO   FCT420                                                           
         NI    OSVFCFLG,X'FF'-OSVFCAMQ                                          
         MVI   LRMODE,LRCHANGE     CHANGE RECORD                                
         LA    RE,OSVLLST                                                       
         LR    R1,R2                                                            
         SR    R1,RE                                                            
         LA    R1,1(R1)                                                         
         STC   R1,LRLINE           R1=(LINE NUMBER)                             
         MVC   LRPARA,OSVPARA      PARAGRAPH NUMBER                             
         MVI   LRCOLUMN,1          COLUMN                                       
         MVI   LRLINELN,MAXLLNQ    LINE LENGTH                                  
         LA    RE,WORKTTXT+MAXLLNQ                                              
         STCM  RE,15,LRALINE       RE=A(LINE)                                   
         GOTO1 ALINEREC                                                         
*                                                                               
FCT420   TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER?                          
         BNO   FCT430                                                           
         SH    R2,=H'1'            BUMP TO PREV LINE INDEX                      
         LA    RE,OSVLLST          RE=A(LINE INDEX LIST)                        
         CR    R2,RE                                                            
         BL    FCT440              GET PREVIOUS PARA                            
         B     FCT170                                                           
*                                                                               
FCT430   LA    R2,1(R2)            BUMP TO NEXT LINE INDEX                      
         SR    RE,RE                                                            
         IC    RE,OSVLACTV                                                      
         LA    RE,OSVLLST-1(RE)                                                 
         CR    R2,RE               IF NO MORE LINES GET NEXT PARAGRAPH          
         BH    FCT450                                                           
         B     FCT170                                                           
*                                  GET PREVIOUS PARA                            
FCT440   SR    RF,RF                                                            
         IC    RF,OSVPOFST                                                      
         CH    RF,=H'1'            FIRST PARA?                                  
         BE    FCT490                                                           
         BCTR  RF,0                                                             
         B     FCT460                                                           
*                                  GET NEXT PARA                                
FCT450   SR    RF,RF                                                            
         IC    RF,OSVPOFST                                                      
         CLM   RF,1,OSVPACTV       LAST PARA?                                   
         BE    FCT470                                                           
         LA    RF,1(RF)                                                         
FCT460   STC   RF,OSVPOFST                                                      
         LA    R1,OSVPOFST                                                      
         L     RF,AGPARA                                                        
         BASR  RE,RF               GET PARAGRAPH                                
         LA    R2,OSVLLST          R2=A(FIRST LINE INDEX)                       
         TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER SET?                      
         BNO   FCT170                                                           
         SR    R2,R2                                                            
         IC    R2,OSVLACTV                                                      
         LA    R2,OSVLLST-1(R2)    R2=A(LAST LINE INDEX)                        
         B     FCT170                                                           
*                                                                               
FCT470   CLI   OSVFCSCP,1          IF SEARCH STARTED AT PARAGRAPH 1             
         BNE   FCT480                                                           
         CLI   OSVFCSCL,1          AND LINE 1                                   
         BNE   FCT480                                                           
         CLI   OSVFCSCC,1          AND COLUMN 1                                 
         BE    FCT510              THEN WHOLE OF TEXT SCANNED                   
FCT480   OI    OSVFCFLG,OSVFCBTQ   ELSE SET BOTTOM OF TEXT REACHED              
         B     FCT510                                                           
*                                                                               
FCT490   CLC   OSVFCSCP,OSVPACTV   IF SEARCH STARTED  AT LAST PARA              
         BNE   FCT500                                                           
         CLC   OSVFCSCL,OSVFCLTL   AND LAST LINE                                
         BNE   FCT500                                                           
         CLI   OSVFCSCC,MAXLLNQ    AND LAST COLUMN                              
         BE    FCT510              THEN WHOLE OF TEXT SCANNED                   
FCT500   OI    OSVFCFLG,OSVFCTPQ   ELSE SET TOP OF TEXT REACHED                 
*                                                                               
FCT510   CLC   OSVPOFST,OSVFCPOF   SAME PARA AS PREVIOUS DISPLAY?               
         BNE   FCT520                                                           
         TM    OSVFCFLG,OSVFCTLQ   LINE TOO LONG FOR AMEND?                     
         BO    FCT530                                                           
         B     FCTX                                                             
FCT520   MVC   OSVPOFST,OSVFCPOF                                                
         LA    R1,OSVFCPOF                                                      
         L     RF,AGPARA                                                        
         BASR  RE,RF               REDISPLAY START PARA                         
FCT530   MVC   OSVSTLIN,OSVFCSTL   RESET LINE/COLUMN STARTS                     
         MVC   OSVSTCOL,OSVFCSTC                                                
*                                                                               
FCTX     CR    RB,RB                                                            
         B     EXIT2                                                            
FCTERRX  LTR   RB,RB                                                            
         B     EXIT2                                                            
         EJECT                                                                  
***********************************************************************         
* SET LINE NUMBER, COLUMN NUMBER AND CURSOR POSITION FOR NEXT DISPLAY *         
* NTRY R2=A(INDEX OF LINE CONTAINING MATCH)                           *         
***********************************************************************         
         SPACE 1                                                                
SETLC    NTR1                                                                   
*                                  SET LINE NUMBER FOR NEXT DISP                
         LA    RE,OSVLLST-1        RE=A(SAVED LINE INDX LST - 1)                
         SR    R2,RE               R2=(LINE NUMBER OF MATCH)                    
         CLC   OSVPARA,OSVFCPRA    SAME PARAGRAPH AS PREV DISP?                 
         BNE   SETLC10                                                          
         CLM   R2,1,OSVFCSTL       MATCHED LINE BEFORE CURRENT SCREEN?          
         BL    SETLC10                                                          
         SR    RE,RE                                                            
         IC    RE,OSVFCSTL                                                      
         LA    RE,MAXSLINQ-1(RE)                                                
         CR    R2,RE               MATCHED LINE AFTER CURRENT SCREEN?           
         BH    SETLC10                                                          
         MVC   OSVSTLIN,OSVFCSTL   NEXT START LINE SAME AS PREVIOUS             
         SR    R0,R0                                                            
         IC    R0,OSVSTLIN                                                      
         SR    R2,R0               R2=(SCREEN LINE NUMBER)                      
         B     SETLC30                                                          
*                                                                               
SETLC10  CH    R2,=H'1'            START OF TEXT?                               
         BE    SETLC20                                                          
         BCTR  R2,0                DISPLAY FROM LINE ABOVE MATCH LINE           
         STC   R2,OSVSTLIN                                                      
         LA    R2,1                                                             
         B     SETLC30                                                          
SETLC20  STC   R2,OSVSTLIN                                                      
         SR    R2,R2                                                            
SETLC30  MH    R2,=Y(SLLNQ)                                                     
         LA    R2,EDTTXT1H(R2)                                                  
         SR    R2,RA                                                            
         L     RE,AINP             RE=A(TIOB)                                   
         STCM  R2,3,TIOBCURD-TIOBD(RE) SAVE DISPLACEMENT TO TEXT FIELD          
*                                  SET COLUMN NUMBER FOR NEXT DISP              
         TM    OSVFCFLG,OSVFCTLQ   LINE TOO LONG TO CHANGE?                     
         BNO   *+10                                                             
         SR    R0,R0               SET CURSOR AT START OF LINE                  
         B     SETLC80                                                          
         SR    R1,R1                                                            
         IC    R1,OSVFDLN          R1=L'(FIND STRING)                           
         TM    OSVFCFLG,OSVFCCHQ   CHANGE MODE?                                 
         BNO   *+12                                                             
         IC    R1,OSVCHLN                                                       
         LA    R1,1(R1)            R1=L'(CHANGE STRING + 1)                     
         LA    RE,WORKTTXT         RE=A(UPPER CASE TEXT LINE)                   
         SR    RF,RE                                                            
         LA    RF,1(RF)            RF=(START COLUMN OF MATCHED STRING)          
         LR    R0,RF               R0=RF                                        
         CLC   OSVPARA,OSVFCPRA    SAME PARAGRAPH AS PREV DISP?                 
         BNE   SETLC40                                                          
         CLM   RF,1,OSVFCSTC       IF MATCHED STRING WITHIN CURR COLS           
         BL    SETLC40                                                          
         SR    RE,RE                                                            
         IC    RE,OSVFCSTC                                                      
         LA    RE,L'EDTTXT1(RE)                                                 
         LA    RF,0(RF,R1)                                                      
         CR    RF,RE                                                            
         BH    SETLC50                                                          
         MVC   OSVSTCOL,OSVFCSTC   DO NOT CHANGE START COLUMN                   
         SR    RF,RF                                                            
         IC    RF,OSVSTCOL         RF=(START COLUMN)                            
         B     SETLC60                                                          
SETLC40  LA    RF,0(RF,R1)                                                      
SETLC50  CH    RF,=Y(MAXLLNQ+1)    MATCHED/CHANGED STRING WILL FIT?             
         BNH   *+8                                                              
         LA    RF,MAXLLNQ+1                                                     
         SH    RF,=Y(L'EDTTXT1)    FIND START COLUMN                            
         BP    *+8                                                              
         LA    RF,1                                                             
         STC   RF,OSVSTCOL                                                      
SETLC60  SR    R0,RF                                                            
         TM    OSVFCFLG,OSVFCCHQ   IF CHNGE CURS GOES AFTER MATCH               
         BNO   SETLC80                                                          
         TM    OSVFCPAR,OSVFCPVQ   PREVIOUS PARAMETER USED?                     
         BNO   SETLC70                                                          
         SH    R0,=H'1'                                                         
         BNM   SETLC80                                                          
         SR    R0,R0                                                            
         B     SETLC80                                                          
SETLC70  AR    R0,R1                                                            
         BCTR  R0,0                                                             
         CH    R0,=Y(L'EDTTXT1-1)                                               
         BNH   *+8                                                              
         LA    R0,L'EDTTXT1-1                                                   
SETLC80  L     RE,AINP             RE=A(TIOB)                                   
         STC   R0,TIOBCURI-TIOBD(RE) SET FIELD INDEX OF CURSOR                  
SETLCX   B     EXIT2                                                            
         EJECT                                                                  
***********************************************************************         
* CHANGE MATCHED STRING                                               *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* CHGPARMS DS    0X                CHANGE ROUTINE PARAMETERS          *         
* CHGALINE DS    A                 ADDRESS OF LINE                    *         
* CHGLINOF DS    X                 OFFSET TO LINE                     *         
* CHGCOLOF DS    X                 OFFSET TO COL                      *         
* OSVFDLN  MUST CONTAIN LENGTH OF MATCHED STRING                      *         
* OSVCHLN  MUST CONTAIN LENGTH OF NEW STRING                          *         
* OSVCHTXT MUST CONTAIN NEW STRING                                    *         
***********************************************************************         
         SPACE 1                                                                
CHGTXT   NTR1                                                                   
         ICM   R2,15,CHGALINE      R2=A(LINE TO BE CHANGED)                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   OSVCHLN,OSVFDLN     CHANGE STRING LEN > FIND STRING LEN?         
         BNH   CHGT10                                                           
         LA    RE,MAXLLNQ-1                                                     
         LA    RF,0(RE,R2)         RF=A(LAST CHAR IN LINE)                      
         CLI   0(RF),C' '          NON SPACE CHAR FOUND?                        
         BH    *+8                                                              
         BCT   RE,*-12                                                          
         LA    RE,1(RE)            RE=L'(LINE TO LAST NON SPACE CHAR)           
         SR    R1,R1                                                            
         IC    R1,OSVCHLN          ADD CHANGE STRING LENGTH                     
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,OSVFDLN          SUBTRACT MATCHED STRING LENGTH               
         SR    RE,R1                                                            
         CH    RE,=Y(MAXLLNQ)                                                   
         BH    CHGTERRX            NEW LINE WOULD BE TOO LONG                   
*                                                                               
CHGT10   LA    R3,FRMLLST          R3=A(AREA TO BE USED FOR NEW LINE)           
         SR    RF,RF                                                            
         ICM   RF,1,CHGCOLOF       RF=L'(LINE BEFORE MATCH STRING)              
         SH    RF,=H'2'                                                         
         BM    CHGT20                                                           
         EX    RF,*+4                                                           
         MVC   0(0,R3),0(R2)       GET LINE UPTO MATCH                          
         LA    R3,1(R3,RF)         R3=A(NEXT FREE CHAR IN NEW LINE)             
         LA    R2,1(R2,RF)                                                      
CHGT20   SR    RF,RF                                                            
         IC    RF,OSVCHLN          RF=L'(CHANGE TEXT)                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R3),OSVCHTXT    GET CHANGE STRING                            
         LA    R3,1(R3,RF)         R3=A(NEXT FREE CHAR IN NEW LINE)             
         SR    RE,RE                                                            
         IC    RE,OSVFDLN          RF=L'(CHANGE TEXT)                           
         LA    R2,0(R2,RE)         R2=A(LINE AFTER MATCH)                       
         LA    RF,FRMLLST+MAXLLNQ                                               
         SR    RF,R3               RF=(MAX REMAINING LEN FOR NEW LINE)          
         BZ    CHGT30                                                           
         ICM   RE,15,CHGALINE                                                   
         LA    RE,MAXLLNQ(RE)                                                   
         SR    RE,R2               RE=(REMAINING LEN FOR OLD LINE)              
         BZ    CHGT30                                                           
         CR    RE,RF                                                            
         BNL   *+6                                                              
         LR    RF,RE                                                            
         EX    RF,*+4                                                           
         MVC   0(0,R3),0(R2)       GET REST OF LINE                             
CHGT30   DS    0H                                                               
         TM    OSVFCPAR,OSVFCALQ   ALL PARAMETER IN USE?                        
         BO    CHGT40                                                           
         MVI   LRMODE,LRCHANGE     CHANGE RECORD                                
         MVC   LRLINE,CHGLINOF     LINE NUMBER                                  
         MVC   LRPARA,OSVPARA      PARAGRAPH                                    
         MVI   LRCOLUMN,1          COLUMN START                                 
         MVI   LRLINELN,MAXLLNQ    L'(LINE)                                     
         LA    RE,FRMLLST                                                       
         STCM  RE,15,LRALINE                                                    
         L     RF,ALINEREC         RF=A(LINE)                                   
         BASR  RE,RF                                                            
         B     CHGTX                                                            
CHGT40   ICM   R2,15,CHGALINE                                                   
         MVC   0(MAXLLNQ,R2),FRMLLST UPDATE ORIGINAL LINE                       
*                                                                               
CHGTX    CR    RB,RB                                                            
         B     EXIT2                                                            
CHGTERRX LTR   RB,RB                                                            
         B     EXIT2                                                            
EXIT2    XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS A STANDARD COMMENT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING SCTABD,R3                                                        
STDCOM   CSECT                                                                  
         NMODL 0,**STDC**,CLEAR=YES                                             
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRK,RC                                                       
         TM    SC1IND,SC1IPROC     TEST PROCESS TABLE CALL                      
         BO    STDCOM50                                                         
         TM    SC1IND,SC1IBUIL     TEST BUILD TABLE CALL                        
         BO    *+6                                                              
         DC    H'0'                UNKNOWN CALL                                 
         TM    SC1IND,SC1IRCRS     TEST RECURSIVE CALL                          
         BO    STDCOM04                                                         
*                                                                               
         L     R3,BOSVALS2         FIND NEXT TABLE POSITION                     
         LA    R0,SCTABN                                                        
         CLI   SCTLIN,EOT                                                       
         BE    STDCOM04                                                         
         LA    R3,SCTABL(R3)                                                    
         BCT   R0,*-12                                                          
         MVC   FVMSGNO,=AL2(AE$TMIDT)                                           
         B     STDCOME                                                          
*                                                                               
         USING SCMRECD,R2                                                       
STDCOM04 LA    R2,IOKEY            R2=A(COMMENT RECORD KEY)                     
         XC    SCMKEY,SCMKEY                                                    
         MVI   SCMKTYP,SCMKTYPQ    RECORD TYPE                                  
         MVC   SCMKCPY,CUABIN      COMPANY CODE                                 
         MVC   SCMKCODE,BCSPACES                                                
         LA    RE,L'SCMKCODE-1(R1)                                              
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R1                                                            
         BM    STDCOM08                                                         
         LA    RF,L'SCMKCODE-1                                                  
         SR    RF,RE                                                            
         LA    RF,SCMKCODE(RF)                                                  
         EX    RE,*+4                                                           
         MVC   0(0,RF),0(R1)       STANDARD COMMENT NUMBER                      
         MVC   FVXTRA(1),MY@NRTV                                                
         MVI   FVXTRA+1,C'='                                                    
         MVC   FVXTRA+2(L'SCMKCODE),SCMKCODE                                    
STDCOM08 GOTO1 AIO,IO2+IOACCMST+IOREAD                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECNF)                                           
         B     STDCOME                                                          
         MVC   FVXTRA,BCSPACES                                                  
         L     R2,AIO2             R2=A(STANDARD COMMENT RECORD)                
         TM    SC1IND,SC1IRCRS     TEST RECURSIVE CALL                          
         BO    STDCOM12            DON'T SET TABLE VALUES                       
         MVC   SCTCOD,SCMKCODE     SET CODE FROM KEY                            
         MVC   SCTDSP,SCDISP       SET DISPLACEMENT                             
         SR    RE,RE                                                            
         IC    RE,NXTLOFF          TAKE KNOWN START LINE                        
         SR    RF,RF                                                            
         IC    RF,SCLTOT           ADD TOTAL LINES ADDED THIS TIME              
         AR    RF,RE                                                            
         STC   RF,SCTLIN           SET SCREEN LINE NUMBER                       
STDCOM12 LA    R2,SCMRFST          R2=A(FIRST ELEMENT)                          
*                                                                               
         USING SCMELD,R2                                                        
STDCOM16 CLI   SCMEL,EOR                                                        
         BE    STDCOM32                                                         
         CLI   SCMEL,SCMELQ        TEST STANDARD COMMENT                        
         BE    STDCOM20                                                         
STDCOM18 SR    R0,R0                                                            
         IC    R0,SCMLN                                                         
         AR    R2,R0                                                            
         B     STDCOM16                                                         
*                                                                               
STDCOM20 TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    STDCOM22                                                         
         MVC   BCHALF(1),MY@NRTV                                                
         MVI   BCHALF+1,C'='                                                    
         CLC   BCHALF,SCMCODE      CODE IS N=123456                             
         BNE   STDCOM28                                                         
STDCOM22 TM    SC1IND,SC1IRCRS     TEST RECURSING                               
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMLSC)                                           
         B     STDCOME                                                          
         L     R0,AIO3             SAVE CURRENT COMMENT RECORD                  
         L     RE,AIO2                                                          
         LA    R1,IOAREALN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   BCDUB,BCSPACES                                                   
         SR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         LH    R0,=Y(SCMLN1Q+1)                                                 
         TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    *+8                                                              
         LH    R0,=Y(SCMLN1Q+3)                                                 
         SR    R1,R0                                                            
         BM    STDCOM24                                                         
         CH    R1,=Y(L'BCDUB-1)                                                 
         BNH   *+8                                                              
         LH    R1,=Y(L'BCDUB-1)                                                 
         LA    RF,SCMCODE                                                       
         TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    *+8                                                              
         LA    RF,SCMCODE+2                                                     
         EX    R1,*+4                                                           
         MVC   BCDUB(0),0(RF)                                                   
STDCOM24 OI    SC1IND,SC1IRCRS     SET RECURSING                                
         GOTO1 ASTDCOM,BCDUB       RECURSE TO READ FOR THIS CODE                
         BNE   STDCOMX             MESSAGE WILL BE SET                          
         NI    SC1IND,X'FF'-(SC1IRCRS)                                          
         L     R0,AIO2                                                          
         L     RE,AIO3                                                          
         LA    R1,IOAREALN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               RESTORE SAVED RECORD                         
         B     STDCOM18            CONTINUE LOOKING FOR ELEMENTS                
*                                                                               
STDCOM28 CLI   SCTNUM,X'FF'        TEST TOO MANY COMMENT LINES                  
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMIDT)                                           
         B     STDCOME                                                          
         SR    R0,R0                                                            
         IC    R0,SCMLN                                                         
         SH    R0,=Y(SCMLN1Q)                                                   
         SR    RF,RF                                                            
         IC    RF,SCTDSP                                                        
         AR    RF,R0                                                            
         CH    RF,=Y(MAXLLNQ)                                                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECTB)                                           
         B     STDCOME                                                          
         SR    RF,RF                                                            
         IC    RF,SCTNUM           BUMP NUMBER OF COMMENT LINES                 
         LA    RF,1(RF)                                                         
         STC   RF,SCTNUM           BUMP BLOCK TOTAL IN TABLE                    
         B     STDCOM18                                                         
*                                                                               
STDCOM32 TM    SC1IND,SC1IRCRS     TEST RECURSING                               
         BO    STDCOM36                                                         
         SR    RF,RF                                                            
         ICM   RF,1,SCTNUM         TOTAL LINES IN THIS BLOCK                    
         BZ    STDCOM36                                                         
         BCTR  RF,0                FIRST LINE ALREADY EXISTS                    
         SR    R0,R0                                                            
         IC    R0,SCLTOT           TOTAL LINES ADDED SO FAR                     
         AR    RF,R0                                                            
         STC   RF,SCLTOT           UPDATE TOTAL LINES ADDED SO FAR              
*                                                                               
STDCOM36 B     STDCOMY                                                          
*                                                                               
STDCOM50 TM    SC1IND,SC1IRCRS     TEST RECURSIVE CALL                          
         BO    STDCOM54                                                         
         L     R3,BOSVALS2         R3=A(STANDARD COMMENT TABLE)                 
         MVC   LRCOLUMN,OSVSTCOL   SET START COLUMN                             
         MVC   LRPARA,OSVPARA      SET PARAGRAPH                                
         MVI   LRMODE,LRMERGE      FIRST LINE IS A MERGE                        
*                                                                               
STDCOM52 CLI   SCTABD,EOR                                                       
         BE    STDCOM80                                                         
         MVC   LRLINE,SCTLIN       SCREEN LINE NUMBER                           
         LA    R1,SCTCOD                                                        
*                                                                               
         USING SCMRECD,R2                                                       
STDCOM54 LA    R2,IOKEY            R2=A(COMMENT RECORD KEY)                     
         XC    SCMKEY,SCMKEY                                                    
         MVI   SCMKTYP,SCMKTYPQ    RECORD TYPE                                  
         MVC   SCMKCPY,CUABIN      COMPANY CODE                                 
         MVC   SCMKCODE,0(R1)                                                   
         GOTO1 AIO,IO2+IOACCMST+IOREAD                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2             R2=A(STANDARD COMMENT RECORD)                
         LA    R2,SCMRFST          R2=A(FIRST ELEMENT)                          
*                                                                               
         USING SCMELD,R2                                                        
STDCOM58 CLI   SCMEL,EOR                                                        
         BE    STDCOM74                                                         
         CLI   SCMEL,SCMELQ        TEST STANDARD COMMENT                        
         BE    STDCOM62                                                         
STDCOM60 SR    R0,R0                                                            
         IC    R0,SCMLN                                                         
         AR    R2,R0                                                            
         B     STDCOM58                                                         
*                                                                               
STDCOM62 TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    STDCOM64                                                         
         MVC   BCHALF(1),MY@NRTV                                                
         MVI   BCHALF+1,C'='                                                    
         CLC   BCHALF,SCMCODE      CODE IS N=123456                             
         BNE   STDCOM70                                                         
STDCOM64 TM    SC1IND,SC1IRCRS     TEST RECURSING                               
         BZ    *+6                                                              
         DC    H'0'                TOO MANY LEVELS OF EMBEDDING                 
         L     R0,AIO3             SAVE CURRENT COMMENT RECORD                  
         L     RE,AIO2                                                          
         LA    R1,IOAREALN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   BCDUB,BCSPACES                                                   
         SR    RE,RE                                                            
         IC    RE,SCMLN                                                         
         LH    R0,=Y(SCMLN1Q+1)                                                 
         TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    *+8                                                              
         LH    R0,=Y(SCMLN1Q+3)                                                 
         SR    RE,R0                                                            
         BM    STDCOM66                                                         
         CH    RE,=Y(L'BCDUB-1)                                                 
         BNH   *+8                                                              
         LH    RE,=Y(L'BCDUB-1)                                                 
         LH    RF,=Y(L'SCMKCODE-1)                                              
         SR    RF,RE                                                            
         BNM   *+6                                                              
         SR    RF,RF                                                            
         LA    RF,BCDUB(RF)                                                     
         LA    R1,SCMCODE                                                       
         TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    *+8                                                              
         LA    R1,SCMCODE+2                                                     
         EX    RE,*+4                                                           
         MVC   0(0,RF),0(R1)       STANDARD COMMENT NUMBER                      
STDCOM66 OI    SC1IND,SC1IRCRS     SET RECURSING                                
         GOTO1 ASTDCOM,BCDUB       RECURSE TO READ FOR THIS CODE                
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    SC1IND,X'FF'-(SC1IRCRS)                                          
         L     R0,AIO2                                                          
         L     RE,AIO3                                                          
         LA    R1,IOAREALN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               RESTORE SAVED RECORD                         
         B     STDCOM60            CONTINUE LOOKING FOR ELEMENTS                
*                                                                               
STDCOM70 MVC   WORKTTXT(MAXLLNQ),BCSPACES                                       
         SR    RE,RE                                                            
         IC    RE,SCTDSP                                                        
         LA    RF,WORKTTXT(RE)                                                  
         SR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         SH    R1,=Y(SCMLN1Q)                                                   
         AR    RE,R1                                                            
         STC   RE,LRLINELN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),SCMCODE                                                  
         LA    RF,WORKTTXT                                                      
         ST    RF,LRALINE                                                       
*                                                                               
         GOTO1 ALINEREC                                                         
*                                                                               
         MVI   LRMODE,LRADDQ       SUBSEQUENT LINES ARE ADDS                    
         SR    RF,RF                                                            
         IC    RF,LRLINE                                                        
         LA    RF,1(RF)                                                         
         STC   RF,LRLINE           SET NEXT LINE                                
         B     STDCOM60            CONTINUE LOOKING FOR ELEMENTS                
*                                                                               
STDCOM74 TM    SC1IND,SC1IRCRS     TEST RECURSING                               
         BO    STDCOMY                                                          
         LA    R3,SCTABL(R3)                                                    
         MVI   LRMODE,LRMERGE      NEXT ENTRY FIRST LINE IS A MERGE             
         B     STDCOM52                                                         
*                                                                               
STDCOM80 B     STDCOMY                                                          
*                                                                               
STDCOMY  CR    RB,RB               SET CC EQU - OK                              
         B     STDCOMX                                                          
*                                                                               
STDCOME  L     R0,BOSVALS2         ERROR PROCEDURE                              
         LA    R1,SCTABN*SCTABL                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR STANDARD COMMENT TABLE                 
         LTR   RB,RB               SET CC NEQ                                   
         B     STDCOMX                                                          
*                                                                               
STDCOMX  XIT1  ,                                                                
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL EQUATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
MAXSLINQ EQU   ((EDTLINL-EDTLIN1)/(EDTLIN2-EDTLIN1))+1 MAX LINE ON SCRN         
MAXPLINQ EQU   200                 MAX LINES IN PARAGRAPH                       
TXTSTLIN EQU   7                   SCREEN LINE NUMBER FOR 1ST TEXT LINE         
TXTENLIN EQU   22                  SCREEN LINE NUMBER FOR LST TEXT LINE         
PSCRNLN  EQU   80                  WIDTH OF PHYSICAL SCREEN LINE                
INSERTQ  EQU   C'.'                INSERT CHARACTER                             
PFKFINDQ EQU   1                   FIND TEXT STRING PFKEY                       
PFKCHGQ  EQU   2                   CHANGE TEXT STRING PFKEY                     
PFKADDP  EQU   3                   NEW PARAGRAPH PFKEY                          
PFKPREVP EQU   5                   PREVIOUS PARAGRAPH PFKEY                     
PFKNEXTP EQU   6                   NEXT PARAGRAPH PFKEY                         
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
       ++INCLUDE ACCLBWORKC                                                     
         EJECT                                                                  
TWAD     DSECT                     OVERLAY SAVED STORAGE                        
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF5D                                                       
         EJECT                                                                  
SLINED   DSECT                     DSECT FOR SCREEN LINE                        
SLLCM1H  DS    CL(L'EDTLCM1H)      LINE COMMAND HEADER                          
SLLCM1   DS    CL(L'EDTLCM1)       LINE COMMAND FIELD                           
         ORG   SLINED+EDTTXT1H-EDTLCM1H                                         
SLTXT1H  DS    CL(L'EDTTXT1H)      TEXT LINE HEADER                             
SLTXT1   DS    CL(L'EDTTXT1)       TEXT LINE FIELD                              
         ORG   SLINED+EDTLIN1H-EDTLCM1H                                         
SLLIN1H  DS    CL(L'EDTLIN1H)      LINE NUMBER HEADER                           
SLLIN1   DS    CL(L'EDTLIN1)       LINE NUMBER FIELD                            
         ORG   SLINED+EDTLCM2H-EDTLCM1H                                         
SLLNQ    EQU   *-SLINED                                                         
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
LENINSQ  EQU   X'80'               INSERT                                       
LENDELQ  EQU   X'40'               DELETE                                       
LENMOVEQ EQU   X'20'               MOVE                                         
LENCOPYQ EQU   X'10'               COPY                                         
LENREPQ  EQU   X'08'               REPEAT                                       
LENAFTQ  EQU   X'04'               AFTER                                        
LENBEFQ  EQU   X'02'               BEFORE                                       
LENOVERQ EQU   X'01'               OVER                                         
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
CMNHEDQ  EQU   6                   HEADER                                       
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
CPARSDFQ EQU   X'20'               PARAMETER VALID FOR DRAFT BILLS ONLY         
CPARINCM DS    6AL1                INCOMPATIBLE PARAMETER LIST                  
CPARLNQ  EQU   *-CPARTABD          LENGTH OF COMMAND TABLE ENTRY                
         SPACE 1                                                                
OSVALSD  DSECT                     OVERLAY SAVED VALUES                         
OSVMX@OF DS    CL4                 OF                                           
OSVCURD  DS    XL(L'TIOBCURD)      DISPLACEMENT TO CURSOR                       
*                                                                               
OSVEVAL  DS    0X                  LINE EDIT VALUES                             
OSVECPYB DS    XL(BLKLNQ)          COPY BLOCK LIST                              
OSVEMVEB DS    XL(BLKLNQ)          MOVE BLOCK LIST                              
OSVECMPA DS    X                   COPY/MOVE BLOCK PARAGRAPH INDEX              
OSVEDELB DS    XL(16*BLKLNQ)       DELETE BLOCK LIST                            
OSVEREPB DS    XL(16*BLKLNQ)       REPEAT BLOCK                                 
OSVEINSB DS    XL(16*BLKLNQ)       INSERT BLOCK                                 
OSVETOLN DS    0XL(3*BLKLNQ)                                                    
OSVEBFLN DS    XL(BLKLNQ)          TO LINE (BEFORE)                             
OSVEAFLN DS    XL(BLKLNQ)          TO LINE (AFTER)                              
OSVEOVLN DS    XL(BLKLNQ)          TO LINE (OVER)                               
OSVETOAC DS    X                   NUMBER OF ACTIVE LINES IN TO PARA            
OSVETOTY DS    X                   TO TYPE AFTER/BEFORE OR OVER                 
OSVETOPA DS    X                   TO PARAGRAPH                                 
OSVETOPO DS    X                   TO PARAGRAPH OFFSET                          
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
OSVFCPAR DS    X                   FIND/ CHANGE SUB COMMAND LINE TYPES          
OSVFCALQ EQU   X'80'               ALLQ                                         
OSVFCPFQ EQU   X'40'               PREFFIX                                      
OSVFCSFQ EQU   X'20'               SUFFIX                                       
OSVFCPVQ EQU   X'10'               PREVIOUS                                     
OSVFCWDQ EQU   X'08'               WORD                                         
OSVFCFSQ EQU   X'04'               FIRST                                        
OSVFCLSQ EQU   X'02'               LAST                                         
OSVCPAR  DS    X                   COLUMNS SUB COMMAND TYPE                     
OSVCYESQ EQU   X'80'               YES                                          
OSVCNOQ  EQU   X'40'               NO                                           
OSVHPAR  DS    X                   COLUMNS SUB COMMAND TYPE                     
OSVHTIMQ EQU   X'80'               TIME                                         
OSVHCSTQ EQU   X'40'               COST                                         
OSVHNOQ  EQU   X'20'               NO                                           
OSVDPAR  DS    X                   DELETE SUB COMMAND TYPE                      
OSVDALLQ EQU   X'80'               ALL LINES                                    
OSVDPARQ EQU   X'40'               PARAGRAPH                                    
OSVFDVL  DS    0X                                                               
OSVFDLN  DS    X                                                                
OSVFDTXT DS    CL20                                                             
OSVFDLNQ EQU   *-OSVFDVL                                                        
OSVFCCOL DS    X                   FIND/CHANGE COLUMN                           
OSVCHVL  DS    0X                                                               
OSVCHLN  DS    X                                                                
OSVCHTXT DS    CL20                                                             
OSVCHLNQ EQU   *-OSVCHVL                                                        
OSVCLLNQ EQU   *-OSVCLVAL                                                       
*                                                                               
OSVCLFLG DS    X                   COLUMN DISPLAY FLAG                          
OSVCLRLQ EQU   X'80'               DISP RULE LINE (DEFAULT IS 'TEXT')           
OSVCLTMQ EQU   X'40'               DISP TIME HEADLINE                           
OSVCLCTQ EQU   X'20'               DISP COST HEADLINE                           
OSVFCFLG DS    X                   COLUMN DISPLAY FLAG                          
OSVFCTPQ EQU   X'80'               TOP OF TEXT PREVIOUSLY REACHED               
OSVFCBTQ EQU   X'40'               BOTTOM OF TEXT PREVIOUSLY REACHED            
OSVFCMTQ EQU   X'20'               MATCH FOUND                                  
OSVFCIGQ EQU   X'10'               IGNORE FIRST MATCH TEST                      
OSVFCFNQ EQU   X'08'               FIND MODE                                    
OSVFCCHQ EQU   X'04'               CHANGE MODE                                  
OSVFCAMQ EQU   X'02'               LINE AMNEDED                                 
OSVFCTLQ EQU   X'01'               LINE TO LONG TO AMEND                        
OSVFCCNT DS    XL2                 NUMBER OF MATCHES FOUND                      
OSVFCLST DS    0XL2                LAST LINE/COL USED FOR PREV PARAM            
OSVFCLTL DS    X                   FIRST SEARCH PARAGRAPH OFFSET                
OSVFCLTC DS    X                   FIRST SEARCH PARAGRAPH OFFSET                
OSVFCSCH DS    0XL3                SEARCH START ATTRIBUTES                      
OSVFCSCP DS    X                   FIRST SEARCH PARAGRAPH OFFSET                
OSVFCSCL DS    X                   FIRST SEARCH LINE                            
OSVFCSCC DS    X                   FIRST SEARCH COLIMN                          
OSVFCPOF DS    XL(L'OSVPOFST)      SAVE CURRENT SCREEN PARA OFFSET              
OSVFCPRA DS    XL(L'OSVPARA)       SAVE CURRENT SCREEN PARA INDEX               
OSVFCSTL DS    XL(L'OSVSTLIN)      SAVE CURRENT SCREEN START LINE               
OSVFCSTC DS    XL(L'OSVSTCOL)      SAVE CURRENT SCREEN START COL                
OSVFCCRS DS    XL(L'TIOBCURS)      SAVE CURRENT SCREEN CURSOR ADDRESS           
OSVJOB   DS    CL(L'TRNKACT)       JOB                                          
OSVSEQ   DS    XL(L'PBRKSEQ)       SEQUENCE# (COMPLEMENT)                       
OSVPARA  DS    XL(L'PBRKPARA)      PARAGRAPH#                                   
OSVPOFST DS    XL(L'PBRKPARA)      PARAGRAPH# OFFSET                            
OSVLVALS DS    0X                  LINE VALUES                                  
OSVLHIGH DS    XL(L'NDXHIGH)       HIGHEST LINE INDEX RECORD                    
OSVLACTV DS    XL(L'NDXACTV)       HIGHEST ACTIVE LINE INDEX                    
OSVLLST  DS    XL200               LINE INDEX LIST                              
OSVLLNQ  EQU   *-OSVLVALS          LENGTH OF LINE VALUES                        
OSVPVALS DS    0X                  PARAGRAPH VALUES                             
OSVPHIGH DS    XL(L'NDXHIGH)       HIGHEST PARAGRAPH INDEX                      
OSVPACTV DS    XL(L'NDXACTV)       HIGHEST ACTIVE PARAGRAPH LINE INDEX          
OSVPLST  DS    XL200               PARAGRAPH INDEX LIST                         
OSVPLNQ  EQU   *-OSVPVALS          LENGTH OF PARAGRAPH VALUES                   
OSVBILNO DS    CL6                                                              
OSVBFORM DS    X                   BILL FORMAT                                  
OSVREPWD DS    XL(L'BOFMAXWD)      MAXIMUM PAGE WIDTH OF BILL                   
OSVPIND  DS    XL(L'PBRPIND)       SAVED PASSIVE INDICATOR (PBRPIND)            
OSVBLANG DS    X                   BILL FORMAT LANGUAGE                         
OSVALSND DS    XL(OSVALSL-(*-OSVALSD)) N/D                                      
         DS    0H                                                               
         SPACE 1                                                                
OSSAVED  DSECT                                                                  
OSSVDSLW DS    0C                                                               
LC@TEXT  DS    CL(L'EDTBAN)                                                     
OSSAVEND DS    XL(OSSAVEL-(*-OSSAVED)) N/D                                      
         DS    0H                                                               
         SPACE 1                                                                
SCTABD   DSECT                     STANDARD COMMENT TABLE                       
SCTLIN   DS    XL(L'LRLINE)        START LINE NUMBER                            
SCTDSP   DS    XL1                 START DISPLACEMENT INTO LINE                 
SCTNUM   DS    XL1                 NUMBER OF LINES IN COMMENT                   
SCTCOD   DS    XL(L'SCMKCODE)      STANDARD COMMENT CODE                        
SCTABL   EQU   *-SCTABD                                                         
SCTABN   EQU   16                  MAXIMUM TABLE ENTRIES                        
         DS    XL(OSVALS2L-(SCTABL*SCTABN))                                     
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
ASTDCOM  DS    A                   BUILD/PROCESS STANDARD COMMENT TABLE         
ATRUPTAB DS    A                   TRANSLATE TABLE                              
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
UNUSEINS DS    XL(MAXSLINQ+1)                                                   
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
LRMERGE  EQU   X'10'               MERGE EXISTING RECORD     (ACTIVE)           
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
LSCIBLKQ EQU   X'40'               STANDARD COMMENT IN BLOCK                    
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
FRMLLST  DS    CL200                                                            
BILLHED  DS    CL132                                                            
FFMTBLK  DS    XL(L'FBLK)          FORMAT RECORD BLOCK                          
*                                                                               
MY@NRTV  DS    CL1                                                              
*                                                                               
SCVALS   DS    0X                  STANDARD COMMENT ROUTINE VALUES              
SC1IND   DS    X                   INDICATOR BYTE - 1                           
SC1IBUIL EQU   X'80'               READ COMMENT AND BUILD ENTRY                 
SC1IPROC EQU   X'40'               PROCESS TABLE ENTRIES                        
SC1IRCRS EQU   X'01'               RECURSING FOR EMBEDDED COMMENT               
SCDISP   DS    X                   DISPLACEMENT TO STANDARD COMMENT             
SCLTOT   DS    X                   TOTAL LINES TO BE ADDED IN STDCOM            
SCVALSL  EQU   *-SCVALS                                                         
         SPACE 1                                                                
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACCLB0A   08/16/00'                                      
         END                                                                    
