*          DATA SET ACCLB1E    AT LEVEL 023 AS OF 08/16/00                      
*PHASE T6211EA                                                                  
*&&      SET   NOP=N                                                            
CLB1E    TITLE '- BILL PROGRAM VALIDATE BILLING FIELDS'                         
CLB1E    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CB1E**,RR=RE,R8                                              
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
         USING MIXLST,LSMIXLST                                                  
         USING PBLKD,R7                                                         
         USING POSTVALS,PBPOSTV                                                 
         L     RC,AOVERWRK                                                      
         USING BWORKD,RC                                                        
         ST    RE,RELO                                                          
*                                                                               
         LA    R1,ROUTS                                                         
         LA    R0,ROUTSN                                                        
MAIN02   L     RF,0(R1)                                                         
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BE    MAIN08                                                           
         CLI   PBMODE,PBDISQ                                                    
         BNE   EXITN                                                            
MAIN08   LA    R1,L'ROUTS(R1)                                                   
         BCT   R0,MAIN02                                                        
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BILL DRAFT NUMBER FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALDRA   NTR1  ,                                                                
         TM    PBINDS1,PBILDRA     TEST USE LAST BILL #                         
         BZ    VDRA02                                                           
         GOTO1 LSTDRA                                                           
         BNE   EXIT                                                             
*                                                                               
VDRA02   ICM   R3,15,PBADRAH       TEST HAVE FIELD                              
         BNZ   VDRA10                                                           
         CLC   PBDRAFT#,BCSPACES   NO - TEST HAVE BILL #                        
         BH    *+12                                                             
         NI    PBTYPES,FF-PBBILQ                                                
         B     EXITY                                                            
         BAS   RE,GETDRA           GET DRAFT BILL                               
         B     EXIT                                                             
*                                                                               
         USING FHD,R3                                                           
VDRA10   TM    FHII,FHIIVA                                                      
         BO    *+8                                                              
         OI    PBINDS1,PBINDRA     SET NEW DRAFT BILL                           
*                                                                               
         CLI   PBMODE,PBDISQ       TEST DISPLAY MODE                            
         BNE   VDRA12                                                           
         CLC   PBDRAFT#,BCSPACES   TEST GIVEN BILL#                             
         BNH   VALDRAY                                                          
         MVC   FHDA(L'PBDRAFT#),PBDRAFT#                                        
*                                                                               
VDRA12   OI    FHOI,FHOITR                                                      
         GOTO1 AFVAL,FHD                                                        
         BE    *+12                                                             
         NI    PBTYPES,FF-PBBILQ   NO INPUT - NOT BILLING                       
         B     VALDRAY                                                          
         TM    FVIIND,FVINUM                                                    
         BO    VDRA20                                                           
*        LH    RE,=Y(UC@AUTO-TWAD) TEST 'AUTO' ENTERED                          
*        LA    RE,TWAD(RE)                                                      
*        IC    RF,FVXLEN                                                        
*        EX    RF,*+8                                                           
*        BE    VDRA14                                                           
*        CLC   FVIFLD(0),0(RE)                                                  
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALDRAN                                                          
*                                                                               
*DRA14   GOTO1 LSTDRA                                                           
*        BNE   VALDRAN                                                          
*        B     VDRA22                                                           
*                                                                               
VDRA20   OI    BCDUB+L'BCDUB-1,X'0F'                                            
         UNPK  PBDRAFT#,BCDUB                                                   
*                                                                               
VDRA22   MVC   FHDA(L'PBDRAFT#),PBDRAFT#                                        
         BAS   RE,GETDRA                                                        
         BE    VALDRAY                                                          
         TM    PBINDS1,PBILDRA     TEST GOT LAST BILL #                         
         BZ    VALDRAN                                                          
         MVC   PBDRAFT#,BCSPACES   YES - CLEAR IT                               
         MVC   FHDA(L'PBDRAFT#),BCSPACES                                        
         NI    PBTYPES,FF-PBBILQ                                                
*                                                                               
VALDRAY  OI    FHII,FHIIVA                                                      
         B     EXITY                                                            
*                                                                               
VALDRAN  NI    FHII,FF-FHIIVA                                                   
         B     EXITN                                                            
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* GET MOST RECENT DRAFT BILL                                          *         
***********************************************************************         
         SPACE 1                                                                
LSTDRA   NTR1  ,                                                                
         PUSH  USING                                                            
         USING PBRRECD,IOKEY                                                    
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ                                                 
         MVC   PBRKCPY,CUABIN                                                   
         MVI   PBRKSUB,PBRKACTQ                                                 
         MVC   PBRKJOB,BCJOBCOD                                                 
*                                                                               
LDRA02   GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BNE   LSTDRAN                                                          
         CLC   PBRKEY(PBRKSEQ-PBRKEY),IOKEYSAV                                  
         BNE   LSTDRAN                                                          
         OC    PBRKBILD,PBRKBILD   TEST DRAFT BILL                              
         BZ    LDRA04                                                           
         ICM   RF,3,PBRKSEQ        NO - READ FOR NEXT DRAFT BILL                
         LA    RF,1(RF)                                                         
         STCM  RF,3,PBRKSEQ                                                     
         B     LDRA02                                                           
         POP   USING                                                            
*                                                                               
LDRA04   GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         LA    R3,PBRRFST-PBRRECD(R3)                                           
         USING BLHELD,R3                                                        
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
LSTDRAY  MVC   PBDRAFT#,BLHBLNO                                                 
         B     EXITY                                                            
         DROP  R3                                                               
*                                                                               
LSTDRAN  MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
         SPACE 1                                                                
***********************************************************************         
* GET DRAFT BILL                                                      *         
***********************************************************************         
         SPACE 1                                                                
GETDRA   NTR1  ,                                                                
         PUSH  USING                                                            
         USING PBRRECD,IOKEY                                                    
         XC    PBRPAS,PBRPAS                                                    
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,PBDRAFT#                                                
         MVI   PBRPIND,PBRPIDFT                                                 
         GOTO1 AIO,IOHIGH+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV                                 
         BNE   *+14                                                             
         CLC   PBRPJOB,BCJOBCOD                                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BLNOF)                                           
         B     EXITN                                                            
         POP   USING                                                            
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING PBRRECD,R2          R2=(BILL HEADER RECORD)                      
*                                                                               
         LA    R3,PBRRFST                                                       
         USING BLHELD,R3           R3=A(BLHELD)                                 
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
         ZAP   PBSRCPC,BLHSCH      SET SURCHARGE/DISCOUNT %                     
         ZAP   PBDSCPC,BLHDSC                                                   
         MVC   CSFMLANG,BLHLANG    SET BILL FORMAT LANGUAGE                     
*                                                                               
         CLC   BLHCUR,CSBILCUR     TEST BILLING CURRENCY THE SAME               
         BE    GDRA02                                                           
         MVC   FVXTRA(L'BLHCUR),BLHCUR                                          
         MVC   FVMSGNO,=AL2(AE$BCDMA)                                           
         B     EXITN                                                            
*                                                                               
GDRA02   CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY                        
         BE    GDRA04                                                           
         CLC   BLHRATE,CSEXCVAL    TEST EXCHANGE RATE THE SAME                  
         BE    GDRA04                                                           
         GOTO1 AEDTRAT,BOPARM,(L'FVXTRA,FVXTRA),BLHRATE,0                       
         MVC   FVMSGNO,=AL2(AE$BRDMA)                                           
         B     EXITN                                                            
         DROP  R3                                                               
*                                  TOTAL NET AND COMMISSION ON BILL             
GDRA04   LA    R3,PBRRFST                                                       
         USING NDXELD,R3           R3=A(PARAGRAPH INDEX ELEMENT)                
         XR    RF,RF                                                            
         CLI   NDXEL,NDXELQ                                                     
         BE    *+12                                                             
         IC    RF,NDXLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
         ZAP   PBBHAPN,BCPZERO     SET ALLOCATION (NET) TO ZERO                 
         ZAP   PBBHAPC,BCPZERO     SET ALLOCATION (COMMISSION) TO ZERO          
*                                                                               
         TM    PBRRSTA2,PBRSAUTR   TEST THIS IS AUTOREV BILL                    
         BNO   GDRA10                                                           
         LA    RE,PBRRFST          FIND ORIGINAL BILL NUMBER                    
         SR    R0,R0                                                            
         B     *+8                                                              
GDRA06   IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             TEST E-O-R                                   
         BE    GDRA14                                                           
         CLI   0(RE),FFTELQ                                                     
         BNE   GDRA06                                                           
         CLI   FFTTYPE-FFTELD(RE),FFTTAUTR                                      
         BNE   GDRA06                                                           
*                                                                               
         L     RF,AIOA             GET A(JOB RECORD)                            
         LA    RF,ACCORLEN(RF)     RECORD IS IN EMULATED ACCFIL FORMAT          
         SR    R0,R0                                                            
         B     *+8                                                              
GDRA08   IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0             TEST E-O-R                                   
         BE    GDRA14                                                           
         CLI   0(RF),FFTELQ                                                     
         BNE   GDRA08                                                           
         CLI   FFTTYPE-FFTELD(RF),FFTTAUTR                                      
         BNE   GDRA08                                                           
*                                                                               
         CLC   FFTDATA-FFTELD(L'CSBILNUM,RE),FFTDATA-FFTELD(RF)                 
         BNE   GDRA14                                                           
*                                                                               
GDRA10   XR    R0,R0                                                            
         ICM   R0,1,NDXACTV        R0=NO. OF ACTIVE ENTRIES                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R4,NDXINDX          R3=A(LIST OF ACTIVE ENTRIES)                 
*                                                                               
K        USING PBRKEY,IOKEY                                                     
GDRA12   MVC   K.PBRKEY,PBRKEY                                                  
         MVC   K.PBRKPARA,0(R4)                                                 
         CLI   K.PBRKPARA,0                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  K                                                                
         L     R1,AIO2                                                          
         LA    R1,PBRRFST-PBRRECD(R1)                                           
         USING PGHELD,R1                                                        
         XR    RF,RF                                                            
         CLI   PGHEL,PGHELQ                                                     
         BE    *+12                                                             
         IC    RF,PGHLN                                                         
         BXH   R1,RF,*-12                                                       
         AP    PBBHAPN,PGHNET      UPDATE AMOUNTS                               
         AP    PBBHAPC,PGHCOM                                                   
         LA    R4,1(R4)                                                         
         BCT   R0,GDRA12                                                        
*                                                                               
         TM    PBRRSTA2,PBRSAUTR   TEST THIS IS AUTOREV BILL                    
         BO    GETDRAX             YES - AMOUNTS WILL NOT MATCH                 
         DROP  R1,R2,R3                                                         
*                                                                               
         CP    PBBHAPN,PBCBAPN     TEST NET/COMMISSION MATCH                    
         BNE   *+14                                                             
         CP    PBBHAPC,PBCBAPC                                                  
         BE    GETDRAX                                                          
         MVC   FVMSGNO,=AL2(AE$DBNEJ)                                           
         B     EXITN                                                            
*                                                                               
GDRA14   MVC   FVMSGNO,=AL2(AE$UGABL)                                           
         B     EXITN                                                            
*                                                                               
GETDRAX  MVC   CSBILNUM,PBDRAFT#                                                
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIVE BILL NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALLIV   NTR1  ,                                                                
         ICM   R3,15,PBALIVH       TEST FIELD ON SCREEN                         
         BNZ   VLIV02                                                           
         CLI   PBMODE,PBLIVEQ      TEST LIVE BILL# REQUIRED                     
         BNE   EXITY                                                            
         TM    PBTYPES,PBBILQ                                                   
         BZ    EXITY                                                            
         BAS   RE,GETBNO                                                        
         B     VALLIVY                                                          
*                                                                               
         USING FHD,R3                                                           
VLIV02   CLI   PBMODE,PBLIVEQ      TEST LIVE MODE                               
         BNE   *+12                                                             
         TM    PBTYPES,PBBILQ      YES - TEST BILLING                           
         BO    VLIV04                                                           
         CLI   FHIL,0              TEST FIELD ENTERED                           
         BE    EXITY                                                            
         ST    R3,FVADDR           YES - IT SHOULDN'T BE                        
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
*                                                                               
VLIV04   MVI   FVMINL,1            LIVE BILL INPUT REQUIRED                     
         GOTO1 AFVAL,FHD           VALIDATE LIVE BILL NUMBER                    
         BNE   EXITN                                                            
*                                                                               
         OC    PBDRAFT#,PBDRAFT#   TEST HAVE DRAFT#                             
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DBNOF)                                           
         B     EXITN                                                            
*                                                                               
         TM    FVIIND,FVINUM       TEST NUMERIC FIELD                           
         BNZ   VLIV10                                                           
         LH    RE,=Y(UC@AUTO-TWAD) TEST 'AUTO' ENTERED                          
         LA    RE,TWAD(RE)                                                      
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)                                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         MVC   FHDA(L'UC@AUTO),0(RE)                                            
         OI    FHOI,FHOITR                                                      
*                                                                               
         BAS   RE,GETBNO           ESTABLISH LIVE BILL# (PBLIVE#)               
         B     VALLIVY                                                          
*                                                                               
VLIV10   CLI   BCP103,C'N'         TEST MANUAL BILL NUMBERS ALLOWED             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         OI    BCDUB+L'BCDUB-1,X'0F'                                            
         UNPK  PBLIVE#,BCDUB                                                    
         OI    FHOI,FHOITR                                                      
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS       TEST BILL NUMBER ALREADY USED                
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,PBLIVE#                                                 
         MVI   PBRPIND,PBRPILVE                                                 
*                                                                               
         CLI   P#DUPEOK,C'Y'       TEST DUPLICATE INVOICES ALLOWED              
         BNE   *+16                                                             
         MVC   PBRPUSER,CUUSER                                                  
         MVC   PBRPJOB,BCJOBCOD                                                 
*                                                                               
         GOTO1 AIO,IOHIUP+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,PBRPCRED-PBRPAS-1                                             
         CLI   P#DUPEOK,C'Y'                                                    
         BE    *+8                                                              
         LA    RE,PBRPUSER-PBRPAS-1                                             
         EX    RE,*+8                                                           
         B    *+10                                                              
         CLC   PBRPAS(0),IOKEYSAV                                               
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECAE)                                           
         B     EXITN                                                            
*                                                                               
VALLIVY  MVC   CSBILNUM,PBLIVE#                                                 
         LTR   R3,R3                                                            
         BZ    *+14                                                             
         MVC   FHDA(L'PBLIVE#),PBLIVE#                                          
         OI    FHOI,FHOITR                                                      
         B     EXITY                                                            
         DROP  R2,R3                                                            
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
         STC   R0,BWBMNTH                                                       
         SRL   R1,28                                                            
         STC   R1,BWBMNTH+1                                                     
         OC    BWBMNTH,=X'F0F0'                                                 
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
         CLC   BWBMNTH,PBLIVE#                                                  
         BE    GETBNO08                                                         
         MVC   PBLIVE#(L'BWBMNTH),BWBMNTH                                       
         MVC   PBLIVE#+L'BWBMNTH(L'BNCRSET),BNCRSET                             
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
         CLC   BWBMNTH,PBLIVE#                                                  
         BE    GETBNO20                                                         
         MVC   PBLIVE#(L'BWBMNTH),BWBMNTH                                       
         MVC   PBLIVE#+L'BWBMNTH(L'PMDRBILL),PMDRBILL                           
GETBNO20 PACK  BODUB1,PBLIVE#                                                   
         AP    BODUB1,=P'1'                                                     
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  PMDLBILL,BODUB1                                                  
*                                                                               
         USING PBRRECD,R2                                                       
GETBNO22 XC    PBRPAS,PBRPAS       TEST BILL NUMBER ALREADY USED                
         MVI   PBRPTYP,PBRPTYPQ                                                 
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
* VALIDATE BILL DATE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALDAT   NTR1  ,                                                                
         ICM   R3,15,PBADATH                                                    
         BNZ   VDAT02                                                           
         MVC   PBDATC,BCTODAYC                                                  
         MVC   PBDATP,BCTODAYP                                                  
*                                                                               
         USING FHD,R3                                                           
VDAT02   CLI   FHIL,0                                                           
         BNE   VDAT04                                                           
         GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(17,FHDA)                            
*                                                                               
         PUSH  USING                                                            
         USING PERVALD,BOELEM                                                   
VDAT04   GOTO1 AFVAL,FHD                                                        
         GOTO1 VPERVAL,BOPARM,(FVILEN,FVIFLD),(X'40',PERVALD)                   
         CLI   4(R1),0             TEST VALID DATE                              
         BE    VDAT06                                                           
         CLI   4(R1),4                                                          
         BE    VDAT06                                                           
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     VALDATN                                                          
*                                                                               
VDAT06   MVC   PBDATC,PVALCSTA                                                  
         MVC   PBDATP,PVALPSTA                                                  
         TM    FHII,FHIIVA         TEST PREVIOUSLY VALIDATED                    
         BO    VALDATY                                                          
         GOTO1 XCFLD,FHD                                                        
         GOTO1 VDATCON,BOPARM,(2,PBDATC),(17,FHDA)                              
         OI    FHOI,FHOITR                                                      
*                                                                               
         MVC   BOWORK1+00(6),PVALESTA                                           
         GOTO1 VDATCON,BOPARM,(2,BCTODAYC),(0,BOWORK1+6)                        
         SR    R0,R0                                                            
         ICM   R0,1,BCP104         NUMBER OF DAYS                               
         BNZ   *+8                                                              
         LA    R0,60               DEFAULT IS 60 DAYS                           
         GOTO1 VADDAY,BOPARM,BOWORK1,BOWORK1+12,(R0)                            
         CLC   BOWORK1+6(6),BOWORK1+12                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
         B     VALDATN                                                          
         LNR   R0,R0                                                            
         GOTO1 VADDAY,BOPARM,BOWORK1,BOWORK1+12,(R0)                            
         CLC   BOWORK1+6(6),BOWORK1+12                                          
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
         B     VALDATN                                                          
         POP   USING                                                            
*                                                                               
VALDATY  OI    FHII,FHIIVA                                                      
         B     EXITY                                                            
*                                                                               
VALDATN  NI    FHII,FF-FHIIVA                                                   
         B     EXITN                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DUE-DATE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALDUE   NTR1  ,                                                                
         L     R6,AGOPBLK                                                       
         USING GOBLOCKD,R6                                                      
         ICM   R3,15,PBADUEH       TEST INPUT FIELD                             
         BNZ   VDUE02                                                           
         BAS   RE,DEFDUE           NO - SET DEFAULT DATE                        
         B     EXIT                                                             
         USING FHD,R3                                                           
*                                                                               
VDUE02   GOTO1 AFVAL,FHD                                                        
*&&UK                                                                           
         BE    VDUE04              ANY INPUT?                                   
         OC    GOIDUE,GOIDUE       NO - DISPLAY RULE IF DEFINED                 
         BZ    VDUE04                                                           
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONACTN,CONATRAQ                                                 
         MVI   CONFLD,CONFIDUE                                                  
         LA    RF,GOIDUE                                                        
         STCM  RF,15,CONIADD                                                    
         LA    RF,FHDA                                                          
         STCM  RF,15,CONOADD                                                    
         MVI   CONILEN,L'GOIDUE                                                 
         MVC   CONCOMF,ACOM                                                     
         GOTO1 VCONVERT,CONBLK                                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
         GOTO1 AFVAL,FHD                                                        
*&&                                                                             
VDUE04   CLI   PBMODE,PBDISQ       TEST DISPLAY MODE                            
         BE    EXITY                                                            
*                                                                               
         CLI   FVILEN,0            SET TO DEFAULT IF NO INPUT                   
         BNE   VDUE06                                                           
         BAS   RE,DEFDUE                                                        
         BE    VALDUEY                                                          
         GOTO1 VDATCON,BOPARM,(2,PBDUE),(17,FHD)                                
         OI    FHOI,FHOITR                                                      
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
*                                  VALIDATE INPUT EXPRESSION                    
VDUE06   XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONACTN,CONAVGTQ                                                 
         MVI   CONFLD,CONFIDUE                                                  
         MVC   CONIDATE,PBDATP                                                  
         MVC   CONILEN,FVILEN                                                   
         LA    R0,FVIFLD                                                        
         STCM  R0,15,CONIADD                                                    
         LA    R0,PBDUE                                                         
         STCM  R0,15,CONOADD                                                    
         MVC   CONCOMF,ACOM                                                     
         GOTO1 VCONVERT,CONBLK                                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
*                                                                               
VALDUEY  GOTO1 XCFLD,FHD                                                        
         GOTO1 VDATCON,BOPARM,(2,PBDUE),(17,FHDA)                               
         OI    FHOI,FHOITR                                                      
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SET DEFAULT DUE DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
DEFDUE   NTR1  ,                                                                
         OC    GOIDUE,GOIDUE       TEST RULE DEFINED                            
         BNZ   *+14                                                             
         MVC   PBDUE,PBDATC        NO - DUE-DATE = BILL DATE                    
         B     EXITY                                                            
*                                                                               
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONACTN,CONAGETQ                                                 
         MVI   CONFLD,CONFIDUE                                                  
         MVC   CONIDATE,PBDATP                                                  
         LA    R0,GOIDUE                                                        
         STCM  R0,15,CONIADD                                                    
         LA    R0,PBDUE                                                         
         STCM  R0,15,CONOADD                                                    
         MVC   CONCOMF,ACOM                                                     
         GOTO1 VCONVERT,CONBLK                                                  
         BE    EXITY                                                            
         CLI   CONERR,CERRLOW      DUE DATE LOW IS ONLY GOOD ERROR              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AE$DULOW)                                           
         B     EXITN                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE LANGUAGE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALLAN   NTR1  ,                                                                
         ICM   R3,15,PBALANH       TEST HAVE A(FIELD)                           
         BZ    EXITY                                                            
         USING FHD,R3                                                           
         GOTO1 XCFLD,FHD                                                        
         LR    R4,RF               R4=EX. LENGTH OF FIELD                       
*                                                                               
         L     R1,ALANG                                                         
         LH    RE,0(R1)            RE=ENTRY LENGTH                              
         L     RF,2(R1)            RF=A(END OF TABLE-1)                         
         LA    R1,6(R1)            R1=A(FIRST LANGUAGE ENTRY)                   
         MVC   BOBYTE1,CSFMLANG                                                 
         CLI   BOBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   BOBYTE1,CULANG                                                   
         B     VLAN02                                                           
         CLI   BOBYTE1,LANGEUK                                                  
         BNE   VLAN02                                                           
         MVI   BOBYTE1,LANGENG                                                  
         USING LANGTABD,R1                                                      
VLAN02   CLC   LANGCODE,BOBYTE1                                                 
         BE    VLAN04                                                           
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
VLAN04   CLC   BOBYTE1,CULANG      TEST CONNECTED IN NATIVE LANGUAGE            
         BE    VLAN06                                                           
         LA    RF,L'LANGFUL-1      TRY ENGLISH LANGUAGE FULL NAME               
         LA    RE,LANGFUL                                                       
         CR    RF,R4                                                            
         BNH   *+12                                                             
         LA    RF,L'LANGSHR-1      TRY ENGLISH LANGUAGE SHORT NAME              
         LA    RE,LANGSHR                                                       
         CR    RF,R4                                                            
         BNH   *+6                                                              
         LR    RF,R4               TRUNCATE SHORT NAME TO FIT SCREEN            
         EX    RF,*+4                                                           
         MVC   FHDA(0),0(RE)                                                    
         B     VLAN10                                                           
VLAN06   LA    RF,L'LANGFULN-1     TRY NATIVE LANGUAGE FULL NAME                
         LA    RE,LANGFULN                                                      
         CR    RF,R4                                                            
         BNH   *+12                                                             
         LA    RF,L'LANGSHRN-1     TRY NATIVE LANGUAGE SHORT NAME               
         LA    RE,LANGSHRN                                                      
         CR    RF,R4                                                            
         BNH   *+6                                                              
         LR    RF,R4               TRUNCATE SHORT NAME TO FIT SCREEN            
         EX    RF,*+4                                                           
         MVC   FHDA(0),0(RE)                                                    
*                                                                               
VLAN10   OI    FHAT,FHATPR         ??                                           
         OI    FHOI,FHOITR                                                      
*                                                                               
         B     EXITY                                                            
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BILL BATCH REF FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
VALREF   NTR1  ,                                                                
         CLI   PBMODE,PBDISQ                                                    
         BE    EXITY                                                            
         ICM   R3,15,PBBILBAT+(PBAREFH-PBBATCHD)                                
         USING FHD,R3              R3=A(FIELD)                                  
*                                                                               
         TM    PBTYPES,PBBILQ      TEST DOING BILLING                           
         BO    VREF02              NO - FIELD SHOULD BE BLANK                   
         LTR   R3,R3                                                            
         BZ    EXITY                                                            
         CLI   FHIL,0                                                           
         BE    EXITY                                                            
         MVC   FVADDR,PBADRAH                                                   
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITN                                                            
*                                                                               
VREF02   ICM   R3,15,PBBILBAT+(PBAREFH-PBBATCHD)                                
         BZ    VREF10                                                           
         USING FHD,R3              TEST ANY INPUT TO FIELD                      
         CLI   FHIL,0                                                           
         BE    VREF10                                                           
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,FHD                                                        
         BNE   EXITN                                                            
         XR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
VREF04   CLI   0(R1),C'A'                                                       
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITN                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VREF04                                                        
         MVC   PBBILREF,FVIFLD                                                  
         B     EXITY                                                            
*                                                                               
VREF10   CLI   PBMODE,PBLIVEQ      TEST UPDATING                                
         BE    VREF12                                                           
         OC    PBBILREF,PBDRAFT#   NO - USE PART OF DRAFT NUMBER                
         BNZ   VREF14                                                           
VREF12   XR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,BCJOBCOD(RE)                                                  
         MVC   PBBILREF(1),0(RE)                                                
         MVC   PBBILREF+1(3),PBLIVE#+3                                          
*                                                                               
VREF14   B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE BILL BATCH MOA FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
VALMOA   NTR1  ,                                                                
         CLI   PBMODE,PBDISQ                                                    
         BE    EXITY                                                            
         ICM   R3,15,PBBILBAT+(PBAMOAH-PBBATCHD)                                
         USING FHD,R3              R3=A(FIELD)                                  
*                                                                               
         TM    PBTYPES,PBBILQ      TEST DOING BILLING                           
         BO    VMOA02              NO - FIELD SHOULD BE BLANK                   
         LTR   R3,R3                                                            
         BZ    EXITY                                                            
         CLI   FHIL,0                                                           
         BE    EXITY                                                            
         MVC   FVADDR,PBADRAH                                                   
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITN                                                            
         CLI   PBMODE,PBDISQ                                                    
         BE    EXITY                                                            
*                                                                               
VMOA02   LTR   R3,R3               TEST DATE INPUT                              
         BZ    VMOA04                                                           
         GOTO1 AFVAL,FHD                                                        
         BE    VMOA10                                                           
VMOA04   MVC   FVIFLD,BCSPACES     NO - USE BILL DATE                           
         GOTO1 VDATCON,BOPARM,(1,PBDATP),(9,FVIFLD)                             
         LA    R0,16                                                            
         GOTO1 AFVAL,0                                                          
         ST    R3,FVADDR                                                        
*                                                                               
VMOA10   MVC   CSBSECL,BCCPYEL+(CPYBSEC-CPYELD)                                 
         PUSH  USING                                                            
         USING BMONVALD,BOWORK1                                                 
         GOTO1 VBMONVAL,BOPARM,(FVILEN,FVIFLD),('POSTBILL',ACOM),      *        
               (CULANG,BMONVALD),(CUABIN,0)                                     
         MVC   BOBYTE1,0(R1)       BATCH SECURITY LEVEL                         
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    *+14                                                             
         MVC   FVMSGNO,BMOMSG                                                   
         B     EXITN                                                            
*                                                                               
         LTR   R3,R3               DISPLAY DATE                                 
         BZ    VMOA12                                                           
         GOTO1 XCFLD,FHD                                                        
         OI    FHOI,FHOITR                                                      
         MVC   BODUB1,BMOMOSP                                                   
         MVI   BODUB1+L'BMOMOSP,X'01'                                           
         GOTO1 VDATCON,BOPARM,(1,BODUB1),(9,FHDA)                               
*                                                                               
VMOA12   MVC   PBBILMP,BMOMOSP                                                  
         MVC   PBBILMC,BMOMOSC                                                  
         MVC   CSBSECL,BOBYTE1     SET BATCH SECURITY                           
*                                                                               
         LA    RF,X'80'            BATCH REF MAY BE INCREMENTED                 
         ICM   R3,15,PBBILBAT+(PBAREFH-PBBATCHD)                                
         BZ    VMOA14                                                           
         CLI   FHIL,0                                                           
         BNE   VMOA14                                                           
         XR    RF,RF                                                            
VMOA14   GOTO1 AADDOBH,BOPARM,('POSTBILL',PBBILREF),((RF),FHD)                  
         BNE   EXITN                                                            
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DEBTORS FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
VALDEB   NTR1  ,                                                                
         MVC   PBDEBULA,BCCMPPRF+(PPRRECVU-PPRELD)  DEFAULT VALUE               
*                                                                               
         ICM   R3,15,PBADEBH       FIELD DEFINED?                               
         BZ    EXITY                                                            
         USING FHD,R3                                                           
*                                                                               
         OI    FHOI,FHOITR                                                      
         CLI   FHIL,0                                                           
         BNE   VDEB02              FIELD EMPTY - SET TO DEFAULT                 
         MVC   FHDA(L'PBDEBACT),PBDEBACT                                        
         CLC   BCCPYREC,PBDEBUL                                                 
         BE    VDEB02                                                           
         MVI   FHDA,C'*'                                                        
         MVC   FHDA+1(L'PBDEBULA),PBDEBULA                                      
*                                                                               
VDEB02   MVI   FVMINL,1                                                         
         GOTO1 AFVAL,FHD                                                        
         BNE   EXITN                                                            
*                                                                               
         PUSH  USING                                                            
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES     VALIDATE ACCOUNT                             
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYREC),BCCPYREC                                     
         MVC   ACTKACT,FVIFLD                                                   
         CLI   FVIFLD,C'*'         TEST OVERRIDE LEDGER                         
         BNE   *+10                                                             
         MVC   ACTKUNT(L'PBDEBULA),FVIFLD+1                                     
         MVC   PBDEBULA,ACTKUNT                                                 
         POP   USING                                                            
         GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
*                                                                               
         ICM   R4,15,PBADEBNH      NAME FIELD?                                  
         BZ    VALDEBY                                                          
         MVC   FHDAD(L'ACNAME,R4),ACNAME                                        
         OI    FHOID(R4),FHOITR                                                 
*                                                                               
VALDEBY  B     EXITY                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SURCHARGE PERCENT                                          *         
***********************************************************************         
         SPACE 1                                                                
VALSPC   NTR1  ,                                                                
         ICM   R3,15,PBASRCPH      FIELD ON SCREEN?                             
         BZ    EXITY                                                            
         USING FHD,R3                                                           
         OC    PBDRAFT#,PBDRAFT#   HAVE A BILL?                                 
         BZ    EXITY                                                            
*                                                                               
         CLI   PBMODE,PBDISQ       DISPLAY MODE?                                
         BE    VALSPCY                                                          
         TM    PBINDS1,PBINDRA     NEW BILL NUMBER?                             
         BZ    *+12                                                             
         TM    FHII,FHIITH         YES - DISPLAY UNLESS INPUT THIS TIME         
         BZ    VALSPCY                                                          
*                                                                               
         GOTO1 AVALAMT,BOPARM,(X'82',FHD),(L'PBSRCPC,PBSRCPC)                   
         BH    VALSPCN                                                          
*                                                                               
VALSPCY  GOTO1 XCFLD,FHD                                                        
         OI    FHOI,FHOITR                                                      
         CP    PBSRCPC,BCPZERO                                                  
         BE    EXITY                                                            
         CURED (P6,PBSRCPC),(6,FHDA),2,ALIGN=LEFT,DMCB=BODMCB                   
         B     EXITY                                                            
*                                                                               
VALSPCN  NI    FHII,FF-FHIIVA                                                   
         B     EXITN                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SURCHARGE ACCOUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
VALSRC   NTR1  ,                                                                
         L     RF,AGOPBLK          DEFAULT VALUE                                
         L     RF,GOABEXT-GOBLOCKD(RF)                                          
         MVC   PBSRCULA,GOSRGAC-GOBBLOCK+(ACTKUNT-ACTKCPY)(RF)                  
*                                                                               
         ICM   R3,15,PBASRCH       FIELD DEFINED?                               
         BNZ   VSRC02                                                           
         CP    PBSRCPC,BCPZERO     NO - SURCHARGE PERCENT?                      
         BE    EXITY                                                            
         CLC   PBSRCULA,BCSPACES   YES - ERROR IF NO DEFAULT                    
         BH    EXITY                                                            
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITN                                                            
*                                                                               
         USING FHD,R3                                                           
VSRC02   OI    FHOI,FHOITR                                                      
         CLI   FHIL,0              FIELD EMPTY - SET TO DEFAULT                 
         BNE   *+10                                                             
         MVC   FHDA(L'PBSRCULA),PBSRCULA                                        
*                                                                               
         GOTO1 AFVAL,FHD                                                        
         BE    VSRC04                                                           
         OC    PBDRAFT#,PBDRAFT#   HAVE A BILL?                                 
         BZ    EXITY                                                            
         CP    PBSRCPC,BCPZERO     NO INPUT ALLOWED IF NO %                     
         BE    EXITY                                                            
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITN                                                            
*                                                                               
         PUSH  USING                                                            
         USING ACTRECD,IOKEY                                                    
VSRC04   MVC   ACTKEY,BCSPACES     VALIDATE ACCOUNT                             
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,FVIFLD                                                   
         POP   USING                                                            
         GOTO1 AGETACT,LEDGSRC                                                  
         BNE   EXITN                                                            
         MVC   PBSRCULA,FVIFLD                                                  
         MVC   PBSRCAN,BCSPACES                                                 
         MVC   PBSRCAN(L'ACCOST),ACCOST                                         
*&&US                                                                           
         ICM   RF,15,ACASPA                                                     
         BZ    *+10                                                             
         MVC   PBSRCAN,SPAAULA-SPAELD(RF)                                       
*&&                                                                             
*                                                                               
         ICM   R4,15,PBASRCNH      NAME FIELD?                                  
         BZ    VALSRCY                                                          
         MVC   FHDAD(L'ACNAME,R4),ACNAME                                        
         OI    FHOID(R4),FHOITR                                                 
*                                                                               
VALSRCY  B     EXITY                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISCOUNT PERCENT                                           *         
***********************************************************************         
         SPACE 1                                                                
VALDPC   NTR1  ,                                                                
         ICM   R3,15,PBADSCPH      FIELD ON SCREEN?                             
         BZ    EXITY                                                            
         USING FHD,R3                                                           
         OC    PBDRAFT#,PBDRAFT#   HAVE A BILL?                                 
         BZ    EXITY                                                            
*                                                                               
         CLI   PBMODE,PBDISQ       DISPLAY MODE?                                
         BE    VALDPCY                                                          
         TM    PBINDS1,PBINDRA     NEW BILL NUMBER?                             
         BZ    *+12                                                             
         TM    FHII,FHIITH         YES - DISPLAY UNLESS INPUT THIS TIME         
         BZ    VALDPCY                                                          
*                                                                               
         GOTO1 AVALAMT,BOPARM,(X'82',FHD),(L'PBDSCPC,PBDSCPC)                   
         BH    VALDPCN                                                          
*                                                                               
VALDPCY  GOTO1 XCFLD,FHD                                                        
         OI    FHOI,FHOITR                                                      
         CP    PBDSCPC,BCPZERO                                                  
         BE    EXITY                                                            
         CURED (P6,PBDSCPC),(6,FHDA),2,ALIGN=LEFT,DMCB=BODMCB                   
         B     EXITY                                                            
*                                                                               
VALDPCN  NI    FHII,FF-FHIIVA                                                   
         B     EXITN                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISCOUNT ACCOUNT                                           *         
***********************************************************************         
         SPACE 1                                                                
VALDSC   NTR1  ,                                                                
         L     RF,AGOPBLK          DEFAULT VALUE                                
         L     RF,GOABEXT-GOBLOCKD(RF)                                          
         MVC   PBDSCULA,GODSCAC-GOBBLOCK+(ACTKUNT-ACTKCPY)(RF)                  
*                                                                               
         ICM   R3,15,PBADSCH       FIELD DEFINED?                               
         BNZ   VDSC02                                                           
         CP    PBDSCPC,BCPZERO     NO - DISCOUNT PERCENT?                       
         BE    EXITY                                                            
         CLC   PBDSCULA,BCSPACES   YES - ERROR IF NO DEFAULT                    
         BH    EXITY                                                            
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITN                                                            
*                                                                               
         USING FHD,R3                                                           
VDSC02   OI    FHOI,FHOITR                                                      
         CLI   FHIL,0              FIELD EMPTY - SET TO DEFAULT                 
         BNE   *+10                                                             
         MVC   FHDA(L'PBDSCULA),PBDSCULA                                        
*                                                                               
         GOTO1 AFVAL,FHD                                                        
         BE    VDSC04                                                           
         OC    PBDRAFT#,PBDRAFT#   HAVE A BILL?                                 
         BZ    EXITY                                                            
         CP    PBDSCPC,BCPZERO     NO INPUT ALLOWED IF NO %                     
         BE    EXITY                                                            
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITN                                                            
*                                                                               
         PUSH  USING                                                            
         USING ACTRECD,IOKEY                                                    
VDSC04   MVC   ACTKEY,BCSPACES     VALIDATE ACCOUNT                             
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,FVIFLD                                                   
         POP   USING                                                            
         GOTO1 AGETACT,LEDGDSC                                                  
         BNE   EXITN                                                            
         MVC   PBDSCULA,FVIFLD                                                  
*                                                                               
         ICM   R4,15,PBADSCNH      NAME FIELD?                                  
         BZ    VALDSCY                                                          
         MVC   FHDAD(L'ACNAME,R4),ACNAME                                        
         OI    FHOID(R4),FHOITR                                                 
*                                                                               
VALDSCY  B     EXITY                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ACCRUAL MONTHS AND AMOUNT/PERCENTS              *         
***********************************************************************         
         SPACE 1                                                                
VALMON   NTR1  ,                                                                
         ICM   R2,15,PBAMNTHH      TEST MONTHS ON SCREEN                        
         BZ    EXITY                                                            
         USING FHD,R2                                                           
*                                                                               
         LA    R3,PBMNTHS                                                       
         USING PBMNTHSD,R3                                                      
PREVMOA  USING PBMNTHSD,RE                                                      
         LA    R6,12                                                            
VMON02   XC    PBMNTHSD(PBMNTHSL),PBMNTHSD                                      
         ZAP   PBMAMT,BCPZERO                                                   
         STCM  R2,15,FVADDR                                                     
         CLI   FHIL,0                                                           
         BE    VMON18              NOTHING IN THIS FIELD                        
         SR    R0,R0                                                            
         ICM   R0,12,=C',='                                                     
         ICM   R0,2,BCSLASH                                                     
         GOTO1 VSCANNER,BODMCB,(R2),(2,BOWORK1),(R0)                            
         CLI   BODMCB+4,2                                                       
         BNE   VMONERR             THERE MUST BE TWO BITS                       
         LA    R4,BOWORK1                                                       
         USING SCANBLKD,R4                                                      
         GOTO1 VPERVAL,BODMCB,(SC1STLEN,SC1STFLD),(CULANG,BOELEM)               
         TM    4(R1),X'01'                                                      
         BO    VMONERM             BAD DATE                                     
         CLC   =Y(1),BOELEM+(PVALNMNS-PERVALD)                                  
         BNE   VMONERM             ONLY ALLOW 1 MONTH                           
         MVC   PBMMP,BOELEM+(PVALPSTA+0-PERVALD)                                
         MVC   PBMMC(1),BOELEM+(PVALPSTA+0-PERVALD)                             
         OI    PBMMC,X'F0'         YEAR                                         
         MVC   PBMMC+1(1),BOELEM+(PVALPSTA+1-PERVALD)                           
         TR    PBMMC+1(1),MONTR                                                 
         LA    RE,PBMNTHS          TEST THIS IS THE FIRST MONTH                 
         CR    RE,R3                                                            
         BNE   VMON04                                                           
         CLC   PBBILMP,PBMMP       FIRST MOA MUST EQUAL BILL MOA                
         BNE   VMONERF                                                          
         B     VMON06                                                           
*                                                                               
VMON04   CR    RE,R3                                                            
         BE    VMON06                                                           
         CLC   PBMMP,PBBILMP                                                    
         BNH   VMONERG             ACCRUALS MUST BE FORWARD                     
         CLC   PBMMP,PREVMOA.PBMMP                                              
         BE    VMONERD             DUPLICATED MONTH                             
         LA    RE,PBMNTHSL(RE)                                                  
         B     VMON04                                                           
*                                                                               
VMON06   LA    R4,L'SCLINE(R4)     NEXT SCANNER ENTRY IS PERCENT                
         ZAP   PBMPCT,BCPZERO                                                   
         CLI   SC1STFLD,C'*'       * MEANS EQUAL - WORK IT OUT AT END           
         BE    VMON10                                                           
         MVC   FVIFLD(L'SC1STFLD),SC1STFLD                                      
         MVC   FVILEN,SC1STLEN                                                  
         GOTO1 AVALAMT,BODMCB,(X'84',FVIHDR),(L'PBMPCT,PBMPCT)                  
         BNE   VMONERA                                                          
         CP    PBMPCT,=P'1000000'                                               
         BNL   VMONERA                                                          
         CP    PBMPCT,BCPZERO                                                   
         BNH   VMONERA                                                          
*                                                                               
VMON10   LA    R3,PBMNTHSL(R3)                                                  
         IC    RE,PBMNTHSN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,PBMNTHSN                                                      
*                                                                               
VMON18   XR    RF,RF               BUMP R2 TO NEXT FIELD                        
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         BCT   R6,VMON02                                                        
*                                                                               
VMON20   LA    R3,PBMNTHS          ENSURE IT ALL COMES TO 100%                  
         ICM   R2,15,PBAMNTHH                                                   
         STCM  R2,15,FVADDR                                                     
         XR    R6,R6                                                            
         ICM   R6,1,PBMNTHSN       TEST ANYTHING AT ALL                         
         BZ    EXITY               NO - FINE                                    
         ZAP   BODUB1,BCPZERO      TOTAL PERCENTS                               
         ZAP   BODUB2,BCPZERO      NO OF MONTHS FOR EQUAL SPLIT                 
VMON22   AP    BODUB1,PBMPCT                                                    
         CP    PBMPCT,BCPZERO                                                   
         BNE   *+10                                                             
         AP    BODUB2,=P'1'                                                     
         LA    R3,PBMNTHSL(R3)                                                  
         BCT   R6,VMON22                                                        
*                                                                               
         CP    BODUB2,BCPZERO      TEST ANY NULL MONTHS TO FILL IN              
         BNE   VMON30              YES                                          
         CP    BODUB1,=P'1000000'  THEN THE TOTAL MUST BE EXACTLY 100%          
         BE    VMON40                                                           
         B     VMONERP                                                          
VMON30   CP    BODUB1,=P'1000000'  TEST 100 ALREADY INPUT                       
         BNL   VMONERP                                                          
         XC    BODUB3,BODUB3       DIVIDE REM. % BY # OF NULL MONTHS            
         ZAP   BODUB4,=P'1000000'                                               
         SP    BODUB4,BODUB1                                                    
         DP    BODUB3(2*L'BODUB3),BODUB2                                        
*                                                                               
         XR    R6,R6                                                            
         IC    R6,PBMNTHSN                                                      
         LA    R3,PBMNTHS                                                       
VMON32   CP    PBMPCT,BCPZERO                                                   
         BNE   VMON38                                                           
         ZAP   PBMPCT,BODUB3                                                    
         AP    PBMPCT,BODUB4                                                    
         ZAP   BODUB4,BCPZERO      REMAINDER ADDED TO FIRST ONE ONLY            
VMON38   LA    R3,PBMNTHSL(R3)                                                  
         BCT   R6,VMON32                                                        
*                                                                               
VMON40   ICM   R2,15,PBAMNTHH      CLEAR ALL FIELDS                             
         LA    R6,12                                                            
VMON42   GOTO1 XCFLD,FHD                                                        
         OI    FHOI,FHOITR                                                      
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         BCT   R6,VMON42                                                        
*                                                                               
         LA    R3,PBMNTHS          RE-DISPLAY MONTHS AND PERCENTS               
         ICM   R2,15,PBAMNTHH                                                   
         XR    R6,R6                                                            
         IC    R6,PBMNTHSN                                                      
VMON52   MVC   BODUB1(L'PBMMP),PBMMP                                            
         MVI   BODUB1+L'PBMMP,X'01'                                             
         MVC   BOWORK1,BCSPACES                                                 
         GOTO1 VDATCON,BODMCB,(1,BODUB1),(18,BOWORK1)                           
         LA    R4,BOWORK1+16                                                    
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(1,R4),BCSLASH                                                  
         LA    R4,2(R4)                                                         
         ZAP   BODUB1,PBMPCT                                                    
         CURED BODUB1,(7,0(R4)),4,ALIGN=LEFT,DMCB=BODMCB                        
         AR    R4,R0               ADD EDITED LENGTH                            
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BO    *+8                                                              
         LA    RF,FHDAD(RF)                                                     
         EX    RF,*+4                                                           
         MVC   FHDA(0),BOWORK1                                                  
         STC   RF,FHIL                                                          
*                                                                               
         LA    R3,PBMNTHSL(R3)                                                  
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         BCT   R6,VMON52                                                        
*                                                                               
         LA    R3,PBMNTHS          VALIDATE BATCH HEADERS                       
         ICM   R2,15,PBAMNTHH                                                   
         XR    R6,R6                                                            
         IC    R6,PBMNTHSN                                                      
VMON62   MVC   PBMREF,PBBILREF                                                  
         ICM   RF,15,PBBILBAT+(PBAREFH-PBBATCHD)                                
         BNZ   *+6                                                              
         LR    RF,R2                                                            
         GOTO1 AADDOBH,BOPARM,('POSTBILL',PBMREF),(X'80',(RF))                  
         BNE   EXITN                                                            
*                                                                               
         LA    R3,PBMNTHSL(R3)                                                  
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         AR    R2,RF                                                            
         BCT   R6,VMON62                                                        
*                                                                               
         B     EXITY                                                            
*                                                                               
VMONERR  MVC   FVMSGNO,=AL2(AE$INVIF)    INVALID INPUT                          
         B     EXITN                                                            
VMONERA  MVC   FVMSGNO,=AL2(AE$INAMT)    INVALID AMOUNT                         
         B     EXITN                                                            
VMONERP  MVC   FVMSGNO,=AL2(AE$INPCT)    NOT EQUAL TO 100%                      
         B     EXITN                                                            
VMONERD  MVC   FVMSGNO,=AL2(AE$DUPIF)    DUPLICATE INPUT                        
         B     EXITN                                                            
VMONERM  MVC   FVMSGNO,=AL2(AE$INMON)    INVALID MONTH                          
         B     EXITN                                                            
VMONERF  MVC   FVMSGNO,=AL2(AE$FMEBM)    FIRST MOA MUST=BILLMOA                 
         B     EXITN                                                            
VMONERG  MVC   FVMSGNO,=AL2(AE$AMABM)    ACCRUAL MOA MUST BE FUTURE             
         B     EXITN                                                            
         SPACE 1                                                                
         DROP  R2,R3                                                            
*                                                                               
MONTR    DC    C'.123456789......ABC'                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCRUAL ACCOUNT                                            *         
***********************************************************************         
         SPACE 1                                                                
VALACC   NTR1  ,                                                                
         ICM   R3,15,PBAACCH       TEST FIELD ON SCREEN                         
         BZ    EXITY                                                            
         USING FHD,R3                                                           
*                                                                               
         CLI   PBMNTHSN,0          TEST MONTHS DEFINED                          
         BNE   VACC02                                                           
         CLI   FHIL,0              NO - NO INPUT IS OKAY                        
         BE    EXITY                                                            
         MVC   FVADDR,PBAMNTHH                                                  
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITN                                                            
*                                                                               
VACC02   OI    FHOI,FHOITR                                                      
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,FHD                                                        
         BNE   EXITN                                                            
         PUSH  USING                                                            
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,FVIFLD                                                   
         GOTO1 AGETACT,LEDGACC                                                  
         BNE   EXITN                                                            
         MVC   PBACCULA,FVIFLD                                                  
         POP   USING                                                            
*                                                                               
         ICM   R4,15,PBAACCNH      NAME FIELD?                                  
         BZ    VALACCY                                                          
         MVC   FHDAD(L'ACNAME,R4),ACNAME                                        
         OI    FHOID(R4),FHOITR                                                 
*                                                                               
VALACCY  B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLEAR FIELD                                              *         
*                                                                     *         
* NTRY: R1 = A(FIELD)                                                 *         
* EXIT: RF = EXECUTABLE LENGTH OF FIELD DATA                          *         
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R1                                                           
XCFLD    XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BO    *+8                                                              
         LA    RF,FHDAD(RF)                                                     
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   FHDA(0),BCSPACES                                                 
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
LEDGSRC  DC    C'SI',C'SE',X'00'                                                
LEDGDSC  DC    C'SI',C'SE',X'00'                                                
LEDGACC  DC    C'SQ',X'00'                                                      
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ULBIL    DC    C'11'                                                            
ULREV    DC    C'12'                                                            
         SPACE 1                                                                
***********************************************************************         
* GENERAL EQUATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE LIST                                                        *         
***********************************************************************         
         SPACE 1                                                                
ROUTS    DS    0A                                                               
         DC    A(VALDAT)                                                        
         DC    A(VALDRA)                                                        
         DC    A(VALLIV)                                                        
         DC    A(VALDUE)                                                        
         DC    A(VALLAN)                                                        
         DC    A(VALREF)                                                        
         DC    A(VALMOA)                                                        
         DC    A(VALDEB)                                                        
         DC    A(VALSPC)                                                        
         DC    A(VALSRC)                                                        
         DC    A(VALDPC)                                                        
         DC    A(VALDSC)                                                        
         DC    A(VALMON)                                                        
         DC    A(VALACC)                                                        
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
ROUTSX   DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
BWORKD   DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
BWBMNTH  DS    CL2                 EBCIDIC MONTH NUMBER FOR BILL RESET          
*                                                                               
         DS    0A                                                               
       ++INCLUDE DDCONBLK                                                       
*                                                                               
         DS     (OVERWRKL-(*-BWORKD))X                                          
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACCLB1E   08/16/00'                                      
         END                                                                    
