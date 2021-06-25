*          DATA SET ACCLB23    AT LEVEL 070 AS OF 08/16/00                      
*PHASE T62123A                                                                  
CLB23    TITLE '- BILL PROGRAM - SECNAME AND SECDEF'                            
* NB THIS PHASE CURRENTLY SUPPORTS BOTH THE OLD AND NEW BSDELS                  
* (THE NEW ONE HAS THE SECTION NUMBER IN). IF THE RECORD CONTAINS               
* OLD BSDELS THESE ARE DISPLAYED. ON CHANGE OF RECORD THE OLD ONES              
* ARE DELETED AND THE NEW ONES ADDED                                            
CLB23    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB23*,R6,CLEAR=YES,RR=RE                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING OVERWRK,RC                                                       
         LH    R8,=Y(BSDICT-TWAD)                                               
         LA    R8,TWAD(R8)                                                      
         USING BSDICT,R8                                                        
         ST    RE,BORELO                                                        
         LA    R7,OSVALS                                                        
         USING OSVALSD,R7                                                       
                                                                                
         CLC   TWASCRN,CSSCRN      LOAD SCREEN IF FIRST TIME                    
         BNE   INIT00                                                           
         CLI   CSACT,ACTSNM                                                     
         BE    VNAMKEY                                                          
         B     VDEFKEY                                                          
                                                                                
INIT00   GOTO1 AOVRSCR,BOPARM,(CSSCRN,BASOLAYH)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    THISKEY,THISKEY                                                  
         XC    LASTKEY,LASTKEY                                                  
         CLI   CUCTRY,CTRYGER                                                   
         BE    INIT04                                                           
                                                                                
         CLI   CSACT,ACTSNM                                                     
         BNE   INIT02                                                           
         XC    SNMLNGW,SNMLNGW     CLEAR & PROTECT LANGUAGE FIELD               
         XC    SNMLNGX,SNMLNGX                                                  
         OI    SNMLNGH+1,X'20'                                                  
         B     INIT04                                                           
                                                                                
INIT02   XC    SDFLNGW,SDFLNGW                                                  
         XC    SDFLNGX,SDFLNGX                                                  
         OI    SDFLNGH+1,X'20'                                                  
                                                                                
INIT04   CLI   CSACT,ACTSDF                                                     
         BE    VDEFKEY                                                          
                                                                                
***********************************************************************         
* VALIDATE SECNAME KEY                                                *         
***********************************************************************         
                                                                                
VNAMKEY  GOTO1 VALFKEY             VALIDATE FORMAT KEY                          
         BL    EXITN               MISSING OR INVALID NUMBER/LANGUAGE           
         BH    ERRRECNF            RECORD NOT FOUND                             
         CLC   THISFKEY,LASTFKEY                                                
         BE    VNAME00                                                          
         MVC   LASTFKEY,THISFKEY                                                
                                                                                
DNAME00  LA    R2,SNM1H            R2=A(FIELD HEADER)                           
         LA    R3,SECNM#Q          R3=NO. OF SECTIONS                           
         LA    R4,1                R4=SECTION NUMBER                            
         MVI   BCHALF,FFTTBSNM     SET SEARCH ARGUMENT FOR HELLO                
         LA    R5,IOKEY            READ SECDEF RECORD                           
         USING PBSRECD,R5                                                       
         XC    PBSKEY,PBSKEY                                                    
         MVI   PBSKTYP,PBSKTYPQ                                                 
         MVC   PBSKCPY,CUABIN                                                   
         MVI   PBSKSUB,PBSKDEFQ                                                 
         MVC   PBSKFMT,THISFORM                                                 
                                                                                
DNAM10   XC    L'SNM1H(L'SNM1,R2),L'SNM1H(R2)                                   
         STC   R4,PBSKSEC                                                       
         MVC   PBSKLANG,THISLANG                                                
         GOTO1 AIO,IO3+IOACCMST+IORD                                            
         BNE   DNAM15                                                           
         STC   R4,BCHALF+1           SET FFTEL SEQ# IN SEARCH ARGUMENT          
         GOTO1 DISPLAY                                                          
                                                                                
DNAM15   OI    6(R2),X'80'         TRANSMIT                                     
         LA    R4,1(R4)            BUMP SECTION NUMBER                          
         XR    RE,RE                                                            
         IC    RE,0(R2)            BUMP TO NEXT INPUT FIELDS                    
         AR    R2,RE                                                            
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         MVC   IOKEY,IOKEYSAV      RESET KEY                                    
         BCT   R3,DNAM10           REPEAT FOR ALL SECTION NUMBERS               
                                                                                
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         LA    R1,SNM1H            POSITION CURSOR                              
         ST    R1,FVADDR                                                        
         MVC   CSFORMAT,THISFORM                                                
         MVC   CSFMLANG,THISLANG                                                
         B     EXITY                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND SAVE CHANGED SECTION NAMES                             *         
***********************************************************************         
                                                                                
VNAME00  LA    R2,SNM1H            R2=A(FIELD HEADER)                           
         LA    R3,SECNM#Q          R3=NO. OF SECTIONS                           
         LA    R4,1                R4=SECTION NUMBER                            
         MVI   BCHALF,FFTTBSNM     SET SEARCH ARGUMENT FOR HELLO                
         LA    R5,IOKEY                                                         
         USING PBSRECD,R5                                                       
         XC    PBSKEY,PBSKEY                                                    
         MVI   PBSKTYP,PBSKTYPQ                                                 
         MVC   PBSKCPY,CUABIN                                                   
         MVI   PBSKSUB,PBSKDEFQ                                                 
         MVC   PBSKFMT,THISFORM                                                 
                                                                                
VNAM05   STC   R4,PBSKSEC                                                       
         STC   R4,BCHALF+1           SET FFTEL SEQ# IN SEARCH ARGUMENT          
         MVC   PBSKLANG,THISLANG                                                
         GOTO1 AIO,IO3+IOACCMST+IORDUP  READ FOR UPDATE                         
         BE    *+14                                                             
         XC    L'SNM1H(L'SNM1,R2),L'SNM1H(R2)                                   
         B     VNAM10                                                           
         CLI   5(R2),0              ANYTHING INPUT ?                            
         BNE   VNAM08               RE DISPLAY EXISTING NAME                    
         GOTO1 DISPLAY                                                          
         B     VNAM10                                                           
                                                                                
VNAM08   GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'DB',AIO3),(2,BCHALF)              
         LA    RF,BCWORK             BUILD NEW FFTEL                            
         XC    BCWORK,BCWORK                                                    
         USING FFTELD,RF                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTBSNM                                                 
         STC   R4,FFTSEQ                                                        
         MVC   FFTDLEN,5(R2)                                                    
         XR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FFTDATA(0),8(R2)                                                 
         LA    RE,1(RE)                                                         
         LA    RE,(FFTDATA-FFTEL)(RE)                                           
         STC   RE,FFTLN                                                         
         DROP  RF                                                               
                                                                                
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO3,(RF)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO3+IOACCMST+IOWRITE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VNAM10   OI    6(R2),X'80'                                                      
         LA    R4,1(R4)            BUMP SECTION NUMBER                          
         XR    RE,RE                                                            
         IC    RE,0(R2)            BUMP TO NEXT INPUT FIELD                     
         AR    R2,RE                                                            
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         MVC   IOKEY,IOKEYSAV      RESET KEY                                    
         BCT   R3,VNAM05           REPEAT FOR ALL SECTION NUMBERS               
                                                                                
         LA    R1,SNM1H            POSITION CURSOR & EXIT                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$AOKNX)                                           
         B     EXITY                                                            
         DROP  R5                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FOR SECDEF SCREEN (FORMAT NO. & SECTION NO.)           *         
* IO AREAS: IO1 - GENERAL, IO2 - FORMAT RECORD, IO3 - SECDEF RECORD   *         
***********************************************************************         
                                                                                
VDEFKEY  MVI   RECSTAT,0                                                        
         GOTO1 VALFKEY             VALIDATE FORMAT NUM/LANG                     
         BL    EXITN                                                            
         BH    VDEFK14                                                          
         LA    R5,IOKEY                                                         
         USING PBCRECD,R5                                                       
         MVI   PBCKSEQ,1                                                        
         LH    R1,=Y(IO4+IOACCMST+IORD)                                         
         GOTO1 AIO                 IF THHERE'S A FORMAT CONT. RECORD            
         BNE   VDEFK16             READ IT INTO IO4                             
         OI    RECSTAT,RSFMTCNT    SET FORMAT CONTINUATION RECORD FOUND         
         B     VDEFK16                                                          
                                                                                
VDEFK14  CLC   THISFKEY,LASTFKEY                                                
         BNE   *+12                                                             
         OI    RECSTAT,RSNEWFMT    SET NEW FORMAT RECORD                        
         B     VDEFK20                                                          
         MVC   LASTFKEY,THISFKEY   IF FIRST TIME FOR NEW FORMAT                 
         XC    SDFFNM,SDFFNM       GET THEM TO INPUT A NEW NAME                 
         LA    R1,SDFFNMH                                                       
         OI    6(R1),X'80'                                                      
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$NFEDA)                                           
         B     EXITY                                                            
                                                                                
VDEFK16  MVC   CSFORMAT,THISFORM                                                
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(X'20',AIO2) GET FORMAT NAME         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,12(R1)                                                        
         XR    RE,RE                                                            
         IC    RE,1(RF)                                                         
         SH    RE,=H'3'            RE=L'FORMAT NAME                             
         CLC   THISFKEY,LASTFKEY   SAME FORMAT AS LAST TIME                     
         BNE   VDEFK18                                                          
         CLI   SDFFNMH+5,0         OR ZERO INPUT                                
         BE    VDEFK18                                                          
                                                                                
         EX    RE,*+8              HAS NAME CHANGED ?                           
         B     *+10                                                             
         CLC   SDFFNM(0),2(RF)                                                  
         BE    VDEFK20             NO                                           
         OI    RECSTAT,RSNEWNAM    SET NEW FORMAT NAME INDICATOR                
         B     VDEFK20                                                          
                                                                                
VDEFK18  XC    SDFFNM,SDFFNM                                                    
         EX    RE,*+8              MOVE OUT FORMAT NAME                         
         B     *+10                                                             
         MVC   SDFFNM(0),2(RF)                                                  
         OI    SDFFNMH+6,X'80'     TRANSMIT                                     
                                                                                
VDEFK20  MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SDFSNOH       VALIDATE SECTION NUMBER                      
         BNE   ERRMISS                                                          
         TM    FVIIND,FVINUM                                                    
         BZ    ERRNOTV                                                          
         OC    BCFULL(3),BCFULL                                                 
         BNZ   ERRNOTV                                                          
         CLI   BCFULL+3,0                                                       
         BE    ERRNOTV                                                          
         CLI   BCFULL+3,SECNM#Q                                                 
         BH    ERRNOTV                                                          
         MVC   THISSECT,BCFULL+3                                                
         DROP  R5                                                               
                                                                                
         LA    R5,IOKEY                                                         
         USING PBSRECD,R5                                                       
         XC    PBSKEY,PBSKEY       READ FOR SECDEF RECORD                       
         MVI   PBSKTYP,PBSKTYPQ                                                 
         MVC   PBSKCPY,CUABIN                                                   
         MVI   PBSKSUB,PBSKDEFQ                                                 
         MVC   PBSKFMT,THISFORM                                                 
         MVC   PBSKSEC,THISSECT                                                 
         MVC   PBSKLANG,THISLANG                                                
         GOTO1 AIO,IO3+IOACCMST+IORDUPD READ FOR UPDATE & DELETES               
                                                                                
         L     R5,AIO3                                                          
         ST    R5,APBS             APBS = A(PBSREC) = AIO3                      
         BE    VDEFK22                                                          
         MVC   PBSKEY,IOKEYSAV                                                  
         CLC   THISSECT,LASTSECT                                                
         BNE   *+12                                                             
         OI    RECSTAT,RSNEWSEC    SET NEW SECDEF RECORD                        
         B     VDEFK24                                                          
         MVC   LASTSECT,THISSECT                                                
         XC    SDFSNM,SDFSNM                                                    
         LA    R1,SDFSNMH                                                       
         OI    6(R1),X'80'                                                      
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$NFEDA)                                           
         B     EXITY                                                            
                                                                                
VDEFK22  CLC   THISSECT,LASTSECT   TEST DIFFERENT SECTION                       
         BE    VDEFK23                                                          
         XC    SDFSNM,SDFSNM       DISPLAY SECTION NAME                         
         OI    SDFSNMH+6,X'80'                                                  
         MVI   BCHALF,FFTTBSNM     SET SEARCH ARGUMENT FOR HELLO                
         MVC   BCHALF+1(1),THISSECT                                             
         LA    R2,SDFSNMH                                                       
         GOTO1 DISPLAY                                                          
VDEFK23  TM    PBSRSTAT,X'80'      TEST DELETED                                 
         BZ    VDEFK24                                                          
         NI    PBSRSTAT,X'FF'-X'80'                                             
         OI    RECSTAT,RSRESSEC    SET RESTORING SECDEF RECORD                  
         DROP  R5                                                               
                                                                                
VDEFK24  TM    RECSTAT,RSNEWFMT    IF NEW THEN VALIDATE                         
         BO    *+14                                                             
         CLC   THISKEY,LASTKEY     ARE KEYS THE SAME ?                          
         BNE   DDEF00              NO, DISPLAY NEW RECORD                       
         MVC   LASTKEY,THISKEY                                                  
         MVC   NUMBER,THISSECT                                                  
         OI    NUMBER,X'F0'                                                     
         B     VALL                                                             
                                                                                
***********************************************************************         
* VALIDATE ALL CHARGES                                                *         
***********************************************************************         
                                                                                
VALL     LA    R3,SCANBLK                                                       
         USING SCANBLKD,R3                                                      
         USING FLDHDRD,R2                                                       
         LA    R2,SDFALLH                                                       
         CLI   FLDIIND,FINPTHIS                                                 
         BZ    VALLX                                                            
         MVC   BCHALF(1),NUMBER                                                 
         MVI   BCHALF+1,BSDSALL                                                 
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'CC',APBS),(2,BCHALF)              
         GOTO1 VHELLO,BOPARM,,,(1,BCHALF+1)                                     
         GOTO1 AFVAL,SDFALLH                                                    
         BNE   VALLX                                                            
                                                                                
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),UC@YES                                                 
         BE    VALL05                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),UC@NO                                                  
         BNE   ERRNOTV                                                          
         B     VALLX                                                            
                                                                                
VALL05   L     R1,APBS                                                          
         MVI   BCFULL+1,BSDSALL                                                 
         MVC   BCFULL+2(1),UC@YES                                               
         MVC   IOKEY(L'PBSKEY),0(R1)                                            
         MVI   IOKEY+(PBSKSEC-PBSKEY),0                                         
         GOTO1 AIO,IO1+IOACCMST+IOHIGH                                          
         B     VALL12                                                           
VALL10   GOTO1 AIO,IO1+IOACCMST+IOSEQ                                           
VALL12   CLC   IOKEY(PBSKSEC-PBSKEY),IOKEYSAV                                   
         BNE   VALL20                                                           
         CLC   IOKEY+PBSKSEC-PBSKEY(1),THISSECT                                 
         BE    VALL10                                                           
         MVC   BCFULL(1),IOKEY+PBSKSEC-PBSKEY                                   
         OI    BCFULL,X'F0'                                                     
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(X'CC',AIO1),(3,BCFULL)              
         CLI   12(R1),0                                                         
         BNE   VALL10                                                           
         MVC   FVMSGNO,=AL2(AE$SADAC)  SECTION &T ALREADY IS ALL CHRGES         
         MVC   FVXTRA(1),IOKEY+(PBSKSEC-PBSKEY)                                 
         OI    FVXTRA,C'0'                                                      
         B     EXITN                                                            
                                                                                
VALL20   LA    RF,BCWORK                                                        
         XC    BCWORK,BCWORK                                                    
         USING BSDELD,RF                                                        
         MVI   BSDEL,BSDELQ                                                     
         MVC   BSDSEC,NUMBER                                                    
         MVI   BSDSTYP,BSDSALL                                                  
         MVC   BSDDATA(1),FVIFLD                                                
         MVI   BSDLN,BSDLN1Q+1                                                  
         DROP  RF                                                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(RF)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
VALLX    OI    FLDOIND,FOUTTRN                                                  
                                                                                
***********************************************************************         
* VALIDATE WORKCODE TYPE                                              *         
***********************************************************************         
                                                                                
VWCT     LA    R2,SDFWCTH                                                       
         CLI   FLDIIND,FINPTHIS                                                 
         BZ    VWCTX                                                            
         MVC   BCHALF(1),NUMBER                                                 
         MVI   BCHALF+1,BSDSWCT                                                 
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'CC',APBS),(2,BCHALF)              
         GOTO1 VHELLO,BOPARM,,,(1,BCHALF+1)                                     
         GOTO1 AFVAL,SDFWCTH                                                    
         BNE   VWCTX                                                            
         CLC   SDFALL(1),UC@YES                                                 
         BE    ERRFLDIN                                                         
                                                                                
         GOTO1 VSCANNER,BOPARM,SDFWCTH,SCANBLK                                  
         CLI   4(R1),0             INVALID INPUT                                
         BE    ERRNOTV                                                          
         CLI   4(R1),2                                                          
         BH    ERRNOTV             MORE THAN 2 TYPES INPUT                      
         CLC   SC1STFLD(1),SC1STFLD+SCBLKLQ                                     
         BE    ERRDUPIF            DUPLICATE INPUT                              
         ZIC   R4,4(R1)                                                         
                                                                                
         LA    R5,BCWORK                                                        
         XC    BCWORK,BCWORK                                                    
         USING BSDELD,R5                                                        
         MVI   BSDEL,BSDELQ                                                     
         MVC   BSDSEC,NUMBER                                                    
         MVI   BSDSTYP,BSDSWCT                                                  
         MVI   BSDLN,BSDLN1Q+1                                                  
                                                                                
VWCT05   ZIC   RE,SC1STLEN                                                      
         BCTR  RE,0                                                             
         CLI   CUCTRY,CTRYGER                                                   
         BE    VWCT10                                                           
         EX    RE,TESTTIME                                                      
         BE    VWCT15                                                           
         EX    RE,TESTCOST                                                      
         BE    VWCT16                                                           
         B     ERRIVTYP                                                         
VWCT10   EX    RE,TESTINT                                                       
         BE    VWCT15                                                           
         EX    RE,TESTEXT                                                       
         BE    VWCT16                                                           
         B     ERRIVTYP                                                         
                                                                                
VWCT15   MVI   BSDDATA,BSDTIMEQ                                                 
         B     *+8                                                              
VWCT16   MVI   BSDDATA,BSDCOSTQ                                                 
         DROP  R5                                                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(R5)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,SCBLKLQ(R3)                                                   
         BCT   R4,VWCT05                                                        
         OI    FLDOIND,FOUTTRN                                                  
         B     VWCTX                                                            
                                                                                
TESTTIME CLC   SC1STFLD(0),UC@TIME                                              
TESTCOST CLC   SC1STFLD(0),UC@COST                                              
TESTINT  CLC   SC1STFLD(0),UC@INT                                               
TESTEXT  CLC   SC1STFLD(0),UC@EXT                                               
                                                                                
VWCTX    DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE WORKCODE GROUP                                             *         
***********************************************************************         
                                                                                
VWCG     LA    R2,SDFWCGH                                                       
         LA    R3,SCANBLK                                                       
         CLI   FLDIIND,FINPTHIS                                                 
         BZ    VWCGX                                                            
         MVC   BCHALF(1),NUMBER                                                 
         MVI   BCHALF+1,BSDSWCG                                                 
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'CC',APBS),(2,BCHALF)              
         GOTO1 VHELLO,BOPARM,,,(1,BCHALF+1)                                     
         GOTO1 AFVAL,SDFWCGH                                                    
         BNE   VWCGX                                                            
         CLC   SDFALL(1),UC@YES                                                 
         BE    ERRFLDIN                                                         
         GOTO1 VSCANNER,BOPARM,SDFWCGH,SCANBLK                                  
         CLI   4(R1),0             INVALID INPUT                                
         BE    ERRNOTV                                                          
         ZIC   R4,4(R1)                                                         
                                                                                
         LA    R5,IOKEY                                                         
         USING WGRRECD,R5          BUILD WORKCODE GROUP RECORD                  
         XC    WGRKEY,WGRKEY                                                    
         MVI   WGRKTYP,WGRKTYPQ                                                 
         MVI   WGRKSUB,WGRKSUBQ                                                 
         MVC   WGRKCPY,CUABIN                                                   
         MVC   WGRKUNT(L'ULSJ),ULSJ                                             
                                                                                
VWCG05   CLI   SC1STLEN,L'WGRKCODE                                              
         BH    ERRNOTV             WORKCODE GROUPS ARE SINGLE CHARS             
         MVC   WGRKCODE,SC1STFLD                                                
         GOTO1 AIO,IO1+IOACCDIR+IORD                                            
         BNE   ERRNOTV             NOT FOUND                                    
                                                                                
         MVC   BCFULL(1),NUMBER    CHECK FOR DUPLICATES                         
         MVI   BCFULL+1,BSDSWCG                                                 
         MVC   BCFULL+2(L'WGRKCODE),WGRKCODE                                    
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(X'CC',APBS),(3,BCFULL)              
         CLI   12(R1),0                                                         
         BE    ERRDUPIF                                                         
                                                                                
         LA    RF,BCWORK           ADD BSDEL FOR THIS WC GROUP                  
         XC    BCWORK,BCWORK                                                    
         USING BSDELD,RF                                                        
         MVI   BSDEL,BSDELQ                                                     
         MVC   BSDSEC,NUMBER                                                    
         MVI   BSDSTYP,BSDSWCG                                                  
         MVC   BSDDATA(L'WGRKCODE),WGRKCODE                                     
         MVI   BSDLN,BSDLN1Q+L'WGRKCODE                                         
         DROP  RF,R5                                                            
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(RF)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,SCBLKLQ(R3)      BUMP TO NEXT SCANBLK ENTRY                   
         BCT   R4,VWCG05           REPEAT FOR EACH ENTRY                        
         OI    FLDOIND,FOUTTRN                                                  
VWCGX    DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE WORKCODE                                                   *         
***********************************************************************         
                                                                                
VWC      LA    R2,SDFWCH                                                        
         LA    R3,SCANBLK                                                       
         CLI   FLDIIND,FINPTHIS                                                 
         BZ    VWCX                                                             
         MVC   BCHALF(1),NUMBER                                                 
         MVI   BCHALF+1,BSDSWC                                                  
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'CC',APBS),(2,BCHALF)              
         GOTO1 VHELLO,BOPARM,,,(1,BCHALF+1)                                     
         GOTO1 AFVAL,SDFWCH                                                     
         BNE   VWCX                                                             
         CLC   SDFALL(1),UC@YES                                                 
         BE    ERRFLDIN                                                         
         GOTO1 VSCANNER,BOPARM,SDFWCH,SCANBLK                                   
         CLI   4(R1),0             INVALID INPUT                                
         BE    ERRNOTV                                                          
         ZIC   R4,4(R1)                                                         
                                                                                
         LA    R5,IOKEY                                                         
         USING WCORECD,R5          READ FOR WORKCODE GROUP RECORD               
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'ULSJ),ULSJ                                             
                                                                                
VWC05    CLI   SC1STLEN,L'WCOKWRK                                               
         BNE   ERRINWRK            WORKCODES ARE 2 CHARS                        
         MVC   WCOKWRK,SC1STFLD                                                 
         GOTO1 AIO,IO1+IOACCDIR+IORD                                            
         BNE   ERRINWRK            NOT FOUND                                    
                                                                                
         MVC   BCFULL(1),NUMBER    CHECK FOR DUPLICATES                         
         MVI   BCFULL+1,BSDSWC                                                  
         MVC   BCFULL+2(L'WCOKWRK),WCOKWRK                                      
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(X'CC',APBS),(4,BCFULL)              
         CLI   12(R1),0                                                         
         BE    ERRDUPIF                                                         
                                                                                
         LA    RF,BCWORK           ADD BSDEL FOR THIS WC                        
         XC    BCWORK,BCWORK                                                    
         USING BSDELD,RF                                                        
         MVI   BSDEL,BSDELQ                                                     
         MVC   BSDSEC,NUMBER                                                    
         MVI   BSDSTYP,BSDSWC                                                   
         MVC   BSDDATA(L'WCOKWRK),WCOKWRK                                       
         MVI   BSDLN,BSDLN1Q+L'WCOKWRK                                          
         DROP  RF,R5                                                            
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(RF)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,SCBLKLQ(R3)      BUMP TO NEXT SCANBLK ENTRY                   
         BCT   R4,VWC05            REPEAT FOR EACH ENTRY                        
         OI    FLDOIND,FOUTTRN                                                  
VWCX     DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE TRANSACTION TYPE                                           *         
***********************************************************************         
                                                                                
VTRN     LA    R2,SDFTRNH                                                       
         LA    R3,SCANBLK                                                       
         CLI   FLDIIND,FINPTHIS                                                 
         BZ    VTRNX                                                            
         MVC   BCHALF(1),NUMBER                                                 
         MVI   BCHALF+1,BSDSTRN                                                 
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'CC',APBS),(2,BCHALF)              
         GOTO1 VHELLO,BOPARM,,,(1,BCHALF+1)                                     
         GOTO1 AFVAL,SDFTRNH                                                    
         BNE   VTRNX                                                            
         CLC   SDFALL(1),UC@YES                                                 
         BE    ERRFLDIN                                                         
         GOTO1 VSCANNER,BOPARM,SDFTRNH,SCANBLK                                  
         CLI   4(R1),0             INVALID INPUT                                
         BE    ERRNOTV                                                          
         ZIC   R4,4(R1)                                                         
                                                                                
VTRN05   TM    SC1STVAL,SCNUMQ                                                  
         BZ    ERRIVTYP            TRAN TYPES ARE NUMERIC                       
         OC    SC1STNUM(3),SC1STNUM                                             
         BNZ   ERRIVTYP            TRAN TYPES ARE 1 BYTE                        
         LA    R5,TYPTAB           R5=A(TRAN TYPE TABLE)                        
         USING TYPTABD,R5                                                       
                                                                                
VTRN10   CLI   TYPNUM,EOT          CHECK EOT                                    
         BE    ERRIVTYP                                                         
         CLC   TYPNUM,SC1STNUM+3   MATCH ON NUMBER                              
         BE    *+12                                                             
VTRN15   LA    R5,TYPTABLN(R5)     BUMP TO NEXT TABLE ENTRY                     
         B     VTRN10                                                           
         CLI   TYPCTRY,CTRYALL     VALID FOR ALL COUNTRIES ?                    
         BE    *+14                                                             
         CLC   TYPCTRY,CUCTRY      VALID FOR THIS COUNTRY ?                     
         BNE   VTRN15                                                           
         TM    TYPIND1,TYPIDDS     DDS ONLY BATCH ?                             
         BZ    *+12                                                             
         CLI   CUSTAT,CUSDDS       DDS TERMINAL ?                               
         BNE   ERRIVTYP                                                         
                                                                                
         MVC   BCFULL(1),NUMBER    CHECK FOR DUPLICATES                         
         MVI   BCFULL+1,BSDSTRN                                                 
         MVC   BCFULL+2(L'TYPNUM),TYPNUM                                        
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(X'CC',APBS),(3,BCFULL)              
         CLI   12(R1),0                                                         
         BE    ERRDUPIF                                                         
                                                                                
         LA    RF,BCWORK           ADD BSDEL FOR THIS TRAN TYPE                 
         XC    BCWORK,BCWORK                                                    
         USING BSDELD,RF                                                        
         MVI   BSDEL,BSDELQ                                                     
         MVC   BSDSEC,NUMBER                                                    
         MVI   BSDSTYP,BSDSTRN                                                  
         MVC   BSDDATA(L'TYPNUM),TYPNUM                                         
         MVI   BSDLN,BSDLN1Q+L'TYPNUM                                           
         DROP  RF,R5                                                            
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(RF)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,SCBLKLQ(R3)      BUMP TO NEXT SCANBLK ENTRY                   
         BCT   R4,VTRN05           REPEAT FOR EACH ENTRY                        
         OI    FLDOIND,FOUTTRN                                                  
VTRNX    DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE CONTRA ACCOUNT                                             *         
***********************************************************************         
                                                                                
VCAC     LA    R2,SDFCACH                                                       
         LA    R3,SCANBLK                                                       
         CLI   FLDIIND,FINPTHIS                                                 
         BZ    VCACX                                                            
         MVC   BCHALF(1),NUMBER                                                 
         MVI   BCHALF+1,BSDSCAC                                                 
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'CC',APBS),(2,BCHALF)              
         GOTO1 VHELLO,BOPARM,,,(1,BCHALF+1)                                     
         GOTO1 AFVAL,SDFCACH                                                    
         BNE   VCACX                                                            
         CLC   SDFALL(1),UC@YES                                                 
         BE    ERRFLDIN                                                         
         GOTO1 VSCANNER,BOPARM,SDFCACH,SCANBLK                                  
         CLI   4(R1),0             INVALID INPUT                                
         BE    ERRNOTV                                                          
         ZIC   R4,4(R1)                                                         
                                                                                
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5          READ FOR ACCOUNT RECORD                      
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
                                                                                
VCAC05   CLI   SC1STLEN,3                                                       
         BL    ERRINACC            ULA IS AT LEAST 3 CHARS                      
         MVC   ACTKULA,BCSPACES                                                 
         ZIC   R1,SC1STLEN         GET INPUT LENGTH                             
         BCTR  R1,0                SUB 1                                        
         EX    R1,*+4                                                           
         MVC   ACTKULA(0),SC1STFLD                                              
         GOTO1 AIO,IO1+IOACCDIR+IORD                                            
         BNE   ERRINACC            NOT FOUND                                    
                                                                                
         LA    RF,BCWORK                                                        
         XC    BCWORK,BCWORK       CHECK FOR DUPLICATES                         
         USING BSDELD,RF                                                        
         MVI   BSDEL,BSDELQ                                                     
         MVC   BSDSEC,NUMBER                                                    
         MVI   BSDSTYP,BSDSCAC                                                  
         MVC   BSDDATA(L'ACTKULA),ACTKULA                                       
         IC    RE,SC1STLEN                                                      
         LA    RE,BSDLN1Q(RE)                                                   
         STC   RE,BSDLN                                                         
         L     R1,APBS                                                          
         LA    R1,(PBSRFST-PBSKEY)(R1)                                          
         XR    R0,R0                                                            
VCAC06   CLI   0(R1),0             EOR                                          
         BE    VCAC10                                                           
         CLI   0(R1),BSDELQ        TEST BSDEL                                   
         BNE   VCAC08                                                           
         CLI   3(R1),BSDSCAC       TEST CAC TYPE                                
         BNE   VCAC08                                                           
         CLC   BSDLN,1(R1)         TEST SAME LENGTH                             
         BNE   VCAC08                                                           
         ZIC   RE,BSDLN                                                         
         SH    RE,=Y(BSDLN1Q+1)                                                 
         EX    RE,*+8                                                           
         BE    ERRDUPIF            SAME CAC                                     
         CLC   BSDDATA(0),BSDLN1Q(R1)                                           
VCAC08   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VCAC06                                                           
                                                                                
         DROP  RF,R5               ADD BSDEL FOR THIS CONTRA ACCOUNT            
VCAC10   GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(RF)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,SCBLKLQ(R3)      BUMP TO NEXT SCANBLK ENTRY                   
         BCT   R4,VCAC05           REPEAT FOR EACH ENTRY                        
         OI    FLDOIND,FOUTTRN                                                  
VCACX    DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE SCHEME / CATEGORY                                          *         
***********************************************************************         
                                                                                
VCAT     LA    R2,SDFSCHH          INPUT SCHEME                                 
         LA    R3,SCANBLK                                                       
         MVC   BCHALF(1),NUMBER                                                 
         MVI   BCHALF+1,BSDSCAT                                                 
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'CC',APBS),(2,BCHALF)              
         GOTO1 VHELLO,BOPARM,,,(1,BCHALF+1)                                     
         GOTO1 AFVAL,SDFSCHH       ANY SCHEME INPUT ?                           
         BE    *+18                                                             
         MVC   SDFCAT,BCSPACES     NO, SO CLEAR ANY CATEGORIES                  
         OI    SDFCATH+6,X'80'                                                  
         B     VCATX                                                            
         CLC   SDFALL(1),UC@YES                                                 
         BE    ERRFLDIN                                                         
                                                                                
         GOTO1 AFVAL,SDFCATH       ANY CATEGORY INPUT ?                         
         BNE   ERRMISS             REQUIRED INPUT                               
         GOTO1 VSCANNER,BOPARM,SDFCATH,SCANBLK                                  
         CLI   4(R1),0             INVALID INPUT                                
         BE    ERRNOTV                                                          
         ZIC   R4,4(R1)            R4=NO. OF CATS INPUT                         
                                                                                
         LA    R5,IOKEY                                                         
         USING CATRECD,R5          BUILD CATEGORY RECORD                        
         XC    CATKEY,CATKEY                                                    
         MVI   CATKTYP,CATKTYPQ                                                 
         MVI   CATKSUB,CATKSUBQ                                                 
         MVC   CATKCPY,CUABIN                                                   
         MVC   CATKUNT(2),ULSJ                                                  
         MVC   CATKSCH,BCSPACES                                                 
         ZIC   R1,SDFSCHH+5        GET SCHEME CODE I/P LENGTH                   
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   CATKSCH(0),SDFSCH   MOVE INTO KEY                                
                                                                                
VCAT05   CLI   SC1STLEN,L'CATKCODE                                              
         BH    ERRNOTV                                                          
         MVC   CATKCODE,BCSPACES                                                
         ZIC   R1,SC1STLEN         GET CATEGORY CODE I/P LENGTH                 
         BCTR  R1,0                SUB 1                                        
         EX    R1,*+4                                                           
         MVC   CATKCODE(0),SC1STFLD  MOVE INTO KEY                              
         GOTO1 AIO,IO1+IOACCDIR+IORD                                            
         BNE   ERRRECNF            NOT FOUND                                    
                                                                                
         MVC   BCWORK(1),NUMBER    CHECK FOR DUPLICATES                         
         MVI   BCWORK+1,BSDSCAT                                                 
         MVC   BCWORK+2(L'CATKSCH+L'CATKCODE),CATKSCH                           
         LA    RF,L'CATKSCH+L'CATKCODE+2                                        
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(X'CC',APBS),((RF),BCWORK)           
         CLI   12(R1),0                                                         
         BE    ERRDUPIF                                                         
                                                                                
         LA    RF,BCWORK           ADD BSDEL FOR THIS SCH/CAT                   
         XC    BCWORK,BCWORK                                                    
         USING BSDELD,RF                                                        
         MVI   BSDEL,BSDELQ                                                     
         MVC   BSDSEC,NUMBER                                                    
         MVI   BSDSTYP,BSDSCAT                                                  
         MVC   BSDDATA(L'CATKSCH+L'CATKCODE),CATKSCH                            
         MVI   BSDLN,L'CATKSCH+L'CATKCODE+BSDLN1Q                               
         DROP  RF,R5                                                            
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(RF)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,SCBLKLQ(R3)      BUMP TO NEXT SCANBLK ENTRY                   
         BCT   R4,VCAT05           REPEAT FOR EACH ENTRY                        
         OI    FLDOIND,FOUTTRN     TRANSMIT SCHEME                              
         OI    SDFCATH+6,FOUTTRN   TRANSMIT CATS                                
VCATX    DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE PRINT NAME FOR SECTION                                     *         
***********************************************************************         
                                                                                
VSEC     LA    R2,SDFSECH                                                       
         CLI   FLDIIND,FINPTHIS                                                 
         BZ    VSECX                                                            
         MVI   BCBYTE1,FFTTSECN                                                 
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'DB',APBS),(1,BCBYTE1)             
         GOTO1 AFVAL,SDFSECH                                                    
         BNE   VSECX                                                            
                                                                                
         LA    RF,BCWORK                                                        
         XC    BCWORK,BCWORK                                                    
         USING FFTELD,RF                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTSECN    PRINT NAME FOR SECTION                       
         MVC   FFTSEQ,THISSECT                                                  
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   FFTDATA(0),FVIFLD   MOVE IN DATA                                 
         LA    R1,1(R1)                                                         
         STC   R1,FFTDLEN          MOVE IN DATA LENGTH                          
         LA    R1,FFTLN1Q+1(R1)                                                 
         STC   R1,FFTLN            MOVE IN ELEMENT LENGTH                       
         DROP  RF                                                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(RF)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    FLDOIND,FOUTTRN                                                  
VSECX    DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE PRINT NAME FOR SUB TOTAL                                   *         
***********************************************************************         
                                                                                
VSUB     LA    R2,SDFSUBH                                                       
         CLI   FLDIIND,FINPTHIS                                                 
         BZ    VSUBX                                                            
         MVI   BCBYTE1,FFTTSUBT                                                 
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),(X'DB',APBS),(1,BCBYTE1)             
         GOTO1 AFVAL,SDFSUBH                                                    
         BNE   VSUBX                                                            
                                                                                
         LA    RF,BCWORK                                                        
         XC    BCWORK,BCWORK                                                    
         USING FFTELD,RF                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTSUBT    PRINT NAME FOR SUB TOTAL                     
         MVC   FFTSEQ,THISSECT                                                  
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   FFTDATA(0),FVIFLD   MOVE IN DATA                                 
         LA    R1,1(R1)                                                         
         STC   R1,FFTDLEN          MOVE IN DATA LENGTH                          
         LA    R1,FFTLN1Q+1(R1)                                                 
         STC   R1,FFTLN            MOVE IN ELEMENT LENGTH                       
         DROP  RF                                                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(RF)                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    FLDOIND,FOUTTRN                                                  
VSUBX    DS    0H                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* VALIDATE SECTION NAME                                               *         
***********************************************************************         
                                                                                
VSECNAM  MVI   BCHALF,FFTTBSNM      DELETE EXISTING SECTION NAME                
         MVC   BCHALF+1(1),THISSECT                                             
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FFTELQ',APBS),(2,BCHALF)           
         LA    R2,SDFSNMH                                                       
         GOTO1 AFVAL,SDFSNMH                                                    
         CLI   FVILEN,0                                                         
         BE    ERRMISS                                                          
                                                                                
         LA    RF,BCWORK                                                        
         XC    BCWORK,BCWORK                                                    
         USING FFTELD,RF           ADD NEW SECTION NAME                         
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTBSNM                                                 
         MVC   FFTSEQ,THISSECT                                                  
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   FFTDATA(0),FVIFLD   MOVE IN DATA                                 
         LA    R1,1(R1)                                                         
         STC   R1,FFTDLEN          MOVE IN DATA LENGTH                          
         LA    R1,FFTLN1Q+1(R1)                                                 
         STC   R1,FFTLN            MOVE IN ELEMENT LENGTH                       
         DROP  RF,R2                                                            
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),APBS,(RF)                            
         CLI   12(R1),0                                                         
         BE    RIO                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE/ADD NEW SECDEF/FORMAT RECORDS                                 *         
***********************************************************************         
                                                                                
RIO      GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('BSDELQ',APBS),0                    
         CLI   12(R1),0            ANYTHING DEFINED ?                           
         BE    RIO20                                                            
         TM    RECSTAT,RSNEWFMT+RSNEWSEC+RSRESSEC                               
         BZ    RIO10                     THEN MUST BE DELETING                  
         LA    R1,SDFWCTH                NEED SOMETHING DEFINED                 
         ST    R1,FVADDR                                                        
         B     ERRMISS                                                          
                                                                                
RIO10    L     RE,AIO2             R1=A(FORMAT RECORD)                          
         XR    R0,R0                                                            
RIO11    LA    R1,PBCRFST-PBCKEY(RE)                                            
         OI    RECSTAT,RSDELSEC    SET DELETING SECDEF REC                      
         USING BLFELD,R1                                                        
RIO12    CLI   BLFEL,0             TEST IF ANY BLFELS FOR THIS SECTION          
         BE    RIO14               NO                                           
         CLI   BLFEL,BLFELQ                                                     
         BNE   *+14                                                             
         CLC   BLFSECT,THISSECT                                                 
         BE    RIO16               YES, CAN'T DELETE SECDEF RECORD THEN         
         IC    R0,BLFLN                                                         
         AR    R1,R0                                                            
         B     RIO12                                                            
         DROP  R1                                                               
                                                                                
RIO14    C     RE,AIO4             REPEAT FOR FMT CONT. REC IF THERE            
         BE    RIO15                                                            
         TM    RECSTAT,RSFMTCNT                                                 
         BZ    RIO15                                                            
         L     RE,AIO4                                                          
         B     RIO11                                                            
RIO15    L     R1,APBS                                                          
         OI    PBSRSTAT-PBSKEY(R1),X'80' SET DELETED BIT ON SECDEF              
         B     RIO22                                                            
                                                                                
RIO16    L     R1,AIO3             READ BACK UNCHANGED SECDEF RECORD            
         MVC   IOKEY(L'PBCKEY),0(R1)                                            
         GOTO1 AIO,IO3+IOACCMST+IORD                                            
         BE    DDEF00              AND REDISPLAY                                
         DC    H'0'                                                             
                                                                                
RIO20    TM    RECSTAT,RSNEWSEC    TEST NEW SECDEF RECORD                       
         BNO   RIO22               YES, ADD IT THEN                             
         GOTO1 AIO,IO3+IOACCMST+IOADDREC                                        
         BE    RIO30                                                            
         DC    H'0'                                                             
                                                                                
RIO22    GOTO1 AIO,IO3+IOACCMST+IOWRITE   WRITE BACK CHANGED SECDEF REC         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RECSTAT,RSDELSEC+RSRESSEC  TEST DELETING OR RESTORING IT         
         BZ    RIO30                                                            
                                                                                
         L     R1,AIO3             YES, THEN CHANGE DIRECTORIES ASWELL          
         MVC   IOKEY(L'PBSKEY),0(R1)                                            
         GOTO1 AIO,IO1+IOACCDIR+IORDUPD                                         
         BE    *+12                                                             
         NI    IOKEY+(PBSKSTAT-PBSKEY),X'FF'-X'80' RESTORE                      
         B     *+8                                                              
         OI    IOKEY+(PBSKSTAT-PBSKEY),X'80' SET DELETED                        
         GOTO1 AIO,IO1+IOACCDIR+IOWRITE      WRITE BACK                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   FVMSGNO,=AL2(AI$RDLNX)  RECORD DELETED MESSAGE                   
         TM    RECSTAT,RSDELSEC                                                 
         BO    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$RECAD)  RECORD ADDED MESSAGE                     
         LA    R1,SDFSNOH                                                       
         ST    R1,FVADDR                                                        
         B     EXITY                                                            
                                                                                
RIO30    TM    RECSTAT,RSNEWFMT+RSNEWNAM  TEST NEW/CHANGED FORMAT REC           
         BZ    RIO36                                                            
         L     R1,AIO1                                                          
         USING PBCRECD,R1                                                       
         XC    PBCKEY,PBCKEY       BUILD FORMAT RECORD KEY                      
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,THISFORM                                                 
         MVC   PBCKLANG,THISLANG                                                
         DROP  R1                                                               
         TM    RECSTAT,RSNEWFMT    TEST NEW FORMAT RECORD                       
         BO    RIO32                                                            
         MVC   IOKEY(L'PBCKEY),0(R1)                                            
         GOTO1 AIO,IO1+IOACCMST+IORDUP  NO, THEN READ IT                        
         BE    *+6                                                              
         DC    H'0'                DELETE CURRENT NAME                          
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('NAMELQ',AIO1),0                    
                                                                                
RIO32    GOTO1 AFVAL,SDFFNMH       VALIDATE AND ADD NEW NAME                    
         CLI   FVILEN,0                                                         
         BE    ERRMISS                                                          
         XC    BOELEM,BOELEM                                                    
         LA    R1,BOELEM                                                        
         USING NAMELD,R1                                                        
         MVI   NAMEL,NAMELQ                                                     
         IC    RF,FVILEN                                                        
         LA    RF,NAMLN1Q(RF)                                                   
         STC   RF,NAMLN                                                         
         MVC   NAMEREC,FVIFLD                                                   
         DROP  R1                                                               
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,BOELEM                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    RECSTAT,RSNEWFMT    TEST NEW FORMAT RECORD                       
         BO    RIO34                                                            
         GOTO1 AIO,IO1+IOACCMST+IOWRITE  WRITE                                  
         BE    RIO36                                                            
         DC    H'0'                                                             
RIO34    GOTO1 AIO,IO1+IOACCMST+IOADDREC  ADD                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AI$RECAD)  RECORD ADDED MESAGE                      
         B     *+10                                                             
                                                                                
RIO36    MVC   FVMSGNO,=AL2(AI$AOKNX)  RECORD CHANGED MESSAGE                   
         LA    R1,SDFWCTH                                                       
         ST    R1,FVADDR                                                        
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY SECDEF RECORD                                               *         
***********************************************************************         
                                                                                
DDEF00   L     RF,APBS                                                          
         USING PBSRECD,RF                                                       
         LA    R1,PBSRFST                                                       
         DROP  RF                                                               
         SR    R0,R0                                                            
         MVC   SDFWCT,BCSPACES                                                  
         MVC   SDFWCG,BCSPACES                                                  
         MVC   SDFWC,BCSPACES                                                   
         MVC   SDFTRN,BCSPACES                                                  
         MVC   SDFCAC,BCSPACES                                                  
         MVC   SDFSCH,BCSPACES                                                  
         MVC   SDFCAT,BCSPACES                                                  
         MVC   SDFALL,BCSPACES                                                  
         MVC   SDFSEC,BCSPACES                                                  
         MVC   SDFSUB,BCSPACES                                                  
                                                                                
         USING BSDELD,R1                                                        
DDEF10   CLI   BSDEL,0                                                          
         BE    DDEF100                                                          
         ST    R1,SAVER                                                         
         CLI   0(R1),BSDELQ                                                     
         BNE   DDEF15                                                           
         CLI   2(R1),C'0'        IF FIRST BYTE AFTER LENGTH IS < F0             
         BNL   *+6               THEN THIS IS AN OLD ELEMENT                    
         BCTR  R1,0                                                             
         CLI   BSDSTYP,BSDSWCT     WORKCODE TYPE                                
         BE    DWCT                                                             
         CLI   BSDSTYP,BSDSWCG     WORKCODE GROUP                               
         BE    DWCG                                                             
         CLI   BSDSTYP,BSDSWC      WORKCODE                                     
         BE    DWC                                                              
         CLI   BSDSTYP,BSDSTRN     TRANSACTION TYPE                             
         BE    DTRN                                                             
         CLI   BSDSTYP,BSDSCAC     CONTRA ACCOUNT                               
         BE    DCAC                                                             
         CLI   BSDSTYP,BSDSCAT     ESTIMATE CATEGORY                            
         BE    DEST                                                             
         CLI   BSDSTYP,BSDSALL     ALL                                          
         BE    DALL                                                             
         DC    H'0'                                                             
DDEF15   CLI   0(R1),FFTELQ                                                     
         BNE   DDEF20                                                           
         CLI   BSDEL+(FFTTYPE-FFTEL),FFTTSECN                                   
         BE    DSEC                                                             
         CLI   BSDEL+(FFTTYPE-FFTEL),FFTTSUBT                                   
         BE    DSUB                                                             
DDEF20   L     R1,SAVER                                                         
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DDEF10                                                           
                                                                                
DWCT     LA    R2,SDFWCT           DISPLAY WORKCODE TYPE                        
         BAS   RE,FINDEND                                                       
         CLI   BSDDATA,BSDTIMEQ                                                 
         BNE   DWCT02                                                           
         CLI   CUCTRY,CTRYGER                                                   
         BE    *+14                                                             
         MVC   0(L'UC@TIME,R2),UC@TIME                                          
         B     DDEF20                                                           
         MVC   0(L'UC@INT,R2),UC@INT                                            
         B     DDEF20                                                           
DWCT02   CLI   CUCTRY,CTRYGER                                                   
         BE    *+14                                                             
         MVC   0(L'UC@COST,R2),UC@COST                                          
         B     DDEF20                                                           
         MVC   0(L'UC@EXT,R2),UC@EXT                                            
         B     DDEF20                                                           
                                                                                
DWCG     LA    R2,SDFWCG           DISPLAY WORKCODE GROUP                       
         BAS   RE,FINDEND                                                       
         MVC   0(L'WGRKCODE,R2),BSDDATA                                         
         B     DDEF20                                                           
                                                                                
DWC      LA    R2,SDFWC            DISPLAY WORKCODE                             
         BAS   RE,FINDEND                                                       
         MVC   0(L'WCOKWRK,R2),BSDDATA                                          
         B     DDEF20                                                           
                                                                                
DTRN     LA    R2,SDFTRN           DISPLAY TRANSACTION TYPE                     
         BAS   RE,FINDEND                                                       
         EDIT  (B1,BSDDATA),(3,(R2)),0,ALIGN=LEFT,DUB=BCDUB,WRK=BCWORK          
         B     DDEF20                                                           
                                                                                
DCAC     LA    R2,SDFCAC           DISPLAY CONTRA ACCOUNT                       
         BAS   RE,FINDEND                                                       
         L     RF,SAVER            *                                            
         IC    RE,1(RF)            *                                            
         SH    RE,=Y(BSDLN1Q+1)                                                 
         CR    RF,R1               *                                            
         BE    *+8                 *                                            
         LA    RE,1(RE)            *                                            
         EX    RE,*+8                                                           
         B     DDEF20                                                           
         MVC   0(0,R2),BSDDATA                                                  
                                                                                
DEST     CLC   SDFSCH,BCSPACES     DISPLAY SCHEME / CATEGORY                    
         BH    *+10                                                             
         MVC   SDFSCH(L'CATKSCH),BSDDATA                                        
         LA    R2,SDFCAT                                                        
         BAS   RE,FINDEND                                                       
         MVC   0(L'CATKCODE,R2),BSDDATA+L'CATKSCH                               
         B     DDEF20                                                           
                                                                                
DALL     LA    R2,SDFALL           DISPLAY IF ALL CHARGES                       
         CLC   BSDDATA(1),UC@YES                                                
         BNE   *+14                                                             
         MVC   0(L'UC@YES,R2),UC@YES                                            
         B     *+10                                                             
         MVC   0(L'UC@NO,R2),UC@NO                                              
         B     DDEF20                                                           
                                                                                
         USING FFTELD,R1                                                        
DSEC     ZIC   RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   SDFSEC(0),FFTDATA                                                
         B     DDEF20                                                           
                                                                                
DSUB     ZIC   RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   SDFSUB(0),FFTDATA                                                
         B     DDEF20                                                           
         DROP  R1                                                               
                                                                                
DDEF100  OI    SDFWCTH+6,X'80'                                                  
         OI    SDFWCGH+6,X'80'                                                  
         OI    SDFWCH+6,X'80'                                                   
         OI    SDFTRNH+6,X'80'                                                  
         OI    SDFCACH+6,X'80'                                                  
         OI    SDFSCHH+6,X'80'                                                  
         OI    SDFCATH+6,X'80'                                                  
         OI    SDFALLH+6,X'80'                                                  
         OI    SDFSECH+6,X'80'                                                  
         OI    SDFSUBH+6,X'80'                                                  
         TM    RECSTAT,RSDELSEC    TEST REDISPLAYING                            
         BO    ERRNODEL                                                         
         LA    R1,SDFWCTH                                                       
         ST    R1,FVADDR                                                        
         MVC   LASTKEY,THISKEY                                                  
         TM    RECSTAT,RSNEWSEC                                                 
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AI$NFEDA)                                           
         B     *+10                                                             
         MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE FORMAT NUMBER/LANGUAGE AND GET FORMAT RECORD INTO IO2      *         
* EXIT CC EQU:OK, HIGH:RECORD NOT FOUND, LOW:MISSING/INVALID NUM/LANG *         
***********************************************************************         
                                                                                
VALFKEY  NTR1  ,                                                                
         LA    R5,IOKEY                                                         
         MVI   FVMINL,1                                                         
         LA    R2,SNMFNOH                                                       
         CLI   CSACT,ACTSNM                                                     
         BE    *+8                                                              
         LA    R2,SDFFNOH                                                       
         GOTO1 AFVAL,(R2)          ANY FORMAT NUMBER ?                          
         BE    VALFK08                                                          
         L     R1,ALSVALS                                                       
         USING LSVALSD,R1                                                       
         USING TLSTD,LSTLST                                                     
         CLI   TLACT,ACTLFT                                                     
         BNE   VALFK04                                                          
         LA    R1,TLCDIR                                                        
         MVC   THISFKEY,PBCKFMT-PBCKEY(R1)                                      
         B     VALFK06                                                          
         DROP  R1                                                               
VALFK04  CLI   CSFORMAT,0          TEST IF CSFORMAT SET                         
         BE    ERRMISS                                                          
         MVC   THISFORM,CSFORMAT                                                
         MVC   THISLANG,CSFMLANG                                                
VALFK06  EDIT  (B1,THISFORM),(3,8(R2)),ALIGN=LEFT,DUB=BCDUB,WRK=BCWORK          
         GOTO1 DISLNG,THISLANG                                                  
         B     VALFK10                                                          
                                                                                
VALFK08  TM    FVIIND,FVINUM       VALIDATE FORMAT NUMBER                       
         BZ    ERRNOTV                                                          
         OC    BCFULL(3),BCFULL                                                 
         BNZ   ERRNOTV                                                          
         CLI   BCFULL+3,0                                                       
         BE    ERRNOTV                                                          
         MVC   THISFORM,BCFULL+3                                                
                                                                                
         USING PBCRECD,R5                                                       
VALFK10  XC    PBCKEY,PBCKEY       CHECK FOR FORMAT RECORD                      
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,THISFORM                                                 
         GOTO1 VALLNG,THISLANG                                                  
         BNE   EXITL               INVALID LANGUAGE                             
         MVC   PBCKLANG,THISLANG                                                
         GOTO1 AIO,IO2+IOACCMST+IORD                                            
         BE    EXITY                                                            
         B     EXITH               RECORD NOT FOUND                             
         DROP  R5                                                               
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE LANGUAGE FIELD FOR GERMANY                      *         
* R1=A(FOR OUTPUT OF VALID LANGUAGE CODE)                             *         
***********************************************************************         
                                                                                
VALLNG   NTR1  ,                                                                
         LR    R3,R1                                                            
         CLI   CUCTRY,CTRYGER                                                   
         BNE   EXITY                                                            
         LA    R2,SDFLNGH                                                       
         CLI   CSACT,ACTSDF                                                     
         BE    *+8                                                              
         LA    R2,SNMLNGH                                                       
         GOTO1 AFVAL,(R2)                                                       
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   0(R3),0                                                          
         B     EXITY                                                            
                                                                                
         L     R2,ALANG                                                         
         LA    R2,6(R2)                                                         
         USING LANGTABD,R2                                                      
VLNG02   CLI   LANGTABD,FF                                                      
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFELANG)                                           
         B     EXITN                                                            
         GOTO1 CMPWRD,LANGSHR                                                   
         BE    VLNG10                                                           
         GOTO1 CMPWRD,LANGSHRN                                                  
         BE    VLNG10                                                           
         GOTO1 CMPWRD,LANGFUL                                                   
         BE    VLNG10                                                           
         GOTO1 CMPWRD,LANGFULN                                                  
         BE    VLNG10                                                           
VLNG08   LA    R2,LANGTABL(R2)                                                  
         B     VLNG02                                                           
                                                                                
VLNG10   MVC   0(L'PBCKLANG,R3),LANGCODE                                        
         CLC   0(L'PBCKLANG,R3),CULANG                                          
         BNE   *+12                                                             
         MVI   0(R3),0                                                          
         B     EXITY                                                            
         CLI   0(R3),LANGENG                                                    
         BNE   EXITY                                                            
         MVI   0(R3),LANGEUK                                                    
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY LANGUAGE FIELD FOR GERMANY   R1=A(LANG CODE)     *         
***********************************************************************         
                                                                                
DISLNG   NTR1  ,                                                                
         CLI   CUCTRY,CTRYGER                                                   
         BNE   EXITY                                                            
         L     R2,ALANG                                                         
         LA    R2,6(R2)                                                         
         MVC   BOBYTE1,0(R1)                                                    
         CLI   BOBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   BOBYTE1,CULANG                                                   
         B     DLNG02                                                           
         CLI   BOBYTE1,LANGEUK                                                  
         BNE   DLNG02                                                           
         MVI   BOBYTE1,LANGENG                                                  
         USING LANGTABD,R2                                                      
DLNG02   CLI   LANGTABD,FF                                                      
         BE    EXITN                                                            
         CLC   LANGCODE,BOBYTE1                                                 
         BE    DLNG10                                                           
         LA    R2,LANGTABL(R2)                                                  
         B     DLNG02                                                           
                                                                                
DLNG10   LA    R1,SDFLNGH                                                       
         CLI   CSACT,ACTSDF                                                     
         BE    *+8                                                              
         LA    R1,SNMLNGH                                                       
         MVC   L'SDFLNGH(L'LANGFULN,R1),LANGFULN                                
         MVI   5(R1),L'LANGFULN                                                 
         OI    6(R1),X'80'                                                      
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO GET FFTEL FOR SECTION NAME AND DISPLAY                   *         
***********************************************************************         
                                                                                
DISPLAY  NTR1  ,                                                                
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(X'DB',AIO3),(2,BCHALF)              
         CLI   12(R1),0                                                         
         BNE   EXIT                SECTION NUMBER NOT FOUND                     
         L     R3,12(R1)                                                        
         USING FFTELD,R3                                                        
         LA    RE,SECNMLNQ         MOVE OUT SECTION NAME                        
         CLI   FFTDLEN,SECNMLNQ                                                 
         BH    *+8                                                              
         IC    RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   8(0,R2),FFTDATA                                                  
         B     EXIT                                                             
         DROP  R3                                                               
***********************************************************************         
* FINDEND ROUTINE FINDS END OF TEXT STRING AT R2, ADDS SEPERATOR      *         
***********************************************************************         
                                                                                
FINDEND  LR    RF,R2                                                            
         CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         CR    RF,R2                                                            
         BER   RE                                                               
         MVI   0(R2),C','                                                       
         CLI   CULANG,LANGGER                                                   
         BNE   *+8                                                              
         MVI   0(R2),C'#'                                                       
         LA    R2,1(R2)                                                         
         BR    RE                                                               
                                                                                
***********************************************************************         
* COMPARE INPUT WITH SOME KIND OF A WORD                              *         
* NTRY: R1=A(WORD)                                                    *         
***********************************************************************         
                                                                                
CMPWRD   LA    RF,2                                                             
         CLI   FVXLEN,2                                                         
         BH    *+8                                                              
         IC    RF,FVXLEN                                                        
         EX    RF,*+6                                                           
         BR    RE                                                               
         CLC   FVIFLD(0),0(R1)                                                  
                                                                                
ERRMISS  MVC   FVMSGNO,=AL2(FVFNONE)   MISSING INPUT FIELD                      
         B     EXITN                                                            
                                                                                
ERRNOTV  MVC   FVMSGNO,=AL2(FVFNOTV)   FIELD NOT VALID                          
         B     EXITN                                                            
                                                                                
ERRRECNF MVC   FVMSGNO,=AL2(AE$RECNF)  RECORD NOT FOUND                         
         B     EXITN                                                            
                                                                                
ERRDUPIF MVC   FVMSGNO,=AL2(AE$DUPIF)  DUPLICATE INPUT FIELD                    
         B     EXITN                                                            
                                                                                
ERRINACC MVC   FVMSGNO,=AL2(AE$INACC)  INVALID ACCOUNT                          
         B     EXITN                                                            
                                                                                
ERRINWRK MVC   FVMSGNO,=AL2(AE$INWRK)  INVALID WORKCODE                         
         B     EXITN                                                            
                                                                                
ERRIVTYP MVC   FVMSGNO,=AL2(AE$IVTYP)  INVALID TYPE                             
         B     EXITN                                                            
                                                                                
ERRFLDIN MVC   FVMSGNO,=AL2(AE$FLDIN)  INCOMPATIBLE INPUT FIELDS                
         B     EXITN                                                            
                                                                                
ERRNODEL MVC   FVMSGNO,=AL2(AE$NODEL)  CANT DELETE - SECTION IN USE             
         LA    R1,SDFSNOH                                                       
         ST    R1,FVADDR                                                        
         B     EXITN                                                            
                                                                                
EXITH    MVI   FVOMTYP,GTMERR                                                   
         CLI   *,0                                                              
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    MVI   FVOMTYP,GTMERR                                                   
         CLI   *,FF                                                             
         B     EXIT                                                             
EXITY    MVI   FVOMTYP,GTMINF                                                   
         CR    RB,RB                                                            
EXIT     XIT1  ,                                                                
                                                                                
ULSJ     DC    C'SJ'                                                            
ACCMST   DC    C'ACCMST '                                                       
FF       EQU   X'FF'                                                            
         LTORG                                                                  
                                                                                
TYPTAB   DS    0X                  TYPE/INDICATOR/COUNTRY                       
*&&US                                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(3)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(5)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(6)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(7)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(8)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(14)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(15)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(19)                                                          
         DC    AL1(TYPINOD+TYPIDDS)                                             
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(20)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(21)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(22)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(26)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(27)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(30)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(33)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(34)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(36)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(37)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(41)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(45)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(46)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(47)                                                          
         DC    AL1(TYPINOD)                                                     
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(48)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(49)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(51)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(53)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(54)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(55)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(56)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(57)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(58)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(61)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYUSA)                                                     
                                                                                
         DC    AL1(62)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(96)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(97)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(98)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(99)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
*&&                                                                             
*&&UK                                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(3)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(5)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(6)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(7)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(8)                                                           
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(11)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(14)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(17)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
                                                                                
         DC    AL1(18)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(19)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(21)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(22)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(26)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(27)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
                                                                                
         DC    AL1(30)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(34)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(36)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(37)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(45)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(46)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
                                                                                
         DC    AL1(49)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(57)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(58)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(59)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(70)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
                                                                                
         DC    AL1(70)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
                                                                                
         DC    AL1(70)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYHOL)                                                     
                                                                                
         DC    AL1(71)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
                                                                                
         DC    AL1(71)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYGER)                                                     
                                                                                
         DC    AL1(71)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYHOL)                                                     
                                                                                
         DC    AL1(75)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYGBR)                                                     
                                                                                
         DC    AL1(75)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYHOL)                                                     
                                                                                
         DC    AL1(79)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(96)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(97)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(98)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
                                                                                
         DC    AL1(99)                                                          
         DC    AL1(0)                                                           
         DC    AL1(CTRYALL)                                                     
*&&                                                                             
TYPTABX  DC    AL1(EOT)                                                         
                                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
                                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
                                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
                                                                                
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
                                                                                
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
                                                                                
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORKB                                                     
                                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBE5D                                                       
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBE4D                                                       
                                                                                
TYPTABD  DSECT                                                                  
TYPNUM   DS    XL1                 TRANSACTION TYPE NUMBER                      
TYPIND1  DS    XL1                 INDICATORS - ONE                             
TYPIDDS  EQU   X'80'               DDS ONLY BATCH TYPE                          
TYPINOD  EQU   X'40'               DON'T DISPLAY AT ALL                         
TYPCTRY  DS    XL1                 COUNTRY                                      
TYPTABLN EQU   *-TYPTABD                                                        
                                                                                
WORKD    DSECT                                                                  
         ORG   OVERWRK                                                          
SAVER    DS    F                                                                
NUMBER   DS    XL1                 SECTION NUMBER IN EBCDIC                     
APBS     DS    A                   A(PBSREC)                                    
RECSTAT  DS    XL1                 RECORD STATUS                                
RSNEWFMT EQU   X'80'               NEW FORMAT RECORD                            
RSNEWSEC EQU   X'40'               NEW SECDEF RECORD                            
RSNEWNAM EQU   X'20'               NEW FORMAT NAME                              
RSDELSEC EQU   X'10'               DELETE SECDEF RECORD                         
RSRESSEC EQU   X'08'               RESTORE SECDEF RECORD                        
RSFMTCNT EQU   X'04'               FORMAT CONTINUATION RECORD FOUND             
SCANBLK  DS    XL(10*32)           SCANNER OUTPUT FOR UPTO 10 FIELDS            
                                                                                
OSVALSD  DSECT                                                                  
LASTKEY  DS    0XL3                                                             
LASTSECT DS    XL1                 LAST SECTION NUMBER                          
LASTFKEY DS    0XL2                LAST FORMAT KEY                              
LASTFORM DS    XL1                 LAST FORMAT NUMBER                           
LASTLANG DS    XL1                 LAST FORMAT LANGUAGE CODE                    
                                                                                
THISKEY  DS    0XL3                                                             
THISSECT DS    XL1                 THIS SECTION NUMBER                          
THISFKEY DS    0XL2                THIS FORMAT KEY                              
THISFORM DS    XL1                 THIS FORMAT NUMBER                           
THISLANG DS    XL1                 THIS FORMAT LANGUAGE CODE                    
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070ACCLB23   08/16/00'                                      
         END                                                                    
