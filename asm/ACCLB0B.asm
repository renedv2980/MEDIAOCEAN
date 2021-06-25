*          DATA SET ACCLB0B    AT LEVEL 070 AS OF 08/16/00                      
*PHASE T6210BA                                                                  
*INCLUDE ACGETTXT                                                               
CLB0B    TITLE '- BILL PROGRAM - PRINTING '                                     
CLB0B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL TXL,**CLBB**,CLEAR=YES,R8,RR=RE                                  
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         L     R7,AREP                                                          
         USING REPD,R7             R7=A(REPORT BLOCK)                           
         LR    RF,RC                                                            
         L     RC,AOVERWRK                                                      
         USING PWORKD,RC           RC=A(LOCAL W/S)                              
         USING FBLKD,FFMTBLK                                                    
         USING BOFELD,FBBOFEL                                                   
         USING BLHELD,PBLHEL                                                    
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         ST    RF,ATXBLOCK                                                      
         A     RE,=V(ACGETTXT)                                                  
         ST    RE,ACGETTXT                                                      
*                                                                               
         GOTO1 VDICTAT,BOPARM,C'LL  ',DICI,DICO                                 
         MVI   RPTINDS,0                                                        
         CLI   CSACT,ACTUPD        TEST PRINTING LIVE REPORT                    
         BNE   *+14                                                             
         OI    RPTINDS,RPTILIVE                                                 
         MVC   RPTPRGID,=C'BL'                                                  
         CLI   CSACT,ACTDRA        TEST PRINTING DRAFT REPORT                   
         BNE   *+14                                                             
         OI    RPTINDS,RPTIDRFT                                                 
         MVC   RPTPRGID,=C'BD'                                                  
         CLI   CSACT,ACTLST        TEST RE-PRINTING DRAFT REPORT                
         BNE   *+14                                                             
         OI    RPTINDS,RPTIREPR                                                 
         MVC   RPTPRGID,=C'BL'                                                  
*                                                                               
         BAS   RE,BILLINIT                                                      
         BAS   RE,TXTINIT                                                       
         BAS   RE,TXTHEAD                                                       
         BAS   RE,BILLPRT                                                       
         BAS   RE,BILLTOTS                                                      
         BAS   RE,BILLVAT                                                       
         BAS   RE,PREVBILL                                                      
         BAS   RE,TXTFOOT                                                       
*                                                                               
         MVI   REPACTN,REPACLO                                                  
         GOTO1 REPORT                                                           
*                                                                               
         TM    RPTINDS,RPTILIVE+RPTIREPR                                        
         BZ    PRINT02             UPDATE BILL HEADER RECORD                    
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRINT02  MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RPSPL)                                           
         MVI   FVPARMS,9                                                        
         MVC   FVPARMS+1(L'REPSUBID),REPSUBID                                   
         MVC   FVPARMS+1+L'REPSUBID(1),BCCOMMA                                  
         LA    RF,FVPARMS+1+L'REPSUBID+1                                        
         EDIT (2,REPREPNO),(5,(RF)),ALIGN=LEFT                                  
         LA    RE,BASACTH                                                       
         ST    RE,FVADDR                                                        
         CLI   P#DQU,C'Y'          TEST ALWAYS OUTPUT DQU                       
         BE    PRINT04                                                          
         TM    RPTINDS,RPTIDRFT                                                 
         BO    *+16                                                             
         CLI   P#DQU,C'L'          TEST ONLY FOR LIVE                           
         BE    PRINT04                                                          
         B     PRINTX                                                           
         CLI   P#DQU,C'D'          TEST ONLY FOR DRAFT                          
         BNE   PRINTX                                                           
*                                                                               
PRINT04  MVC   BASSRV,DQU                                                       
         OI    BASSRVH+FHOID,FHOITR                                             
*                                                                               
PRINTX   B     EXITY                                                            
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALZE FOR CONTROL RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
BILLINIT NTR1  ,                                                                
         ZAP   TBILLNET,BCPZERO                                                 
         ZAP   TBILLCOM,BCPZERO                                                 
         ZAP   TBILLGRS,BCPZERO                                                 
*                                                                               
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,CSBILNUM                                                
         GOTO1 AIO,IOHIGH+IOACCDIR                                              
         CLC   PBRPAS(PBRPIND-PBRPAS),IOKEYSAV                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,PBRKDA                                                  
         LA    R1,IOGET+IOACCMST+IO1                                            
         TM    RPTINDS,RPTILIVE+RPTIREPR                                        
         BZ    *+8                 READ FOR UPDATE IF LIVE/REPRINTING           
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
*                                                                               
         XR    RF,RF                                                            
         LA    R3,PBRRFST                                                       
         USING BLHELD,R3                                                        
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
         TM    RPTINDS,RPTILIVE+RPTIREPR                                        
         BZ    BINIT02                                                          
         MVC   BLHPDATE,BCTODAYC   SET DATE/TIME PRINTED                        
         ICM   RF,15,ASTIME                                                     
         SRL   RF,4                                                             
         STCM  RF,7,BLHPTIME                                                    
         IC    RF,BLHPNUM          INCREMENT NO. OF TIMES PRINTED               
         LA    RF,1(RF)                                                         
         STC   RF,BLHPNUM                                                       
BINIT02  MVC   PBLHEL(BLHLNQ),BLHELD                                            
         DROP  R3                                                               
*                                                                               
         OC    BLHSCH,BLHSCH       CHECK SURCHARGE IS SET                       
         BNZ   *+10                                                             
         ZAP   BLHSCH,BCPZERO                                                   
         OC    BLHDSC,BLHDSC       CHECK DISCOUNT IS SET                        
         BNZ   *+10                                                             
         ZAP   BLHDSC,BCPZERO                                                   
         CLC   BLHCUR,BCSPACES     TEST CURRENCY SET IN ELEMENT                 
         BH    *+10                                                             
         MVC   BLHCUR,CSCPYCUR     NO - USE COMPANY CURRENCY                    
*                                                                               
         CLC   BLHCUR,CSBILCUR     COPY BILLING CURRENCY TABLE ENTRY            
         BNE   *+14                                                             
         MVC   PCURBIL,CSCURBIL                                                 
         B     BINIT04                                                          
         CLC   BLHCUR,CSCPYCUR     COPY AGENCY CURRENCY TABLE ENTRY             
         BNE   *+14                                                             
         MVC   PCURBIL,CSCURCPY                                                 
         B     BINIT04                                                          
         GOTO1 VBLDCUR,BOPARM,BLHCUR,(X'80',PCURBIL),ACOM                       
*                                                                               
BINIT04  CLC   CSCPYCUR,BLHCUR     TEST BILLING IN AGENCY CURRENCY              
         BNE   BINIT06                                                          
         OI    RPTINDS,RPTIBAGY                                                 
         CLI   CUCTRY,CTRYGER      TEST GERMAN AGENCY BILLING IN DM             
         BNE   BINIT06             BECAUSE IF SO THE PREFIX MUST BE 3           
         OI    PCURBIL+(CURTPIND-CURTABD),X'03'                                 
*                                                                               
BINIT06  ZAP   TVATAGY,BCPZERO     SET AGENCY VAT TOTAL                         
         XC    TBILLSCH,TBILLSCH   SURCHARGE NOT SET                            
         XC    TBILLDSC,TBILLDSC   DISCOUNT NOT SET                             
*                                                                               
         LA    R1,PBRRFST                                                       
         USING SCIELD,R1                                                        
         XR    RF,RF                                                            
BINIT12  CLI   SCIELD,0                                                         
         BE    BINIT18                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   BINIT16                                                          
         CLI   SCITYPE,SCITTTAX                                                 
         BNE   *+14                                                             
         ZAP   TVATAGY,SCIAMNT     AGENCY VAT TOTAL                             
         B     BINIT16                                                          
         CLI   SCITYPE,SCITCBSG                                                 
         BNE   *+14                                                             
         ZAP   TBILLSCH,SCIAMNT    SURCHARGE AMOUNT                             
         B     BINIT16                                                          
         CLI   SCITYPE,SCITCBDC                                                 
         BNE   *+10                                                             
         ZAP   TBILLDSC,SCIAMNT    DISCOUNT AMOUNT                              
BINIT16  IC    RF,SCILN                                                         
         BXH   R1,RF,BINIT12                                                    
         DROP  R1                                                               
*                                                                               
BINIT18  TM    RPTINDS,RPTIREPR    TEST RE-PRINTING                             
         BO    BINIT20                                                          
         ZAP   TVATAGY,LSUVATT                                                  
         TM    RPTINDS,RPTILIVE    SAVE VAT ON RECORD IF MAKING LIVE            
         BZ    BINIT20                                                          
         PUSH  USING                                                            
         USING SCIELD,BOELEM                                                    
         XC    SCIELD(SCILN1Q),SCIELD                                           
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITTTAX                                                 
         ZAP   SCIAMNT,TVATAGY                                                  
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBRRECD,SCIELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
BINIT20  ZAP   TVATBIL,TVATAGY     CALCULATE BILLING VAT TOTAL                  
         TM    RPTINDS,RPTIBAGY                                                 
         BO    BINIT22                                                          
         GOTO1 EXCHANGE,BOPARM,TVATAGY,TVATBIL                                  
*                                                                               
BINIT22  LA    R2,IOKEY                                                         
         USING PBCRECD,R2          R2=A(CONTROL RECORD)                         
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,BLHFORM                                                  
         CLI   PBCKFMT,0                                                        
         BNE   BINIT24                                                          
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   PBCKFMT,GOBILFRM-GOBBLOCK(RF)                                    
BINIT24  LH    R1,=Y(IO4)                                                       
         GOTO1 AIO,IOACCMST+IOREAD(R1)                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFMTBLK,BOPARM,('FBGET',FBLKD),AIO4                              
         MVC   REPMAXL,BOFMAXLN    MAX LINES PER PAGE                           
*        MVC   REPWIDTH,BOFMAXWD   MAX LINE WIDTH                               
         MVC   REPTOPSA,BOFPAGSP   SPACING AT TOP OF PAGE                       
         TM    BOFINDS2,BOFIUPCA   TEST UPPER CASE PRINTING                     
         BNO   *+8                                                              
         NI    REPIND2,FF-REPILOW                                               
         L     R2,AIO4                                                          
*                                                                               
         LA    RE,TIMEPAR          ZEROISE PARAGRAPH DETAILS                    
         LA    RF,L'TIMEPAR+L'COSTPAR                                           
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R3,PBCRFST                                                       
         USING BLFELD,R3                                                        
BINIT26  CLI   BLFEL,0                                                          
         BE    BINIT40                                                          
         CLI   BLFEL,BLFELQ                                                     
         BNE   BINIT38                                                          
         LA    R4,TIMEPAR                                                       
         CLI   BLFTYPE,BLFTCSTQ                                                 
         BNE   *+8                                                              
         LA    R4,COSTPAR                                                       
         USING PARD,R4             R4=A(PARAGRAPH DETAILS)                      
*                                                                               
         GOTO1 AFMTHED,BOPARM,(X'01',BLFELD)                                    
         BNE   BINIT28                                                          
         LM    RE,RF,0(R1)                                                      
         LA    R1,PARHEAD1(RE)                                                  
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),BOELEM                                                   
BINIT28  GOTO1 AFMTHED,BOPARM,(X'02',BLFELD)                                    
         BNE   BINIT30                                                          
         LM    RE,RF,0(R1)                                                      
         LA    RE,PARHEAD2(RE)                                                  
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BOELEM                                                   
BINIT30  L     RF,8(R1)            TEST PARAGRAPH FIELD TOTAL                   
         TM    FMTINDS1-FMTTABD(RF),FMTITFP                                     
         BZ    BINIT38                                                          
         XR    RF,RF                                                            
         IC    RF,PARABLFN                                                      
         LA    RF,1(RF)                                                         
         STC   RF,PARABLFN                                                      
         SLL   RF,2                                                             
         LA    RF,PARABLFS-L'PARABLFS(RF)                                       
         STCM  R3,15,0(RF)                                                      
*                                                                               
         TM    BLFOPT2,BLFOBOT     SET MAXIMUM LINE # FROM TOP/BOTTOM           
         BO    BINIT32                                                          
         CLC   PARMAXT,BLFLINF                                                  
         BNL   BINIT34                                                          
         MVC   PARMAXT,BLFLINF                                                  
         B     BINIT34                                                          
BINIT32  CLC   PARMAXB,BLFLINF                                                  
         BNL   BINIT34                                                          
         MVC   PARMAXB,BLFLINF                                                  
*                                                                               
BINIT34  CLI   PARCOLF,0           SET LEFT-HAND-SIDE OF TOTALS                 
         BE    *+14                                                             
         CLC   PARCOLF,BLFCOLF                                                  
         BNH   *+10                                                             
         MVC   PARCOLF,BLFCOLF                                                  
*                                                                               
         CLI   BLFFLD,BLFFGRSQ       SET TOTAL COLUMN TO GROSS OR NET           
         BNE   *+12                                                             
         OI    PARINDS,PARIGRS                                                  
         B     BINIT36                                                          
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   BINIT38                                                          
         OC    PARATOT,PARATOT                                                  
         BNZ   BINIT38                                                          
BINIT36  STCM  R3,15,PARATOT                                                    
*                                                                               
BINIT38  XR    RF,RF                                                            
         IC    RF,BLFLN                                                         
         BXH   R3,RF,BINIT26                                                    
         DROP  R3,R4                                                            
*                                                                               
BINIT40  XC    REPAPHS,REPAPHS                                                  
         XC    REPSUBID,REPSUBID                                                
         OC    REPSUBID,CSREPID    SET REPORT ID FROM INPUT OVERRIDE            
         BNZ   BINIT42                                                          
         MVI   REPSUBID,C'A'       ELSE USE DEFAULT                             
         MVC   REPSUBID+1(L'RPTPRGID),RPTPRGID                                  
BINIT42  MVC   REPPRGID,RPTPRGID                                                
         MVI   BCWORK,C'A'                                                      
         MVC   BCWORK+1(2),REPPRGID                                             
         MVC   BCWORK+3(2),REPUSRID                                             
         L     RF,ACOM                                                          
         L     RF,CPQPROF-COMFACSD(RF)                                          
         GOTO1 (RF),BODMCB,BCWORK,(2,REPBLK),ACOM                               
         MVC   REPDESC,BCSPACES                                                 
         MVC   REPDESC(2),=C'BL'                                                
         MVC   REPDESC+2(L'CSBILNUM),CSBILNUM                                   
         LA    RF,TXTTITLE         SET TITLE HOOK ROUTINE                       
         ST    RF,REPAUSR                                                       
         MVI   REPHEADH,1                                                       
         MVI   REPACTN,REPAINI                                                  
         GOTO1 REPORT                                                           
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
*                                                                               
BILLINIX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE THE TEXT BLOCK                                *         
***********************************************************************         
         SPACE 1                                                                
TXTINIT  NTR1  ,                                                                
         L     R4,ATXBLOCK                                                      
         USING TXBLOCK,R4                                                       
         L     R3,AGOPBLK                                                       
         USING GOBLOCK,R3                                                       
*                                                                               
         MVC   TXACOM,ACOM                                                      
         LA    RE,L'REPH1                                                       
         ST    RE,TXLPRT                                                        
         XC    TXAMAST,TXAMAST                                                  
*                                                                               
         MVC   TXACOMP,GOACOMP                                                  
         MVC   TXALEDG,GOALEDG                                                  
         MVC   TXACLI,GOACLI                                                    
         MVC   TXAPRO,GOAPRO                                                    
         MVC   TXAJOB,GOAJOB                                                    
*                                                                               
         MVC   TXSELCUL,GOSELCUL                                                
         MVI   TXSELFRM,C'B'       B FOR BILLING                                
         MVI   TXSELDFT,C'L'                                                    
         CLI   CSACT,ACTDRA                                                     
         BNE   *+8                                                              
         MVI   TXSELDFT,C'D'                                                    
         LA    RE,BCORGNAM                                                      
         ST    RE,TXAORIG                                                       
         LA    RE,BCDSTNAM                                                      
         ST    RE,TXADEST                                                       
*                                                                               
         MVC   TXSELCLI,GOSELCLI                                                
         MVC   TXSELPRO,GOSELPRO                                                
         MVC   TXSELJOB,GOSELJOB                                                
         MVC   TXSELOFC,CSOFFICE                                                
         MVC   TXSELBIL,CSBILNUM                                                
         MVC   TXSELCUR,BLHCUR                                                  
         MVC   TXSELBDT,BLHTRND                                                 
         MVC   TXSELDDT,BLHDUED                                                 
         MVC   TXSELTDT,BCTODAYC                                                
         TM    RPTINDS,RPTIDRFT    IF DRAFT                                     
         BZ    TINIT02               USE BILL DATE/DUE DATE ON SCREEN           
         MVC   TXSELBDT,LSUBILDC                                                
         MVC   TXSELDDT,LSUDUEDT                                                
*                                                                               
TINIT02  LA    R2,IOKEY                                                         
         USING PMDRECD,R2          R2=A(PRODUCTION MEDIA RECORD KEY)            
         MVC   PMDKEY,BCSPACES                                                  
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUABIN                                                   
         MVC   PMDKMED,TXSELJOB                                                 
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         LA    R1,PMDRFST                                                       
         CLI   0(R1),PMDELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TXMEDBUF,0(R1)      MEDIA BUFFER=MEDIA ELEMENT                   
         DROP  R2                                                               
*                                                                               
         LA    RE,TXMEDBUF         SET A(BUFFERS)                               
         ST    RE,TXAMED                                                        
         LA    RE,TXPANBUF                                                      
         ST    RE,TXAPAN                                                        
*                                                                               
         B     EXIT                                                             
         DROP  R3,R4                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO PRINT TEXT BLOCK TITLE (ON EACH PAGE)                    *         
***********************************************************************         
         SPACE 1                                                                
TXTTITLE NTR1  ,                                                                
         L     R4,ATXBLOCK                                                      
         USING TXBLOCK,R4                                                       
         LA    RE,REPH1            RE-BUILD TXTBLOCK TITLE PANEL                
         ST    RE,TXAPRT                                                        
         MVC   TXSELMAX,REPHEADN                                                
         MVC   TXSELWHR,=C'T '                                                  
         MVI   TXSELFUN,TXGETHD                                                 
         XC    TXAHOOK,TXAHOOK                                                  
         ICM   RE,3,TXPAGE                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,TXPAGE                                                      
         GOTO1 ACGETTXT,BOPARM,TXBLOCK                                          
         TM    BOFINDS2,BOFIUPCA   CONVERT TO UPPER CASE IF NECC.               
         BZ    EXIT                                                             
         GOTO1 UPPER,BOPARM,(REPHEADN,REPH1)                                    
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO PRINT TEXT BLOCK HEADERS (ON FIRST PAGE ONLY)            *         
***********************************************************************         
         SPACE 1                                                                
TXTHEAD  NTR1  ,                                                                
         L     R4,ATXBLOCK                                                      
         LA    R2,HEADWHR                                                       
         USING TXBLOCK,R4                                                       
THEAD02  MVI   TXSELFUN,TXGETHD                                                 
         LA    RE,REPP1                                                         
         ST    RE,TXAPRT                                                        
         MVC   TXSELMAX,REPPRNTN                                                
         MVC   TXSELWHR,0(R2)                                                   
         GOTO1 ACGETTXT,BOPARM,TXBLOCK                                          
         CLI   TXACTNUM,0                                                       
         BE    THEAD04                                                          
         GOTO1 REPORT                                                           
THEAD04  LA    R2,L'TXSELWHR(R2)                                                
         CLI   0(R2),EOT                                                        
         BNE   THEAD02                                                          
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO PRINT TEXT BLOCK FOOT LINES (ON LAST PAGE ONLY)          *         
***********************************************************************         
         SPACE 1                                                                
TXTFOOT  NTR1  ,                                                                
         L     R4,ATXBLOCK                                                      
         USING TXBLOCK,R4                                                       
         LA    R2,FOOTWHR                                                       
TFOOT02  LA    RE,REPP1                                                         
         ST    RE,TXAPRT                                                        
         LA    RE,TXTHOOK                                                       
         ST    RE,TXAHOOK                                                       
         MVC   TXSELMAX,REPPRNTN                                                
         MVI   TXSELFUN,TXGETFT                                                 
         MVC   TXSELWHR,0(R2)                                                   
         GOTO1 ACGETTXT,BOPARM,TXBLOCK                                          
         LA    R2,L'FOOTWHR(R2)                                                 
         CLI   0(R2),EOT                                                        
         BNE   TFOOT02                                                          
TXTFOOTX B     EXIT                                                             
*                                                                               
TXTHOOK  NTR1  ,                                                                
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT THE BILL                                           *         
***********************************************************************         
         SPACE 1                                                                
BILLPRT  NTR1  ,                                                                
*                                                                               
         LA    R0,SUBTOTSN         CLEAR SUBTOTAL ACCUMULATORS                  
         LA    RF,SUBTOTS                                                       
         ZAP   0(L'SUBTOTS,RF),BCPZERO                                          
         LA    RF,L'SUBTOTS(RF)                                                 
         BCT   R0,*-10                                                          
         XC    SUBTHTYP,SUBTHTYP                                                
         XC    SUBTHTY2,SUBTHTY2                                                
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,PBRRFST-PBRRECD(R3)                                           
         USING NDXELD,R3                                                        
         XR    RF,RF                                                            
         CLI   NDXEL,NDXELQ                                                     
         BE    *+12                                                             
         IC    RF,NDXLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,NDXACTV                                                     
         BZ    BILLPRTX                                                         
         LA    R1,NDXINDX                                                       
         LA    RF,BILLPARA                                                      
         BASR  RE,RF                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-6                                                           
         DROP  R3                                                               
*                                                                               
         USING PARD,R6             R6=A(TIME/COST PARAGRAPH DETAILS)            
         L     R6,APAR                                                          
*                                                                               
         CLI   BOFSUBT,BOFSBOTH    BOTH TIME & COST SUB-TOTALS                  
         BE    BILLP02                                                          
         CLI   BOFSUBT,BOFSNO      NO TIME/COST SUB-TOTALS                      
         BE    BILLP20                                                          
         CLI   BOFSUBT,BOFSTIME    TIME SUB-TOTALS ONLY                         
         BNE   *+16                                                             
         CLI   SUBTHTYP,PGHHHRSQ   TIME SUB-TOTAL PENDING                       
         BE    BILLP02                                                          
         B     BILLP20                                                          
         CLI   BOFSUBT,BOFSCOST    COST SUB-TOTALS ONLY                         
         BNE   BILLP20                                                          
         CLI   SUBTHTYP,PGHHCSTQ   COST SUB-TOTAL PENDING                       
         BNE   BILLP20                                                          
*                                                                               
BILLP02  SR    R0,R0                                                            
         ICM   R0,1,PARABLFN                                                    
         BNZ   *+6                                                              
         DC    H'0'                NET/GROSS TOTAL NEEDED AT LEAST              
         LA    R4,PARABLFS                                                      
BILLP04  ICM   R3,15,0(R4)                                                      
         USING BLFELD,R3                                                        
         SR    RF,RF                                                            
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   *+8                                                              
         LA    RF,TTIMNET                                                       
         CLI   BLFFLD,BLFFCOMQ                                                  
         BNE   *+8                                                              
         LA    RF,TTIMCOM                                                       
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   *+8                                                              
         LA    RF,TTIMGRS                                                       
         LTR   RF,RF                                                            
         BZ    BILLP06                                                          
         CLI   SUBTHTYP,PGHHHRSQ   TEST TIME                                    
         BE    *+8                                                              
         LA    RF,TTIMLEN(RF)      RF=SAME TOTAL IN COST BLOCK                  
         GOTO1 FMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                          
         DROP  R3                                                               
BILLP06  LA    R4,L'PARABLFS(R4)                                                
         BCT   R0,BILLP04                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PARCOLF                                                       
         SH    RF,=Y(TOTDSCLQ+2)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,BOFMAXWD                                                      
         SH    RE,=Y(PVATL-1)                                                   
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         LA    RF,REPP1(RF)        RF=A(TOTAL LINE START)                       
         MVC   0(L'FBPTIM,RF),FBPTIM                                            
         CLI   SUBTHTYP,PGHHHRSQ   TEST TIME                                    
         BE    *+10                                                             
         MVC   0(L'FBPCST,RF),FBPCST                                            
         GOTO1 REPORT                                                           
         LA    R0,SUBTTCN          CLEAR TIME/COST SUBTOTALS                    
         LA    RF,SUBTTC                                                        
         ZAP   0(L'SUBTOTS,RF),BCPZERO                                          
         LA    RF,L'SUBTOTS(RF)                                                 
         BCT   R0,*-10                                                          
*                                                                               
BILLP20  CLI   BOFSUBT2,BOFS2BTH   BOTH INTERNAL & EXTERNAL SUB-TOTALS          
         BE    BILLP22                                                          
         CLI   BOFSUBT2,BOFS2NO    NO INTERNAL/EXTERNAL SUB-TOTALS              
         BE    BILLP30                                                          
         CLI   BOFSUBT2,BOFS2INT   INTERNAL SUB-TOTALS ONLY                     
         BNE   *+16                                                             
         CLI   SUBTHTYP,PGHHINTQ   INTERNAL SUB-TOTAL PENDING                   
         BE    BILLP22                                                          
         B     BILLP30                                                          
         CLI   BOFSUBT2,BOFS2EXT   EXTERNAL SUB-TOTALS ONLY                     
         BNE   BILLP30                                                          
         CLI   SUBTHTYP,PGHHEXTQ   EXTERNAL SUB-TOTAL PENDING                   
         BNE   BILLP30                                                          
*                                                                               
BILLP22  SR    R0,R0                                                            
         ICM   R0,1,PARABLFN                                                    
         BNZ   *+6                                                              
         DC    H'0'                NET/GROSS TOTAL NEEDED AT LEAST              
         LA    R4,PARABLFS                                                      
BILLP24  ICM   R3,15,0(R4)                                                      
         USING BLFELD,R3                                                        
         SR    RF,RF                                                            
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   *+8                                                              
         LA    RF,TINTNET                                                       
         CLI   BLFFLD,BLFFCOMQ                                                  
         BNE   *+8                                                              
         LA    RF,TINTCOM                                                       
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   *+8                                                              
         LA    RF,TINTGRS                                                       
         LTR   RF,RF               UNSUPPORTED TYPE                             
         BZ    BILLP26                                                          
         CLI   SUBTHTY2,PGHHINTQ   TEST INTERNAL                                
         BE    *+8                                                              
         LA    RF,TINTLEN(RF)      RF=SAME TOTAL IN EXTERNAL BLOCK              
         GOTO1 FMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                          
         DROP  R3                                                               
BILLP26  LA    R4,L'PARABLFS(R4)                                                
         BCT   R0,BILLP24                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PARCOLF                                                       
         SH    RF,=Y(TOTDSCLQ+2)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,BOFMAXWD                                                      
         SH    RE,=Y(PVATL-1)                                                   
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         LA    RF,REPP1(RF)        RF=A(TOTAL LINE START)                       
         MVC   0(TOTDSCLQ,RF),AC@STINT                                          
         CLI   SUBTHTY2,PGHHINTQ   TEST INTERNAL                                
         BE    *+10                                                             
         MVC   0(TOTDSCLQ,RF),AC@STEXT                                          
         GOTO1 REPORT                                                           
         LA    R0,SUBTIEN          CLEAR INTERNAL/EXTERNAL SUBTOTALS            
         LA    RF,SUBTIE                                                        
         ZAP   0(L'SUBTOTS,RF),BCPZERO                                          
         LA    RF,L'SUBTOTS(RF)                                                 
         BCT   R0,*-10                                                          
*                                                                               
BILLP30  DS    0H                                                               
*                                                                               
BILLPRTX B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT PARAGRAPH                                          *         
*                                                                     *         
* NTRY: R1=A(NUMBER OF PARAGRAPH)                                     *         
*       IO1=BILL RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BILLPARA NTR1  ,                                                                
*                                                                               
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         L     RF,AIO1                                                          
         MVC   PBRKEY,0(RF)                                                     
         MVC   PBRKPARA,0(R1)                                                   
         GOTO1 AIO,IORD+IOACCMST+IO2                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
*                                                                               
         LA    R1,PBRRFST          FIND PARAGRAPH HEADER ELEMENT                
         XR    RF,RF                                                            
         CLI   0(R1),PGHELQ                                                     
         BE    *+12                                                             
         IC    RF,1(R1)                                                         
         BXH   R1,RF,*-12                                                       
         MVC   SPGHEL,0(R1)                                                     
         USING PGHELD,SPGHEL                                                    
*                                                                               
         USING PARD,R6             R6=A(TIME/COST PARAGRAPH DETAILS)            
         L     R6,APAR                                                          
         CLI   SUBTHTYP,0          TEST FIRST TIME                              
         BE    BPARA20                                                          
         CLC   SUBTHTYP,PGHHTYP    TEST COST/TIME CHANGE                        
         BE    BPARA10                                                          
         CLI   BOFSUBT,BOFSBOTH    BOTH TIME & COST SUB-TOTALS                  
         BE    BPARA02                                                          
         CLI   BOFSUBT,BOFSNO      NO TIME/COST SUB-TOTALS                      
         BE    BPARA10                                                          
         CLI   BOFSUBT,BOFSTIME    TIME SUB-TOTALS ONLY                         
         BNE   *+16                                                             
         CLI   SUBTHTYP,PGHHHRSQ   TIME SUB-TOTAL PENDING                       
         BE    BPARA02                                                          
         B     BPARA10                                                          
         CLI   BOFSUBT,BOFSCOST    COST SUB-TOTALS ONLY                         
         BNE   BPARA10                                                          
         CLI   SUBTHTYP,PGHHCSTQ   COST SUB-TOTAL PENDING                       
         BNE   BPARA10                                                          
*                                                                               
BPARA02  SR    R0,R0                                                            
         ICM   R0,1,PARABLFN                                                    
         BNZ   *+6                                                              
         DC    H'0'                NET/GROSS TOTAL NEEDED AT LEAST              
         LA    R4,PARABLFS                                                      
BPARA04  ICM   R3,15,0(R4)                                                      
         USING BLFELD,R3                                                        
         SR    RF,RF                                                            
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   *+8                                                              
         LA    RF,TTIMNET                                                       
         CLI   BLFFLD,BLFFCOMQ                                                  
         BNE   *+8                                                              
         LA    RF,TTIMCOM                                                       
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   *+8                                                              
         LA    RF,TTIMGRS                                                       
         LTR   RF,RF               UNSUPPORTED TYPE                             
         BZ    BPARA08                                                          
         CLI   SUBTHTYP,PGHHHRSQ   TEST TIME                                    
         BE    *+8                                                              
         LA    RF,TTIMLEN(RF)      RF=SAME TOTAL IN COST BLOCK                  
         GOTO1 FMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                          
         DROP  R3                                                               
BPARA08  LA    R4,L'PARABLFS(R4)                                                
         BCT   R0,BPARA04                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PARCOLF                                                       
         SH    RF,=Y(TOTDSCLQ+2)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,BOFMAXWD                                                      
         SH    RE,=Y(PVATL-1)                                                   
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         LA    RF,REPP1(RF)        RF=A(TOTAL LINE START)                       
         MVC   0(L'FBPTIM,RF),FBPTIM                                            
         CLI   SUBTHTYP,PGHHHRSQ   TEST TIME                                    
         BE    *+10                                                             
         MVC   0(L'FBPCST,RF),FBPCST                                            
         GOTO1 REPORT                                                           
         LA    R0,SUBTTCN          CLEAR TIME/COST SUBTOTALS                    
         LA    RF,SUBTTC                                                        
         ZAP   0(L'SUBTOTS,RF),BCPZERO                                          
         LA    RF,L'SUBTOTS(RF)                                                 
         BCT   R0,*-10                                                          
*                                                                               
BPARA10  CLC   SUBTHTY2,PGHHTYP2   TEST INTERNAL/EXTERNAL CHANGE                
         BE    BPARA20                                                          
         CLI   BOFSUBT2,BOFS2BTH   BOTH INTERNAL & EXTERNAL SUB-TOTALS          
         BE    BPARA12                                                          
         CLI   BOFSUBT2,BOFS2NO    NO INTERNAL/EXTERNAL SUB-TOTALS              
         BE    BPARA20                                                          
         CLI   BOFSUBT2,BOFS2INT   INTERNAL SUB-TOTALS ONLY                     
         BNE   *+16                                                             
         CLI   SUBTHTYP,PGHHINTQ   INTERNAL SUB-TOTAL PENDING                   
         BE    BPARA12                                                          
         B     BPARA20                                                          
         CLI   BOFSUBT2,BOFS2EXT   EXTERNAL SUB-TOTALS ONLY                     
         BNE   BPARA20                                                          
         CLI   SUBTHTYP,PGHHEXTQ   EXTERNAL SUB-TOTAL PENDING                   
         BNE   BPARA20                                                          
*                                                                               
BPARA12  SR    R0,R0                                                            
         ICM   R0,1,PARABLFN                                                    
         BNZ   *+6                                                              
         DC    H'0'                NET/GROSS TOTAL NEEDED AT LEAST              
         LA    R4,PARABLFS                                                      
BPARA14  ICM   R3,15,0(R4)                                                      
         USING BLFELD,R3                                                        
         SR    RF,RF                                                            
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   *+8                                                              
         LA    RF,TINTNET                                                       
         CLI   BLFFLD,BLFFCOMQ                                                  
         BNE   *+8                                                              
         LA    RF,TINTCOM                                                       
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   *+8                                                              
         LA    RF,TINTGRS                                                       
         LTR   RF,RF               UNSUPPORTED TYPE                             
         BZ    BPARA18                                                          
         CLI   SUBTHTY2,PGHHINTQ   TEST INTERNAL                                
         BE    *+8                                                              
         LA    RF,TINTLEN(RF)      RF=SAME TOTAL IN EXTERNAL BLOCK              
         GOTO1 FMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                          
         DROP  R3                                                               
BPARA18  LA    R4,L'PARABLFS(R4)                                                
         BCT   R0,BPARA14                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PARCOLF                                                       
         SH    RF,=Y(TOTDSCLQ+2)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,BOFMAXWD                                                      
         SH    RE,=Y(PVATL-1)                                                   
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         LA    RF,REPP1(RF)        RF=A(TOTAL LINE START)                       
         MVC   0(TOTDSCLQ,RF),AC@STINT                                          
         CLI   SUBTHTY2,PGHHINTQ   TEST INTERNAL                                
         BE    *+10                                                             
         MVC   0(TOTDSCLQ,RF),AC@STEXT                                          
         GOTO1 REPORT                                                           
         LA    R0,SUBTIEN          CLEAR INTERNAL/EXTERNAL SUBTOTALS            
         LA    RF,SUBTIE                                                        
         ZAP   0(L'SUBTOTS,RF),BCPZERO                                          
         LA    RF,L'SUBTOTS(RF)                                                 
         BCT   R0,*-10                                                          
*                                                                               
BPARA20  MVC   SUBTHTYP,PGHHTYP                                                 
         MVC   SUBTHTY2,PGHHTYP2                                                
*                                                                               
         ZAP   TPARNET,PGHNET      PARAGRAPH NET                                
         ZAP   TPARCOM,PGHCOM      PARAGRAPH COMMISSION                         
         OC    PGHOALLC,PGHOALLC   TEST ANY OVERALLOCATION AMOUNT               
         BZ    *+16                                                             
         AP    TPARNET,PGHOALLC    OVERALLOCATION PRINTS AS NET                 
         SP    TPARCOM,PGHOALLC    BUT WAS SAVED IN COMMISSION FIELD            
         ZAP   TPARGRS,TPARNET                                                  
         AP    TPARGRS,TPARCOM                                                  
*                                                                               
         LA    RF,TTIMGRS          ADD TO RELEVANT SUBTOTALS                    
         CLI   PGHHTYP,PGHHHRSQ                                                 
         BE    *+8                                                              
         LA    RF,TCSTGRS                                                       
         LA    RE,TINTGRS                                                       
         CLI   PGHHTYP2,PGHHINTQ                                                
         BE    *+8                                                              
         LA    RE,TEXTGRS                                                       
         AP    0(L'TCSTGRS,RF),TPARGRS                                          
         AP    0(L'TCSTGRS,RE),TPARGRS                                          
         LA    RF,L'TCSTGRS(RF)                                                 
         LA    RE,L'TCSTGRS(RE)                                                 
         AP    0(L'TCSTGRS,RF),TPARNET                                          
         AP    0(L'TCSTGRS,RE),TPARNET                                          
         LA    RF,L'TCSTGRS(RF)                                                 
         LA    RE,L'TCSTGRS(RE)                                                 
         AP    0(L'TCSTGRS,RF),TPARCOM                                          
         AP    0(L'TCSTGRS,RE),TPARCOM                                          
*                                                                               
         AP    TBILLNET,TPARNET                                                 
         AP    TBILLCOM,TPARCOM                                                 
         AP    TBILLGRS,TPARGRS                                                 
         LA    R6,TIMEPAR                                                       
         CLI   PGHHTYP,PGHHCSTQ                                                 
         BNE   *+8                                                              
         LA    R6,COSTPAR                                                       
         ST    R6,APAR                                                          
*                                                                               
         TM    RPTINDS,RPTIPARA    TEST 1ST PARAGRAPH PRINTED                   
         BO    BPARA22                                                          
         OI    RPTINDS,RPTIPARA                                                 
         TM    BOFINDS1,BOFIHPAG   TEST HEADLINES FOR EACH PAGE                 
         BZ    BPARA22                                                          
         MVC   REPP1,PARHEAD1      PRINT HEADLINES ON FIRST PAGE                
         MVC   REPP2,PARHEAD2                                                   
         MVI   REPPRNSA,2                                                       
         GOTO1 REPORT                                                           
         MVC   REPM1,PARHEAD1      ENSURE HEADLINES ON SUBSEQUENT PAGES         
         MVC   REPM2,PARHEAD2                                                   
         TM    BOFINDS2,BOFIUPCA                                                
         BZ    BPARA22                                                          
         GOTO1 UPPER,BOPARM,(2,REPM1)                                           
*                                                                               
BPARA22  LA    R1,PBRRFST          PRINT PARAGRAPH DESCRIPTION                  
         USING FFTELD,R1                                                        
         XR    RF,RF                                                            
BPARA24  CLI   FFTEL,0                                                          
         BE    BPARA26                                                          
         CLI   FFTEL,FFTELQ                                                     
         BE    *+12                                                             
         IC    RF,FFTLN                                                         
         BXH   R1,RF,BPARA24                                                    
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   REPP1(0),FFTDATA                                                 
         CLC   REPP1,BCSPACES                                                   
         BE    BPARA26                                                          
         GOTO1 REPORT                                                           
         DROP  R1                                                               
*                                                                               
BPARA26  TM    BOFINDS1,BOFIHPAR   TEST HEADLINES PER PARAGRAPH                 
         BZ    BPARA28                                                          
         MVC   REPP1,PARHEAD1                                                   
         MVC   REPP2,PARHEAD2                                                   
         GOTO1 REPORT                                                           
*                                                                               
BPARA28  LA    R3,PBRRFST                                                       
         USING NDXELD,R3                                                        
         XR    RF,RF                                                            
         CLI   NDXEL,NDXELQ                                                     
         BE    *+12                                                             
         IC    RF,NDXLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
         MVI   PTOP#,1                                                          
         MVC   PBOT#,NDXACTV                                                    
         LA    RF,BILLLINE         RF=A(BILLLINE ROUTINE)                       
*                                                                               
         XR    R0,R0                                                            
         IC    R0,PARMAXT                                                       
         XR    RE,RE                                                            
         IC    RE,PARMAXB                                                       
         AR    R0,RE               R0=NO. OF LINES REQUIRED FOR TOTALS          
         CLM   R0,1,PBOT#          TEST ENOUGH IN PARAGRAPH                     
         BNH   BPARA30                                                          
         STC   R0,PBOT#            NO - RESET BOTTOM LINE NUMBER                
*                                                                               
         CLI   PARMAXT,0           TEST ANY TOTAL AT THE TOP                    
         BNE   BPARA30                                                          
         IC    RE,NDXACTV          NO - PRINT DUMMY LINES BEFORE                
         SR    R0,RE                                                            
         XR    R1,R1                                                            
         BASR  RE,RF                                                            
         BCT   R0,*-2                                                           
*                                                                               
BPARA30  XR    R0,R0               PRINT OUT LINES IN PARAGRAPH                 
         IC    R0,NDXACTV                                                       
         LA    R1,NDXINDX                                                       
         BASR  RE,RF                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-6                                                           
*                                                                               
         XR    R1,R1               PRINT OUT DUMMY AFTER LINES                  
         CLI   PBOT#,0                                                          
         BE    *+10                                                             
         BASR  RE,RF                                                            
         B     *-10                                                             
*                                                                               
         CLI   BOFPARSP,0          PRINT SPACES BETWEEN PARAGRAPHS              
         BE    BILLPARX                                                         
         MVC   REPPRNSA,BOFPARSP                                                
         GOTO1 REPORT                                                           
*                                                                               
BILLPARX B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT A LINE                                             *         
*                                                                     *         
* NTRY: R1 = A(NUMBER OF LINE)                                        *         
*       R1 = 0 TO PRINT DUMMY LINE                                    *         
*      IO2 = PARAGRAGH HEADER RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
BILLLINE NTR1  ,                                                                
         LTR   R1,R1               TEST DUMMY LINE                              
         BZ    BLINE01                                                          
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         L     RF,AIO2                                                          
         MVC   PBRKEY,0(RF)                                                     
         MVC   PBRKLINE,0(R1)                                                   
         GOTO1 AIO,IORD+IOACCMST+IO3                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
*                                                                               
         LA    R1,PBRRFST                                                       
         USING FFTELD,R1                                                        
         XR    RF,RF                                                            
         CLI   FFTEL,FFTELQ                                                     
         BE    *+12                                                             
         IC    RF,FFTLN                                                         
         BXH   R1,RF,*-12                                                       
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   REPP1(0),FFTDATA                                                 
         DROP  R1                                                               
*                                                                               
BLINE01  TM    BOFINDS2,BOFIPASP   TEST SHOW AMOUNT ON SPACE ONLY PARA          
         BO    *+14                                                             
         CP    TPARGRS,BCPZERO     TEST ZERO AMOUNT                             
         BE    BLINE10                                                          
         XR    R0,R0                                                            
         ICM   R0,1,PARABLFN                                                    
         BNZ   *+6                                                              
         DC    H'0'                NET/GROSS TOTAL NEEDED AT LEAST              
         LA    R4,PARABLFS                                                      
BLINE02  ICM   R3,15,0(R4)                                                      
         USING BLFELD,R3                                                        
         TM    BLFOPT2,BLFOBOT                                                  
         BO    BLINE04                                                          
         CLC   BLFLINF,PTOP#       TEST TOTAL GOES ON CURRENT LINE              
         BNE   BLINE08                                                          
         B     BLINE06                                                          
BLINE04  CLC   BLFLINF,PBOT#                                                    
         BNE   BLINE08                                                          
*                                                                               
BLINE06  XR    RF,RF                                                            
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   *+8                                                              
         LA    RF,TPARNET                                                       
         CLI   BLFFLD,BLFFCOMQ                                                  
         BNE   *+8                                                              
         LA    RF,TPARCOM                                                       
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   *+8                                                              
         LA    RF,TPARGRS                                                       
         GOTO1 FMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                          
         DROP  R3                                                               
BLINE08  LA    R4,L'PARABLFS(R4)                                                
         BCT   R0,BLINE02                                                       
*                                                                               
BLINE10  GOTO1 REPORT                                                           
         IC    RE,PTOP#            UPDATE TOP LINE#                             
         LA    RE,1(RE)                                                         
         STC   RE,PTOP#                                                         
         IC    RE,PBOT#            UPDATE BOTTOM LINE#                          
         BCTR  RE,0                                                             
         STC   RE,PBOT#                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         SPACE 1                                                                
         DROP  R6                                                               
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT THE TOTALS                                         *         
***********************************************************************         
         SPACE 1                                                                
BILLTOTS NTR1  ,                                                                
         MVC   REPM1,BCSPACES      SUPPRESS HEADLINES FOR TOTALS                
         MVC   REPM2,BCSPACES                                                   
         GOTO1 REPORT                                                           
         L     R6,APAR                                                          
         USING PARD,R6                                                          
         XC    AFMTLIN,AFMTLIN                                                  
         MVC   FMTLINE,BCSPACES                                                 
*                                                                               
         XR    R0,R0                                                            
         IC    R0,PARABLFN                                                      
         LA    R2,PARABLFS                                                      
BTOTS02  ICM   R3,15,0(R2)                                                      
         USING BLFELD,R3                                                        
         XR    RF,RF               FORMAT TOTAL INTO PRINT LINE                 
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   *+8                                                              
         LA    RF,TBILLNET                                                      
         CLI   BLFFLD,BLFFCOMQ                                                  
         BNE   *+8                                                              
         LA    RF,TBILLCOM                                                      
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   *+8                                                              
         LA    RF,TBILLGRS                                                      
         GOTO1 FMTTOT,BOPARM,(RF),PCURBIL,FMTLINE,BLFELD                        
         LA    R2,L'PARABLFS(R2)                                                
         BCT   R0,BTOTS02                                                       
         DROP  R3                                                               
*                                                                               
         XR    R4,R4                                                            
         IC    R4,PARCOLF                                                       
         SH    R4,=Y(TOTDSCLQ+2)                                                
         BNM   *+6                                                              
         DC    H'0'                                                             
         XR    RE,RE                                                            
         IC    RE,BOFMAXWD                                                      
         SH    RE,=Y(PVATL-1)                                                   
         CR    R4,RE                                                            
         BNH   *+6                                                              
         LR    R4,RE                                                            
         STC   R4,PARCOLF                                                       
         LA    R4,FMTLINE(R4)      R4=A(TOTAL LINE START)                       
*                                                                               
         MVC   0(L'FBPNET,R4),FBPNET                                            
         TM    PARINDS,PARIGRS                                                  
         BNO   *+10                                                             
         MVC   0(L'FBPGRS,R4),FBPGRS                                            
         BAS   RE,FMTPRT                                                        
*                                                                               
         TM    PARINDS,PARIGRS                                                  
         BO    BTOTS04                                                          
         MVC   0(L'FBPCMN,R4),FBPCMN                                            
         GOTO1 FMTTOT,BOPARM,TBILLCOM,PCURBIL,FMTLINE,PARATOT                   
         BAS   RE,FMTPRT                                                        
         MVC   0(L'FBPGRS,R4),FBPGRS                                            
         GOTO1 FMTTOT,BOPARM,TBILLGRS,PCURBIL,FMTLINE,PARATOT                   
         BAS   RE,FMTPRT                                                        
*                                                                               
BTOTS04  BAS   RE,FMTPRT                                                        
*                                                                               
         OC    TBILLSCH,TBILLSCH   TEST SURCHARGE ALREADY SET                   
         BNZ   BTOTS06                                                          
         ZAP   BOPL81(16),BLHSCH   NO - CALCULATE IT FROM PERCENTAGE            
         MP    BOPL81(16),TBILLGRS                                              
         SRP   BOPL81(16),64-4,5                                                
         ZAP   TBILLSCH,BOPL82                                                  
BTOTS06  OC    TBILLDSC,TBILLDSC   TEST DISCOUNT ALREADY SET                    
         BNZ   BTOTS08                                                          
         ZAP   BOPL81(16),BLHDSC   NO - CALCULATE IT FROM PERCENTAGE            
         MP    BOPL81(16),TBILLGRS                                              
         SRP   BOPL81(16),64-4,5                                                
         ZAP   TBILLDSC,BOPL82                                                  
*                                                                               
BTOTS08  ZAP   TBILLINV,TBILLGRS                                                
         GOTO1 FMTPCT,BOPARM,FBPSUR,('BOFISSUR',BLHSCH),TBILLSCH,(R4)           
         AP    TBILLINV,TBILLSCH                                                
         GOTO1 (RF),(R1),FBPDIS,('BOFISDIS',BLHDSC),TBILLDSC,(R4)               
         SP    TBILLINV,TBILLDSC                                                
*                                                                               
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    BTOTS12                                                          
         MVC   0(L'FBPVAT,R4),FBPVAT                                            
         GOTO1 FMTTOT,BOPARM,TVATBIL,PCURBIL,FMTLINE,PARATOT                    
         BAS   RE,FMTPRT                                                        
         AP    TBILLINV,TVATBIL                                                 
*                                                                               
BTOTS12  BAS   RE,FMTPRT                                                        
         MVI   0(R4),C'-'                                                       
         L     RF,PARATOT                                                       
         IC    RE,BLFCOLF-BLFELD(RF)                                            
         IC    RF,BLFCOLN-BLFELD(RF)                                            
         AR    RE,RF                                                            
         IC    RF,PARCOLF                                                       
         SR    RE,RF                                                            
         LA    RF,3                                                             
         SR    RE,RF                                                            
         EX    RE,*+4                                                           
         MVC   1(0,R4),0(R4)                                                    
         MVC   BOELEM(L'FMTLINE),FMTLINE                                        
         BAS   RE,FMTPRT                                                        
         XR    R0,R0                                                            
         TM    BOFINDS2,BOFISCIT                                                
         BO    *+8                                                              
         LA    R0,X'80'                                                         
         GOTO1 FMTTOT,BOPARM,((R0),TBILLINV),PCURBIL,FMTLINE,PARATOT            
         MVC   0(L'FBPINT,R5),FBPINT                                            
         BAS   RE,FMTPRT                                                        
         CP    TBILLINV,BCPZERO    TEST NEGATIVE AMOUNT                         
         BNL   BTOTS14                                                          
         MVC   0(L'AC@CRAM,R4),AC@CRAM                                          
         BAS   RE,FMTPRT                                                        
         B     BTOTS16                                                          
BTOTS14  TM    BOFINDS1,BOFIPPAY   TEST 'PLEASE PAY THIS AMOUNT'                
         BNO   BTOTS16                                                          
         MVC   0(L'AC@PPTA,R4),AC@PPTA                                          
         BAS   RE,FMTPRT                                                        
BTOTS16  MVC   FMTLINE,BOELEM                                                   
         BAS   RE,FMTPRT                                                        
*                                                                               
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    BTOTS20                                                          
         TM    RPTINDS,RPTIBAGY    IF NOT BILLING IN AGENCY CURRENCY            
         BO    BTOTS20               DO VAT NOW                                 
         TM    BOFINDS1,BOFISVAT   UNLESS FORMAT RECORD SAYS NOT                
         BNO   BTOTS20                                                          
         BAS   RE,FMTPRT                                                        
         MVC   0(L'FBPVAT,R4),FBPVAT                                            
         GOTO1 FMTTOT,BOPARM,(X'80',TVATAGY),CSCURCPY,FMTLINE,PARATOT           
         BAS   RE,FMTPRT                                                        
*                                                                               
BTOTS20  MVI   REPPRNSA,2                                                       
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO PRINT PERCENT AMOUNT                                     *         
*                                                                     *         
* NTRY: P1        = A(OUTPUT WORD)                                    *         
*       P2 BYTE 0 = MASK TO TEST BOFINDS2 FOR % SUPPRESSANT           *         
*             1-3 = A(PL6 PERCENT)                                    *         
*       P3        = A(PL8 AMOUNT)                                     *         
*       P4        = A(OUTPUT)                                         *         
***********************************************************************         
         SPACE 1                                                                
FMTPCT   NTR1  ,                                                                
         LM    R1,R4,0(R1)                                                      
         CP    0(8,R3),BCPZERO     IGNORE IF AMOUNT=0                           
         BE    FMTPCTX                                                          
         MVC   0(L'FBPDIS,R4),0(R1)                                             
*                                                                               
         NC    4(1,R1),BOFINDS2    TEST % AMOUNT TO BE SUPPRESSED               
         BNZ   FPCT02                                                           
         LA    R4,L'FBPDIS+1(R4)                                                
         CLI   CUCTRY,CTRYGBR      GERMANY - NO '@' CHARACTER                   
         BNE   FPCT01                                                           
*                                  **** THIS IS TEMPORARY ONLY ****             
         CLC   CUAALF,=C'BP'                                                    
         BE    FPCT01                                                           
         CLC   CUAALF,=C'YA'                                                    
         BE    FPCT01                                                           
*                                                                               
         MVI   0(R4),C'@'                                                       
         LA    R4,1(R4)                                                         
FPCT01   CURED (P6,(R2)),(8,(R4)),2,ALIGN=LEFT                                  
         AR    R4,R0                                                            
         MVI   0(R4),C'%'                                                       
*                                                                               
FPCT02   GOTO1 FMTTOT,BOPARM,(R3),PCURBIL,FMTLINE,PARATOT                       
         BAS   RE,FMTPRT                                                        
*                                                                               
FMTPCTX  B     EXIT                                                             
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT VAT ANALYSIS                                       *         
***********************************************************************         
         SPACE 1                                                                
BILLVAT  NTR1  ,                                                                
         TM    BOFINDS1,BOFIPVSA+BOFIPVSF                                       
         BZ    EXIT                NO VAT ANALYSIS REQUIRED                     
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    EXIT                                                             
         XC    AFMTLIN,AFMTLIN                                                  
         MVC   FMTLINE,BCSPACES                                                 
         CLI   CUCTRY,CTRYHOL      DUTCH DON'T WANT THE VAT-TYPE                
         BNE   *+8                                                              
         OI    RPTINDS,RPTINOVT                                                 
         L     RF,APAR                                                          
         XR    R4,R4                                                            
         IC    R4,PARCOLF-PARD(RF)                                              
         LA    R4,FMTLINE(R4)                                                   
         USING PVAT,R4             R4=A(TOTAL LINE START)                       
*                                                                               
         MVC   PVAT(L'AC@VATAN),AC@VATAN                                        
         BAS   RE,FMTPRT                                                        
         MVC   PVAT(L'AC$VATAN),AC$VATAN                                        
         BAS   RE,FMTPRT                                                        
         TM    RPTINDS,RPTINOVT                                                 
         BZ    *+12                                                             
         SH    R4,=Y(PVNET-PVTYPE)                                              
         B     *+10                                                             
         MVC   PVTYPE,AC@TYPE                                                   
         MVC   PVNET(L'AC@NETPV),AC@NETPV                                       
         MVC   PVRATE(L'AC@VATRT),AC@VATRT                                      
         MVC   PVAMT(L'AC@VATAM),AC@VATAM                                       
         BAS   RE,FMTPRT                                                        
         TM    RPTINDS,RPTINOVT                                                 
         BO    *+10                                                             
         MVC   PVTYPE(L'AC$TYPE),AC$TYPE                                        
         MVC   PVNET(L'AC$NETPV),AC$NETPV                                       
         MVC   PVRATE(L'AC$VATRT),AC$VATRT                                      
         MVC   PVAMT(L'AC$VATAM),AC$VATAM                                       
         BAS   RE,FMTPRT                                                        
*                                                                               
         TM    RPTINDS,RPTIREPR    TEST REPRINTING                              
         BZ    BVAT10                                                           
         L     R3,AIO1                                                          
         LA    R3,PBRRFST-PBRRECD(R3)                                           
         USING FFTELD,R3                                                        
BVAT02   CLI   FFTEL,0             PRINT OUT SAVED VAT ANALYSIS TEXT            
         BE    BVAT20                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   BVAT08                                                           
         CLI   FFTTYPE,FFTTVATA                                                 
         BNE   BVAT08                                                           
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PVAT(0),FFTDATA                                                  
         BAS   RE,FMTPRT                                                        
BVAT08   XR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         BXH   R3,RF,BVAT02                                                     
         DROP  R3                                                               
*                                                                               
BVAT10   TM    RPTINDS,RPTILIVE    INITIALIZE FFTEL IF LIVE                     
         BZ    BVAT12                                                           
         PUSH  USING                                                            
         USING FFTELD,BOELEM                                                    
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+PVATL                                    
         MVI   FFTTYPE,FFTTVATA                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,PVATL                                                    
*                                                                               
BVAT12   XC    TLKEY,TLKEY         GET VAT RECORDS                              
         MVI   TLKSES,TLKUVATQ                                                  
         LA    R1,TSARDH                                                        
         B     *+8                                                              
BVAT14   LA    R1,TSANXT                                                        
         GOTO1 ATSARIO,(R1)                                                     
         BL    BVAT20                                                           
         CLI   TLKSES,TLKUVATQ                                                  
         BNE   BVAT20                                                           
*                                                                               
         TM    RPTINDS,RPTINOVT                                                 
         BO    *+10                                                             
         MVC   PVTYPE,TLDUVTNM                                                  
         CURED (B2,TLDUVRAT),(L'PVRATE,PVRATE),2,COMMAS=YES,MINUS=YES           
         LA    R0,CSCURCPY                                                      
         ZAP   BODUB1,TLDUVATA                                                  
         ZAP   BODUB2,TLDUVAT                                                   
         MVI   BOBYTE1,0                                                        
         TM    RPTINDS,RPTIBAGY                                                 
         BO    BVAT16                                                           
         MVI   BOBYTE1,X'80'                                                    
         TM    BOFINDS1,BOFIPVSF   TEST VAT SUMMARY IN FOREIGN CURRENCY         
         BZ    BVAT16                                                           
         GOTO1 EXCHANGE,BOPARM,TLDUVATA,BODUB1                                  
         GOTO1 (RF),(R1),TLDUVAT,BODUB2                                         
         LA    R0,PCURBIL                                                       
BVAT16   GOTO1 FMTTOT,BOPARM,(BOBYTE1,BODUB1),(R0),PVAT,(L'PVNET,PVNET)         
         GOTO1 (RF),(R1),(BOBYTE1,BODUB2),,,(L'PVAMT,PVAMT)                     
*                                                                               
         TM    RPTINDS,RPTILIVE                                                 
         BZ    BVAT18                                                           
         IC    RE,FFTSEQ                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FFTSEQ                                                        
         MVC   FFTDATA(PVATL),PVAT                                              
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,FFTELD                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BVAT18   BAS   RE,FMTPRT                                                        
         B     BVAT14                                                           
         POP   USING                                                            
*                                                                               
BVAT20   MVI   REPPRNSA,2                                                       
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT PREVIOUS BILL DETAILS                              *         
***********************************************************************         
         SPACE 1                                                                
PREVBILL NTR1  ,                                                                
         TM    BOFINDS1,BOFIAPRE                                                
         BZ    EXIT                                                             
         MVI   PRVINDS,0                                                        
         LA    R4,FMTLINE                                                       
         USING PBILL,R4                                                         
         XC    AFMTLIN,AFMTLIN                                                  
         PUSH  USING                                                            
         USING TRNRECD,PRVKEY      INITIALIZE PRVKEY                            
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,WC99                                                    
         POP   USING                                                            
*                                                                               
         LA    RF,BLHTRND          PRVDATE = BILL DATE (PWOS)                   
         TM    RPTINDS,RPTIDRFT                                                 
         BZ    *+8                                                              
         LA    RF,LSUBILDC                                                      
         GOTO1 VDATCON,BOPARM,(2,(RF)),(1,PRVDATE)                              
*                                                                               
         PUSH  USING                                                            
         USING TRNRECD,IOKEY                                                    
PBILL02  MVC   TRNKEY,PRVKEY                                                    
         GOTO1 AIO,IOHIGH+IOACCDIR                                              
PBILL04  GOTO1 AIO,IOSEQ+IOACCDIR                                               
         BNE   PBILL30                                                          
         CLC   TRNKEY(TRNKCULC-TRNKEY),PRVKEY                                   
         BNE   PBILL30                                                          
         CLC   TRNKDATE,BCSPACES                                                
         BE    PBILL04                                                          
         OC    TRNKDATE,TRNKDATE                                                
         BZ    PBILL04                                                          
         CLC   TRNKDATE,PRVDATE                                                 
         BL    PBILL06             A PREVIOUS BILL IF DATE < BILL DATE          
         BH    PBILL04             A LATER BILL IF DATE > BILL DATE             
         TM    RPTINDS,RPTIDRFT                                                 
         BO    PBILL06                                                          
         CLC   TRNKREF,BLHBLNO     IF (RE-)PRINTING BILL# MUST BE LESS          
         BNL   PBILL04                                                          
PBILL06  MVC   IODAOVER,TRNKDA                                                  
         POP   USING                                                            
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
*                                                                               
         ZAP   TPBGROSS,TRNAMNT    SET GROSS=NET                                
         ZAP   TPBVAT,BCPZERO                                                   
         CLI   TRNLN,TRNLNBQ       TEST OLD STYLE BILLING EXTENSION             
         BNL   PBILL16                                                          
PBILL10  LA    R1,TRNRFST                                                       
         USING SCIELD,R1                                                        
         XR    RF,RF                                                            
PBILL12  CLI   SCIEL,0                                                          
         BE    PBILL20                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   PBILL14                                                          
         CLI   SCITYPE,SCITCOMM    ADD COMMISSION TO GROSS TOTAL                
         BNE   *+14                                                             
         AP    TPBGROSS,SCIAMNT                                                 
         B     PBILL14                                                          
         TM    SCITYPE,X'40'       TEST VAT BUCKET                              
         BO    PBILL14                                                          
         AP    TPBVAT,SCIVBIL                                                   
PBILL14  IC    RF,SCILN                                                         
         BXH   R1,RF,PBILL12                                                    
         DROP  R1                                                               
*                                                                               
PBILL16  AP    TPBGROSS,TRNCOMM    OLD SYTLE BILLING EXTENSTION                 
         AP    TPBVAT,TRNVATR1                                                  
         AP    TPBVAT,TRNVATR2                                                  
         AP    TPBVAT,TRNVATR3                                                  
         AP    TPBVAT,TRNVATR4                                                  
         AP    TPBVAT,TRNVATR5                                                  
*                                                                               
DEBTORS  USING TRNRECD,IOKEY       READ DEBTORS POSTING                         
PBILL20  MVC   DEBTORS.TRNKEY,BCSPACES                                          
         MVC   DEBTORS.TRNKCULA,TRNKCULC                                        
         MVC   DEBTORS.TRNKCACT,PRVMDESC                                        
         MVC   DEBTORS.TRNKDATE,TRNKDATE                                        
         MVC   DEBTORS.TRNKREF,TRNKREF                                          
         MVI   DEBTORS.TRNKSBR,0                                                
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BNE   PBILL24                                                          
         MVC   IODAOVER,DEBTORS.TRNKDA                                          
         DROP  DEBTORS                                                          
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING SCIELD,R1                                                        
         XR    RF,RF                                                            
PBILL22  CLI   SCIEL,0             ADD -DISCOUNT TO GROSS                       
         BE    PBILL24                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCDSC                                                 
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   R1,RF,PBILL22                                                    
         AP    TPBGROSS,SCIAMNT                                                 
         DROP  R1                                                               
*                                                                               
PBILL24  ZAP   TPBTOTAL,TPBGROSS                                                
         AP    TPBTOTAL,TPBVAT                                                  
*                                                                               
         MVC   PRVKEY,TRNKEY                                                    
         TM    PRVINDS,PRVIPRT     TEST 1ST TIME PRINTING                       
         BO    *+8                                                              
         BAS   RE,PREVINIT                                                      
         MVC   PBNUM,TRNKREF                                                    
         GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(17,PBDATE)                          
         GOTO1 FMTTOT,BOPARM,TPBGROSS,CSCURCPY,PBILL,                  *        
               (L'PBGROSS,PBGROSS)                                              
         GOTO1 (RF),(R1),TPBVAT,,,(L'PBVAT,PBVAT)                               
         XR    R0,R0                                                            
         TM    RPTINDS,RPTIBAGY    TEST BILLING IN AGENCY CURRENCY              
         BO    *+8                                                              
         LA    R0,X'80'            NO - OUTPUT CURRENCY PREFIX                  
         GOTO1 (RF),(R1),((R0),TPBTOTAL),,,(L'PBTOTAL,PBTOTAL)                  
*                                                                               
         ICM   R1,15,AFMTLIN       ENSURE LIST KEPT ON 1 PAGE                   
         BZ    PBILL26               IF LESS THAN 12 LINES WORTH                
         BAS   RE,FMTPRT                                                        
         LA    RE,REPPC                                                         
         CR    R1,RE                                                            
         BL    PBILL28                                                          
         XC    AFMTLIN,AFMTLIN                                                  
         LA    R4,REPP1                                                         
PBILL26  GOTO1 REPORT                                                           
*                                                                               
PBILL28  B     PBILL02                                                          
         DROP  R2                                                               
*                                                                               
PBILL30  TM    PRVINDS,PRVIPRT                                                  
         BZ    EXIT                                                             
         OC    AFMTLIN,AFMTLIN                                                  
         BZ    PBILL32                                                          
         GOTO1 REPORT                                                           
PBILL32  GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
*  - INITIALIZE PRINTING                                              *         
***********************************************************************         
         SPACE 1                                                                
PREVINIT NTR1  ,                                                                
         MVC   PBHEAD,AC@PRVBS                                                  
         BAS   RE,FMTPRT                                                        
         MVC   PBHEAD,AC$PRVBS                                                  
         BAS   RE,FMTPRT                                                        
         MVC   PBHNUM,AC@BILC                                                   
         MVC   PBHDATE,AC@BILDT                                                 
         MVC   PBHGROSS,AC@GROSS                                                
         MVC   PBHVAT,AC@VAT2                                                   
         MVC   PBHTOTAL,AC@TOTAL                                                
         BAS   RE,FMTPRT                                                        
         MVC   PBHNUM,AC$BILC                                                   
         MVC   PBHDATE,AC$BILDT                                                 
         MVC   PBHGROSS,AC$GROSS                                                
         MVC   PBHVAT,AC$VAT2                                                   
         MVC   PBHTOTAL,AC$TOTAL                                                
         BAS   RE,FMTPRT                                                        
*                                                                               
         PUSH  USING                                                            
         USING PMDRECD,IOKEY                                                    
         MVC   PMDKEY,BCSPACES                                                  
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUABIN                                                   
         XR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,BCJOBCOD(RE)                                                  
         MVC   PMDKMED,0(RE)                                                    
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,PMDKDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         LA    R1,PMDRFST-PMDRECD(R1)                                           
         USING PMDELD,R1                                                        
         XR    RF,RF                                                            
         CLI   PMDEL,PMDELQ                                                     
         BE    *+12                                                             
         IC    RF,PMDLN                                                         
         BXH   R1,RF,*-12                                                       
         MVC   PRVMDESC,PMDDESC                                                 
         POP   USING                                                            
*                                                                               
PREVINIX OI    PRVINDS,PRVIPRT                                                  
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT A TOTAL                                           *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' TO PRINT CURRENCY SYMBOL                    *         
*             1-3 = A(PL8 NUMBER)                                     *         
*       P2        = A(CURRENCY)                                       *         
*       P3        = A(PRINT LINE)                                     *         
*       P4        = A(BLFEL ELEMENT)                                  *         
*    OR P4 BYTE 0 = L(PRINT FIELD)                                    *         
*             1-3 = A(PRINT FIELD)                                    *         
* EXIT: TOTAL IS FORMATTED ONTO PRINT LINE                            *         
***********************************************************************         
         SPACE 1                                                                
FMTTOT   NTR1  ,                                                                
         LR    R4,R1               R4=A(PARAMETER LIST)                         
*                                                                               
         LM    R2,R3,0(R4)                                                      
         CURED (P8,(R2)),(30,BOWORK1),(R3),COMMAS=YES,MINUS=YES,       *        
               ALIGN=LEFT                                                       
         ORG   *-2                                                              
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         NI    0(R1),FF-X'04'      CURSYMB=YES (NOT NO)                         
         BASR  RE,RF                                                            
*                                                                               
         LA    RF,BOWORK1                                                       
         AR    RF,R0               RF=END OF NO. +1                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         TM    0(RF),C'0'          TEST LAST FIGURE IS NUMERICL                 
         BNO   *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,BOWORK1                                                       
         SR    RF,RE               RF=EXECUTABLE LENGTH OF NUMBER               
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,12(R4)         RE=L(FIELD)                                  
         BZ    FTOT02                                                           
         L     R1,12(R4)           R1=A(FIELD)                                  
         B     FTOT04                                                           
FTOT02   L     R2,12(R4)                                                        
         USING BLFELD,R2                                                        
         IC    RE,BLFCOLF          RE=L(FIELD)                                  
         XR    R1,R1                                                            
         IC    R1,BLFCOLN                                                       
         A     R1,8(R4)                                                         
         BCTR  R1,0                R1=A(FIELD)                                  
         DROP  R2                                                               
*                                                                               
FTOT04   AR    R1,RE                                                            
         BCTR  R1,0                                                             
         SR    R1,RF               R1=START POSN. OF NUMBER                     
         C     R1,12(R4)           ENSURE R1 IS ON PRINT LINE                   
         BNL   *+8                                                              
         L     R1,12(R4)                                                        
         EX    RF,*+4              COPY WHOLE OF NUMBER                         
         MVC   0(0,R1),BOWORK1                                                  
*                                                                               
FMTTOTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT PRINT LINES INTO PRINT BLOCK                      *         
***********************************************************************         
         SPACE 1                                                                
FMTPRT   ICM   RF,15,AFMTLIN                                                    
         BNZ   *+8                                                              
         LA    RF,REPP1                                                         
         MVC   0(L'REPP1,RF),FMTLINE                                            
         LA    RF,L'REPP1(RF)                                                   
         ST    RF,AFMTLIN                                                       
         MVC   FMTLINE,BCSPACES                                                 
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO CONVERT AGENCY TO BILLING CURRENCY                       *         
*                                                                     *         
* P1=A(PL8 AGENCY CURRENCY INPUT)                                     *         
* P2=A(PL8 BILLING CURRENCY OUTPUT)                                   *         
***********************************************************************         
         SPACE 1                                                                
EXCHANGE NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         ZAP   EXDUB1,0(8,R2)                                                   
         EXCHP EXDUB1,BLHRVAL,DUB=EXDUB2                                        
         ZAP   0(8,R3),EXDUB2                                                   
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO CALL VREPORT, CONVERTING TO UPPER CASE IF NECC.          *         
***********************************************************************         
         SPACE 1                                                                
REPORT   NTR1  ,                                                                
         CLI   REPACTN,REPAPUT                                                  
         BNE   REPORT02                                                         
         TM    BOFINDS2,BOFIUPCA                                                
         BZ    REPORT02                                                         
         GOTO1 UPPER,BOPARM,(REPPRNTN,REPP1)                                    
REPORT02 GOTO1 VREPORT,REPD                                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO CONVERT PRINT LINES TO UPPER CASE ONLY                   *         
*                                                                     *         
* NTRY: P1 BYTE 0 = NUMBER OF PRINT LINES                             *         
*             1-3 = A(1ST PRINT LINE)                                 *         
***********************************************************************         
         SPACE 1                                                                
UPPER    NTR1  ,                                                                
         XR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
UPPER02  TR    0(L'REPP1,R2),UPPTAB                                             
         LA    R2,L'REPP1(R2)                                                   
         BCT   R0,UPPER02                                                       
         B     EXIT                                                             
         EJECT                                                                  
FF       EQU   X'FF'                                                            
TOTDSCLQ EQU   30                  LENGTH FOR TOTAL DESCRIPTIONS (ISH)          
DUB      EQU   BODUB1                                                           
DMCB     EQU   BODMCB                                                           
WORK     EQU   BOWORK1                                                          
BYTE     EQU   BOBYTE1                                                          
         SPACE 1                                                                
DQU      DC    CL(L'BASSRV)'=DQU'                                               
ACCMST   DC    C'ACCMST '                                                       
WC99     DC    C'99'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
HEADWHR  DS    0CL2                TXBLOCK HEADLINE "WHERE'S"                   
         DC    C'ST',C'H ',C'SH'                                                
HEADWHRX DC    AL1(EOT)                                                         
*                                                                               
FOOTWHR  DS    0CL2                TXBLOCK FOOTLINE "WHERE'S"                   
         DC    C'F ',C'SF'                                                      
FOOTWHRX DC    AL1(EOT)                                                         
*                                                                               
DICI     DS    0X                  * DICTIONARY IN *                            
         DCDDL AC#VAT2,TOTDSCLQ,L,LABEL=AC@VAT                                  
         DCDDL AC#INVT2,TOTDSCLQ,L                                              
         DCDDL AC#PPTA,TOTDSCLQ,L                                               
         DCDDL AC#CRAM,TOTDSCLQ,L                                               
         DCDDL AC#VATAN,30,L                                                    
         DCDDL AC#VATAN,30,LU                                                   
         DCDDL AC#TYPE,L'PVTYPE,L                                               
         DCDDL AC#TYPE,L'PVTYPE,LU                                              
         DCDDL AC#NETPV,L'PVNET-1,R                                             
         DCDDL AC#NETPV,L'PVNET-1,RU                                            
         DCDDL AC#VATRT,L'PVRATE-1,R                                            
         DCDDL AC#VATRT,L'PVRATE-1,RU                                           
         DCDDL AC#VATAM,L'PVAMT-1,R                                             
         DCDDL AC#VATAM,L'PVAMT-1,RU                                            
         DCDDL AC#PRVBS,L'PBHEAD,L                                              
         DCDDL AC#PRVBS,L'PBHEAD,LU                                             
         DCDDL AC#BILC,L'PBHNUM,L                                               
         DCDDL AC#BILC,L'PBHNUM,LU                                              
         DCDDL AC#BILDT,L'PBHDATE,L                                             
         DCDDL AC#BILDT,L'PBHDATE,LU                                            
         DCDDL AC#GROSS,L'PBHGROSS,R                                            
         DCDDL AC#GROSS,L'PBHGROSS,RU                                           
         DCDDL AC#VAT2,L'PBHVAT,R                                               
         DCDDL AC#VAT2,L'PBHVAT,RU                                              
         DCDDL AC#TOTAL,L'PBHTOTAL,R                                            
         DCDDL AC#TOTAL,L'PBHTOTAL,RU                                           
         DCDDL AC#STTIM,TOTDSCLQ,L                                              
         DCDDL AC#STCST,TOTDSCLQ,L                                              
         DCDDL AC#STINT,TOTDSCLQ,L                                              
         DCDDL AC#STEXT,TOTDSCLQ,L                                              
DICIX    DC    AL1(EOT)                                                         
         SPACE 1                                                                
UPPTAB   DS    0X                  LOWER TO UPPER CASE TRANSLATE TABLE          
         DC    X'000102030405060708090A0B0C0D0E0F'                              
         DC    X'101112131415161718191A1B1C1D1E1F'                              
         DC    X'202122232425262728292A2B2C2D2E2F'                              
         DC    X'303132333435363738393A3B3C3D3E3F'                              
         DC    X'404142434445464748494A4B4C4D4E4F'                              
         DC    X'505152535455565758595A5B5C5D5E5F'                              
         DC    X'606162636465666768696A6B6C6D6E6F'                              
         DC    X'707172737475767778797A7B7C7D7E7F'                              
         DC    X'80C1C2C3C4C5C6C7C8C98A8B8C8D8E8F'                              
         DC    X'90D1D2D3D4D5D6D7D8D99A9B9C9D9E9F'                              
         DC    X'A0A1E2E3E4E5E6E7E8E9AAABACADAEAF'                              
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'                              
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'                              
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'                              
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'                              
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'                              
UPPTABX  DS    0X                                                               
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
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORKC                                                     
         SPACE 1                                                                
TXBLOCKD DSECT                                                                  
       ++INCLUDE ACTXBLOCK                                                      
TXMEDBUF DS    XL256               MEDIA BUFFER                                 
TXPANBUF DS    XL2048              PANEL BUFFER                                 
TXL      EQU   *-TXBLOCK                                                        
         SPACE 1                                                                
***********************************************************************         
* VAT ANALYSIS PRINT LINE                                             *         
***********************************************************************         
         SPACE 1                                                                
PVAT     DSECT                                                                  
PVTYPE   DS    CL10                VAT TYPE NAME                                
         DS    CL2                                                              
PVNET    DS    CL12                NET PRE VAT                                  
         DS    CL2                                                              
PVRATE   DS    CL12                VAT RATE                                     
         DS    CL2                                                              
PVAMT    DS    CL12                VAT AMOUNT                                   
PVATL    EQU   *-PVAT                                                           
         SPACE 1                                                                
***********************************************************************         
* PREVIOUS BILLS PRINT LINE                                           *         
***********************************************************************         
         SPACE 1                                                                
PBILL    DSECT                                                                  
PBHNUM   DS    0CL12               'BILL NUMBER'                                
PBNUM    DS    CL6                 BILL NUMBER                                  
         DS    CL7                                                              
PBHDATE  DS    0CL12               'BILL DATE'                                  
PBDATE   DS    CL8                 BILL DATE                                    
         DS    CL5                                                              
PBHGROSS DS    0CL15               'GROSS AMOUNT'                               
PBGROSS  DS    CL16                GROSS AMOUNT                                 
         DS    CL1                                                              
PBHVAT   DS    0CL14               'VAT AMOUNT'                                 
PBVAT    DS    CL15                VAT AMOUNT                                   
         DS    CL1                                                              
PBHTOTAL DS    0CL15               'TOTAL AMOUNT'                               
PBTOTAL  DS    CL16                TOTAL AMOUNT                                 
PBILLL   EQU   *-PBILL                                                          
*                                                                               
         ORG   PBILL                                                            
PBHEAD   DS    CL32                'PREVIOUS BILLS' HEADLINE                    
         SPACE 1                                                                
***********************************************************************         
* PARAGRAPH DSECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PARD     DSECT                                                                  
PARATOT  DS    A                   A(BLFEL FOR THE TOTAL)                       
PARABLFS DS    10A                 LIST OF A(AMOUNT BLFEL ELEMENTS)             
PARABLFN DS    XL1                 NUMBER OF PARABLFS                           
PARMAXT  DS    XL1                 MAXIMUM LINE NO. FROM TOP                    
PARMAXB  DS    XL1                 MAXIMUM LINE NO. FROM BOTTOM                 
PARCOLF  DS    XL1                 LEFT-MOST COLUMN                             
PARINDS  DS    XL1                 INDICATORS BYTE                              
PARIGRS  EQU   X'80'               PARAGRAPH HAS A GROSS COLUMN                 
PARHEAD1 DS    CL(L'REPP1)         HEADLINE 1                                   
PARHEAD2 DS    CL(L'REPP1)         HEADLINE 2                                   
PARL     EQU   *-PARD                                                           
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
PWORKD   DSECT                                                                  
ATXBLOCK DS    A                                                                
ACGETTXT DS    A                                                                
AFMTLIN  DS    A                                                                
APAR     DS    A                                                                
*                                                                               
EXDUB1   DS    D                   DUB 1 FOR EXCHANGE ROUTINE                   
EXDUB2   DS    D                   DUB 2 FOR EXCHANGE ROUTINE                   
*                                                                               
RPTINDS  DS    XL1                 * REPORT INDICATORS *                        
RPTILIVE EQU   X'80'               PRINTING LIVE REPORT FOR 1ST TIME            
RPTIDRFT EQU   X'40'               PRINTING DRAFT REPORT                        
RPTIREPR EQU   X'20'               RE-PRINTING LIVE REPORT                      
RPTIBAGY EQU   X'10'               BILLING IN AGENCY CURRENCY                   
RPTIPARA EQU   X'08'               A PARAGRAPH HAS BEEN PRINTED                 
RPTINOVT EQU   X'04'               DON'T PRINT VAT TYPE ON VAT SUMMARY          
RPTPRGID DS    CL2                 REPORT PROGRAM ID                            
*                                                                               
PCURBIL  DS    XL(L'CSCURBIL)      BILLING CURRENCY TABLE ENTRY                 
PBLHEL   DS    XL(BLHLNQ)          SAVED BILL HEADER ELEMENT                    
*                                                                               
PTOP#    DS    XL1                 PARAGRAPH LINE NUMBER FROM THE TOP           
PBOT#    DS    XL1                 PARAGRAPH LINE NUMBER FROM BOTTOM            
*                                                                               
FMTLINE  DS    CL132                                                            
*                                                                               
         DS    0A                                                               
TIMEPAR  DS    XL(PARL)            TIME PARAGRAPH DETAILS                       
         DS    0A                                                               
COSTPAR  DS    XL(PARL)            COST PARAGRAPH DETAILS                       
*                                                                               
TPARGRS  DS    PL8                 PARAGRAPH GROSS AMOUNT                       
TPARNET  DS    PL8                 PARAGRAPH NET AMOUNT                         
TPARCOM  DS    PL8                 PARAGRAPH COMMISSION AMOUNT                  
*                                                                               
SUBTOTS  DS    0PL8                                                             
SUBTIE   DS    0PL8                                                             
TINTGRS  DS    PL8                 SUB-TOTAL INTERNAL GROSS AMOUNT              
TINTNET  DS    PL8                 SUB-TOTAL INTERNAL NET AMOUNT                
TINTCOM  DS    PL8                 SUB-TOTAL INTERNAL COMMISSION AMOUNT         
TINTLEN  EQU   *-TINTGRS           SUB-TOTAL INTERNAL LENGTH                    
TEXTGRS  DS    PL8                 SUB-TOTAL EXTERNAL GROSS AMOUNT              
TEXTNET  DS    PL8                 SUB-TOTAL EXTERNAL NET AMOUNT                
TEXTCOM  DS    PL8                 SUB-TOTAL EXTERNAL COMMISSION AMOUNT         
TEXTLEN  EQU   *-TEXTGRS           SUB-TOTAL EXTERNAL LENGTH                    
SUBTIEN  EQU   (*-SUBTIE)/L'SUBTOTS                                             
SUBTTC   DS    0PL8                                                             
TTIMGRS  DS    PL8                 SUB-TOTAL TIME GROSS AMOUNT                  
TTIMNET  DS    PL8                 SUB-TOTAL TIME NET AMOUNT                    
TTIMCOM  DS    PL8                 SUB-TOTAL TIME COMMISSION AMOUNT             
TTIMLEN  EQU   *-TTIMGRS           SUB-TOTAL TIME LENGTH                        
TCSTGRS  DS    PL8                 SUB-TOTAL COST GROSS AMOUNT                  
TCSTNET  DS    PL8                 SUB-TOTAL COST NET AMOUNT                    
TCSTCOM  DS    PL8                 SUB-TOTAL COST COMMISSION AMOUNT             
TCSTLEN  EQU   *-TCSTGRS           SUB-TOTAL COST LENGTH                        
SUBTTCN  EQU   (*-SUBTTC)/L'SUBTOTS                                             
SUBTOTSN EQU   (*-SUBTOTS)/L'SUBTOTS                                            
*                                                                               
SPGHEL   DS    XL64                SAVED PARAGRAPH HEADER ELEMENT               
*                                                                               
SUBTHTYP DS    XL(L'PGHHTYP)       LAST PARAGRAPH TYPE - TIME/COST              
SUBTHTY2 DS    XL(L'PGHHTYP2)      LAST PARAGRAPH TYPE 2 - INT/EXT              
*                                                                               
TBILLSCH DS    PL8                 TOTAL BILL SURCHARGE AMOUNT                  
TBILLDSC DS    PL8                 TOTAL BILL DISCOUNT AMOUNT                   
TBILLGRS DS    PL8                 TOTAL BILL GROSS AMOUNT                      
TBILLNET DS    PL8                 TOTAL BILL NET AMOUNT                        
TBILLCOM DS    PL8                 TOTAL BILL COMMISSION AMOUNT                 
TBILLINV DS    PL8                 TOTAL BILL INVOICE                           
TVATAGY  DS    PL8                 TOTAL VAT IN AGENCY CURRENCY                 
TVATBIL  DS    PL8                 TOTAL VAT IN BILLING CURRENCY                
*                                                                               
PRVINDS  DS    XL1                 PREVBILL INDICATORS                          
PRVIPRT  EQU   X'80'               1ST PREVBILL HAS BEEN PRINTED                
PRVKEY   DS    XL42                SAVED BILL KEY                               
PRVMDESC DS    CL12                MEDIA DESCRIPTION                            
PRVDATE  DS    PL3                 CURRENT BILL DATE                            
TPBGROSS DS    PL8                 PREVIOUS BILL GROSS AMOUNT                   
TPBVAT   DS    PL8                 PREVIOUS BILL VAT AMOUNT                     
TPBTOTAL DS    PL8                 PREVIOUS BILL TOTAL AMOUNT                   
*                                                                               
DICO     DS    0X                  * DICTIONARY OUT *                           
         DSDDL                                                                  
DICOX    DS    0X                                                               
*                                                                               
FFMTBLK  DS    XL(L'FBLK)                                                       
         DS    0X                                                               
         ORG   PWORKD+OVERWRKL                                                  
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070ACCLB0B   08/16/00'                                      
         END                                                                    
