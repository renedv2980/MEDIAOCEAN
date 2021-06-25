*          DATA SET ACCLB0BB   AT LEVEL 057 AS OF 12/22/99                      
*PHASE T6210BB                                                                  
*INCLUDE ACGETTXT                                                               
***CLUDE ACGETTXB                                                               
***CLUDE ACGETTXC                                                               
CLB0B    TITLE '- BILL PROGRAM - PRINTING "B" VERSION'                          
***********************************************************************         
* NTRY: P1 = A(PBLKD) IF DRAFT/LIVE                                   *         
*       P1 = 0        IF RE-PRINTING                                  *         
***********************************************************************         
* TKLU 053 - PRINT DEB'S ADDR ON BILL IF DEB A/C OVERRIDDEN (ACGETTXT)          
         SPACE 1                                                                
CLB0B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLBB**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         L     RC,AOVERWRK                                                      
         USING PWORKD,RC           RC=A(LOCAL W/S)                              
         ST    R7,APBLOCK          SAVE ADDR OF PBLOCK FOR DEB ACC              
         L     R7,AREP                                                          
         USING REPD,R7             R7=A(REPORT BLOCK)                           
         USING FBLKD,FFMTBLK                                                    
         USING BOFELD,FBBOFEL                                                   
         USING BLHELD,PBLHEL                                                    
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         ST    RE,RELO                                                          
         MVC   ATXBLOCK,ACOLTAB                                                 
         LA    RE,TIMEPAR                                                       
         ST    RE,APAR                                                          
*                                                                               
         MVI   RPTINDS,0                                                        
         ICM   RF,15,0(R1)         TEST RE-PRINTING                             
         BNZ   PRINT02                                                          
         OI    RPTINDS,RPTIREPR                                                 
         MVC   RPTPRGID,=C'BL'                                                  
         B     PRINT10                                                          
         USING PBLKD,RF                                                         
PRINT02  MVC   PVATT,PBVATT                                                     
         CLC   CSBILCUR,BCCPYSEC                                                
         BNE   *+10                                                             
         MVC   PVATT,PBVATTF                                                    
         MVC   PVATTF,PBVATTF                                                   
         MVC   PBILDAT,PBDATC                                                   
         MVC   PDUEDAT,PBDUE                                                    
         CLI   PBMODE,PBDRAFTQ     TEST DRAFT REPORT                            
         BNE   PRINT04                                                          
         OI    RPTINDS,RPTIDRFT                                                 
         MVC   RPTPRGID,=C'BD'                                                  
         B     PRINT10                                                          
PRINT04  CLI   PBMODE,PBLIVEQ      TEST LIVE REPORT                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    RPTINDS,RPTILIVE                                                 
         MVC   RPTPRGID,=C'BL'                                                  
         DROP  RF                                                               
*                                                                               
PRINT10  L     RF,=V(ACGETTXT)                                                  
         A     RF,RELO                                                          
         ST    RF,ACGETTXT                                                      
         LA    R0,AROUTN           R0=(NUMBER OF ROUTINES)                      
         LA    RF,AREROUT          RF=A(RELOCATED ADDRESSES)                    
         XR    RE,RE                                                            
         LA    R1,ROUT                                                          
PRINT12  ST    R1,0(RF)                                                         
         STC   RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,PRINT12                                                       
*                                                                               
         MVC   PTOTDISQ,=Y(TOTDSCLQ+2)                                          
         MVC   PTOTEXCQ,=Y(TOTDSCLQ-1)                                          
         CLI   BCP116,0            TEST TOTAL WIDTH OVERRIDE                    
         BE    PRINT14                                                          
         SR    RE,RE                                                            
         IC    RE,BCP116                                                        
         BCTR  RE,0                                                             
         STH   RE,PTOTEXCQ                                                      
         LA    RE,3(RE)                                                         
         STH   RE,PTOTDISQ                                                      
*                                                                               
PRINT14  GOTO1 VDICTAT,BOPARM,C'LL  ',DICI,DICO                                 
*                                                                               
         GOTO1 ABILLINI                                                         
         GOTO1 ATXTINIT                                                         
         GOTO1 ATXTHEAD                                                         
         GOTO1 ABILLPRT                                                         
         GOTO1 ABILLSUB                                                         
         GOTO1 ABILLPRE                                                         
         GOTO1 ABILLTOT                                                         
         GOTO1 ABILLVAT                                                         
         GOTO1 APREVBIL                                                         
         GOTO1 ATXTFOOT                                                         
*                                                                               
         MVI   REPACTN,REPACLO                                                  
         GOTO1 AREPORT                                                          
*                                                                               
         TM    RPTINDS,RPTILIVE+RPTIREPR                                        
         BZ    PRINT22             UPDATE BILL HEADER RECORD                    
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRINT22  MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RPSPL)                                           
         MVI   FVPARMS,9                                                        
         MVC   FVPARMS+1(L'REPSUBID),REPSUBID                                   
         MVC   FVPARMS+1+L'REPSUBID(1),BCCOMMA                                  
         LA    RF,FVPARMS+1+L'REPSUBID+1                                        
         EDIT  (2,REPREPNO),(5,(RF)),ALIGN=LEFT                                 
         LA    RE,BASACTH                                                       
         ST    RE,FVADDR                                                        
         CLI   P#DQU,C'Y'          TEST ALWAYS OUTPUT DQU                       
         BE    PRINT24                                                          
         TM    RPTINDS,RPTIDRFT                                                 
         BO    *+16                                                             
         CLI   P#DQU,C'L'          TEST ONLY FOR LIVE                           
         BE    PRINT24                                                          
         B     PRINTX                                                           
         CLI   P#DQU,C'D'          TEST ONLY FOR DRAFT                          
         BNE   PRINTX                                                           
*                                                                               
PRINT24  MVC   BASSRV,DQU                                                       
         OI    BASSRVH+FHOID,FHOITR                                             
*                                                                               
PRINTX   B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* BRANCH TO ROUTINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
ROUT     DS    0H                                                               
         NTR1  ,                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         L     R8,AROUT(RF)                                                     
         A     R8,RELO                                                          
         BR    R8                                                               
         SPACE 1                                                                
AROUT    DS    0F                  ROUTINES                                     
         DC    A(BILLINIT)                                                      
         DC    A(TXTINIT)                                                       
         DC    A(TXTHEAD)                                                       
         DC    A(TXTFOOT)                                                       
         DC    A(BILLPRT)                                                       
         DC    A(BILLPARA)                                                      
         DC    A(BILLSUBT)                                                      
         DC    A(BILLPREV)                                                      
         DC    A(BILLTOTS)                                                      
         DC    A(BILLVAT)                                                       
         DC    A(PREVBILL)                                                      
         DC    A(EXCHANGE)                                                      
         DC    A(FMTTOT)                                                        
         DC    A(FMTPRT)                                                        
         DC    A(REPORT)                                                        
         DC    A(UPPER)                                                         
AROUTN   EQU   (*-AROUT)/L'AROUT                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO SET SECTION DETAILS                                      *         
*                                                                     *         
* NTRY: R1 = A(PARAGRAPH TYPE)                                        *         
***********************************************************************         
         SPACE 1                                                                
SECTDETL NTR1                                                                   
         MVC   BCBYTE1,0(R1)                                                    
         L     RE,APAR             CLEAR PARAGRAPH DETAILS BLOCK                
         LA    RF,PARL                                                          
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING PBCRECD,R2                                                       
         L     R4,APAR                                                          
         L     R2,AIO4             A(CONTROL RECORD)                            
         LA    R3,PBCRFST                                                       
         USING BLFELD,R3                                                        
SDETL26  CLI   BLFEL,0                                                          
         BE    SDETL50                                                          
         CLI   BLFEL,BLFELQ                                                     
         BNE   SDETL40                                                          
         CLC   BLFTYPE,BCBYTE1     TEST MATCHING SECTION CODE                   
         BNE   SDETL40                                                          
*                                                                               
         USING PARD,R4             R4=A(PARAGRAPH DETAILS)                      
         SR    R0,R0               GET LINE INDEX                               
         IC    R0,BLFHEDL          0-2 (PAGE), 0-8 (PARA)                       
         MH    R0,=Y(L'REPP1)      INDEX TO LINE 1-3 (PAGE) 1-9 (PARA)          
         GOTO1 AFMTHED,BOPARM,(X'01',BLFELD)                                    
         BNE   SDETL30                                                          
         TM    BLFOPT2,BLFOHPG+BLFOHPR                                          
         BZ    SDETL30                                                          
         LM    RE,RF,0(R1)                                                      
         LA    R1,PARPARH1(RE)                                                  
         TM    BLFOPT2,BLFOHPG     TEST HEADING PER PAGE                        
         BZ    *+8                                                              
         LA    R1,PARPAGH1(RE)                                                  
         AR    R1,R0               INDEX TO CORRECT LINE                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),BOELEM      FIRST LINE                                   
*                                                                               
SDETL30  GOTO1 AFMTHED,BOPARM,(X'02',BLFELD)                                    
         BNE   SDETL32                                                          
         TM    BLFOPT2,BLFOHPG+BLFOHPR                                          
         BZ    SDETL32                                                          
         LM    RE,RF,0(R1)                                                      
         LA    R1,PARPARH2(RE)                                                  
         TM    BLFOPT2,BLFOHPG     TEST HEADING PER PAGE                        
         BZ    *+8                                                              
         LA    R1,PARPAGH2(RE)                                                  
         AR    R1,R0               INDEX TO CORRECT LINE                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),BOELEM      SECOND LINE                                  
*                                                                               
SDETL32  TM    BLFOPT1,BLFONPRQ    TEST IF THIS COLUMN NOT PRINTING             
         BNO   SDETL33                                                          
         CLI   PARLMNPR,0          NON-PRINT COLUMN SET LHS                     
         BE    *+14                                                             
         CLC   PARLMNPR,BLFCOLF                                                 
         BL    SDETL33                                                          
         MVC   PARLMNPR,BLFCOLF                                                 
*                                                                               
SDETL33  L     RF,BOPARM+8         TEST PARAGRAPH FIELD TOTAL                   
         TM    FMTINDS1-FMTTABD(RF),FMTITFP                                     
         BZ    SDETL40                                                          
         XR    RF,RF                                                            
         IC    RF,PARABLFN                                                      
         LA    RF,1(RF)                                                         
         STC   RF,PARABLFN                                                      
         SLL   RF,2                                                             
         LA    RF,PARABLFS-L'PARABLFS(RF)                                       
         STCM  R3,15,0(RF)                                                      
*                                                                               
         TM    BLFOPT2,BLFOBOT     SET MAXIMUM LINE # FROM TOP/BOTTOM           
         BO    SDETL34                                                          
         CLC   PARMAXT,BLFLINF                                                  
         BNL   SDETL36                                                          
         MVC   PARMAXT,BLFLINF                                                  
         B     SDETL36                                                          
SDETL34  CLC   PARMAXB,BLFLINF                                                  
         BNL   SDETL36                                                          
         MVC   PARMAXB,BLFLINF                                                  
*                                                                               
SDETL36  CLI   PARCOLF,0           SET LEFT-HAND-SIDE OF TOTALS                 
         BE    *+14                                                             
         CLC   PARCOLF,BLFCOLF                                                  
         BNH   *+10                                                             
         MVC   PARCOLF,BLFCOLF                                                  
*                                                                               
SDETL37  CLI   BLFFLD,BLFFNETQ     SET TOTAL COLUMN TO GROSS OR NET             
         BE    SDETL37A                                                         
         CLI   BLFFLD,BLFFNET2                                                  
         BE    SDETL37A                                                         
         CLI   BLFFLD,BLFFGRSQ                                                  
         BE    SDETL37A                                                         
         CLI   BLFFLD,BLFFGRS2                                                  
         BNE   SDETL40                                                          
SDETL37A OC    TOTLBLF,TOTLBLF     TEST IF THIS COLUMN START HIGHER             
         BZ    SDETL38             NO SETTING YET - USE THIS ONE                
         CLC   BLFCOLF,TOTLBLF+(BLFCOLF-BLFELD)                                 
         BNH   SDETL40             STICK WITH PREVIOUS                          
SDETL38  TM    BLFOPT1,BLFONPRQ    TEST A NON-PRINT COLUMN                      
         BO    SDETL40             DON'T USE FOR THE TOTAL SECTION              
         MVC   TOTLBLF,0(R3)                                                    
         OI    PARINDS,PARIGRS     SET COLUMN IS GROSS                          
         CLI   BLFFLD,BLFFGRSQ                                                  
         BE    SDETL40                                                          
         CLI   BLFFLD,BLFFGRS2                                                  
         BE    SDETL40                                                          
         NI    PARINDS,FF-PARIGRS  UNSET GROSS COLUMN SETTING                   
*                                                                               
SDETL40  XR    RF,RF                                                            
         IC    RF,BLFLN                                                         
         BXH   R3,RF,SDETL26                                                    
         DROP  R3                                                               
*                                                                               
SDETL50  CLI   PARLMNPR,0          TEST ANY COLUMNS NOT PRINTING                
         BE    SDETLX              NO - LEAVE HEADINGS ALONE                    
         LA    RF,PARPAGH1                                                      
         SR    RE,RE                                                            
         IC    RE,PARLMNPR         RE=DISPLACEMENT TO NON-PRINT AREA            
         BCTR  RE,0                                                             
         LA    R1,L'REPP1                                                       
         SR    R1,RE               R1=LENGTH FOR CLEARING                       
         BCTR  R1,0                                                             
         LA    R0,PARPAGNQ+PARPARNQ                                             
SDETL52  LA    R2,0(RF,RE)                                                      
         EX    R1,*+4                                                           
         XC    0(0,R2),0(R2)                                                    
         LA    RF,L'REPP1(RF)                                                   
         BCT   R0,SDETL52                                                       
         DROP  R4                                                               
*                                                                               
SDETLX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         SPACE 1                                                                
***********************************************************************         
* SPACE FOR LITERAL POOL                                              *         
***********************************************************************         
         SPACE 1                                                                
LTORG    DS    0H                                                               
         ORG   CLB0B+X'0600'                                                    
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALZE FOR CONTROL RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
BILLINIT DS    0H                                                               
         USING *,R8                                                             
         ZAP   TBILLNET,BCPZERO                                                 
         ZAP   TBILLCOM,BCPZERO                                                 
         ZAP   TBILLGRS,BCPZERO                                                 
*                                                                               
         ZAP   TPREVNET,BCPZERO                                                 
         ZAP   TPREVCOM,BCPZERO                                                 
         ZAP   TPREVGRS,BCPZERO                                                 
*                                                                               
         LA    R2,IOKEY                                                         
         USING PBRRECD,R2                                                       
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,CSBILNUM                                                
         LA    R1,IOHIGH+IOACCDIR                                               
         B     *+8                                                              
BINIT01  LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO,(R1)                                                         
         CLC   PBRPAS(PBRPIND-PBRPAS),IOKEYSAV                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RPTINDS,RPTILIVE+RPTIREPR                                        
         BZ    *+14                                                             
         CLC   PBRPJOB,BCJOBCOD    LIVE MAY HAVE DUPLICATE - MATCH JOB          
         BNE   BINIT01                                                          
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
         BE    BINIT05                                                          
         CLC   BCCPYSEC,BLHCUR     TEST BILLING IN SECOND CURRENCY              
         BNE   BINIT06                                                          
         B     BINIT05A                                                         
BINIT05  OI    RPTINDS,RPTIBAGY                                                 
BINIT05A CLI   CUCTRY,CTRYGER      TEST GERMAN AGENCY BILLING IN DM             
         BNE   BINIT06             BECAUSE IF SO THE PREFIX MUST BE 3           
         OI    PCURBIL+(CURTPIND-CURTABD),X'03'                                 
*                                                                               
BINIT06  ZAP   TVATAGY,BCPZERO     SET AGENCY VAT TOTAL                         
         XC    TVATBIL,TVATBIL     SET BILLING CURR VAT TOTAL                   
         XC    TBILLSCH,TBILLSCH   SURCHARGE NOT SET                            
         XC    TBILLDSC,TBILLDSC   DISCOUNT NOT SET                             
*                                                                               
         LA    R1,PBRRFST                                                       
         USING SCIELD,R1                                                        
         XR    RF,RF                                                            
         MVC   TDEBACC,BCSPACES                                                 
BINIT12  CLI   SCIELD,0                                                         
         BE    BINIT18                                                          
         CLI   SCIEL,SPAELQ        FIRST LOOK FOR SPAEL (DEB ACC)               
         BE    BINIT16A                                                         
         CLI   SCIEL,SCIELQ                                                     
         BNE   BINIT16                                                          
         CLI   SCITYPE,SCITTTAX                                                 
         BNE   BINIT14                                                          
         ZAP   TVATAGY,SCIAMNT     AGENCY VAT TOTAL                             
         CLI   SCILN,SCILN2Q       TEST FOREIGN AMOUNT SAVED TOO                
         BNE   BINIT16                                                          
         ZAP   TVATBIL,SCIADMN                                                  
         B     BINIT16                                                          
BINIT14  CLI   SCITYPE,SCITCBSG                                                 
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
         USING SPAELD,R1                                                        
BINIT16A CLI   SPATYPE,SPATDEBT    DEBTOR?                                      
         BNE   BINIT16                                                          
         MVC   TDEBACC,SPAAULA     GET DEBTORS ACC                              
         B     BINIT16                                                          
         DROP  R1                                                               
*                                                                               
BINIT18  TM    RPTINDS,RPTIREPR    TEST RE-PRINTING                             
         BO    BINIT20                                                          
         ZAP   TVATAGY,PVATT                                                    
         ZAP   TVATBIL,PVATT                                                    
         CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN BILLING                         
         BE    *+10                                                             
         ZAP   TVATBIL,PVATTF                                                   
         TM    RPTINDS,RPTILIVE    SAVE VAT ON RECORD IF MAKING LIVE            
         BZ    BINIT20                                                          
         PUSH  USING                                                            
         USING SCIELD,BOELEM                                                    
         XC    SCIELD(SCILN1Q),SCIELD                                           
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN2Q                                                    
         MVI   SCITYPE,SCITTTAX                                                 
         ZAP   SCIAMNT,TVATAGY                                                  
         ZAP   SCIADMN,TVATBIL                                                  
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBRRECD,SCIELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SPAELD,BOELEM                                                    
         XC    SPAELD(SPALNQ),SPAELD                                            
         MVI   SPAEL,SPAELQ        STORE DEB ACC FOR REPRINT                    
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATDEBT                                                 
         L     RE,APBLOCK          RETRIEVE DEBTORS A/C                         
         MVC   SPAAULA,PBDEBULA-PBLKD(RE)                                       
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBRRECD,SPAELD                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
BINIT20  OC    TVATBIL,TVATBIL     TEST BILLING CURRENCY VAT                    
         BNZ   BINIT22                                                          
         ZAP   TVATBIL,TVATAGY     CALCULATE BILLING VAT TOTAL                  
         TM    RPTINDS,RPTIBAGY                                                 
         BO    BINIT22                                                          
         CLC   CSBILCUR,BCCPYSEC                                                
         BE    BINIT22                                                          
         GOTO1 AEXCHANG,BOPARM,TVATAGY,TVATBIL                                  
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
BINIT24  MVC   PBCKLANG,BLHLANG    SET LANGUAGE OR 0                            
         MVC   CSFMLANG,BLHLANG                                                 
         CLI   CSFMLANG,0          TEST DEFAULT LANGUAGE                        
         BE    BINIT25             YES - DICTATE ALREADY CALLED                 
         MVC   BOPARM(3),=C'LL '                                                
         MVC   BOPARM+3(1),CSFMLANG                                             
         CLI   CSFMLANG,LANGEUK                                                 
         BNE   *+8                                                              
         MVI   BOPARM+3,LANGENG                                                 
         GOTO1 VDICTAT,BOPARM,,DICI,DICO                                        
BINIT25  LH    R1,=Y(IOACCMST+IOREAD+IO4)                                       
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFMTBLK,BOPARM,('FBGET',FBLKD),AIO4                              
         MVC   REPMAXL,BOFMAXLN    MAX LINES PER PAGE                           
*        MVC   REPWIDTH,BOFMAXWD   MAX LINE WIDTH                               
         MVC   REPTOPSA,BOFPAGSP   SPACING AT TOP OF PAGE                       
         TM    BOFINDS2,BOFIUPCA   TEST UPPER CASE PRINTING                     
         BNO   *+8                                                              
         NI    REPIND2,FF-REPILOW                                               
         MVI   PBCKSEQ,X'01'       SEE IF CONTINUATION RECORD                   
         LH    R1,=Y(IOACCMST+IOREAD+IO2)                                       
         GOTO1 AIO                                                              
         BNE   BINIT42                                                          
*                                                                               
         SR    R0,R0                                                            
         L     R2,AIO4                                                          
         ICM   R0,3,PBCRLEN                                                     
         SH    R0,=Y(1)                                                         
         AR    R0,R2                   R0=(E-O-FIRST RECORD)                    
         L     RE,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PBCRLEN-PBCRECD(RE)                                         
         SH    RF,=Y(PBCRFST-PBCRECD)                                           
         LA    RE,PBCRFST-PBCRECD(RE)  RE=A(DATA TO MOVE)                       
         LR    R1,RF                   R1/RF=L'ELEMENTS TO MOVE                 
         MVCL  R0,RE                                                            
*                                                                               
BINIT42  XC    REPAPHS,REPAPHS                                                  
         XC    REPSUBID,REPSUBID                                                
         OC    REPSUBID,CSREPID    SET REPORT ID FROM INPUT OVERRIDE            
         BNZ   BINIT44                                                          
         MVI   REPSUBID,C'A'       ELSE USE DEFAULT                             
         MVC   REPSUBID+1(L'RPTPRGID),RPTPRGID                                  
BINIT44  MVC   REPPRGID,RPTPRGID                                                
*&&UK*&& MVI   REPCLASS,C'B'                                                    
         MVI   BCWORK,C'A'                                                      
         MVC   BCWORK+1(2),REPPRGID                                             
         MVC   BCWORK+3(2),REPUSRID                                             
         L     RF,ACOM                                                          
         L     RF,CPQPROF-COMFACSD(RF)                                          
         GOTO1 (RF),BODMCB,BCWORK,(2,REPBLK),ACOM                               
         MVC   REPDESC,BCSPACES                                                 
         MVC   REPDESC(2),=C'BL'                                                
         MVC   REPDESC+2(L'CSBILNUM),CSBILNUM                                   
         MVI   REPHEADH,1                                                       
         MVI   REPACTN,REPAINI                                                  
         GOTO1 AREPORT                                                          
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
TXTINIT  DS    0H                                                               
         USING *,R8                                                             
         L     R4,ATXBLOCK                                                      
         USING TXBLOCK,R4                                                       
         L     R3,AGOPBLK                                                       
         USING GOBLOCK,R3                                                       
*                                                                               
         LA    RE,TXBLOCK          CLEAR BLOCK                                  
         LA    RF,TXL                                                           
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
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
         TM    RPTINDS,RPTIDRFT                                                 
         BZ    *+8                                                              
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
         TM    RPTINDS,RPTIREPR    TEST RE-PRINTING                             
         BZ    *+14                                                             
         MVC   TXDEBACC,TDEBACC    GET IT FROM SPAEL                            
         B     *+14                                                             
         L     R2,APBLOCK          STORE DEBTORS A/C FOR ACGETTXT               
         MVC   TXDEBACC,PBDEBULA-PBLKD(R2)                                      
         TM    RPTINDS,RPTIDRFT    IF DRAFT                                     
         BZ    TINIT02               USE BILL DATE/DUE DATE ON SCREEN           
         MVC   TXSELBDT,PBILDAT                                                 
         MVC   TXSELDDT,PDUEDAT                                                 
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
* ROUTINE TO PRINT TEXT BLOCK HEADERS (ON FIRST PAGE ONLY)            *         
***********************************************************************         
         SPACE 1                                                                
TXTHEAD  DS    0H                                                               
         USING *,R8                                                             
         L     R4,ATXBLOCK                                                      
         LA    R2,HEADWHR                                                       
         USING TXBLOCK,R4                                                       
THEAD02  MVI   TXSELFUN,TXGETHD                                                 
         LA    RE,REPP1                                                         
         ST    RE,TXAPRT                                                        
         MVC   TXSELMAX,REPPRNTN                                                
         MVC   TXSELWHR,0(R2)                                                   
         MVC   TXCUCTRY,CUCTRY     PASS COUNTRY CODE TO GETTXT                  
         MVC   TXEXCHNG,BLHRATE                                                 
         GOTO1 ACGETTXT,BOPARM,(CSFMLANG,TXBLOCK)                               
         CLI   TXACTNUM,0                                                       
         BE    THEAD04                                                          
         GOTO1 AREPORT                                                          
THEAD04  LA    R2,L'TXSELWHR(R2)                                                
         CLI   0(R2),EOT                                                        
         BNE   THEAD02                                                          
         GOTO1 AREPORT                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO PRINT TEXT BLOCK FOOT LINES (ON LAST PAGE ONLY)          *         
***********************************************************************         
         SPACE 1                                                                
TXTFOOT  DS    0H                                                               
         USING *,R8                                                             
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
         MVC   TXCUCTRY,CUCTRY     PASS COUNTRY CODE TO GETTXT                  
         MVC   TXEXCHNG,BLHRATE                                                 
         GOTO1 ACGETTXT,BOPARM,(CSFMLANG,TXBLOCK)                               
         LA    R2,L'FOOTWHR(R2)                                                 
         CLI   0(R2),EOT                                                        
         BNE   TFOOT02                                                          
TXTFOOTX B     EXIT                                                             
*                                                                               
TXTHOOK  NTR1  ,                                                                
         GOTO1 AREPORT                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT THE BILL                                           *         
***********************************************************************         
         SPACE 1                                                                
BILLPRT  DS    0H                                                               
         USING *,R8                                                             
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
         L     RF,ABILLPAR                                                      
BILLP01  BASR  RE,RF                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,BILLP01                                                       
         DROP  R3                                                               
*                                                                               
         USING PARD,R6             R6=A(TIME/COST PARAGRAPH DETAILS)            
         L     R6,APAR                                                          
*                                                                               
*&&UK                                                                           
         B     BILLP30             ************REMOVE************               
*        CLI   BOFSUBT,BOFSBOTH    BOTH TIME & COST SUB-TOTALS                  
*        BE    BILLP02                                                          
*        CLI   BOFSUBT,BOFSNO      NO TIME/COST SUB-TOTALS                      
*        BE    BILLP20                                                          
*        CLI   BOFSUBT,BOFSTIME    TIME SUB-TOTALS ONLY                         
*        BNE   *+16                                                             
*        CLI   SUBTHTYP,PGHHHRSQ   TIME SUB-TOTAL PENDING                       
*        BE    BILLP02                                                          
*        B     BILLP20                                                          
*        CLI   BOFSUBT,BOFSCOST    COST SUB-TOTALS ONLY                         
*        BNE   BILLP20                                                          
*        CLI   SUBTHTYP,PGHHCSTQ   COST SUB-TOTAL PENDING                       
*        BNE   BILLP20                                                          
*                                                                               
*ILLP02  SR    R0,R0                                                            
*        ICM   R0,1,PARABLFN                                                    
*        BNZ   *+6                                                              
*        DC    H'0'                NET/GROSS TOTAL NEEDED AT LEAST              
*        LA    R4,PARABLFS                                                      
*ILLP04  ICM   R3,15,0(R4)                                                      
*        USING BLFELD,R3                                                        
*        SR    RF,RF                                                            
*        CLI   BLFFLD,BLFFNET2                                                  
*        BE    *+12                                                             
*        CLI   BLFFLD,BLFFNETQ                                                  
*        BNE   *+8                                                              
*        LA    RF,TTIMNET                                                       
*        CLI   BLFFLD,BLFFCOMQ                                                  
*        BNE   *+8                                                              
*        LA    RF,TTIMCOM                                                       
*        CLI   BLFFLD,BLFFGRS2                                                  
*        BE    *+12                                                             
*        CLI   BLFFLD,BLFFGRSQ                                                  
*        BNE   *+8                                                              
*        LA    RF,TTIMGRS                                                       
*        LTR   RF,RF                                                            
*        BZ    BILLP06                                                          
*        CLI   SUBTHTYP,PGHHHRSQ   TEST TIME                                    
*        BE    *+8                                                              
*        LA    RF,TTIMLEN(RF)      RF=SAME TOTAL IN COST BLOCK                  
*        GOTO1 AFMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                         
*        DROP  R3                                                               
*ILLP06  LA    R4,L'PARABLFS(R4)                                                
*        BCT   R0,BILLP04                                                       
*                                                                               
*        SR    RF,RF                                                            
*        IC    RF,PARCOLF                                                       
*        SH    RF,PTOTDISQ                                                      
*        BNM   *+6                                                              
*        DC    H'0'                                                             
*        SR    RE,RE                                                            
*        IC    RE,BOFMAXWD                                                      
*        SH    RE,=Y(PVATL-1)                                                   
*        CR    RF,RE                                                            
*        BNH   *+6                                                              
*        LR    RF,RE                                                            
*        LA    RF,REPP1(RF)        RF=A(TOTAL LINE START)                       
*        LH    RE,PTOTEXCQ                                                      
*        EX    RE,*+4                                                           
*        MVC   0(0,RF),FBPTIM                                                   
*        CLI   SUBTHTYP,PGHHHRSQ   TEST TIME                                    
*        BE    *+14                                                             
*        EX    RE,*+4                                                           
*        MVC   0(0,RF),FBPCST                                                   
*        GOTO1 AREPORT                                                          
*        LA    R0,SUBTTCN          CLEAR TIME/COST SUBTOTALS                    
*        LA    RF,SUBTTC                                                        
*        ZAP   0(L'SUBTOTS,RF),BCPZERO                                          
*        LA    RF,L'SUBTOTS(RF)                                                 
*        BCT   R0,*-10                                                          
*&&                                                                             
*                                                                               
*ILLP20  CLI   BOFSUBT2,BOFS2BTH   BOTH INTERNAL & EXTERNAL SUB-TOTALS          
*        BE    BILLP22                                                          
*        CLI   BOFSUBT2,BOFS2NO    NO INTERNAL/EXTERNAL SUB-TOTALS              
*        BE    BILLP30                                                          
*        CLI   BOFSUBT2,BOFS2INT   INTERNAL SUB-TOTALS ONLY                     
*        BNE   *+16                                                             
*        CLI   SUBTHTYP,PGHHINTQ   INTERNAL SUB-TOTAL PENDING                   
*        BE    BILLP22                                                          
*        B     BILLP30                                                          
*        CLI   BOFSUBT2,BOFS2EXT   EXTERNAL SUB-TOTALS ONLY                     
*        BNE   BILLP30                                                          
*        CLI   SUBTHTYP,PGHHEXTQ   EXTERNAL SUB-TOTAL PENDING                   
*        BNE   BILLP30                                                          
*                                                                               
*ILLP22  SR    R0,R0                                                            
*        ICM   R0,1,PARABLFN                                                    
*        BNZ   *+6                                                              
*        DC    H'0'                NET/GROSS TOTAL NEEDED AT LEAST              
*        LA    R4,PARABLFS                                                      
*ILLP24  ICM   R3,15,0(R4)                                                      
*        USING BLFELD,R3                                                        
*        SR    RF,RF                                                            
*        CLI   BLFFLD,BLFFNET2                                                  
*        BE    *+12                                                             
*        CLI   BLFFLD,BLFFNETQ                                                  
*        BNE   *+8                                                              
*        LA    RF,TINTNET                                                       
*        CLI   BLFFLD,BLFFCOMQ                                                  
*        BNE   *+8                                                              
*        LA    RF,TINTCOM                                                       
*        CLI   BLFFLD,BLFFGRS2                                                  
*        BE    *+12                                                             
*        CLI   BLFFLD,BLFFGRSQ                                                  
*        BNE   *+8                                                              
*        LA    RF,TINTGRS                                                       
*        LTR   RF,RF               UNSUPPORTED TYPE                             
*        BZ    BILLP26                                                          
*        CLI   SUBTHTY2,PGHHINTQ   TEST INTERNAL                                
*        BE    *+8                                                              
*        LA    RF,TINTLEN(RF)      RF=SAME TOTAL IN EXTERNAL BLOCK              
*        GOTO1 AFMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                         
*        DROP  R3                                                               
*ILLP26  LA    R4,L'PARABLFS(R4)                                                
*        BCT   R0,BILLP24                                                       
*                                                                               
*        SR    RF,RF                                                            
*        IC    RF,PARCOLF                                                       
*        SH    RF,PTOTDISQ                                                      
*        BNM   *+6                                                              
*        DC    H'0'                                                             
*        SR    RE,RE                                                            
*        IC    RE,BOFMAXWD                                                      
*        SH    RE,=Y(PVATL-1)                                                   
*        CR    RF,RE                                                            
*        BNH   *+6                                                              
*        LR    RF,RE                                                            
*        LA    RF,REPP1(RF)        RF=A(TOTAL LINE START)                       
*        LH    RE,PTOTEXCQ                                                      
*        EX    RE,*+4                                                           
*        MVC   0(0,RF),AC@STINT                                                 
*        CLI   SUBTHTY2,PGHHINTQ   TEST INTERNAL                                
*        BE    *+14                                                             
*        EX    RE,*+4                                                           
*        MVC   0(0,RF),AC@STEXT                                                 
*        GOTO1 AREPORT                                                          
*        LA    R0,SUBTIEN          CLEAR INTERNAL/EXTERNAL SUBTOTALS            
*        LA    RF,SUBTIE                                                        
*        ZAP   0(L'SUBTOTS,RF),BCPZERO                                          
*        LA    RF,L'SUBTOTS(RF)                                                 
*        BCT   R0,*-10                                                          
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
BILLPARA DS    0H                                                               
         USING *,R8                                                             
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
         CLI   PGHHTYP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 SECTDETL,PGHHTYP                                                 
         B     BPARA01                                                          
*        CLI   PGHHTYP,PGHHPREQ    TEST IF PREVIOUS BILLING PARA                
*        CLI   PGHHTYP,3                                                        
*        BNE   BPARA01                                                          
         AP    TPREVNET,PGHNET     SAVE THE AMOUNTS - PRINT LATER               
         AP    TPREVGRS,PGHNET                                                  
         AP    TPREVCOM,PGHCOM                                                  
         AP    TPREVGRS,PGHCOM                                                  
         B     BILLPARX                                                         
*                                                                               
         USING PARD,R6             R6=A(TIME/COST PARAGRAPH DETAILS)            
BPARA01  L     R6,APAR                                                          
         CLI   SUBTHTYP,0          TEST FIRST TIME                              
         BE    BPARA20                                                          
*&&UK                                                                           
         B     BPARA10             **********REMOVE*********                    
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
         CLI   BLFFLD,BLFFNET2                                                  
         BE    *+12                                                             
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   *+8                                                              
         LA    RF,TTIMNET                                                       
         CLI   BLFFLD,BLFFCOMQ                                                  
         BNE   *+8                                                              
         LA    RF,TTIMCOM                                                       
         CLI   BLFFLD,BLFFGRS2                                                  
         BE    *+12                                                             
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   *+8                                                              
         LA    RF,TTIMGRS                                                       
         LTR   RF,RF               UNSUPPORTED TYPE                             
         BZ    BPARA08                                                          
         CLI   SUBTHTYP,PGHHHRSQ   TEST TIME                                    
         BE    *+8                                                              
         LA    RF,TTIMLEN(RF)      RF=SAME TOTAL IN COST BLOCK                  
         GOTO1 AFMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                         
         DROP  R3                                                               
BPARA08  LA    R4,L'PARABLFS(R4)                                                
         BCT   R0,BPARA04                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PARCOLF                                                       
         SH    RF,PTOTDISQ                                                      
         BNM   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,BOFMAXWD                                                      
         SH    RE,=Y(PVATL-1)                                                   
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         LA    RF,REPP1(RF)        RF=A(TOTAL LINE START)                       
         LH    RE,PTOTEXCQ                                                      
         EX    RE,*+4                                                           
         MVC   0(0,RF),=C'????????????????????????????????????'                 
         CLI   SUBTHTYP,PGHHHRSQ   TEST TIME                                    
         BE    *+14                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),=C'????????????????????????????????????'                 
         GOTO1 AREPORT                                                          
         LA    R0,SUBTTCN          CLEAR TIME/COST SUBTOTALS                    
         LA    RF,SUBTTC                                                        
         ZAP   0(L'SUBTOTS,RF),BCPZERO                                          
         LA    RF,L'SUBTOTS(RF)                                                 
         BCT   R0,*-10                                                          
*&&                                                                             
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
         CLI   BLFFLD,BLFFNET2                                                  
         BE    *+12                                                             
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   *+8                                                              
         LA    RF,TINTNET                                                       
         CLI   BLFFLD,BLFFCOMQ                                                  
         BNE   *+8                                                              
         LA    RF,TINTCOM                                                       
         CLI   BLFFLD,BLFFGRS2                                                  
         BE    *+12                                                             
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   *+8                                                              
         LA    RF,TINTGRS                                                       
         LTR   RF,RF               UNSUPPORTED TYPE                             
         BZ    BPARA18                                                          
         CLI   SUBTHTY2,PGHHINTQ   TEST INTERNAL                                
         BE    *+8                                                              
         LA    RF,TINTLEN(RF)      RF=SAME TOTAL IN EXTERNAL BLOCK              
         GOTO1 AFMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                         
         DROP  R3                                                               
BPARA18  LA    R4,L'PARABLFS(R4)                                                
         BCT   R0,BPARA14                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PARCOLF                                                       
         SH    RF,PTOTDISQ                                                      
         BNM   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         IC    RE,BOFMAXWD                                                      
         SH    RE,=Y(PVATL-1)                                                   
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         LA    RF,REPP1(RF)        RF=A(TOTAL LINE START)                       
         LH    RE,PTOTEXCQ                                                      
         EX    RE,*+4                                                           
         MVC   0(0,RF),AC@STINT                                                 
         CLI   SUBTHTY2,PGHHINTQ   TEST INTERNAL                                
         BE    *+14                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),AC@STEXT                                                 
         GOTO1 AREPORT                                                          
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
         L     R6,APAR                                                          
*                                                                               
         TM    RPTINDS,RPTIPARA    TEST 1ST PARAGRAPH PRINTED                   
         BO    BPARA26                                                          
         OI    RPTINDS,RPTIPARA                                                 
         LA    R0,4                UP TO FOUR PER PAGE HEADLINES                
         SR    R1,R1                                                            
         LA    RF,REPP4                                                         
         LA    RE,PARPAGH4                                                      
BPARA22  OC    0(L'REPP1,RE),0(RE)                                              
         BZ    *+14                                                             
         MVC   0(L'REPP1,RF),0(RE)                                              
         LA    R1,1                                                             
         SH    RE,=Y(L'REPP1)                                                   
         SH    RF,=Y(L'REPP1)                                                   
         BCT   R0,BPARA22                                                       
         LTR   R1,R1               TEST ANYTHING TO PRINT                       
         BZ    BPARA26                                                          
         MVI   REPPRNSA,2                                                       
         GOTO1 AREPORT                                                          
*                                                                               
         LA    R0,4                ENSURE HEADLINES ON SUBSEQUENT PAGES         
         LA    R1,4                                                             
         LA    RF,REPM4                                                         
         LA    RE,PARPAGH4                                                      
BPARA24  OC    0(L'REPP1,RE),0(RE)                                              
         BNZ   *+8                                                              
         BCT   R0,*+10                                                          
         MVC   0(L'REPP1,RF),0(RE)                                              
         SH    RE,=Y(L'REPP1)                                                   
         SH    RF,=Y(L'REPP1)                                                   
         BCT   R1,BPARA24                                                       
         TM    BOFINDS2,BOFIUPCA                                                
         BZ    BPARA26                                                          
         GOTO1 AUPPER,BOPARM,((R0),REPM1)                                       
*                                                                               
BPARA26  LA    R1,PBRRFST          PRINT PARAGRAPH DESCRIPTION                  
         USING FFTELD,R1                                                        
         XR    RF,RF                                                            
BPARA28  CLI   FFTEL,0                                                          
         BE    BPARA30                                                          
         CLI   FFTEL,FFTELQ                                                     
         BE    *+12                                                             
         IC    RF,FFTLN                                                         
         BXH   R1,RF,BPARA28                                                    
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   REPP1(0),FFTDATA                                                 
         CLC   REPP1,BCSPACES                                                   
         BNH   BPARA30                                                          
         LA    RF,REPP1(RF)                                                     
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   REPP2,C'-'                                                       
         LA    RE,REPP1+1                                                       
         SR    RF,RE                                                            
         EX    RF,*+4                                                           
         MVC   REPP2+1(0),REPP2                                                 
         GOTO1 AREPORT                                                          
         DROP  R1                                                               
*                                                                               
BPARA30  LA    R0,10                                                            
         SR    R1,R1                                                            
         LA    RF,REPPA                                                         
         LA    RE,PARPARHA                                                      
BPARA32  OC    0(L'REPP1,RE),0(RE)                                              
         BZ    *+14                                                             
         MVC   0(L'REPP1,RF),0(RE)                                              
         LA    R1,1                                                             
         SH    RE,=Y(L'REPP1)                                                   
         SH    RF,=Y(L'REPP1)                                                   
         BCT   R0,BPARA32                                                       
         LTR   R1,R1               TEST ANYTHING TO PRINT                       
         BZ    BPARA34                                                          
         GOTO1 AREPORT                                                          
*                                                                               
BPARA34  LA    R3,PBRRFST                                                       
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
         BNH   BPARA36                                                          
         STC   R0,PBOT#            NO - RESET BOTTOM LINE NUMBER                
*                                                                               
         CLI   PARMAXT,0           TEST ANY TOTAL AT THE TOP                    
         BNE   BPARA36                                                          
         IC    RE,NDXACTV          NO - PRINT DUMMY LINES BEFORE                
         SR    R0,RE                                                            
         XR    R1,R1                                                            
         BASR  RE,RF                                                            
         BCT   R0,*-2                                                           
*                                                                               
BPARA36  XR    R0,R0               PRINT OUT LINES IN PARAGRAPH                 
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
         GOTO1 AREPORT                                                          
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
         BZ    BLINE04                                                          
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
         CLI   PARLMNPR,0                                                       
         BE    BLINE02                                                          
         CLC   PARLMNPR,FFTDLEN    CHECK WHETHER NON PRINT PART                 
         BH    BLINE02                                                          
         IC    RF,PARLMNPR         USE NARROW LINE                              
         BCTR  RF,0                                                             
BLINE02  BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   REPP1(0),FFTDATA                                                 
         DROP  R1                                                               
*                                                                               
BLINE04  DS    0H                                                               
*                                                                               
BLINE12  GOTO1 AREPORT                                                          
         IC    RE,PTOP#            UPDATE TOP LINE#                             
         LA    RE,1(RE)                                                         
         STC   RE,PTOP#                                                         
         IC    RE,PBOT#            UPDATE BOTTOM LINE#                          
         BCTR  RE,0                                                             
         STC   RE,PBOT#                                                         
BLINEX   B     EXIT                                                             
         DROP  R2                                                               
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT BILL LEVEL SUBTOTAL                               *         
***********************************************************************         
         SPACE 1                                                                
BILLSUBT DS    0H                                                               
         USING *,R8                                                             
         CLI   FBPBST,C'*'         TEST NOT PRINTING                            
         BE    EXIT                                                             
         USING PARD,R6                                                          
         L     R6,APAR             PICK UP PREVIOUS PARAGRAPH DETAILS           
         SR    R0,R0                                                            
         ICM   R0,1,PARABLFN                                                    
         BNZ   *+6                                                              
         DC    H'0'                NET/GROSS TOTAL NEEDED AT LEAST              
         LA    R4,PARABLFS                                                      
BILLS02  ICM   R3,15,0(R4)                                                      
         USING BLFELD,R3                                                        
         LA    RF,TBILLNET                                                      
         CLI   BLFFLD,BLFFNET2                                                  
         BE    BILLS04                                                          
         CLI   BLFFLD,BLFFNETQ                                                  
         BE    BILLS04                                                          
         LA    RF,TBILLCOM                                                      
         CLI   BLFFLD,BLFFCOMQ                                                  
         BE    BILLS04                                                          
         LA    RF,TBILLGRS                                                      
         CLI   BLFFLD,BLFFGRS2                                                  
         BE    BILLS04                                                          
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   BILLS06                                                          
BILLS04  GOTO1 AFMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                         
         DROP  R3                                                               
BILLS06  LA    R4,L'PARABLFS(R4)                                                
         BCT   R0,BILLS02                                                       
         MVC   REPP1(L'FBPBST),FBPBST                                           
         GOTO1 AREPORT                                                          
         CLI   BOFPARSP,0                                                       
         BE    EXIT                                                             
         MVC   REPPRNSA,BOFPARSP                                                
         GOTO1 AREPORT                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT PREVIOUS BILLING LINE                             *         
***********************************************************************         
         SPACE 1                                                                
BILLPREV DS    0H                                                               
         USING *,R8                                                             
         CLI   FBPPRV,C'*'         TEST NOT PRINTING                            
         BE    EXIT                                                             
         USING PARD,R6                                                          
         L     R6,APAR             PICK UP PREVIOUS PARAGRAPH DETAILS           
         SR    R0,R0                                                            
         ICM   R0,1,PARABLFN                                                    
         BNZ   *+6                                                              
         DC    H'0'                NET/GROSS TOTAL NEEDED AT LEAST              
         LA    R4,PARABLFS                                                      
BILLPR02 ICM   R3,15,0(R4)                                                      
         USING BLFELD,R3                                                        
         LA    RF,TPREVNET                                                      
         CLI   BLFFLD,BLFFNET2                                                  
         BE    BILLPR04                                                         
         CLI   BLFFLD,BLFFNETQ                                                  
         BE    BILLPR04                                                         
         LA    RF,TPREVCOM                                                      
         CLI   BLFFLD,BLFFCOMQ                                                  
         BE    BILLPR04                                                         
         LA    RF,TPREVGRS                                                      
         CLI   BLFFLD,BLFFGRS2                                                  
         BE    BILLPR04                                                         
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   BILLPR06                                                         
BILLPR04 GOTO1 AFMTTOT,BOPARM,(RF),PCURBIL,REPP1,BLFELD                         
         DROP  R3                                                               
BILLPR06 LA    R4,L'PARABLFS(R4)                                                
         BCT   R0,BILLPR02                                                      
         MVC   REPP1(L'FBPPRV),FBPPRV                                           
         GOTO1 AREPORT                                                          
         AP    TBILLNET,TPREVNET   NOW BILL TOTALS REFLECT PREVIOUS             
         AP    TBILLCOM,TPREVCOM                                                
         AP    TBILLGRS,TPREVGRS                                                
         CLI   BOFPARSP,0                                                       
         BE    EXIT                                                             
         MVC   REPPRNSA,BOFPARSP                                                
         GOTO1 AREPORT                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT THE TOTALS                                         *         
***********************************************************************         
         SPACE 1                                                                
BILLTOTS DS    0H                                                               
         USING *,R8                                                             
         MVC   REPM1,BCSPACES      SUPPRESS HEADLINES FOR TOTALS                
         MVC   REPM2,BCSPACES                                                   
         MVC   REPM3,BCSPACES                                                   
         MVC   REPM4,BCSPACES                                                   
         GOTO1 AREPORT                                                          
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
         CLI   BLFFLD,BLFFNET2                                                  
         BE    *+12                                                             
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   *+8                                                              
         LA    RF,TBILLNET                                                      
         CLI   BLFFLD,BLFFCOMQ                                                  
         BNE   *+8                                                              
         LA    RF,TBILLCOM                                                      
         CLI   BLFFLD,BLFFGRS2                                                  
         BE    *+12                                                             
         CLI   BLFFLD,BLFFGRSQ                                                  
         BNE   *+8                                                              
         LA    RF,TBILLGRS                                                      
         CLI   BLFFLD,BLFFCDSQ                                                  
         BNE   *+8                                                              
         LA    RF,TBILLDSC                                                      
         CLC   BLFFLD,TOTLBLF+(BLFFLD-BLFELD)                                   
         BNE   *+8                                                              
         LA    R3,TOTLBLF          USE RIGHTMOST FOR THE TOTAL                  
         CLI   PARLMNPR,0          TEST NON-PRINT COLUMNS                       
         BE    *+14                                                             
         CLC   BLFCOLF,PARLMNPR                                                 
         BNL   BTOTS03                                                          
         GOTO1 AFMTTOT,BOPARM,(RF),PCURBIL,FMTLINE,BLFELD                       
BTOTS03  LA    R2,L'PARABLFS(R2)                                                
         BCT   R0,BTOTS02                                                       
         DROP  R3                                                               
*                                                                               
         XR    R4,R4                                                            
         IC    R4,PARCOLF                                                       
         SH    R4,PTOTDISQ                                                      
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
         CLI   0(R4),C'*'          TEST NOT PRINTING                            
         BE    BTOTS04                                                          
         GOTO1 AFMTPRT                                                          
BTOTS04  MVC   FMTLINE,BCSPACES                                                 
*                                                                               
         TM    PARINDS,PARIGRS                                                  
         BO    BTOTS08                                                          
         CLI   FBPCMN,C'*'         TEST NOT PRINTING                            
         BE    BTOTS06                                                          
         MVC   0(L'FBPCMN,R4),FBPCMN                                            
         GOTO1 AFMTTOT,BOPARM,TBILLCOM,PCURBIL,FMTLINE,TOTLBLF                  
         GOTO1 AFMTPRT                                                          
*                                                                               
BTOTS06  CLI   FBPGRS,C'*'         TEST NOT PRINTING                            
         BE    BTOTS08                                                          
         MVC   0(L'FBPGRS,R4),FBPGRS                                            
         GOTO1 AFMTTOT,BOPARM,TBILLGRS,PCURBIL,FMTLINE,TOTLBLF                  
         GOTO1 AFMTPRT                                                          
*                                                                               
BTOTS08  GOTO1 AFMTPRT                                                          
*                                                                               
         OC    TBILLSCH,TBILLSCH   TEST SURCHARGE ALREADY SET                   
         BNZ   BTOTS10                                                          
         ZAP   BOPL81(16),BLHSCH   NO - CALCULATE IT FROM PERCENTAGE            
         MP    BOPL81(16),TBILLGRS                                              
         SRP   BOPL81(16),64-4,5                                                
         ZAP   TBILLSCH,BOPL82                                                  
BTOTS10  OC    TBILLDSC,TBILLDSC   TEST DISCOUNT ALREADY SET                    
         BNZ   BTOTS12                                                          
         ZAP   BOPL81(16),BLHDSC   NO - CALCULATE IT FROM PERCENTAGE            
         MP    BOPL81(16),TBILLGRS                                              
         SRP   BOPL81(16),64-4,5                                                
         ZAP   TBILLDSC,BOPL82                                                  
*                                                                               
BTOTS12  ZAP   TBILLINV,TBILLGRS                                                
         GOTO1 FMTPCT,BOPARM,FBPSUR,('BOFISSUR',BLHSCH),TBILLSCH,(R4)           
         AP    TBILLINV,TBILLSCH                                                
         GOTO1 (RF),(R1),FBPDIS,('BOFISDIS',BLHDSC),TBILLDSC,(R4)               
         SP    TBILLINV,TBILLDSC                                                
*                                                                               
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    BTOTS14                                                          
         LH    RE,PTOTEXCQ                                                      
         EX    RE,*+4                                                           
         MVC   0(0,R4),FBPVAT                                                   
         GOTO1 AFMTTOT,BOPARM,TVATBIL,PCURBIL,FMTLINE,TOTLBLF                   
         GOTO1 AFMTPRT                                                          
         AP    TBILLINV,TVATBIL                                                 
*                                                                               
BTOTS14  GOTO1 AFMTPRT                                                          
         MVI   0(R4),C'-'                                                       
         IC    RE,TOTLBLF+(BLFCOLF-BLFELD)                                      
         IC    RF,TOTLBLF+(BLFCOLN-BLFELD)                                      
         AR    RE,RF                                                            
         IC    RF,PARCOLF                                                       
         SR    RE,RF                                                            
         LA    RF,3                                                             
         SR    RE,RF                                                            
         EX    RE,*+4                                                           
         MVC   1(0,R4),0(R4)                                                    
         MVC   BOELEM(L'FMTLINE),FMTLINE                                        
         GOTO1 AFMTPRT                                                          
         LH    RE,PTOTEXCQ                                                      
         EX    RE,*+4                                                           
         MVC   0(0,R4),FBPINT                                                   
         XR    R0,R0                                                            
         TM    BOFINDS2,BOFISCIT                                                
         BO    *+8                                                              
         LA    R0,X'80'                                                         
         GOTO1 AFMTTOT,BOPARM,((R0),TBILLINV),PCURBIL,FMTLINE,TOTLBLF           
         GOTO1 AFMTPRT                                                          
         CP    TBILLINV,BCPZERO    TEST NEGATIVE AMOUNT                         
         BNL   BTOTS16                                                          
         CLI   FBCRAM,C'*'         TEST THIS IS SUPPRESSED                      
         BE    BTOTS16                                                          
         MVC   0(L'FBCRAM,R4),FBCRAM                                            
         GOTO1 AFMTPRT                                                          
         B     BTOTS18                                                          
BTOTS16  TM    BOFINDS1,BOFIPPAY   TEST 'PLEASE PAY THIS AMOUNT'                
         BNO   BTOTS18                                                          
         MVC   0(L'FBPPTA,R4),FBPPTA                                            
         GOTO1 AFMTPRT                                                          
BTOTS18  MVC   FMTLINE,BOELEM                                                   
         GOTO1 AFMTPRT                                                          
*                                                                               
         TM    BCGLOB1,BCGLVAT                                                  
         BZ    BTOTS20                                                          
         TM    RPTINDS,RPTIBAGY    IF NOT BILLING IN AGENCY CURRENCY            
         BO    BTOTS20               DO VAT NOW                                 
         TM    BOFINDS1,BOFISVAT   UNLESS FORMAT RECORD SAYS NOT                
         BNO   BTOTS20                                                          
         GOTO1 AFMTPRT                                                          
         LH    RE,PTOTEXCQ                                                      
         EX    RE,*+4                                                           
         MVC   0(0,R4),FBPVAT                                                   
         GOTO1 AFMTTOT,BOPARM,(X'80',TVATAGY),CSCURCPY,FMTLINE,TOTLBLF          
         GOTO1 AFMTPRT                                                          
*                                                                               
BTOTS20  MVI   REPPRNSA,2                                                       
         GOTO1 AREPORT                                                          
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
FPCT02   GOTO1 AFMTTOT,BOPARM,(R3),PCURBIL,FMTLINE,TOTLBLF                      
         GOTO1 AFMTPRT                                                          
*                                                                               
FMTPCTX  B     EXIT                                                             
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT VAT ANALYSIS                                       *         
***********************************************************************         
         SPACE 1                                                                
BILLVAT  DS    0H                                                               
         USING *,R8                                                             
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
         GOTO1 AFMTPRT                                                          
         MVC   PVAT(L'AC$VATAN),AC$VATAN                                        
         GOTO1 AFMTPRT                                                          
         TM    RPTINDS,RPTINOVT                                                 
         BZ    *+12                                                             
         SH    R4,=Y(PVNET-PVTYPE)                                              
         B     *+10                                                             
         MVC   PVTYPE,AC@TYPE                                                   
         MVC   PVNET(L'AC@NETPV),AC@NETPV                                       
         MVC   PVRATE(L'AC@VATRT),AC@VATRT                                      
         MVC   PVAMT(L'AC@VATAM),AC@VATAM                                       
         GOTO1 AFMTPRT                                                          
         TM    RPTINDS,RPTINOVT                                                 
         BO    *+10                                                             
         MVC   PVTYPE(L'AC$TYPE),AC$TYPE                                        
         MVC   PVNET(L'AC$NETPV),AC$NETPV                                       
         MVC   PVRATE(L'AC$VATRT),AC$VATRT                                      
         MVC   PVAMT(L'AC$VATAM),AC$VATAM                                       
         GOTO1 AFMTPRT                                                          
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
         GOTO1 AFMTPRT                                                          
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
         ZAP   BODUB1,TLDUVTFA                                                  
         ZAP   BODUB2,TLDUVTF                                                   
         LA    R0,PCURBIL                                                       
BVAT16   GOTO1 AFMTTOT,BOPARM,(BOBYTE1,BODUB1),(R0),PVAT,              *        
               (L'PVNET,PVNET)                                                  
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
BVAT18   GOTO1 AFMTPRT                                                          
         B     BVAT14                                                           
         POP   USING                                                            
*                                                                               
BVAT20   MVI   REPPRNSA,2                                                       
         GOTO1 AREPORT                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT PREVIOUS BILL DETAILS                              *         
***********************************************************************         
         SPACE 1                                                                
PREVBILL DS    0H                                                               
         USING *,R8                                                             
         TM    BOFINDS1,BOFIAPRE                                                
         BZ    PBILLX                                                           
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
         LA    RF,PBILDAT                                                       
         GOTO1 VDATCON,BOPARM,(2,(RF)),(1,PRVDATE)                              
*                                                                               
         PUSH  USING                                                            
         USING TRNRECD,IOKEY                                                    
PBILL02  MVC   TRNKEY,PRVKEY                                                    
         GOTO1 AIO,IOHIGH+IOACCDIR                                              
PBILL04  GOTO1 AIO,IOSEQ+IOACCDIR                                               
         BNE   PBILL50                                                          
         CLC   TRNKEY(TRNKCULC-TRNKEY),PRVKEY                                   
         BNE   PBILL50                                                          
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
         USING TRNRECD,R2                                                       
         L     R2,AIO2                                                          
         MVC   PRVKEY,TRNKEY                                                    
         TM    PRVINDS,PRVIPRT     TEST 1ST TIME PRINTING                       
         BO    *+8                                                              
         BAS   RE,PREVINIT                                                      
*                                                                               
         USING TRNELD,TRNRFST                                                   
*                                                                               
         XC    TPBAFCEL,TPBAFCEL                                                
         ZAP   TPBGROSS,TRNAMNT    SET GROSS=NET                                
         ZAP   TPBVAT,BCPZERO                                                   
         CLI   TRNLN,TRNLNBQ       TEST OLD STYLE BILLING EXTENSION             
         BNL   PBILL16                                                          
PBILL10  LA    R1,TRNRFST                                                       
         USING SCIELD,R1                                                        
         XR    RF,RF                                                            
PBILL12  CLI   SCIEL,0                                                          
         BE    PBILL20                                                          
         CLI   SCIEL,AFCELQ                                                     
         BNE   *+14                                                             
         MVC   TPBAFCEL,SCIEL                                                   
         B     PBILL14                                                          
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
PBILL20  CLI   TPBAFCEL,AFCELQ     TEST THIS WAS A FOREIGN BILL                 
         BNE   PBILL30                                                          
*&&UK                                                                           
         ZAP   EXDUB1,TPBVAT                                                    
         LA    RF,BOWORK1                                                       
         USING EURKBLKD,RF                                                      
         XC    0(EURKBLKL,RF),0(RF)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,TPBAFCEL+(AFCCURR-AFCELD)                               
         MVC   EURKRULE,TPBAFCEL+(AFCX-AFCELD)                                  
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),EXDUB1,EXDUB2                  
         DROP  RF                                                               
         ZAP   TPBVAT,EXDUB2       NOW IS FOREIGN VAT                           
*&&                                                                             
DEBTORS  USING TRNRECD,IOKEY       READ DEBTORS POSTING                         
PBILL30  MVC   DEBTORS.TRNKEY,BCSPACES                                          
         MVC   DEBTORS.TRNKCULA,TRNKCULC                                        
         MVC   DEBTORS.TRNKCACT,PRVMDESC                                        
         MVC   DEBTORS.TRNKDATE,TRNKDATE                                        
         MVC   DEBTORS.TRNKREF,TRNKREF                                          
         MVI   DEBTORS.TRNKSBR,0                                                
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BNE   PBILL42                                                          
         MVC   IODAOVER,DEBTORS.TRNKDA                                          
         DROP  DEBTORS                                                          
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TPBAFCEL,AFCELQ     TEST THIS WAS A FOREIGN BILL                 
         BNE   PBILL36                                                          
         LA    R0,AFCELQ                                                        
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),((R0),AIO3),0,0                      
         CLI   BOPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,BOPARM+12                                                     
         MVC   TPBAFCEL,0(RE)      SAVE DEBTORS AFCEL                           
         ZAP   TPBGROSS,TPBAFCEL+(AFCAMNT-AFCEL)(L'AFCAMNT)                     
         SP    TPBGROSS,TPBVAT     GROSS IS DEBTOR MINUS VAT                    
*                                                                               
PBILL36  L     R1,AIO3                                                          
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING SCIELD,R1                                                        
         XR    RF,RF                                                            
PBILL38  CLI   SCIEL,0             ADD -DISCOUNT TO GROSS                       
         BE    PBILL42                                                          
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCDSC                                                 
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   R1,RF,PBILL38                                                    
         AP    TPBGROSS,SCIAMNT                                                 
*                                                                               
         ZAP   EXDUB1,SCIAMNT                                                   
         ZAP   EXDUB2,SCIAMNT                                                   
         CLI   TPBAFCEL,AFCELQ                                                  
         BNE   PBILL40                                                          
         DROP  R1                                                               
*&&UK                                                                           
         LA    RF,BOWORK1                                                       
         USING EURKBLKD,RF                                                      
         XC    0(EURKBLKL,RF),0(RF)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,TPBAFCEL+(AFCCURR-AFCELD)                               
         MVC   EURKRULE,TPBAFCEL+(AFCX-AFCELD)                                  
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),EXDUB1,EXDUB2                  
         DROP  RF                                                               
*&&                                                                             
PBILL40  AP    TPBGROSS,EXDUB2     GROSS NOW INCLUDES DISCOUNT                  
*                                                                               
PBILL42  ZAP   TPBTOTAL,TPBGROSS                                                
         AP    TPBTOTAL,TPBVAT                                                  
*                                                                               
         MVC   PBNUM,TRNKREF                                                    
         GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(17,PBDATE)                          
         LA    RF,CSCURCPY                                                      
         CLI   TPBAFCEL,AFCELQ                                                  
         BNE   *+8                                                              
         LA    RF,CSCURBIL                                                      
         GOTO1 AFMTTOT,BOPARM,TPBGROSS,(RF),PBILL,                     *        
               (L'PBGROSS,PBGROSS)                                              
         GOTO1 (RF),(R1),TPBVAT,,,(L'PBVAT,PBVAT)                               
         XR    R0,R0                                                            
         TM    RPTINDS,RPTIBAGY    TEST BILLING IN AGENCY CURRENCY              
         BO    *+8                                                              
         LA    R0,X'80'            NO - OUTPUT CURRENCY PREFIX                  
         GOTO1 (RF),(R1),((R0),TPBTOTAL),,,(L'PBTOTAL,PBTOTAL)                  
*                                                                               
         ICM   R1,15,AFMTLIN       ENSURE LIST KEPT ON 1 PAGE                   
         BZ    PBILL46               IF LESS THAN 12 LINES WORTH                
         GOTO1 AFMTPRT                                                          
         LA    RE,REPPC                                                         
         CR    R1,RE                                                            
         BL    PBILL48                                                          
         XC    AFMTLIN,AFMTLIN                                                  
         LA    R4,REPP1                                                         
PBILL46  GOTO1 AREPORT                                                          
*                                                                               
PBILL48  B     PBILL02                                                          
         DROP  R2                                                               
*                                                                               
PBILL50  TM    PRVINDS,PRVIPRT                                                  
         BZ    PBILLX                                                           
         OC    AFMTLIN,AFMTLIN                                                  
         BZ    PBILL52                                                          
         GOTO1 AREPORT                                                          
PBILL52  GOTO1 AREPORT                                                          
PBILLX   B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
*  - INITIALIZE PRINTING                                              *         
***********************************************************************         
         SPACE 1                                                                
PREVINIT NTR1  ,                                                                
         MVC   PBHEAD,AC@PRVBS                                                  
         GOTO1 AFMTPRT                                                          
         MVC   PBHEAD,AC$PRVBS                                                  
         GOTO1 AFMTPRT                                                          
         MVC   PBHNUM,AC@BILC                                                   
         MVC   PBHDATE,AC@BILDT                                                 
         MVC   PBHGROSS,AC@GROSS                                                
         MVC   PBHVAT,AC@VAT2                                                   
         MVC   PBHTOTAL,AC@TOTAL                                                
         GOTO1 AFMTPRT                                                          
         MVC   PBHNUM,AC$BILC                                                   
         MVC   PBHDATE,AC$BILDT                                                 
         MVC   PBHGROSS,AC$GROSS                                                
         MVC   PBHVAT,AC$VAT2                                                   
         MVC   PBHTOTAL,AC$TOTAL                                                
         GOTO1 AFMTPRT                                                          
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
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT AGENCY TO BILLING CURRENCY                       *         
*                                                                     *         
* P1=A(PL8 AGENCY CURRENCY INPUT)                                     *         
* P2=A(PL8 BILLING CURRENCY OUTPUT)                                   *         
***********************************************************************         
         SPACE 1                                                                
EXCHANGE DS    0H                                                               
         USING *,R8                                                             
*&&UK                                                                           
         LM    R2,R3,0(R1)                                                      
         ZAP   EXDUB1,0(8,R2)                                                   
         LA    RF,BOWORK1                                                       
         USING EURKBLKD,RF                                                      
         XC    0(EURKBLKL,RF),0(RF)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,BLHRVAL                                                 
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),EXDUB1,EXDUB2                  
         DROP  RF                                                               
         ZAP   0(8,R3),EXDUB2                                                   
*&&                                                                             
         B     EXIT                                                             
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
FMTTOT   DS    0H                                                               
         USING *,R8                                                             
         LR    R4,R1               R4=A(PARAMETER LIST)                         
*                                                                               
         LM    R2,R3,0(R4)                                                      
*&&UK                                                                           
         LA    RF,BOWORK1                                                       
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         BNE   *+16                                                             
         TM    0(R4),X'80'         TEST PRINTING CURRNECY CODE                  
         BNO   *+8                                                              
         LA    RF,5(RF)            SHIFT OUTPUT AREA                            
         CURED (P8,(R2)),(30,(RF)),(R3),COMMAS=YES,MINUS=YES,          *        
               ALIGN=LEFT                                                       
         ORG   *-2                                                              
         CLI   CUCTRY,CTRYGER                                                   
         BE    *+16                                                             
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         NI    0(R1),FF-X'04'      CURSYMB=YES (NOT NO)                         
         BASR  RE,RF                                                            
         CLI   CUCTRY,CTRYGER                                                   
         BNE   FTOT01                                                           
         TM    0(R4),X'80'                                                      
         BZ    FTOT01                                                           
         MVC   BOWORK1(3),0(R3)    PUT CURRENCY SYMBOL IN PLACE                 
         MVC   BOWORK1+3(2),BCSPACES                                            
         AH    R0,=Y(5)                                                         
FTOT01   EQU   *                                                                
*&&                                                                             
*&&US                                                                           
         ZAP   BODUB4,0(8,R2)                                                   
         TM    0(R4),X'80'         TEST CURRENCY SYMBOL REQUIRED                
         BO    FTOT03                                                           
         EDIT  (P8,BODUB4),(16,BOWORK1),2,COMMAS=YES,MINUS=YES,        *        
               ALIGN=LEFT                                                       
         B     FTOT04                                                           
FTOT03   EDIT  (P8,BODUB4),(16,BOWORK1),2,COMMAS=YES,MINUS=YES,        *        
               ALIGN=LEFT,FLOAT=$                                               
*&&                                                                             
*                                                                               
FTOT04   LA    RF,BOWORK1                                                       
         AR    RF,R0               RF=END OF NO. +1                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         CP    0(8,R2),BCPZERO     TEST NUMBER IS POSITIVE                      
         BM    *+8                                                              
         LA    RF,1(RF)            YES - ADD ONE TO LENGTH                      
*                                                                               
         CLC   0(L'CURTCUR,R3),=C'TRL'      TEST TURKISH LIRA                   
         BNE   FTOT05                                                           
         TM    0(R4),X'80'         TEST CURRENCY SYMBOL REQUIRED                
         BZ    FTOT05                                                           
         MVC   1(1,RF),0(RF)       INSERT "K" FOR 1000S                         
         MVI   0(RF),C'K'          ("999K " / "999K-")                          
         LA    RF,1(RF)                                                         
*                                                                               
FTOT05   LA    RE,BOWORK1                                                       
         SR    RF,RE               RF=EXECUTABLE LENGTH OF NUMBER               
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,12(R4)         RE=L(FIELD)                                  
         BZ    FTOT06                                                           
         L     R1,12(R4)           R1=A(FIELD)                                  
         B     FTOT08                                                           
FTOT06   L     R2,12(R4)                                                        
         USING BLFELD,R2                                                        
         IC    RE,BLFCOLF          RE=L(FIELD)                                  
         XR    R1,R1                                                            
         IC    R1,BLFCOLN                                                       
         A     R1,8(R4)                                                         
         BCTR  R1,0                R1=A(FIELD)                                  
         DROP  R2                                                               
*                                                                               
FTOT08   AR    R1,RE                                                            
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
FMTPRT   DS    0H                                                               
         USING *,R8                                                             
         ICM   RF,15,AFMTLIN                                                    
         BNZ   *+8                                                              
         LA    RF,REPP1                                                         
         MVC   0(L'REPP1,RF),FMTLINE                                            
         LA    RF,L'REPP1(RF)                                                   
         ST    RF,AFMTLIN                                                       
         MVC   FMTLINE,BCSPACES                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL VREPORT, CONVERTING TO UPPER CASE IF NECC.          *         
***********************************************************************         
         SPACE 1                                                                
REPORT   DS    0H                                                               
         USING *,R8                                                             
         XC    REPAUSR,REPAUSR                                                  
         LA    RF,TXTTITLE         SET TITLE HOOK ROUTINE                       
         ST    RF,REPAUSR                                                       
         CLI   REPACTN,REPAPUT                                                  
         BNE   REPORT02                                                         
         TM    BOFINDS2,BOFIUPCA                                                
         BZ    REPORT02                                                         
         GOTO1 AUPPER,BOPARM,(REPPRNTN,REPP1)                                   
REPORT02 GOTO1 VREPORT,REPD                                                     
*                                                                               
REPORTX  B     EXIT                                                             
*                                                                               
*                                  PRINT TEXT BLOCK TITLE ON EACH PAGE          
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
         MVC   TXCUCTRY,CUCTRY     PASS COUNTRY CODE TO GETTXT                  
         MVC   TXEXCHNG,BLHRATE                                                 
         GOTO1 ACGETTXT,BOPARM,(CSFMLANG,TXBLOCK)                               
         TM    BOFINDS2,BOFIUPCA   CONVERT TO UPPER CASE IF NECC.               
         BZ    EXIT                                                             
         GOTO1 AUPPER,BOPARM,(REPHEADN,REPH1)                                   
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT PRINT LINES TO UPPER CASE ONLY                   *         
*                                                                     *         
* NTRY: P1 BYTE 0 = NUMBER OF PRINT LINES                             *         
*             1-3 = A(1ST PRINT LINE)                                 *         
***********************************************************************         
         SPACE 1                                                                
UPPER    DS    0H                                                               
         USING *,R8                                                             
         XR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
UPPER02  TR    0(L'REPP1,R2),UPPTAB                                             
         LA    R2,L'REPP1(R2)                                                   
         BCT   R0,UPPER02                                                       
         B     EXIT                                                             
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
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
TOTDSCLQ EQU   36                  LENGTH FOR TOTAL DESCRIPTIONS (ISH)          
DUB      EQU   BODUB1                                                           
DMCB     EQU   BODMCB                                                           
WORK     EQU   BOWORK1                                                          
BYTE     EQU   BOBYTE1                                                          
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
DQU      DC    CL(L'BASSRV)'=DQU'                                               
ACCMST   DC    C'ACCMST '                                                       
WC99     DC    C'99'                                                            
*                                                                               
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
         DS    (LTORGX-*)X                                                      
         ORG                                                                    
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
       ++INCLUDE ACCLBWORKB                                                     
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
PARATOT  DS    A                                                                
PARABLFS DS    10A                 LIST OF A(AMOUNT BLFEL ELEMENTS)             
PARABLFN DS    XL1                 NUMBER OF PARABLFS                           
PARMAXT  DS    XL1                 MAXIMUM LINE NO. FROM TOP                    
PARMAXB  DS    XL1                 MAXIMUM LINE NO. FROM BOTTOM                 
PARCOLF  DS    XL1                 LEFT-MOST COLUMN                             
PARINDS  DS    XL1                 INDICATORS BYTE                              
PARIGRS  EQU   X'80'               PARAGRAPH HAS A GROSS COLUMN                 
PARLMNPR DS    XL1                 LEFT-MOST NON-PRINT COLUMN                   
*                                                                               
PARPAGH1 DS    CL(L'REPP1)         PAGE HEADLINE 1                              
PARPAGH2 DS    CL(L'REPP1)         PAGE HEADLINE 2                              
PARPAGH3 DS    CL(L'REPP1)         PAGE HEADLINE 3                              
PARPAGH4 DS    CL(L'REPP1)         PAGE HEADLINE 4                              
PARPAGNQ EQU   4                                                                
*                                                                               
PARPARH1 DS    CL(L'REPP1)         PARA HEADLINE 1                              
PARPARH2 DS    CL(L'REPP1)         PARA HEADLINE 2                              
PARPARH3 DS    CL(L'REPP1)         PARA HEADLINE 3                              
PARPARH4 DS    CL(L'REPP1)         PARA HEADLINE 4                              
PARPARH5 DS    CL(L'REPP1)         PARA HEADLINE 5                              
PARPARH6 DS    CL(L'REPP1)         PARA HEADLINE 6                              
PARPARH7 DS    CL(L'REPP1)         PARA HEADLINE 7                              
PARPARH8 DS    CL(L'REPP1)         PARA HEADLINE 8                              
PARPARH9 DS    CL(L'REPP1)         PARA HEADLINE 9                              
PARPARHA DS    CL(L'REPP1)         PARA HEADLINE A                              
PARPARNQ EQU   10                                                               
*                                                                               
         DS    0F                  ALIGN ON A FULL WORD                         
*                                                                               
PARL     EQU   *-PARD                                                           
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
PWORKD   DSECT                                                                  
ACGETTXT DS    A                                                                
APBLOCK  DS    A                                                                
AREROUT  DS    0A                                                               
ABILLINI DS    A                                                                
ATXTINIT DS    A                                                                
ATXTHEAD DS    A                                                                
ATXTFOOT DS    A                                                                
ABILLPRT DS    A                                                                
ABILLPAR DS    A                                                                
ABILLSUB DS    A                                                                
ABILLPRE DS    A                                                                
ABILLTOT DS    A                                                                
ABILLVAT DS    A                                                                
APREVBIL DS    A                                                                
AEXCHANG DS    A                                                                
AFMTTOT  DS    A                                                                
AFMTPRT  DS    A                                                                
AREPORT  DS    A                                                                
AUPPER   DS    A                                                                
AREROUTN EQU   (*-AREROUT)/L'AREROUT                                            
         DS    (AROUTN-AREROUTN)X ENSURE AROUTN=AREROUTN                        
         DS    (AREROUTN-AROUTN)X                                               
*                                                                               
ATXBLOCK DS    A                                                                
AFMTLIN  DS    A                                                                
APAR     DS    A                                                                
RELO     DS    A                                                                
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
PVATT    DS    PL8                 VAT TOTAL                                    
PVATTF   DS    PL8                 VAT TOTAL IN FOREIGN CURRENCY                
PBILDAT  DS    XL2                 BILL DATE                                    
PDUEDAT  DS    XL2                 DUE DATE                                     
*                                                                               
PCURBIL  DS    XL(L'CSCURBIL)      BILLING CURRENCY TABLE ENTRY                 
PBLHEL   DS    XL(BLHLNQ)          SAVED BILL HEADER ELEMENT                    
TOTLBLF  DS    XL(BLFLNQ)          SAVED RIGHTMOST BLFEL                        
*                                                                               
PTOP#    DS    XL1                 PARAGRAPH LINE NUMBER FROM THE TOP           
PBOT#    DS    XL1                 PARAGRAPH LINE NUMBER FROM BOTTOM            
*                                                                               
PTOTDISQ DS    H                                                                
PTOTEXCQ DS    H                                                                
*                                                                               
FMTLINE  DS    CL132                                                            
*                                                                               
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
TDEBACC  DS    CL14                DEBTORS ACCOUNT FOR ADDR                     
*                                                                               
TPREVGRS DS    PL8                 PREVIOUS GROSS BILLING                       
TPREVNET DS    PL8                 PREVIOUS NET BILLING                         
TPREVCOM DS    PL8                 PREVIOUS COMMISSION BILLED                   
*                                                                               
PRVINDS  DS    XL1                 PREVBILL INDICATORS                          
PRVIPRT  EQU   X'80'               1ST PREVBILL HAS BEEN PRINTED                
PRVKEY   DS    XL42                SAVED BILL KEY                               
PRVMDESC DS    CL12                MEDIA DESCRIPTION                            
PRVDATE  DS    PL3                 CURRENT BILL DATE                            
TPBGROSS DS    PL8                 PREVIOUS BILL GROSS AMOUNT                   
TPBVAT   DS    PL8                 PREVIOUS BILL VAT AMOUNT                     
TPBTOTAL DS    PL8                 PREVIOUS BILL TOTAL AMOUNT                   
TPBAFCEL DS    XL(AFCLNQ)                                                       
*                                                                               
DICO     DS    0X                  * DICTIONARY OUT *                           
         DSDDL                                                                  
DICOX    DS    0X                                                               
*                                                                               
FFMTBLK  DS    XL(L'FBLK)                                                       
*                                                                               
TIMEPAR  DS    XL(PARL)                                                         
*                                                                               
         DS    (OVERWRKL-(*-PWORKD))X                                           
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057ACCLB0BB  12/22/99'                                      
         END                                                                    
